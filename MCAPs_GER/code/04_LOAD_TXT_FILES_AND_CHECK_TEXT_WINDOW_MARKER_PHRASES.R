##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##START <<<<<<<<<<<<<<<<<<< LOAD TXT FILES AND CHECK FOR MARKER PHRASES FOR BEGINNING/END OF TEXT WINDOWS
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
wd.current <- getwd()

##+ READ IN TEXTFILES---------------------------------------
setwd(wd.source)

#list of txt files in folder
files <- list.files(pattern="*.txt")

#initialize list to work with
workingdata.list <- vector(mode = "list", length = length(files))

for (f in 1:length(workingdata.list)) {
  
  text <- readLines(files[f], encoding = "UTF-8")
  filename <- files[f]
  names(workingdata.list)[f] <- filename
  
  text <- list(text)  
  
  workingdata.list[[f]] <- text
  names(workingdata.list[[f]]) <- filename
}
##~----------------------------------------


##+ IDENTIFY EXTRACTION PATTERNS -------------------------
#on basis of manually defined repeating marker phrases for beginning/end of the windows
#suitable strings are defined manually on basis of number of occurences of potential extraction words 
#and total number of known windows (from manual screening of text)

for (t in 1:length(workingdata.list)) {
  
  text <- workingdata.list[[t]][[1]]
  
  ##+ READ ANALYSIS CODE AT FILE HEAD
  processing.code.start <- grep("!Analysis_Code_Start!",text)
  processing.code.end <- grep("!Analysis_Code_End!",text)
  code <- text[processing.code.start:processing.code.end]
  
  startpage <- code[grep("pagerange",code)+1]
  endpage <- code[grep("pagerange",code)+2]
  
  #read potential words for marking text windows
  #that were guessed by looking into the text and 
  #inserted manually in the analysis code at the file head)
  split.phrases.guessed.range <- grep("desired_pattern_end",code)-grep("desired_pattern_start",code)-1
  split.phrases.guessed <- code[grep("desired_pattern_start",code)+rep(1:split.phrases.guessed.range)]  
  ##~
  
  
  ##+ CHECK AVAILABILITY OF PATTERNS IN TEXT 
  text <- text[-c(processing.code.start:processing.code.end)]  
  
  #select the desired page range
  text <- text[grep(startpage,text):grep(endpage,text)]
  
  #check the number of occurence of potential pattern extraction words 
  #and build a matrix: word : number of occurences  
  split.phrases.guessed.availability <- c(sapply(1:length(split.phrases.guessed), function(i) {
    
    sum(grepl(split.phrases.guessed[i], text))
    
  }))
  
  split.pattern.guessed.summary <- cbind(split.phrases.guessed, split.phrases.guessed.availability)
  split.pattern.guessed.summary <- list(split.pattern.guessed.summary)
  names(split.pattern.guessed.summary) <- paste("split_pattern_guessed_summary", names(workingdata.list[t]), sep="")
  workingdata.list[[t]] <- c(workingdata.list[[t]],split.pattern.guessed.summary)
  ##~
  
}

##+Display the availability of patterns
#for post-processing of the pattern matching and extraction

sapply(c(1:length(workingdata.list)),  function(i) {
  workingdata.list[[i]][2]
})

warning("Please identify suitable extraction patterns manually 
        and store them in a txt file in wd.interim in the following format,
        availability of split patterns is displayed in ABOVE LINES: 
        'c(10,22)  | name_of_document_to_be_analyzed.txt'")
#check the availability of patterns manually and select suitable start and end strings for the extraction
#the identified pattern of two stringsper document have to be written manually into the following text file:
#wd.interim/identified_extraction_patterns.txt
#the format has to be as follows:
#       line number of start/end string  |  file_name
#       c(10,22)  | z_layout_GER_Hildesheim_CPC1_main_region_catalogue_of_measures_UTF8.txt

##~-------------------------
time.elapsed <- rbind(time.elapsed, proc.time())
row.names(time.elapsed)[nrow(time.elapsed)] <- "LOAD_TXT_AND_CHECK_MARKER_PHRASES"
time.elapsed
setwd(wd.current)

##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##END <<<<<<<<<<<<<<<<<<< LOAD TXT FILES AND CHECK FOR MARKER PHRASES FOR BEGINNING/END OF TEXT WINDOWS
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

