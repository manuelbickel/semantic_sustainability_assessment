
#Set main working directory, filename extenstions, etc.
wd.main <- c("M:/Science/Programmierung/Github/semantic_sustainability_assessment/")

file.encoding.table.win <- "tm_win1252_misinterpretation_encoding_table_UTF8.txt"
file.encoding.punctuation  <- "punct_meta_ANSI.txt"

file.words.not.matched <- "words_not_matched.txt"

file.empty.processing.code  <- "code_structure_for_setting_text_processing_markers.txt"

file.split.pattern.identified <- "identified_extraction_patterns.txt"

wordlists.prefix <- c("uniq.stem_")

occurrence.filename.tag <- c("__w_occ_mat__MR")
cooccurrence.filename.tag <- c("__cat_coocc_mat")

filenamebeginning.cooccurrence.matrix.mean <- "GER__Lower_Saxony_regional_centers_mean"

######  README  **************************************************************************************************

#This code assumes a semi-automatic approach and has to be run stepwise if other documents than the ones for
#the study of municipal climate action plans of Lower Saxony shall be investigated
#for this study the code and data are prepared already and the code can be run entirely.
#the manual action that would be required for other data sets is described in short below

#MANUAL ACTION
#Manual Action 1
#the documents to be analyzed have to be converted to text files (via tesseract and xpdf) 
#and have to be manipulated by including a predefined analysis code at the file head (see separate R code)
#this analysis code has to be adapted manually according to content of the txt file (semi-automatic approach)
#assumptions for filling the code:
#page range to be analyzed has to be set (by copying the start marker/ end marker words (might be introduced manually into the text) at the respective location of the text into the code)
#single repeating lines containing a certain string (e.g. footnotes) can be deleted (if desired)
#page breaks are marked via: \f (via xpdf)
#range to be analyzed should be defined in a way so that it inlcudes an \f in the beginning and at the end

#Manual Action  2
#before running the section "##+ IDENTIFY EXTRACTION PATTERNS"
#potential repeating marker words to define text windows (beginning and end phrase) have to be guessed
#these are then tested in that section automatically
#the user will then have to decide if the marker phrases are suitable or not
#in the positive case these marker phrases have to be documented manually in the file
#wd.interim/identified_extraction_patterns.txt

#Manual Action 3
#in a separate step which may be supported by computer programs the nouns (or other words) of the
#text windows have to be tagged; also stopwords may be defined
#these word lists have to be stored in the respective folders to be read into R



#FURTHER NOTES

#the internal R workingdata.list structure which is used to handle the files is as follows
#(check e.g. str(workingdata.list) or text <- workingdata.list[[24]][[2]] after creating the list)
#name
#[[ENT]]             list ENTity (=document name)
#[[ENT]][[1-WT]]     Dummy sentence (before: Whole Text of the document including code at filehead)
#[[ENT]][[2-AEXP]]   Available EXtraction Pattern in the complete text, matrix
#[[ENT]][[3-MEAS]]   separated single measures (including linebreaks marked with "~~collapse~~")
#[[ENT]][[4-UnWinM]] Unique Words in the single Measures, words which exist in the available wordlists have an E~X~T at the beginning


#SYNTAX NOTES
# : comments...

##+ DESCRIPTION TEXT : marks beginning of a short code block
##~ : marks the end of a short code block

##+ DESCRIPTION TEXT -------------- : marks the end of a longer code block (may be collapsed, e.g. in R Studio)
##~-------------- : marks the end of a longer code block (may be collapsed, e.g. in R Studio)

##START <<<<<<<<<<<<< description text  : marks the beginning of a very large code block
##END <<<<<<<<<<<<<   : marks the end of a very large code block

#variables are named in lowercase letters only, separating symbol is a dot, e.g. variable.new
#Functions start with an uppercase letter, separating symboal is a hyphen, e.g. Function_new()

#ADDITONAL NOTES
#warnings concerning incomplete final lines may be neglected

#**************************************************************************************************


##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##START <<<<<<<<<<<<< LOAD FUNCTIONS AND DEFINITIONS (incl. directories, libraries, etc.)
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

##+ SET WORKING DIRECTORIES------------------------------------
  #in case the final slash was "forgotten", the final slash is assumed to exist in the following
  if (substr(wd.main, nchar(wd.main), nchar(wd.main)) != "/") {
    wd.main <- paste0(wd.main, "/")
  }
  
  #subdirectories are set automatically on basis of main directory
  wd.source <- paste0(wd.main, "data/text_files/")
  wd.interim <- paste0(wd.main, "data/results_interim/")
  wd.final <- paste0(wd.main, "data/results_final/")
  wd.wordlists <- paste0(wd.main, "thesauri_wordlists/thesauri/")
  wd.stopwords <- paste0(wd.main, "thesauri_wordlists/stopwords/")
  wd.encoding <- paste0(wd.main, "encoding/")
  wd.code <- paste0(wd.main, "code/")
  
  #directory for storing words that are not matched by the wordlists
  wd.notmatched <- paste0(wd.main, "thesauri_wordlists/words_not_matched/")
    #initialize an empty txt.file if there is no file, yet
    if (c(file.words.not.matched) %in% list.files(path = wd.notmatched) == FALSE) {
      writeLines(c(""), paste0(wd.notmatched, file.words.not.matched))
}

##~----------------------------------------------------------------------

##+ LOAD LIBRARIES; SET OPTIONS; INITIAL TIME COUNTER------------------------
##+ INITIAL TIME COUNTER
  time.elapsed <- rbind(c(), proc.time())
  row.names(time.elapsed)[nrow(time.elapsed)] <- "start"
##~

##+ SET R-OPTIONS
  options("nwarnings" = 150)
##~

##+ LOAD LIBRARIES
  library("SnowballC")
  library("plyr")
  library("ggplot2")
  library("reshape2")
  library("grid")
##+~

##~-------------------------------------------------------------------

##+ DEFINITION OF FUNCTIONS------------------------------------------------------------

##+ CHRACTERS TO BE REPLACED BY BASIC LATIN LETTERS
  special.german.characters <- c("ä", "Ä", "ö", "Ö", "ü", "Ü", "ß")
##~


##+ READ CHARACTER ENCODING LIST
  setwd(wd.encoding)
  options(encoding = "native.enc")
  encoding.table.win <- read.table(file.encoding.table.win, 
                                 header = TRUE, 
                                 sep=";", 
                                 colClasses = "character",
                                 encoding="UTF-8")
  
  #manual corrections; misinterpreted characters for space might not be read in correctly on some machines 
  encoding.table.win[121,] <- gsub("space", " ", encoding.table.win[121,])
  encoding.table.win[121,4] <- c("S~P~A~C~E")
##~


##+ READING LIST OF PUNCTUACTION CHARACTERS (supplements [[:punct:]])
  encoding.setting.stored <- getOption("encoding")
  options(encoding = "native.enc")
  
  suppressWarnings(punct <- readLines(paste0(wd.encoding, file.encoding.punctuation), encoding = "ANSI" ))
  punct <- strsplit(punct, "~~")
  
  #generate vector format from list
  left <- c(1:length(punct))
  
  left <- sapply(left, function(x) {
    punct[[x]][[1]]  
  })
  
  punct <- as.character(left)
  options(encoding = encoding.setting.stored)
##~


##+ FUNCTION - HarmonizeMisintChars(CHARACTER VECTOR)
#correction of characters that have been mmisinterpreted during reading of text files
#on basis of the encoding table loaded above
  HarmonizeMisintChars <- function(x.vector) {
    
    for (i in (1:nrow(encoding.table.win))) {
      
      #unicode encoding, e.g.: U+203A
      x.vector <- gsub(as.character(encoding.table.win[i,1]), as.character(encoding.table.win[i,3]), x.vector,  fixed = TRUE) 
      
      #windows encoding, e.g.: 0x9B
      x.vector <- gsub(as.character(encoding.table.win[i,2]), as.character(encoding.table.win[i,3]), x.vector,  fixed = TRUE)
      
      #windows misinterpreted character symbols 
      x.vector <- gsub(as.character(encoding.table.win[i,4]), as.character(encoding.table.win[i,3]), x.vector,  fixed = TRUE) 
     
      #UTF-8 encoding, e.g.: %E2%80%BA
      x.vector <- gsub(as.character(encoding.table.win[i,5]), as.character(encoding.table.win[i,3]), x.vector,  fixed = TRUE) 
      
    }
    
    
    #correct some unicode characters, see e.g. https://en.wikipedia.org/wiki/Typographic_ligature
    x.vector <- gsub("<U+FB00>", "ff",  x.vector, fixed=TRUE)
    x.vector <- gsub("U+FB00", "ff",  x.vector, fixed=TRUE)
    
    x.vector <- gsub("<U+FB01>", "fi",  x.vector, fixed=TRUE)
    x.vector <- gsub("U+FB01", "fi",  x.vector, fixed=TRUE)
    
    x.vector <- gsub("<U+00DF>", special.german.characters[7],  x.vector, fixed=TRUE)
    x.vector <- gsub("U+00DF", special.german.characters[7],  x.vector, fixed=TRUE)
    
    x.vector <- gsub("<U+FB02>", "fl",  x.vector, fixed=TRUE)
    x.vector <- gsub("U+FB02", "fl",  x.vector, fixed=TRUE)
    
    x.vector <- gsub("<U+AB50>", "ui",  x.vector, fixed=TRUE)
    x.vector <- gsub("U+AB50", "ui",  x.vector, fixed=TRUE)
    
    x.vector <- gsub("<U+FB06>", "st",  x.vector, fixed=TRUE)
    x.vector <- gsub("U+FB06", "st",  x.vector, fixed=TRUE)
    
    x.vector <- gsub("<U+FFFD>", "st",  x.vector, fixed=TRUE)
    x.vector <- gsub("U+FFFD", "st",  x.vector, fixed=TRUE)
    
    x.vector <- gsub("<U+FB06>", "st",  x.vector, fixed=TRUE)
    x.vector <- gsub("U+FB06", "st",  x.vector, fixed=TRUE)
    
    
    #special characters which had been misinterpreted and appear in form of a replacement character
    x.vector <- gsub("<U+FB06>", "",  x.vector, fixed=TRUE)
    x.vector <- gsub("U+FB06", "",  x.vector, fixed=TRUE)
    
    x.vector <- gsub("<U+263A>", "",  x.vector, fixed=TRUE)
    x.vector <- gsub("U+263A", "",  x.vector, fixed=TRUE)
    
    x.vector <- gsub("<U+F0E0>", "",  x.vector, fixed=TRUE)
    x.vector <- gsub("U+F0E0", "",  x.vector, fixed=TRUE)
    
    return(x.vector)
  }
##~


##+ FUNCTION - CLEANx.vector(CHARACTER x.vector)
#cleaning of a x.vector from punctuation, etc.
CleanVector <- function(x.vector) {
  
  #when character x.vectors are collapsed, ~~collapse~~ is used as a marker phrase 
  #in case the information of the break point is needed at some point to restore the original format
  x.vector <- gsub("~~collapse~~", "", x.vector)
  
  x.vector <- gsub("[[:cntrl:]]", " ", x.vector)
  x.vector <- gsub("[[:blank:]]{2,}", " ", x.vector)
  
  #in German compounds divided by "und" or "oder" might share a mutual final noun and are therefore
  #combined, since their meaning cannot only be defined on basis of their first noun
  #e.g.: Energie- und Umweltmanagement - management belongs to both energy and environment
  x.vector <- gsub("(-[[:blank:]]und[[:blank:]])([[:upper:]])", "UND\\2", x.vector, perl=TRUE)
  x.vector <- gsub("(-[[:blank:]]oder[[:blank:]])([[:upper:]])", "ODER\\2", x.vector, perl=TRUE)
  
  #for parsing the x.vectors, some marker phrases were introduced 
  #that are deleted during the cleaing step, e.g. [[:blank:]]
  x.vector <-  gsub("([[:)([A-Za-z]+)(:]])", " " , x.vector, perl=TRUE)
  
  
  #replace all puncutation characters
  encoding.setting.stored <- getOption("encoding")
  options(encoding = "native.enc")

  for (i in 1:length(punct)) {
    
    x.vector <- gsub(punct[i],"",x.vector, fixed = TRUE)
  }
  options(encoding = encoding.setting.stored)
  
  x.vector <- gsub("[[:punct:]]", "", x.vector)
  
  
  #replace special.german.characters
  x.vector <- gsub(special.german.characters[1], "ae", x.vector)
  x.vector <- gsub(special.german.characters[2], "Ae", x.vector)
  x.vector <- gsub(special.german.characters[3], "oe", x.vector)
  x.vector <- gsub(special.german.characters[4], "Oe", x.vector) 
  x.vector <- gsub(special.german.characters[5], "ue", x.vector)
  x.vector <- gsub(special.german.characters[6], "Ue", x.vector)
  
  #delete part of the gender formatting of nouns
  x.vector <- gsub("/in |_in |/innen |_innen ", "", x.vector)
  
  #delete various marker phrases, numbers, etc.
  x.vector <- gsub("CO[[:blank:]]*2", "COzwei", x.vector)
  
  x.vector <- gsub("[[:digit:]]", "", x.vector)
  
  x.vector <- gsub("[[:blank:]][[:alpha:]][[:blank:]]", " ", x.vector)
 
  x.vector <- gsub("([[:blank:]]|^)[[:alpha:]]([[:blank:]]|$)", " ", x.vector)
  
  x.vector <- gsub("[[:blank:]]{2,}", " ", x.vector)
 
  x.vector <- gsub("^[[:blank:]]+|[[:blank:]]+$", "", x.vector)
  
  x.vector <- gsub("<U+FFFD>", "", x.vector, fixed=TRUE)
  
  
  #convert all words which contain at least one uppercase letter to uppercase
  #and then all uppercase words to lower case except for the first letter
  #the first letter stays as marker for an uppercase word
  #hence, a "normal" uppercase word is recognized better by the porter stemming algorithm 
  #(this step accounts for misspellings in the documents)
  x.vector <- gsub("(\\b)([\\w]*)(?=[A-Z])([\\w]*)(\\b)", "\\1\\U\\2\\U\\3\\U\\4\\5", x.vector, perl=TRUE)
  x.vector <- gsub("(\\b)([A-Z])([\\w]+)(\\b)", "\\1\\2\\L\\3\\4", x.vector, perl=TRUE)
  
  #in case the x.vector is a x.vector with several entries, empty lines are deleted
  x.vector <- x.vector[x.vector != ""]
  
  return (x.vector)
  
}
##~


##+ FUNCTION - UniqueWords(CHARACTER VECTOR)
#create a character vector of unique words
UniqueWords <- function(x.vector.or.list) {
  
  x.vector.or.list <- paste(unlist(x.vector.or.list, recursive = TRUE), collapse = " ")
  x.vector.or.list <- unlist(strsplit(x.vector.or.list, " "))
  x.vector.or.list <- sort(unique(x.vector.or.list))
  
  return (x.vector.or.list)
  
}
##~


##+ FUNCTION - LOADSTOPWOIRDLIST.TXT.CSV(STOPWORDDIR_wfinalSlash)
#loads the stopwordlists in the stopwordlist folder and creates a character vector of unique stopwords
#txt files may have comments marked via | or # (e.g. like the stopwordlist of the Porter Algorithm: http://snowballstem.org/algorithms/german/stop.txt)
#csv files are assumed to have a tabular format with a HEADER inlcuiding each category of wordtype and NO comments
#the function UniqueWords(text) is required
LoadStopwordlistTxtCsv <- function(stopworddir_wfinalSlash) {
  
  files.stopwords <- list.files(stopworddir_wfinalSlash)
  #initial the wordlist
  stopwordlist <-  c("Dummy")
  
  #for txt files
  files.txt <- files.stopwords[grep(".txt$", files.stopwords)]
  
  for (i in 1:length(files.txt)) {
    stopwords.from.txt <- readLines(paste(stopworddir_wfinalSlash, files.txt[i] ,sep=""))
    stopwords.from.txt <- gsub("\\|.*$", "", stopwords.from.txt) #in case | is used as comment character
    stopwords.from.txt <- gsub("\\#.*$", "", stopwords.from.txt) #in case # is used as comment character
    stopwords.from.txt <- gsub("^[[:blank:]]+|[[:blank:]]+$", "", stopwords.from.txt)
    stopwords.from.txt <- stopwords.from.txt[grep("[[:graph:]]", stopwords.from.txt)]
    
    stopwordlist <- c(stopwordlist, stopwords.from.txt)
  }
  
  
  #for .csv files
 files.csv <- files.stopwords[grep(".csv$", files.stopwords)]
  
  for (i in 1:length(csvfiles)) {
     stopwords.from.csv <- read.csv(paste(stopworddir_wfinalSlash,files.csv[i] ,sep=""), header = TRUE, sep=";")
     stopwords.from.csv <- UniqueWords( stopwords.from.csv)
  
     stopwords.from.csv <- gsub("^[[:blank:]]+|[[:blank:]]+$", "",  stopwords.from.csv)
     stopwords.from.csv <-  stopwords.from.csv[grep("[[:graph:]]",  stopwords.from.csv)]
    
    stopwordlist <- c(stopwordlist,  stopwords.from.csv)
  }
  
  stopwordlist <- unique(stopwordlist)
  stopwordlist <- stopwordlist[grep("[[:graph:]]", stopwordlist)]
  
  return(stopwordlist)
}
##~


##FUNCTION - StemVector(CHARACTER VECTOR)
#applies the porter stemming algorithm with some adaptions for better results in German
#keeps the original format of a vector in case of multiple entries (lines)
StemVector <- function(x.vector) {
  
  x.vector <- paste(x.vector, collapse=" ~c~ ")
  x.vector <- unlist(strsplit(x.vector, " "))
  
  #mark the uppercase words before stemming as some (like Umwelt) are stemmed to lower case
  #and have to be reconverted to upper case after stemming
  
  x.vector <- gsub(paste("(\\b)([A-Z|", special.german.characters[2], "|", special.german.characters[6],"|",special.german.characters[4],"])", sep=""), "X~\\2" , x.vector, perl=TRUE)
  x.vector <- wordStem(x.vector, language = "german")
  
  x.vector <- gsub("(X~)([\\w])", "\\U\\2" , x.vector, perl=TRUE)
  x.vector <- paste(x.vector, collapse=" ")
  x.vector <- unlist(strsplit(x.vector, " ~c~ "))
  
  return (x.vector)
  
}
##~

##~--------------------------------------------

##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##END <<<<<<<<<<<<<<<<<<<
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
time.elapsed <- rbind(time.elapsed, proc.time())
row.names(time.elapsed)[nrow(time.elapsed)] <- "initial_definitions"



##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##START <<<<<<<<<<<<<<<<<<< WRITE EMPTY CODE STRUCTURE FOR TEXT PROCESSING AT FILE HEADS
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##+ WRITE EMPTY CODE STRUCTURE...----------------------------------
empty.processing.code <- readLines(paste0(wd.code, file.empty.processing.code), 
                                   encoding = "UTF-8")

files <- list.files(wd.source, pattern="*.txt")

for (i in files) {
  
  text <- readLines(i, encoding = "UTF-8")
  
  if (length(c(grep("!Analysis_Code_Start!", text), grep("!Analysis_Code_End!", text))) == 2) {
    
    #do nothing, code structure already available in text file
    
  } else if (length(c(grep("!Analysis_Code_Start!", text), grep("!Analysis_Code_End!", text))) == 0) {
    
    text <- c(empty.processing.code, " ", text)
    
    #to suppress any encoding issues and keep UTF-8 useBytes = TRUE is set
    writeLines(text, i, useBytes = TRUE)
    
  } else {
    
  warning("ERROR: text contains the wording !Analysis_Code_Start! or !Analysis_Code_End! which is used by the code as marker phrase Please delete or change this phrase in the text. Another reason for the error could be that the code at the file head was corrupted and is missing partly. Please check.")
  
  }
  
}
##~--------------------------------
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##END <<<<<<<<<<<<<<<<<<< 
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

####MANUAL ACTION  REQUIRED<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##START <<<<<<<<<<<<<<<<<<< IDENTIFY EXTRACTION PATTERNS FOR TEXT WINDOWS AND SPLIT TEXTS
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

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

  print("Please identify suitable extraction patterns manually and store them in a txt file in wd.interim in the format: 'c(10,22)  | name_of_document_to_be_analyzed.txt'")
    #check the availability of patterns manually and select suitable start and end strings for the extraction
  #the identified pattern of two stringsper document have to be written manually into the following text file:
  #wd.interim/identified_extraction_patterns.txt
  #the format has to be as follows:
  #       line number of start/end string  |  file_name
  #       c(10,22)  | z_layout_GER_Hildesheim_CPC1_main_region_catalogue_of_measures_UTF8.txt

##~-------------------------

    
##+ APPEND EXTRACTION PATTERNS IN FILEHEAD-------------------------
split.pattern.identified <-  readLines(paste(wd.interim, file.split.pattern.identified, sep=""))

#clean text from comments
split.pattern.identified <- gsub("#.*$", "", split.pattern.identified)
split.pattern.identified <- split.pattern.identified[split.pattern.identified != ""]

split.pattern.identified <- strsplit(split.pattern.identified, "\\|")
split.pattern.identified <- lapply(split.pattern.identified, function(x) {
                                      gsub("[[:blank:]]+$|^[[:blank:]]+","", x)})


for (i in 1:length(split.pattern.identified )) {#loop through the split.pattern.identified to write them in the fileheads
  
  #check which identified pattern refers to which workingdata.list element
  #and if a suitable element exists at all
  #(e.g. in case the order of documents changed or a additional document are stored in the folder)

  split.pattern.case <- eval(parse(text=split.pattern.identified[[i]][[1]]))

  pattern.id.in.workingdata.list <- grep(split.pattern.identified[[i]][[2]], names(workingdata.list), fixed = TRUE)

 
  if (sum(pattern.id.in.workingdata.list) > 0) { #check if the filename of the stored pattern matches the one in the workingdata.list, if yes, insert extraction pattern
  
    split.pattern.startstring <- workingdata.list[[pattern.id.in.workingdata.list]][[2]][[split.pattern.case[1],1]]
 
    workingdata.list[[pattern.id.in.workingdata.list]][[1]] <- append(workingdata.list[[pattern.id.in.workingdata.list]][[1]], split.pattern.startstring, after = grep("startextract \\|", workingdata.list[[pattern.id.in.workingdata.list]][[1]]))
  
    split.pattern.endstring <- workingdata.list[[pattern.id.in.workingdata.list]][[2]][[split.pattern.case[2],1]]

    workingdata.list[[pattern.id.in.workingdata.list]][[1]] <- append(workingdata.list[[pattern.id.in.workingdata.list]][[1]], split.pattern.endstring, after = grep("endextract \\|", workingdata.list[[pattern.id.in.workingdata.list]][[1]]))
    
    
  } else { #in case the filename of the extraction pattern is not matched in the workingdata.list
    print(paste("Not matched in workingdata.list:", split.pattern.identified[[i]][[2]]))
  }
}

##~-------------------------

##+ SPLIT TEXTS INTO TEXT WINDOWS-------------------

for (t in 1:length(workingdata.list)) {
  
  text <- workingdata.list[[t]][[1]]
  
  #read analysis code at filehead
  #it has been changed manually in the texts and is therefore read in again
  processing.code.start <- grep("!Analysis_Code_Start!",text)
  processing.code.end <- grep("!Analysis_Code_End!",text)
  code <- text[processing.code.start:processing.code.end]
  
  text <- text[-c(processing.code.start:processing.code.end)]
  
  #reading in the pages to be analyzed
  startpage <- code[grep("pagerange",code)+1]
  endpage <- code[grep("pagerange",code)+2]
  
  ##+ CHECK FOR MARKER WORDS F~A~L~S~E AND T~R~U~E
    #the words F~A~L~S~E and T~R~U~E are used in the code at some parts instead of the boolean TRUE/FALSE
    #as the latter might in some steps (e.g. by gsub) be converted to character and not fulfill their purpose
    #in order to avoid mistakes in processing the text, it may not contain these words, this is checked
    if (sum(grepl("F~A~L~S~E", text)) > 0) {
      text <- gsub("F~A~L~S~E", "FALSE", text)
      print(paste("The string F~A~L~S~E was replaced by FALSE in: ", names(workingdata.list)[t], sep=""))
    } 
    
    if (sum(grepl("T~R~U~E", text)) > TRUE) {
      text <- gsub("T~R~U~E", "TRUE", text)
      print(paste("The string T~R~U~E was replaced by TRUE in: ", names(workingdata.list)[t], sep=""))
    } 
  ##~
  
  
  ##+ CLEAN TEXT FROM FOOTNOTES; ETC:
  #manually identified single lines
    if (grep("single_line_deletion_end",code)-grep("single_line_deletion_start",code)-1 >= 1) {
      
      delete.single.lines.range <- grep("single_line_deletion_end",code)-grep("single_line_deletion_start",code)-1
      delete.single.lines <- code[grep("single_line_deletion_start",code)+rep(1:delete.single.lines.range)]
      
    } else {
      delete.single.lines <- "F~A~L~S~E"
    }
  ##~
  
  ##+ READ EXTRACTION PATTERN FROM ANALYSIS CODE AT FILE HEAD
    #if single string pattern (FROM X TO X TO X TO END) option is set in the first line of extraction pattern take this into account
    #or otherwise use two (or more) extraction words
    if (grepl("S~I~N~G~L~E~S~T~R~I~N~G~P~A~T~T~E~R~N:", code[(grep("startextract",code)+1)]) == TRUE) { 
      
      split.start <- gsub("S~I~N~G~L~E~S~T~R~I~N~G~P~A~T~T~E~R~N:", "", code[(grep("startextract",code)+1)] )
      split.end <- "F~A~L~S~E"
      
      delete.single.string.switch <- "T~R~U~E"
      
    } else {
      
      #defining the numbers of words and lines for extraction / deletion
      split.range <- grep("endextract",code)-grep("startextract",code)-1
      
      #defining end/start words based on above ranges to be extracted or deleted - stored in vectors
      split.start <- code[grep("startextract",code)+rep(1:split.range)]
      split.end <- code[grep("endextract",code)+rep(1:split.range)]
      
      delete.single.string.switch <- "F~A~L~S~E"
    }
 
    #clean the text from words of the analysis code
    split.start <- gsub("!~~.*~~!", "", split.start) 
    split.end <- gsub("!~~.*~~!", "", split.end)
    delete.single.lines <- gsub("!~~.*~~!", "", delete.single.lines)
  ##~
  
  
  
  ##+ EXTRACT PAGE RANGE TO BE ANALYZED AND SPLIT INTO TEXT WINDOWS
  text.windows <- text[grep(startpage,text):grep(endpage,text)]

  #find the lines to START / END extraction on basis of a search string / search cases
  #for checking the output connected to the lines, use this command: text.windows[startlines]
    startlines <- c(sapply(seq(length(split.start)), function(e) {
      
      grep(split.start[e], text.windows)
      
    }))

 
  ##+ FIND THE LINES FOR SPLITTING INTO TEXT WINDOWS 
    if (delete.single.string.switch == "T~R~U~E") {
      
      #all rows to be extracted from text.windows
      #set endlines for extraction from one element to the next and last element = last row
      endlines <- startlines
      
      #delete the first entry, hence extraction would start with startlines[1] and end with startlines[2]
      endlines <- endlines[-1]
      #however extraction should be only until stratlines[2]-1 therfore -1 on all rows 
      endlines <- endlines-1 
      
      #as the final element is missing now the end of the text is defined as last split.end line
      endlines <- c(endlines, length(text.windows)) #add the last row as last entry     
      
    } else {
      
      endlines <- c(sapply(seq(length(split.end)), function(e) {
        
        grep(split.end[e], text.windows)
        
      }))
    } #endif
    
    #all rows to be extracted from text.windows
    extractall <- cbind(startlines, endlines)
  ##~
  
  
  ##+ SPLIT INTO TEXT WINDOWS
  #explanation of the code - see deletall block
  
    ##+ CLEAN TEXT FROM ANALYSIS CODE WORDS
    #the extraction lines are fixed now therefore the extraction patterns (as single words) can be cleaned from the text
    #for singleline pattern there is only split.start which can be deleted
    if (delete.single.string.switch == "T~R~U~E") {
      text.windows <- gsub( split.start ,"",text.windows)
    } else {
      text.windows <- gsub( split.start ,"",text.windows)
      text.windows <- gsub( split.end ,"",text.windows)
    }
    ##~
  
    ##+ DO SPLITTING
    #different way of splitting for single string patterns and multiple string patterns
    if (delete.single.string.switch == "T~R~U~E") {
      
      #build a vector with each element including a single text.window - collapsed text from the rows to be extracted
      text.windows.split <- c(sapply(seq(nrow(extractall)), function(x) {
        rows <- (extractall[x,1]:extractall[x,2])
        text.window <- text.windows[rows] #extracted text 
        #for the current analysis it makes sense to collapse the text.window into plain text
        #it might also be stored as a list
        #in case the former structure shall be generated again the wording " ~~collapse~~ " is included at the collaped locations
        
            ##+DELETE FOOTNOTES
            #in case footnotes were defined for deletion
            if (delete.single.lines[1] == "F~A~L~S~E") {  #first if/else
              
              # print(paste("no deletelines, nothing deleted in: ", names(workingdata.list[t])))
              
            } else if  (delete.single.lines[1] != "F~A~L~S~E") { #if there is something to delete, loop through the vector of lines containing the deletion marker string
              
              for (z in 1:length(delete.single.lines)) { #for loop through delete.single.lines
                
                if (sum(c(grep(delete.single.lines[z], text.window))) == 0) {  # second first/ifelse  / in case matching for deletion fails and a integer(o) vector is returned, a safety net is simply not to delete
                  
                  # if (delete.single.lines[z] != "!~delete_line_~!") { #only print something if the deletline text is not a the dummy text 
                  #  print(paste("failed to match deleteline", delete.single.lines[z] , "nothing deleted in: ", names(workingdata.list[t])))
                  # }
                  
                } else if (sum(c(grep(delete.single.lines[z], text.window))) > 0) {
                  text.window <- text.window[-c(grep(delete.single.lines[z], text.window))]
                } #end second if/else
                
              }
          
            } #end first if/else    
            ##~   
        
        paste(text.window, collapse= " ~~collapse~~ ")
        
      }))
      
    } else {
      
      #check if number of start/split.endlines are equal
      if (length(endlines) != length(startlines)) {
        print(paste("ERROR - number of start/endextractlines not equal in", names(workingdata.list[t])))
      stop}
      
      #build a vector with each element including a single text.window - collapsed text from the rows to be extracted
      text.windows.split <- c(sapply(seq(nrow(extractall)), function(x) {
        rows <- (extractall[x,1]:extractall[x,2])
        text.window <- text.windows[rows] #extracted text 
        #for the current analysis it makes sense to collapse the text.window into plain text
        #it might also be stored as a list
        #in case the former structure shall be generated again the wording " ~~collapse~~ " is included at the collaped locations
        
        
            ##+DELETE FOOTNOTES
            if (delete.single.lines[1] == "F~A~L~S~E") {  #first if/else
              
              # print(paste("no deletelines, nothing deleted in: ", names(workingdata.list[t])))
              
            } else if  (delete.single.lines[1] != "F~A~L~S~E") { #if there is something to delete, loop through the vector of lines containing the deletion marker string
              
              for (z in 1:length(delete.single.lines)) { #for loop through delete.single.lines
                
                if (sum(c(grep(delete.single.lines[z], text.window))) == 0) {  # second first/ifelse  / in case matching for deletion fails and a integer(o) vector is returned, a safety net is simply not to delete
                  
                  # if (delete.single.lines[z] != "!~delete_line_~!") { #only print something if the deletline text is not a the dummy text 
                  #  print(paste("failed to match deleteline", delete.single.lines[z] , "nothing deleted in: ", names(workingdata.list[t])))
                  # }
                  
                } else if (sum(c(grep(delete.single.lines[z], text.window))) > 0) {
                  text.window <- text.window[-c(grep(delete.single.lines[z], text.window))]
                } #end second if/else
                
              } #end for loop through delete.single.lines
              
            } #end first if/else    
            ##~
      
        
        paste(text.window, collapse= " ~~collapse~~ ")
        
      }))
    } 
    ##~
  
  text.windows.split <- list(text.windows.split)
  names(text.windows.split) <- paste("text_windows_", names(workingdata.list[t]), sep="")
  workingdata.list[[t]] <- c(workingdata.list[[t]], text.windows.split)    
  
  #in order to save memory and speed up the following calculations the complete text is removed
  workingdata.list[[t]][[1]] <- c("Full text removed in order to free memory and improve calculation performance. Please load again by readLines if the full text is needed.")
  
}
##~-------------------


##+ COMBINE LISTS OF TEXT WINDOWS FOR HANNOVER-----------
#the action plan came in several files which are combined here
  files.combine <- grep("GER__Hannover__", names(workingdata.list))
  
  #only do something if there are more than one list entries with the same entity name
  if (length(files.combine) > 1) { 
    
    #initial the combined list with the first element of elements to be combined
    case.combine <- workingdata.list[files.combine[1]]
    #get the number of first list levels which serve as loop variable
    num.listelements <- length(case.combine[[1]])
    
    #loop through all elements to combine except the intial one
    cases.add <- files.combine[-c(1)]
    
    for (t in cases.add) {
      
      for (u in 1:num.listelements) {
        
        case.combine[[1]][[u]] <- c(case.combine[[1]][[u]], workingdata.list[[t]][[u]])
        
      }
      
    }
    
    #reassign the names
    names(case.combine) <- "GER__Hannover__combined_files.txt"
    names(case.combine[[1]]) <- c("GER__Hannover__combined_files.txt",
                                  "split_pattern_guessed_summary_GER__Hannover__combined_files.txt",
                                  "text_windows__GER__Hannover__combined_files.txt")
    
    #add the combined files list and delete the single files
    workingdata.list <- c(workingdata.list,case.combine)
    workingdata.list <- workingdata.list[-c(files.combine)]
  }
##~-------------

time.elapsed <- rbind(time.elapsed, proc.time())
row.names(time.elapsed)[nrow(time.elapsed)] <- "after_text_loading_and_splitting"
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##END <<<<<<<<<<<<<<<<<<<
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<




##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##START <<<<<<<<<<<<<<<<<<< READ AND PREPARE THESAURI AND STOPWORDLIST DATA
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


##+ PREPARE THESAURI WORDLISTS---------------------------
#read all txt files in folder and build a list of their names
#then do some preliminary cleaning (trim, remove comments, etc.)

##+ READ THE THESAURI DATA AND CLEAN THE WORDLISTS  
#initialize
dirs <- list.dirs(wd.wordlists, recursive=F, full.names = F)
wordlists <- vector(mode = "list", length = length(dirs))

for (f in 1:length(dirs)) {

  ##+ READ
    setwd(paste0(wd.current, dirs[f]))
    files <- list.files(pattern=".txt")
    #initialize
    text <- c("")
    
    if( length(files) > 0) { 
      
      for (t in 1:length(files)) {
        
        text <- c(text, readLines(files[t]))
        
      }
    }
  ##~
  
  ##+ PRELIMINARY CLEANING
    text <- gsub("^.*#.*$", "", text)
    text <- gsub("^.*~~.*$", "", text)
    text <- gsub("^[[:blank:]]+", "", text)
    text <- gsub("[[:blank:]]+$", "", text)
    text <- text[text != " "]
    text <- text[text != ""]
    
    text <- HarmonizeMisintChars(text)
    text <- gsub("[[:blank:]]{2,}", " ", text)
    text <- unique(text)
  ##~
    
  
  ##+ STORE WORDLISTS IN R LIST
  
    #if the list only contains one single word (i.e. the category in the first line) this
    #causes problems in later steps, therefore dummy words are introduced
    if(length(text) <= 1) text <- c(text, c("DUMMYUPPER", "dummylower", "Dummymixed"))
    
    #create wordlistnames on basis of names of the directory
    names(wordlists)[f] <- dirs[f]
    
    wordlists[[f]] <- text
  ##~
  
  
}
##~---------------------------------------

##+ PREPARE STOPWORDLIST-----------------------

##+ COMPILE STOPWORDS FROM FOLDER AND EXTRACTION PATTERN WORDS
  #all words which had been used to identify extraction patterns
  #they are assumed to be very general, hence they can be deleted for the current analysis)
  stopwordlist <- LoadStopwordlistTxtCsv(wd.stopwords)

  stopwordlist.2 <- c(sapply(seq(length(workingdata.list)), function(i) {
    
    text <- workingdata.list[[i]][[2]]
    
  }))
  
  #here unique might be used if n-grams shall be preserved, however, this does not make much sense for stopwords
  stopwordlist.2 <- UniqueWords(stopwordlist.2)
  
  #some words should not be removed later and some others (like the extraction patterns) are added
  stopwordlist.2 <-  stopwordlist.2[-c(grep(paste0("Hannover|Finanzierung|Klima-Allianz|B",special.german.characters[5],"ro|Industrie|W",special.german.characters[1],"rme|Strom|F",special.german.characters[3],"rderprogramme|Kostenaspekte|Verbraucherverhalten|Erneuerbare|Industrie|Wohnen|Klima-Allianz|Wirtschaftlichkeit|Regionen|Wirtschftlichkeit"), 
                                            stopwordlist.2))]
  
  
  stopwordlist.2 <- c(stopwordlist.2, 
                      "~PAGE~RANGE~START~", 
                      "~PAGE~RANGE~END~", 
                      "END_EXTRACTION",
                      "START_EXTRACTION")
  
  
  stopwordlist <- c(stopwordlist, stopwordlist.2)
##~

  
##+ CLEAN AND STEM STOPWORDLIST
  stopwordlist <- HarmonizeMisintChars(stopwordlist)
  
  #in contrast to stopwordlist.2 the loaded stopwordlist might include n-grams which shall be preserved
  #therefore not UniqueWords but unique is applied
  stopwordlist <- unique(CleanVector(stopwordlist))
  
  #stem the stopwordlist
  #lower and uppercase are preserved by StemVector
  stopwordlist.stem <- StemVector(stopwordlist)
  
  #graph does not include blanks, similar like x!=""
  stopwordlist.stem <- x[grep("[[:graph:]]", stopwordlist.stem)]
  
  stopwordlist.stem <- sort(unique(stopwordlist.stem))
  
  stopwordlist.stem <- stopwordlist.stem
  
  #some stopwords were collected during screening of the final draft results 
  #and added at this point for the final run of the code
  stopwordlist.stem <- unique(c(stopwordlist.stem, c("Beschreib", "Eur", "Lag", "lag", 
                                                      #"Investitionskost", "Kost", 
                                                      "Eur", "Kombination", 
                                                      # "Kosteneinschaetz",
                                                      "Zug", "Material", "Ausbau", "Bearbeit", "Bearbeitung",
                                                      #"Finanzier", 
                                                      "Einschaetz", "Rahm", "Lok", "Kontakt", "Einstellung", "Modell", "Konzept",
                                                      "Untersuch", "Land", "Geraet", "Darstell", "Statist", "Hilf", "Reihe", "Reih", "Einricht", "Bestandteil",
                                                      "Vorbereit", "Einstell", "Aufbereit", "Erstellung", "Erstell",
                                                      "Roll", "Wass", "Led", "Mitt", "Bestimm", "Ausricht", "Reis", "Staerk")))
##~

##~-----------------------------------------
  


##+ CLEAN AND STEM THESAURI WORDLISTS AND MAKE WORDS UNIQUE-----------------------
 
  for (i in 1:length(wordlists)) {
    
    text <- unlist(wordlists[[i]])
    
    text <- unique(CleanVector(text))  #here not UniqueWords() is applied as suitable splitting has been done in the process of preparing the wordlists
    
    #for except for word referring to sustainability categories only nouns are considered
    if (grepl("3--SUS--", names(wordlists)[[i]]) == FALSE) {
      
      text <- text[grep("[[:upper:]]", text)]
      
    }
    
    text <- unique(StemVector(text))
    
    text <- text[grep("[[:graph:]]", text)]
    
    text <- gsub("^[[:alnum:]]{1,2}$","" , text)
    
    #remove the stopwords which appeared at the beginning of a sentence and begin with upper case
    rows.delete <- which(tolower(text) %in% tolower(stopwordlist))
    if (length(rows.delete) > 0) {
      text <- text[-rows.delete]
    }
    
    
    text <- text[text!=""]  
 
    
    if(length(text) <= 1) text <- c(text, c("DUMMYUPPER", "dummylower", "Dummymixed"))
  
    text <- unique(gsub("[[:blank:]]{2,}", " ", text))
    
    ##+ CLEAN STOPWORDS WITHIN STRINGS WHILE RESPECTING WORD BOUNDARIES
      rows.ngrams <- which(sapply(regmatches(text, gregexpr("[[:blank:]]", text)), length) > 0)
      
      if (length(rows.ngrams) > 0) {
        
        
        ngrams <- text[rows.ngrams]
        
        ngrams <- sapply(ngrams, function(x) {
          
          y <- x  
          y <- unlist(strsplit(y, " "))
          y <- setdiff(y, stopwordlist.stem)
          
          if(length(y) > 1) {
            y <- paste(y, collapse=" ") 
            y
          } else {
            
            x
          }
          
        })
        
        text[rows.ngrams] <- ngrams
      }
    ##~
    
    text <- list(text)
   
    names(text) <- paste(wordlists.prefix, names(wordlists[i]), sep="")
    
    #in order to free memory and increase performance the original wordlist is replaced
    #it could also be stored for tracing back errors due to stemming 
    #on the computer which was used for the analysis the performance of hardware was not sufficient to take this into account
    wordlists[[i]] <- text 
    
    
  }

##~------------------------------------------


##+ GENERATE COMBINED FLAT STRUCTURE OF ALL THESAURI WORD LISTS-------------------
#in the form word|category

#initialize
wordcategorylist <- matrix(c(0,0), nrow = 1)

  for (i in 1:length(wordlists)) {
  
  category <- rep(gsub(wordlists.prefix, "", names(wordlists[[i]][1])), length(wordlists[[i]][[1]]))
  matrix <- cbind(wordlists[[i]][[1]], category)
  wordcategorylist <- rbind(wordcategorylist, matrix)
  
}

wordcategorylist <- wordcategorylist[-c(1),] #delete first initializing line
colnames(wordcategorylist)[1] <- "word"
colnames(wordcategorylist)[2] <- "category"


##+ ORDER THE WORD-CATEGORY-LIST BY NUMBER OF BLANKS
  #which is later important for order of finding/replacing matches
  text <- wordcategorylist[,1]
  text <- gsub("[[:blank:]]{2,}", " ", text)
  order.numblanks <- order(sapply(regmatches(text, gregexpr("[[:blank:]]", text)), length), decreasing=TRUE)
  wordcategorylist <- wordcategorylist[order.numblanks,]
  
  #delete the words which have more than 9 blanks (to hit a 9-gram seems not probable anyway)
  #a 9-gram which might be matched could be: Wohnmobil mit ein Hubraum von mehr als cm bis cm
  text <- wordcategorylist[,1]
  numblanks.greaternine <-  which(sapply(regmatches(text, gregexpr("[[:blank:]]", text)), length) > 9)
  if (length(numblanks.greaternine) > 0) {
    wordcategorylist <- wordcategorylist[-numblanks.greaternine,]
  }
##~

row.names(wordcategorylist) <- 1:nrow(wordcategorylist)
##~------------------------

time.elapsed <- rbind(time.elapsed, proc.time())
row.names(time.elapsed)[nrow(time.elapsed)] <- "after_wordlist_loading"
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##END <<<<<<<<<<<<<<<<<<<
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<



##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##START <<<<<<<<<<<<<<<<<<< CLEAN AND STEM ALL TEXT WINDOWS
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


#get stopwords which are not in ngrams
#stopwords within ngrams are needed to identify the ngrams
#(its a difference if it a concept refers to "the association of renters" or only "renters [of an area]")
#association as single word would be a stopword
#all other stopwords can be deleted from the measures before starting matching algorithm
ngrams <- unlist(wordlists)
ngrams <- as.character(unlist(strsplit(ngrams[grep("[[:blank:]]", ngrams)], " ")))
stopwords.not.in.ngrams <- setdiff(stopwordlist.stem,ngrams)

##+ CLEAN AND STEM TEXT WINDOWS----------------
for (i in 1:length(workingdata.list)) {
  
  text <- workingdata.list[[i]][[3]]
  
  ##+ FLAT STEMMED VERSION OF THE WORDS IN THE TEXT WINDOWS
    window.stem <- lapply(text, function(x) {
      
      x <- paste(unlist(x), collapse=" ")
      
      x <- CleanVector(x)
      
      x <- StemVector(x)
      
      x <- unlist(strsplit(x, " "))
      
      x <- setdiff(x, stopwords.not.in.ngrams)
      
      x <- paste(x, collapse=" ") 
      
      x
    })
    
    window.stem <- unlist(window.stem)
  ##~
  
  
  ##+ GENERATE MATRIX OF STEMMED AND UNSTEMMED UNIQUE WORDS OF TEXT WINDOWS
  #needed later for generation of list of untagged words (this is not an analytical step)
  #this may be done for the whole text at once, no need to separate between measures as above
  
    #generate base words
    base <- text
    x <- UniqueWords(CleanVector(base))
    
    x <- x[grep("[[:graph:]]", x)]
    
    x <- gsub("^[[:alnum:]]{1,2}$","" , x)
    
    #remove the stopwords which appeared at the beginning of a sentence and begin with upper case
    rows.delete <- which(tolower(x) %in% tolower(stopwordlist))
    
    if (length(rows.delete) > 0) {
      x <- x[-rows.delete]
    }
    
    base <- x[x!=""]
    
    #generate stemmed words and bind both together
    base.stem <- StemVector(base)
    measure.unique.baseANDstem  <- cbind(base, base.stem)
    
    #delete stopwords in stemmed format
    x <- base.stem
    rows.delete <- which(tolower(x) %in% tolower(stopwordlist))
    
    if (length(rows.delete) > 0) {
      measure.unique.baseANDstem <- measure.unique.baseANDstem[-rows.delete, ]
    }
  ##~
 
  #full original measure text is overwritten
  workingdata.list[[i]][[3]] <- window.stem
  names(workingdata.list[[i]])[3] <- paste("measstem_", names(workingdata.list[i]), sep="")
  measure.unique.baseANDstem <- list(measure.unique.baseANDstem) 
  workingdata.list[[i]] <- c(workingdata.list[[i]], measure.unique.baseANDstem) 
  names(workingdata.list[[i]])[4] <- paste("measunibaseANDstem_", names(workingdata.list[i]), sep="")

}

##~-----------------------

time.elapsed <- rbind(time.elapsed, proc.time())
row.names(time.elapsed)[nrow(time.elapsed)] <- "after_cleaning_stemming_measures"
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##END <<<<<<<<<<<<<<<<<<<
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<




##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##START <<<<<<<<<<<<<<<<<<<  SEARCH ALGORITHM - CREATE OCCURRENCE MATRIX FOR CATEGORIES IN TEXT WINDOWS PER DOCUMENT ON BASIS OF TAGGED WORDS
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


words.tag.num  <- nrow(wordcategorylist) 
words.tag <- as.character(wordcategorylist[,1])

#sustainability vocabulary shall be matched also within words 
#therefore their row numbers are identified
#in order to adjust the matching command for these rows
sustainability.rows <- grep("3--SUS--", wordcategorylist[,2])
sustainability.words <- as.character(wordcategorylist[sustainability.rows,1])


for (t in seq(length(workingdata.list))) {
  
  text.windows.split <- workingdata.list[[t]][[3]]
  
  #initialize matrix to count word occurences
  #one row for each tagged word / one column for each measure
  text.windows.num <- length(text.windows.split)
  initial <- rep(0, text.windows.num*words.tag.num)
  occurence.matrix <- matrix(initial, nrow = words.tag.num)
  colnames(occurence.matrix) <- paste("window_",seq(text.windows.num) , sep="")
  
##+ COUNT OCCURRENCE OF WORDS IN TEXT WINDOWS---------------------
  for (m in seq(text.windows.num)) {
    
    text <- as.character(text.windows.split[m])
    
    for (n in seq(length(words.tag)))  { 
      
      
      if (n %in% sustainability.rows) {
        
        text.interim <- paste(setdiff(unlist(strsplit(text, " ")), stopwordlist), collapse = " ")
        
        #check sustainability vocabulary in upper and lower case as well as within words
        #(\\w) does also match _ / 
        searchstring <- paste("(\\b)(\\w*)(",words.tag[n],")(\\w*)(\\b)", sep="")
        count <- sum(ifelse(gregexpr(searchstring, text.interim, ignore.case = TRUE)[[1]]>0,1,0))
        
      } else {
        
        
        if (sum(ifelse(gregexpr(" ", words.tag[n])[[1]]>0,1,0)) > 0) {
          
          searchstring <- paste("(\\b|_)(",words.tag[n],")(\\b|_)", sep="")
          
        } else { 
          
          searchstring <- paste("(\\b)(",words.tag[n],")(\\b)", sep="")
          
        }
        
        #count occurence of word and store result in the overall matrix
        #if there is a match the gregexpr result returns the starting position of each match of the string
        #the greexpr is a list with attributes, the starting positions are converted to 1s and summed up for counting
        count <- sum(ifelse(gregexpr(searchstring, text)[[1]]>0,1,0)) #optionally: ignore.case=TRUE
        
      }
      
      
      occurence.matrix[n,m] <- count
      
      #in case the word is an n-gram it is replaced in the text to be analyzed
      #so it cannot be matched again in the loop with single words
      #e.g. fossile Energietraeger would otherwise match for fossil AND Energietraeger
      #this would be "overmatching", the specific n-gram match is sufficient
      #hence if there is a general category Energietraeger it would not include this n-gram
      #the n-gram will be included in the more specific category "fossil resources"
      #hence to match all words belonging to an overarching category several categories might have to be combined
      #it is assumed that there are only few n-grams throughout the different categorylists 
      #which are duplicates in contrast to single words -> hence an n-gram is only matched for one category, never for more
      #-> this assumption might have to be discussed, but is used for performance reasons
      #for single words double matching is allowed as these are less specific
      
      #if the word is a one-gram it is marked by the identifiers in a way that its word boundaries are preserved for additional matching
      #in order to be deleted after all possible matching words have been checked
      #hence the number of ~ in front of a word also shows how often it was matched
      
      #sustainability words are not marked in a first step as they may appear in ngrams
      #as they are matched also within words (\w*)(sustainabilityword)(\w*) they might separate
      #ngrams which have not yet been matched, also matching of sustainability
      #words which themselves are ngrams would be distracted this way
      if (n %in% sustainability.rows) {
        
        next(n) 
        
      } else if (count > 0) {
        
        #keep in  mind for the following code that the searchstring was written in perl style and has to be backreferenced as 3 elements
        if (sum(ifelse(gregexpr(" ", searchstring)[[1]]>0,1,0)) > 0) {
          
          #ngrams are marked in a way that their word boundaries will not be matched again
          #~ and / have no effect on the word boundary, which is defined by a character followed by a non character
          #therefore the character class symbol _ is introduced
          text <-  gsub(searchstring,  " :_\\1\\2\\3\\4\\5_; ", text, perl=TRUE) #optionally: ignore.case=TRUE
          
        } else {
          
          text <- gsub(searchstring,  "~ \\1\\2\\3\\4\\5 /", text, perl=TRUE) #optionally: ignore.case=TRUE
        }
        
      }
    }
    
    text.windows.split[m] <- gsub("[[:blank:]]{2,}", " ", text)
  }
  
  workingdata.list[[t]][[3]] <- text.windows.split
  
  #evaluation matrix - connect the counts to the actual words and categories
  #first type of columns = tagged word and categories connected to it -> word represents categories represented in a measure
  #second type of columns = how often is a tagged word contained in a measure (column)
  
  #a character data.frame is returned, this is fine in the first step : mode(evaluationmatrix) = "character"
  #columns have to be changed to numeric for calculations
  evaluationmatrix <- cbind(wordcategorylist, occurence.matrix)
##~---------------------------------
 
  
##+ FIND AND WRITE UNTAGGED WORDS----------------------
  #check delete all words which stems have been mapped from the uniquewordlist(baseANDstem) for each text  
  #clean the measure text from matched words in order to get the unmatched words
  
  #!!an approach not using regex might be quicker (the regex approach was rather used for testing)
  
  #select all words which have been matched, make unique as one word may appear as duplicate in different categories
  #words.matched <- unique(evaluationmatrix[which(rowSums(occurence.matrix)>0),"word"])
  #1grams in a first gsub step and n-grams in a next vector step with interim markers
  #words.unmatched <- gsub("(~)(\\s)([\\w]+)(\\s)(/)","", text.windows.split, perl=TRUE)
  
  words.unmatched <- c()
  words.matched <- c()
  
  for (m in seq(text.windows.num)) { 
    
    text <- as.character(text.windows.split[m])
    
    #1grams
    words.matched <- c(words.matched, regmatches(text, gregexpr("(?=~)(.*?)(?=/)", text, perl=TRUE))) 
    
    rest <- regmatches(text, gregexpr("(?=~)(.*?)(?=/)", text, perl=TRUE), invert=T)
    rest <- paste(unlist(rest), collapse = " ")
    rest <- gsub("~|/", " ", rest)
    
    #ngrams
    words.matched <- c(words.matched,regmatches(rest, gregexpr("(?=:)(.*?)(?=;)", rest, perl=TRUE)))
    #this would also have worked without lookaround
    #regmatches(rest, gregexpr("(:)(.*?)(;)", rest, perl=TRUE))
    
    rest <- regmatches(rest, gregexpr("(?=:)(.*?)(?=;)", rest, perl=TRUE), invert=T)
    rest <- paste(unlist(rest), collapse = " ")
    rest <- gsub("[[:punct:]]+", "", rest)
    
    words.unmatched <- c(words.unmatched, unlist(strsplit(unlist(rest), " ")))
    
    words.unmatched <- unique(CleanVector(UniqueWords(words.unmatched)))
    
  }
  
  words.unmatched <- unique(CleanVector(UniqueWords(words.unmatched)))
  
  #in case the information is needed how often a single word was matched,
  #counting of ~ or : has to be performed before
  words.matched <- unique(CleanVector(UniqueWords(words.matched)))                        
  
  ##remove stopwords in upper and lower case
  rows.delete <- which(tolower(words.matched) %in% tolower(stopwordlist.stem))
  
  
  if (length(rows.delete) > 0) {
    words.matched <- words.matched[-rows.delete]
  }
  
  rows.delete <- which(tolower(words.unmatched) %in% tolower(stopwordlist.stem))
  if (length(rows.delete) > 0) {
    words.unmatched <- words.unmatched[-rows.delete]
  }
  
  words.matched <- words.matched[words.matched!=""]
  words.unmatched <- words.unmatched[words.unmatched!=""]
  
  words.matched.upper <- words.matched[grep("[[:upper:]]", words.matched)]
  words.unmatched.upper <- words.unmatched[grep("[[:upper:]]", words.unmatched)]
  
  #matched words are fewer than unmatched, thus, searching for those instead is faster
  rows.notmatched <- which(!(workingdata.list[[t]][[4]][,"base.stem"] %in% words.matched))
  notwords.matched.base <- workingdata.list[[t]][[4]][rows.notmatched,"base"]
  
  #the steps before only identify whole words which have been matched
  #on basis of positive entries in the evaluation matrix, for the sustainability vocabulary these entries
  #are only stems of words. This means that in the evaluationmatrix a word "optim" is counted as positive
  # in the case a word like (combined phrase without hyphen, e.g. in German) "optimizationmeasures" has been matched
  #by the word stem. However, this word in the text does not appear in the evaluation matrix.
  #Therefore, such words have to be deleted additionally from the notmatched list.
  
  
  rows.delete <- c()
  for (d in 1:length(sustainability.words)) {
    rows.delete <- c(rows.delete, grep(paste("(\\b)(\\w*)(",sustainability.words[d],")(\\w*)(\\b)", sep=""), notwords.matched.base, perl=TRUE, ignore.case = TRUE))
    
  }
  
  sustainability.words.matched.not.marked <-  unique(notwords.matched.base[rows.delete])
  
  if (length(rows.delete) > 0) {
    notwords.matched.base <- notwords.matched.base[-rows.delete]
  }
  
  notwords.matched.base <- unique(notwords.matched.base)
  notwords.matched.base <- notwords.matched.base[order(notwords.matched.base)]
  notwords.matched.base <- notwords.matched.base[grep("[[:upper:]]", notwords.matched.base)]
  
  #append the not matched words in the respective file
  write(notwords.matched.base, file=paste(wd.notmatched, file.words.not.matched, sep=""), append=TRUE)
  
##~---------------------------------
  
  
  #calculate initial match rate
  #this rate only considers the uppercase words and shows if any words have not been tagged 
  #that could have been when following the goal of tagging nouns
  match.rate <- round(length(words.matched)/
                        (length(words.matched)+length(words.unmatched)),
                      #as the value is used in the filename no dots should be included
                      digits = 2)*100 

  
  filename <- gsub(".txt",paste(occurrence.filename.tag, match.rate, ".csv", sep=""),names(workingdata.list[t]))

  
  write.csv(evaluationmatrix, paste(wd.interim, filename, sep=""))
  
 
  time.elapsed <- rbind(time.elapsed, proc.time())
  case.name <- gsub("^.*GER_", "", names(workingdata.list[t]))
  case.name <- substr(case.name,1,25)
  row.names(time.elapsed)[nrow(time.elapsed)] <- paste("after_matching_in_", case.name, sep="")

}


time.elapsed <- rbind(time.elapsed, proc.time())
row.names(time.elapsed)[nrow(time.elapsed)] <- "after_matching_all_texts"
write.csv(time.elapsed, paste(wd.final, "time_elapsed.csv", sep=""))


#activate in case the code is run over night and machine shall be shut down after completion (save energy)
#system("shutdown.exe -s")


#clean memory from the largest objects
remove <- intersect(ls(), 
                    c("evaluationmatrix", "occurence.matrix", "initial",
                      "occurrence.boolean", "text", "interim", "case.combine"))

rm(list = remove)

##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##END <<<<<<<<<<<<<<<<<<<
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<




##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##START <<<<<<<<<<<<<<<<<<< CALCULATE NUMBER OF TAGGED CONCEPTS AND MATCH RATE
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#this part of the code was added in a second step of coding and has not been integrated into above
#main code; it therefore contains several repitions and requires more computing time than needed
#for smaller amounts of data time effort is still ok
#but integration and optimization of code is necessary for larger data sets

#initialize match rate table
match.rate.table.colnames <- c("case", 
                               "number of text windows", 
                               "average number of words per text window",
                               "average number of unique stemmed words per text windows", 
                               
                               "average number of unique uppercase stemmed words",
                               "average number of tagged unique uppercase stemmed words"
                                )
match.rate.table <- matrix(nrow=length(workingdata.list),ncol=length(match.rate.table.colnames))
colnames(match.rate.table) <- match.rate.table.colnames

ngrams <- unlist(wordlists)
ngrams <- as.character(ngrams[grep("[[:blank:]]", ngrams)])

ngrams.upper <- unlist(strsplit(ngrams, " "))
ngrams.upper <- ngrams.upper[grep("[[:upper:]]", ngrams.upper)]
ngrams.upper <- setdiff(ngrams.upper, stopwordlist.stem)
ngrams.upper <- setdiff(ngrams.upper, words.tagged)

words.tagged <- as.character(unlist(strsplit(unlist(wordlists), " ")))
words.tagged <- setdiff(words.tagged,  stopwordlist.stem)
words.tagged.upper <- words.tagged[grep("[[:upper:]]", words.tagged)]

words.all <- c()
for (i in 1:length(workingdata.list)) {
  
  text <- workingdata.list[[i]][[3]] 
  
  text <- lapply(text, function(x) {
    
    x <- paste(unlist(x), collapse=" ")
    
    x <- CleanVector(x)
    
    x <- StemVector(x)
    
    x <- unlist(strsplit(x, " "))
    
    x <- setdiff(x, c("~~collapse~~"))
    
    x <- paste(x, collapse=" ") 
    
    x
  })
  
  words.all <- c(words.all, text)
}

words.all.unique <- unique(unlist(strsplit(unlist(words.all), " ")))
words.all.unique <- setdiff(words.all.unique, stopwordlist.stem)
words.all.unique <- setdiff(words.all.unique, words.tagged)
words.all.unique  <- words.all.unique [grep("[[:upper:]]",all.words.unique )]



for (i in 1:length(workingdata.list)) {
  
  
  text <- workingdata.list[[i]][[3]]
  

  window.stem <- lapply(text, function(x) {
    
    x <- paste(unlist(x), collapse=" ")
    
    x <- CleanVector(x)
    
    x <- StemVector(x)
    
    x <- unlist(strsplit(x, " "))
    
    x <- setdiff(x, c("~~collapse~~"))
    
    x <- paste(x, collapse=" ") 
    
    x
  })
  
  window.stem <- unlist(window.stem)

  
  
  words.per.window.average <- round(mean(unlist(lapply(window.stem, function(x) {
    
    x <- unlist(strsplit(x, " "))
    
    length(x)
    
    
  }))), d=0)
  
  
  words.unique.per.window.average <- round(mean(unlist(lapply(window.stem, function(x) {
    
    x <- unlist(strsplit(x, " "))
    
    x <- unique(tolower(x))
    
    length(x)
    
    
  }))), d=0)
  
  
  
  unique.uppercase.stemmed.per.window <- round(mean(unlist(lapply(window.stem, function(x) {
    
    total <- length(unique(tolower(unlist(strsplit(x, " ")))))
    
    
    x <- unique(unlist(strsplit(x, " ")))
  
        if (length(grep("[[:upper:]]", x))>0) {
      
      x <- x[-grep("[[:upper:]]",x)]
      
    }
    
    minusupper <- length(x)
    
    #number of nouns
    total-minusupper
    
  }))), d=0)
  
  
  
  nouns.unique.tag.rate.per.window.average <- round(mean(unlist(lapply(window.stem, function(x) {
    
    
    total <- length(unique(tolower(unlist(strsplit(x, " ")))))
    
    
    x <- unique(unlist(strsplit(x, " ")))
    
    if (length(grep("[[:upper:]]", x))>0) {
      
      x <- x[grep("[[:upper:]]",x)]
      
    }
    
    upper <- length(x)
    
    if (length(which(tolower(x) %in% tolower(words.tagged.upper)))>0) {
      
      x <- x[-which(tolower(x) %in% tolower(words.tagged.upper))] 
      
    }
    
    
    minustagged <- length(x)
    
    #number of nouns
    upper-minustagged
    
    
  }))), d=2)
  
  
  match.rate.table[i,"case"] <- names(workingdata.list)[i]
  
  match.rate.table[i,"number of text windows" ] <- length(window.stem)
  
  match.rate.table[i,"average number of words per text window"] <- words.per.window.average
  
  match.rate.table[i, "average number of unique stemmed words per text windows"] <- words.unique.per.window.average
  
  match.rate.table[i, "average number of unique uppercase stemmed words"] <-   unique.uppercase.stemmed.per.window 
  
  match.rate.table[i, "average number of tagged unique uppercase stemmed words"] <-  nouns.unique.tag.rate.per.window.average
  
  
}

setwd(wd.final)
write.csv(match.rate.table, paste(wd.final, "match_rate_table.csv", sep=""))

##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##END <<<<<<<<<<<<<<<<<<< 
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<



##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##START <<<<<<<<<<<<<<<<<<< GENERATE RELATIVE COOCCURRENCE MATRICES
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##count normalized co-occurrence of categories within each text
#(co-occurrence of category1/category2 within X text windows of a text)/(number of all text windows of a text)

setwd(wd.interim)

files <- list.files(pattern = paste("^.*", occurrence.filename.tag, "[[:digit:]]+.csv$", sep=""))

categories <- list.dirs(wd.wordlists, recursive=F, full.names = F)
categories <- categories[order(categories)]

categories.num <- length(categories)

#initialize
cooccurrence.matrix <- matrix(rep(0, categories.num*categories.num), nrow = categories.num)
row.names(cooccurrence.matrix) <- categories[order(categories)]
colnames(cooccurrence.matrix) <- categories[order(categories)]

#initialize 
#top ten words concerning occurrence value
words.occurrence.topten <- as.data.frame(matrix(c("word", "category", "relative_occurrence", "document"), nrow=1))
colnames(words.occurrence.topten) <- c("word","category", "relative_occurrence", "document")

words.occurrence.topten <- data.frame(word = as.character(),
                                      category = as.character(),
                                      relative_occurrence = as.numeric(),
                                      document = as.character())


for (t in seq(length(files))) {   
  

  evaluationmatrix <- read.csv(files[t]) 
 

  #if reading in of the csv generates columns with numbers (old rownumbers) these are deleted
  delete.columns <- grep("(X$)|(X\\.)(\\d+)($)", colnames(evaluationmatrix), perl=T)
  
  if (length(delete.columns) > 0) {
    row.names(evaluationmatrix) <- as.character(evaluationmatrix[, grep("^X$", colnames(evaluationmatrix))])
    evaluationmatrix <- evaluationmatrix[,-delete.columns]
    colnames(evaluationmatrix) <- gsub("^X", "",  colnames(evaluationmatrix))
    colnames(evaluationmatrix) <- gsub("\\.", "-",  colnames(evaluationmatrix))
  }
  
  #count the number of measures of the case
  text.windows.num <- ncol(evaluationmatrix)-2
  
  case.name <- gsub(paste("(^.*GER__)([A-Za-z]+)(__.*$)"), "\\2",files[t])
  
  #find specific combinations of words and categories
  #that emerged as unsuitable combinations during analysis of data
  delete.rows <- c(intersect(grep("^Haushalt$|^Kind$|^Verbind$|^Materiali$|^Zusammenstell$|^Raeum$|^Unterlag$|^Buero$|^Bueros$|^Lieg$|^Tier$|^Abnehm$", evaluationmatrix[,"word"]), 
                             grep("CF", evaluationmatrix[,"category"])),
                   
                   intersect(grep("^Bau$|^Werk$", evaluationmatrix[,"word"]), 
                             grep("Agriculture", evaluationmatrix[,"category"])),
                   
                   intersect(grep("^Baeum$", evaluationmatrix[,"word"]), 
                             grep("intermediate_products", evaluationmatrix[,"category"])),
                   
                   intersect(grep("Vermi", evaluationmatrix[,"word"]), 
                             grep("sufficiency_avoidance", evaluationmatrix[,"category"]))
                   
  )
  
  #delete
  if (length(delete.rows)>0) {
    delete.rows <- unique(delete.rows)
    evaluationmatrix <- evaluationmatrix[-delete.rows,]
  }
  

  word.frequencies <- cbind(evaluationmatrix[,1:2], rowSums(evaluationmatrix[,3:ncol(evaluationmatrix)]))
  colnames(word.frequencies)[3] <- c("relative_occurrence")
  word.frequencies <- word.frequencies[-which(word.frequencies[,3] == 0),]
  word.frequencies <- split(word.frequencies, word.frequencies$category)
  
  
  invisible(lapply(word.frequencies, function(x) {
    x <- x[order(x[,3], decreasing=TRUE),]
    x <- x[1:10,]
    x <- cbind(x, document = rep(case.name, 10))
    
    delete.rows <- grep("NA", row.names(x))
    if (length(delete.rows) > 0) {
      x <- x[-delete.rows,]
    }
    
    words.occurrence.topten <<- rbind.fill(words.occurrence.topten,data.frame(x))
    
  }))
  
  #delete the words column
  evaluationmatrix <- evaluationmatrix[,-c(1)]
  
  #aggregate the category lines
  evaluationmatrix <- aggregate(evaluationmatrix[2:ncol(evaluationmatrix)], by=list(category=evaluationmatrix$category), FUN=sum)
  
  #make matrix boolean
  evaluationmatrix[,c(2:ncol(evaluationmatrix))] <- ifelse(evaluationmatrix[,c(2:ncol(evaluationmatrix))]>0,1,0)
  
  #generate the same order as in the results matrix
  evaluationmatrix <- evaluationmatrix[order(evaluationmatrix[,1]),]
  
  #initialize results matrix for the case 
  result.cooccurrence.case <- cooccurrence.matrix
  
  evaluationmatrix <- evaluationmatrix[order(evaluationmatrix[,1]),]
  
  #check if categories in matrix are the same as the loaded categories, they should be,
  #but due to naming mistakes etc. there might be errors
  if (identical(as.character(evaluationmatrix[,1]),row.names(result.cooccurrence.case)) == FALSE)  {
    warning("ERROR: categories of wordlists and evaluationmatrix are not identical, only the intersecting categories are used.
            This might lead to incompatibility of the result files of different files.
            Possible reasons might be mistakes in naming the categories or during reading in the wordlists.")
    
    categories.intersect <- intersect(as.character(evaluationmatrix[,1]),row.names(result.cooccurrence.case))
    categories.num <- length(categories.intersect)
    
    evaluationmatrix <- evaluationmatrix[which(evaluationmatrix[,1] == categories.intersect),]
    evaluationmatrix <- evaluationmatrix[order(evaluationmatrix[,1]),]
    
    result.cooccurrence.case <- result.cooccurrence.case[which(row.names(result.cooccurrence.case) == categories.intersect),]
    result.cooccurrence.case <- result.cooccurrence.case[order(result.cooccurrence.case[,1]),]
    
  }
  
  
  ####START loop through all categories and check cooccurrence---------------------------------
  for(i in seq(length(evaluationmatrix[,"category"]))) {
    
    #fix the column/category which shall be filled
    category <- colnames(result.cooccurrence.case)[i]
    
    #select all columns/measures in which the category is fulfilled
    #1:select only the lines which are connected to the category
    category.subset <- evaluationmatrix[which(evaluationmatrix[,1] ==  category),]
    #2:within this subset select only the columns which fulfill the category 
    #first two colulmns word/category are excluded for this numeric check
    category.subset <- category.subset[,c(2:ncol(category.subset))]
    
    #>>>>>in case the category is not fulfilled at all the colSums are zero
    #and the cooccurrence is zero in all rows, no further calculation of results needed
    if (as.integer(rowSums(category.subset)) == 0) {
      
      #write zero results in the results matrix
      cooccurrence <- rep(0, categories.num)
      
      #rounding would not be necessary for the zeros, however to receive same format for all results
      #the operation is conducted anyhow
      result.cooccurrence.case[,category] <- round((cooccurrence/text.windows.num) ,digits=4)
      
      next(i)
      
    }
    #>>>in case the category is matched at least once the "normal" calculation of cooccurrences is conducted
    
    #as the first column was skipped for the colSums evaluation 
    #a plus one offset for the which/colSums>0 indices has to be introduced 
    
    columns.select <- colnames(category.subset)[which(category.subset>0)]
    
    #at least one column has to be in the set
    category.subset <- evaluationmatrix[, columns.select]
    
    #the category.subset contains only columns/measures for which the current.category is fullfilled
    #as a boolean matrix was generated, the rowSums of the matrix represent the cooccurrences
    #of each other category with the category currently fixed in the loop
    #the numeric part of the data.frame was converted to boolean numbers therefore this counting procedure works
    
    
    #>>>>>in case only one column fulfills the current category a numeric vector instead of a matrix is returned
    #this vector already represents the rowSums which has to be calculated in case more columns are fulfilled
    #hence results can directly be written into the results matrix for this case
    if (length(columns.select) == 1) {
      
      cooccurrence <- category.subset
      
      result.cooccurrence.case[,category] <- round((cooccurrence/text.windows.num) ,digits=4)
      
      next(i)
      
      
    }
    
    #>>>in case there are more than one columns fulfilling the current category build rowSums to calculate cooccurrence
    
    cooccurrence <- rowSums(category.subset)
    
    result.cooccurrence.case[,category] <- round((cooccurrence/text.windows.num) ,digits=4)
    
    
  }
  ####END loop through all categories and check cooccurrence---------------------------------

  filename <- gsub(paste(occurrence.filename.tag,".*$" ,sep=""),paste(cooccurrence.filename.tag, ".csv", sep=""),files[t])
  filename <- paste(wd.interim, filename, sep="")
  
  write.csv(result.cooccurrence.case, filename)
  
  
} #end loop through all files


setwd(wd.final)

write.csv(words.occurrence.topten, "words_occurrence_topten_all.csv")


words.occurrence.topten.consolidated <- data.frame(word = as.character(),
                                                   category = as.character(),
                                                   relative_occurrence = as.numeric(),
                                                   document = as.character())



words.frequencies <- split(words.occurrence.topten, words.occurrence.topten$category)


invisible(lapply(words.frequencies, function(x) {
  x <- x[order(x[,3], decreasing=TRUE),]
  
  x <- x[-which(duplicated(x[,1])),]
  
  
  x <- x[1:10,]
  
  delete.rows <- grep("NA", row.names(x))
  if (length(delete.rows) > 0) {
    x <- x[-delete.rows,]
  }
  
  words.occurrence.topten.consolidated <<- rbind.fill(words.occurrence.topten.consolidated,data.frame(x))
  
}))


write.csv(words.occurrence.topten.consolidated, "words_occurrence_topten_consolidated.csv")
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##END <<<<<<<<<<<<<<<<<<<
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<



##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##START <<<<<<<<<<<<<<<<<<< CALCULATE AVERAGE COOCCURRENCE MATRIX
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

##START build mean values----------------------------------
setwd(wd.interim)
files <- list.files(pattern = paste("^.*", cooccurrence.filename.tag, ".csv$", sep=""))


#generation of mean values by loading one file after another and extraction the cooccurrence column only
#-> hence wide format data
#this is added to a new mean value matrix which is finally used to calculate the mean value over all cases

for (t in seq(length(files))) {   
  
  cooccurrence.matrix.case <- read.csv(files[t]) 
  
  
  #if reading in of the csv generates a column with numbers (old rownumbers) these columns are deleted
  delete.columns <- grep("(X$)|(X\\.)(\\d+)($)", colnames( cooccurrence.matrix.case), perl=T)
  
  if (length(delete.columns) > 0) {
    row.names( cooccurrence.matrix.case) <- as.character( cooccurrence.matrix.case[, grep("^X$", colnames( cooccurrence.matrix.case))])
     cooccurrence.matrix.case <-  cooccurrence.matrix.case[,-delete.columns]
    colnames( cooccurrence.matrix.case) <- gsub("^X", "",  colnames( cooccurrence.matrix.case))
    colnames( cooccurrence.matrix.case) <- gsub("\\.", "-",  colnames( cooccurrence.matrix.case))
  }
  
  
  cooccurrence.matrix.case <-  cooccurrence.matrix.case[,order(colnames(cooccurrence.matrix.case))]
  cooccurrence.matrix.case <-  cooccurrence.matrix.case[order(row.names(cooccurrence.matrix.case)),]
  cooccurrence.matrix.case <- as.matrix(cooccurrence.matrix.case)
  
  #from the first file not only the number column but also the categories are used to initialize the mean matrix
  if (t == 1) {
    cooccurrence.matrix.mean <- cooccurrence.matrix.case
    #the last column name is named according to the current case
    #next(t)
  } else {
    
    
    #check if the category columns are really the same otherwise wrong results are produced
    if(identical(colnames(cooccurrence.matrix.mean), colnames(cooccurrence.matrix.case)) == FALSE) {
      warning("The category columns of the cooccurrence files are not identical. The resulting mean values are not correct.")  
      print(paste("t:", t, ", file: ", files[t], sep=""))
    }
    
    if(identical(row.names(cooccurrence.matrix.mean), row.names(cooccurrence.matrix.case)) == FALSE) {
      warning("The category columns of the cooccurrence files are not identical. The resulting mean values are not correct.")  
      print(paste("t:", t, ", file: ", files[t], sep=""))
    }
    
    cooccurrence.matrix.mean <- cooccurrence.matrix.mean+cooccurrence.matrix.case
    
  }
  
}


#after summing up the matrices, divide through the number of cases
cooccurrence.matrix.mean <-  round(cooccurrence.matrix.mean/length(files), d=4)

#write mean results in file
filename <- paste(filenamebeginning.cooccurrence.matrix.mean, cooccurrence.filename.tag, ".csv", sep="")
filename <- paste(wd.interim, filename, sep="")
write.csv(cooccurrence.matrix.mean, filename)
##~----------------------
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##END <<<<<<<<<<<<<<<<<<< 
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<



##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##START <<<<<<<<<<<<<<<<<<< PREPARE DATA FOR FINAL PLOTTING AND ANALYSIS
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

##+ INITIALIZE VARIABLE NAMES ETC.--------------------------
#delete words
#in case stopwords which were only identified during an analytical step 
#and should be removed in addition to the previously defined stopwordlist
stopwords.additional <- c("dummy_stopword")

categories <- list.dirs(wd.wordlists, recursive=F, full.names = F)
categories.num <-length(categories)

occurrence.filename.tag <- c("__w_occ_mat__MR")
files <- list.files(wd.interim, pattern = paste("^.*", occurrence.filename.tag, "[[:digit:]]+.csv$", sep=""))

results.occ.final <- matrix(rep(0, categories.num*length(files)), nrow = categories.num)
row.names(results.occ.final) <- categories
colnames(results.occ.final) <- rep("dummy_casename", ncol(results.occ.final))
##~-------------------------------------------------------------


##+ COUNT OCCURRENCE OF CATEGORIES IN TEXT WINDOWS ON BASIS OF OCCURRENCE OF CATEGORIZED WORDS--------------
for (t in seq(length(files))) {
  
  
  evaluationmatrix <- read.csv(files[t], header = TRUE, check.names=T)

  #if reading in of the csv generates leading columns with numbers 
  #(old rownumbers) these columns are deleted
  if (grepl("^X", colnames(evaluationmatrix)[1])==TRUE) {
    columns.delete <- 1:(grep("^word$", colnames(evaluationmatrix))-1)
    evaluationmatrix <- evaluationmatrix[,-columns.delete]
  }
  
  #count the number of text windows
  text.windows.num <- ncol(evaluationmatrix)-2
  
  #find additional stopwords to be deleted and
  #specific combinations of words and categories
  #that emerged as unsuitable combinations during analysis of data
  delete.rows <- c(which(as.character(evaluationmatrix[,"word"]) %in% stopwords.additional),
                   
                   intersect(grep("^Haushalt$|^Kind$|^Verbind$|^Materiali$|^Zusammenstell$|^Raeum$|^Unterlag$|^Buero$|^Bueros$|^Lieg$|^Tier$|^Abnehm$", evaluationmatrix[,"word"]), 
                             grep("CF", evaluationmatrix[,"category"])),
                   
                   intersect(grep("^Bau$|^Werk$", evaluationmatrix[,"word"]), 
                             grep("Agriculture", evaluationmatrix[,"category"])),
                   
                   intersect(grep("^Baeum$", evaluationmatrix[,"word"]), 
                             grep("intermediate_products", evaluationmatrix[,"category"])),
                   
                   intersect(grep("Vermi", evaluationmatrix[,"word"]), 
                             grep("sufficiency_avoidance", evaluationmatrix[,"category"]))
                   
  )
  
  #delete
  if (length(delete.rows)>0) {
    delete.rows <- unique(delete.rows)
    evaluationmatrix <- evaluationmatrix[-delete.rows,]
  }
  
  
  #delete the words column since from this point only categories will be analyzed
  evaluationmatrix <- evaluationmatrix[,-c(1)]
  
  #aggregate the category lines -> count the overall occurrence of the categories
  evaluationmatrix <- aggregate(evaluationmatrix[2:ncol(evaluationmatrix)], by=list(category=evaluationmatrix$category), FUN=sum)
  
  #make matrix boolean (yes/no occurrence)
  evaluationmatrix[,c(2:ncol(evaluationmatrix))] <- ifelse(evaluationmatrix[,c(2:ncol(evaluationmatrix))]>0,1,0)
  
  #extract case name from filename and write it into the overall results matrix
  colnames(results.occ.final)[t] <- gsub(paste("(^.*GER__)([A-Za-z]+)(__.*$)"), "\\2",files[t])
  
  
  for(i in seq(length(categories))) {
    
    
    category <- categories[i]
    
    category.subset <- evaluationmatrix[which(evaluationmatrix[,"category"] %in% category),]
    
    category.subset.counts <- category.subset[,c(2:ncol(category.subset))]
    
    count <- colSums(category.subset.counts)
    #only "boolean-counting"/"occurrence at all" applied, 
    #word frequency within a measure is not used for evaluation
    #therefore all numbers larger than 0 are converted to 1
    count <- ifelse(count>0,1,0)
    result <- round(sum(count)/text.windows.num, digits=4)
    
    results.occ.final[category,t] <- result
  }
}

#add MEAN over all columns as last column 
results.occ.final  <- cbind(results.occ.final, round((rowSums(results.occ.final)/ncol(results.occ.final)), digits=4))
colnames(results.occ.final)[ncol(results.occ.final)] <- "mean"

##~-----------------------------------------


##+ RENAME CATEGORIES AND SELECT CATEGORY SETS FOR PLOTTING---------------------------------

#change category names to thesaurus style with angle quotes
row.names(results.occ.final) <- gsub("--","><",row.names(results.occ.final) )
row.names(results.occ.final) <- paste0("<",row.names(results.occ.final) )
row.names(results.occ.final) <- paste0(row.names(results.occ.final), ">" )


##+ THESAURUS 1 - SOCIAL SYSTEM
categories.exclude <- grep("<fed_state><ger>", rownames(results.occ.final), ignore.case = T)
results.occ.final <- results.occ.final[-categories.exclude,]

categories.select1 <- grep("<1>", rownames(results.occ.final), ignore.case = T)
results.occ.social <- results.occ.final[categories.select1,]
##~

##+ THESAURUS 2 - ENERGY SYSTEM
categories.select2 <- grep("<2>|<economy>|<industry>|<commerce>|<mobility_sector>|<local_administration_bodies>|<infrastructure>|<Food>|<Residents>", rownames(results.occ.final), ignore.case = T)
results.occ.energy <- results.occ.final[categories.select2,]

#rename categories that have been included from <1> into <2>
row.names(results.occ.energy) <- gsub("<1><SOC>","",row.names(results.occ.energy))
row.names(results.occ.energy) <- gsub("<2><ENG>","",row.names(results.occ.energy))

#exclude some categories not suitable or important for analysis
categories.exclude <- grep("<carrier>|<rental_of_houses>|<social_groups>|<general_reference><saving>|<building_parts_materials>|<Energy><unspecific_reference>|<Energy_Form><unspecific_reference>|<Economy><commerce><sector_unspecific>|<Infrastructure><disposal_unspecific>|<detailed_PPtechnology>|<alternative_wo_bike>|<tariffs_standing_orders>|<Economy><general_reference><employment_workplace>|<Infrastructure><unspecific>|<Infrastructure><supply_unspecific>|<Economy><general_reference><economic_viability>|<Economy><service><personal_services_crafting>", rownames(results.occ.energy), ignore.case = T)
results.occ.energy <- results.occ.energy[-categories.exclude,]

##+  THESAURUS 3 - SUSTAINABILITY
categories.select3 <- grep("<3>|biofuels|renewable|pedestrian|insulation|car_sharing|public_transport|storage", rownames(results.occ.final), ignore.case = T)
results.occ.sustainability <- results.occ.final[categories.select3,]
##~

##~-----------------------------------

##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##END <<<<<<<<<<<<<<<<<<<
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##START <<<<<<<<<<<<< BOXPLOT OF OCCURRENCES OF CATEGORIES IN ENERGY SYSTEM THESAURUS
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

##+ SET ORDER OF CATEGORIES FOR PLOTTING-------------------------
main.categories <- c("<resources><unsp",
                     "(<resources>)(?!<unsp)",
                     "<conversion><unsp", 
                     "(<conversion>)(?!<unsp)",
                     "<distribution><unsp", 
                     "(<distribution>)(?!<unsp)",
                     "<sales_contracts><unsp",
                     "(<sales_contracts>)(?!<unsp)",
                     "<Technology_Option>",
                     "<Energy_Form>",
                     "<end_use><consumption>",
                     "<mobility>(?!<frei)",
                     "<mobility><frei",
                     "<building><unsp",
                     "(<building>)(?!<unsp)",
                     "<electric_application>",
                     "<local_administration_bodies>",
                     "<Mobility_Sector>",
                     "<Infrastructure>",
                     "<Residents>",
                     "<Food>",
                     "<Economy><unspec",
                     "<Economy><service>",
                     "<commerce>",
                     "<industry><unsp",
                     "(<industry>)(?!<unsp)"
)

main.categories.order <- unlist(lapply(main.categories, function(item) {
  
  grep(item, rownames(results.occ.energy), ignore.case=T, perl=T)
  
}))

results.occ.energy <- results.occ.energy[main.categories.order,]
##~------------------------------


##+ WRITE ORDERED ENERGY SYSTEM CATEGORY NAMES INTO FILE----------------------------
categories.ENG <- rownames(results.occ.energy)

categories.ENG <- gsub("(^<[\\d]>)(<[\\w]{3}>)(.*$)", "\\3", categories.ENG, perl=T)
categories.ENG <- gsub("(^<[\\w]+>)(<.*$)", "\\1~#~\\2", categories.ENG, perl=T)

separator <- "~#~"
categories.ENG <- do.call(rbind,strsplit(categories.ENG, separator, perl=T))

wd <- getwd()
setwd(wd.final)
write.csv(categories.ENG, paste("categories_ENG_nMeta_",
                                length(unique(categories.ENG[,1])),
                                "_nCat_",
                                length(categories.ENG[,2]),
                                ".csv"))
setwd(wd)
##~----------------------------



##+ CHECK MEDIAN VALUES OF CATEGORIES------------------------
results.occ.energy.median <-  results.occ.energy[, setdiff(colnames(results.occ.energy), c("mean"))]
results.occ.energy.median <- as.data.frame(apply(results.occ.energy.median,1,function(x) median(x)))
colnames(results.occ.energy.median) <- "median"

rownames(results.occ.energy[which(results.occ.energy[,"mean"] < 0.01),])

wd <- getwd()
setwd(wd.interim)
oc_lower_1p <- rownames(results.occ.energy[which(results.occ.energy[,"mean"] < 0.01),])
writeLines(noquote(oc_lower_1p), "categories_occurrence_lower_1_percent.txt")
setwd(wd.final)
write.csv(results.occ.energy.median, "categories_energy_occurrence_median.csv")
setwd(wd)
##~----------------------------


##+ FORMAT RESULTS TO BE PLOTTED WITH GGPLOT (LONG FORMAT, ROWNAMES)---------------------------
results.occ.energy.plotformat <- t(results.occ.energy)

results.occ.energy.plotformat  <- as.data.frame(results.occ.energy.plotformat )

results.occ.energy.plotformat  <- cbind(rownames(results.occ.energy.plotformat ), results.occ.energy.plotformat )
colnames(results.occ.energy.plotformat )[1] <- "case"
rownames(results.occ.energy.plotformat ) <- NULL

results.occ.energy.plotformat <- melt(results.occ.energy.plotformat , id.vars= c("case"))

colnames(results.occ.energy.plotformat)[2] <- "category"

results.occ.energy.plotformat <- subset(results.occ.energy.plotformat, results.occ.energy.plotformat$case != "mean")

row.names(results.occ.energy.plotformat) <- 1:nrow(results.occ.energy.plotformat)

results.occ.energy.plotformat$category <- factor(results.occ.energy.plotformat$category, levels = unique(results.occ.energy.plotformat$category))


results.occ.energy.plotformat <- results.occ.energy.plotformat[nrow(results.occ.energy.plotformat):1,]
##~-------------------------



##+ CALCULATE ADDITIONAL PLOTTING PARAMETERS (POSITION OF VERTICAL LINES)---------------
vertical.line <- unlist(lapply(main.categories, function(item) {
  
  max(grep(item, unique(results.occ.energy.plotformat$category), perl=T, ignore.case=T))
  
})) 
vertical.line <- vertical.line[-length(vertical.line)]
vertical.line  <- vertical.line+0.5

##~---------------


##~ CREATE BOXPLOT VIA GGPLOT AS GRAPHICAL OBJECT------------------------------
x.axis.label.size <- c(10)
y.axis.label.size <- c(11)
y.axis.title.size <- c(11)

p <- ggplot(results.occ.energy.plotformat, aes(factor(category), value), stat = "identity") +
  
  geom_boxplot(outlier.colour = "black") +
  
  ylab("average normalized occurrence")+
  xlab("") +
  
  
  #theme black and white
  theme_bw() +
  
  #don´t show legend
  theme(legend.position = "none") +
  
  #eliminates background, gridlines, chart border
  theme(plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_rect(colour = "black")
        ,axis.line = element_line(colour = "black")
        ,plot.title = element_blank()
        ,plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")
        
  ) +
  
  
  # Rotate x-axis labels
  theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.15, size=10)
        ,axis.title.x = element_text(size=10)
        
        #set y-axis tick sizes
        ,axis.text.y = element_text(size=10, angle=0, vjust=0.25, hjust=0)
        #,axis.title.y = element_text(size=5)
        
  ) +
  
  #set limits of the y axis
  scale_y_continuous(expand = c(0,0), limits = c(0,1)) + 
  
  #set distance of y axis label to the axis
  theme(axis.title.y = element_text(vjust=1.1, size=11)) +
  
  
  theme(aspect.ratio=3) +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(results.occ.energy.plotformat$category))) +
  
  #draw vertical lines, desired lines have to be turned off/on manually
  # geom_vline(aes(xintercept = c(rep(vertical.line[1],  nrow(results.occ.energy.plotformat)))), linetype= "dashed") +
  geom_vline(aes(xintercept = c(rep(vertical.line[2], nrow(results.occ.energy.plotformat)))), linetype= "dashed") +
  geom_vline(aes(xintercept = c(rep(vertical.line[3], nrow(results.occ.energy.plotformat)))), linetype= "dashed") +
  geom_vline(aes(xintercept = c(rep(vertical.line[4], nrow(results.occ.energy.plotformat)))), linetype= "dashed") +
  geom_vline(aes(xintercept = c(rep(vertical.line[5], nrow(results.occ.energy.plotformat)))), linetype= "dashed") +
  geom_vline(aes(xintercept = c(rep(vertical.line[6], nrow(results.occ.energy.plotformat)))), linetype= "dashed") +
  geom_vline(aes(xintercept = c(rep(vertical.line[7], nrow(results.occ.energy.plotformat)))), linetype= "dashed") +
  geom_vline(aes(xintercept = c(rep(vertical.line[8], nrow(results.occ.energy.plotformat)))), linetype= "dashed") +
  geom_vline(aes(xintercept = c(rep(vertical.line[9], nrow(results.occ.energy.plotformat)))), linetype= "dashed") +
  geom_vline(aes(xintercept = c(rep(vertical.line[10], nrow(results.occ.energy.plotformat)))), linetype= "dashed") +
  geom_vline(aes(xintercept = c(rep(vertical.line[11], nrow(results.occ.energy.plotformat)))), linetype= "dashed") +
  geom_vline(aes(xintercept = c(rep(vertical.line[12], nrow(results.occ.energy.plotformat)))), linetype= "dashed") +
  geom_vline(aes(xintercept = c(rep(vertical.line[13], nrow(results.occ.energy.plotformat)))), linetype= "dashed") +
  geom_vline(aes(xintercept = c(rep(vertical.line[14], nrow(results.occ.energy.plotformat)))), linetype= "dashed") +
  geom_vline(aes(xintercept = c(rep(vertical.line[15], nrow(results.occ.energy.plotformat)))), linetype= "dashed") +
  geom_vline(aes(xintercept = c(rep(vertical.line[16], nrow(results.occ.energy.plotformat)))), linetype= "dashed")
#geom_vline(aes(xintercept = c(rep(vertical.line[17], nrow(results.occ.energy.plotformat)))), linetype= "dashed") +
#geom_vline(aes(xintercept = c(rep(vertical.line[18], nrow(results.occ.energy.plotformat)))), linetype= "dashed") +

p
##~---------------------------------------------------------------

##+ WRITE RESULT PLOTS INTO FILE------------------------
setwd(wd.final)

par(mar=c(0.5,0.5,0.5,0.5))
scale_figure <- c(9,6.5)

win.metafile("Figure_1_energy_system_occurrence_boxplot.emf", height=scale_figure[1] ,width=scale_figure[2])
p
dev.off()


postscript("Figure_1_energy_system_occurrence_boxplot.eps", height=scale_figure[1] ,width=scale_figure[2])
p
dev.off()
##~-----------------------------------------------

##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##END <<<<<<<<<<<<<
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##START <<<<<<<<<<<<< BOXPLOT THESAURUS ONE; SOCIAL SYSTEM
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

##+ BOXPLOT THESAURUS ONE-----------------------------------------------
#details on the following code steps are given above section concerning the
#BOXPLOT ENERGY SYSTEM
#the following lines are analogous

results.occ.social.plotformat <- results.occ.social[order(rownames(results.occ.social)),]

results.occ.social.plotformat <- t(results.occ.social.plotformat)

results.occ.social.plotformat <- as.data.frame(results.occ.social.plotformat)

results.occ.social.plotformat <- cbind(rownames(results.occ.social.plotformat), results.occ.social.plotformat)
colnames(results.occ.social.plotformat)[1] <- "case"
rownames(results.occ.social.plotformat) <- NULL

results.occ.social.plotformat <- melt(results.occ.social.plotformat, id.vars= c("case"))

colnames(results.occ.social.plotformat)[2] <- "category"

results.occ.social.plotformat <- subset(results.occ.social.plotformat, results.occ.social.plotformat$case != "mean")

row.names(results.occ.social.plotformat) <- 1:nrow(results.occ.social.plotformat)

results.occ.social.plotformat$category <- factor(results.occ.social.plotformat$category, levels = unique(results.occ.social.plotformat$category))


results.occ.social.plotformat <- results.occ.social.plotformat[nrow(results.occ.social.plotformat):1,]


vertical.line <- unlist(lapply(main.categories, function(item) {
  
  max(grep(item, unique(results.occ.social.plotformat$category), perl=T, ignore.case=T))
  
})) 
vertical.line <- vertical.line[-length(vertical.line)]
vertical.line  <- vertical.line+0.5


x.axis.label.size <- c(10)

y.axis.label.size <- c(10)
y.axis.title.size <- c(10)


p <- ggplot(results.occ.social.plotformat, aes(factor(category), value), stat = "identity") +
  
  geom_boxplot(outlier.colour = "black") + 
  
  
  ylab("average normalized occurrence")+
  xlab("") +
  
  
  #theme black and white
  theme_bw() +
  
  #eliminates background, gridlines, and chart border
  theme(plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_rect(colour = "black")
        ,axis.line = element_line(colour = "black")
        ,plot.title = element_blank()
        ,plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")
        
  ) +
  
  
  # Rotate x-axis labels
  theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.15, size=7)
        ,axis.title.x = element_text(size=7)
        
        #set y-axis tick sizes
        ,axis.text.y = element_text(size=7, angle=0, vjust=0.25, hjust=0)
        
        
  ) +
  
  #set the limits of the y axis
  scale_y_continuous(expand = c(0,0), limits = c(0,1)) + 
  
  #set distance of y axis label to the axis
  theme(axis.title.y = element_text(vjust=1.1, size=10)) +
  
  theme(aspect.ratio=4.5) +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(results.occ.social.plotformat$category))) + #this generally works but the color bars that were introduced are not reversed
  
  #don´t show legend
  theme(legend.position = "none")

p



setwd(wd.final)

par(mar=c(0.5,0.5,0.5,0.5))
scale_figure <- c(9,5.5)


win.metafile("ESM_S9_societal_subsystems_occurrence_boxplot.emf", height=scale_figure[1] ,width=scale_figure[2])
p
dev.off()


postscript("ESM_S9_societal_subsystems_occurrence_boxplot.eps", height=scale_figure[1] ,width=scale_figure[2])
p
dev.off()
##~-----------------------------------------------

##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##END <<<<<<<<<<<<< BOXPLOT THESAURUS ONE; SOCIAL SYSTEM
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<



##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##START <<<<<<<<<<<<< BOXPLOT THESAURUS THREE , SUSTAINABILITY
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

##+ BOXPLOT THESAURUS THREE--------------------------
#details on code see
#BOXPLOT ENERGY SYSTEM

results.occ.sustainability.plotformat <- results.occ.sustainability[order(rownames(results.occ.sustainability)),]


#rename categories that have been included from the second thesaurus
rownames(results.occ.sustainability.plotformat) <- gsub("<2><ENG><Resources><renewable>" , 
                                                        "<3><SUS><Consistency><Resources><renewable>",
                                                        rownames(results.occ.sustainability.plotformat))

rownames(results.occ.sustainability.plotformat) <- gsub("<2><ENG><Conversion><renewable>", 
                                                        "<3><SUS><Consistency><Conversion><renewable>",
                                                        rownames(results.occ.sustainability.plotformat))

rownames(results.occ.sustainability.plotformat) <- gsub("<2><ENG><Sales_Contracts><renewable>" , 
                                                        "<3><SUS><Consistency><Sales_Contracts><renewable>",
                                                        rownames(results.occ.sustainability.plotformat))

rownames(results.occ.sustainability.plotformat) <- gsub("<2><ENG><End_Use><mobility><biofuels>", 
                                                        "<3><SUS><Consistency><End_Use><mobility><biofuels>",
                                                        rownames(results.occ.sustainability.plotformat))

rownames(results.occ.sustainability.plotformat) <- gsub("<2><ENG><Technology_Option><storage>", 
                                                        "<3><SUS><Efficiency><Technology_Option><storage>",
                                                        rownames(results.occ.sustainability.plotformat))

rownames(results.occ.sustainability.plotformat) <- gsub("<2><ENG><End_Use><building><insulation>" , 
                                                        "<3><SUS><Efficiency><End_Use><building><insulation>",
                                                        rownames(results.occ.sustainability.plotformat))

rownames(results.occ.sustainability.plotformat) <- gsub("<2><ENG><End_Use><mobility><car_sharing>" , 
                                                        "<3><SUS><Sufficiency><End_Use><mobility><car_sharing>",
                                                        rownames(results.occ.sustainability.plotformat))


rownames(results.occ.sustainability.plotformat) <- gsub("<2><ENG><End_Use><mobility><bike_pedestrian>", 
                                                        "<3><SUS><Sufficiency><End_Use><mobility><bike_pedestrian>",
                                                        rownames(results.occ.sustainability.plotformat))

rownames(results.occ.sustainability.plotformat) <- gsub("<2><ENG><End_Use><mobility><public_transport>", 
                                                        "<3><SUS><Sufficiency><End_Use><mobility><public_transport>",
                                                        rownames(results.occ.sustainability.plotformat))



categories.exclude <- grep("<Sustainability><unspecific_reference>|Uncertainty><Uncertainty_Risks_Accidents>", rownames(results.occ.sustainability.plotformat), ignore.case = T)
results.occ.sustainability.plotformat <- results.occ.sustainability.plotformat[-categories.exclude,]



sust.categories <- c("<Sufficiency", "<Efficiency", "<Consistency")

sust.categories.order <- unlist(lapply(sust.categories, function(item) {
  
  grep(item, rownames(results.occ.sustainability.plotformat), perl=T, ignore.case = T)
  
  
}))

results.occ.sustainability.plotformat <- results.occ.sustainability.plotformat[sust.categories.order,]

row.names(results.occ.sustainability.plotformat) <- gsub("<3><SUS>","",row.names(results.occ.sustainability.plotformat))




results.occ.sustainability.plotformat <- t(results.occ.sustainability.plotformat)

results.occ.sustainability.plotformat <- as.data.frame(results.occ.sustainability.plotformat)

results.occ.sustainability.plotformat <- cbind(rownames(results.occ.sustainability.plotformat), results.occ.sustainability.plotformat)
colnames(results.occ.sustainability.plotformat)[1] <- "case"
rownames(results.occ.sustainability.plotformat) <- NULL

results.occ.sustainability.plotformat <- melt(results.occ.sustainability.plotformat, id.vars= c("case"))

colnames(results.occ.sustainability.plotformat)[2] <- "category"

results.occ.sustainability.plotformat <- subset(results.occ.sustainability.plotformat, results.occ.sustainability.plotformat$case != "mean")

row.names(results.occ.sustainability.plotformat) <- 1:nrow(results.occ.sustainability.plotformat)

results.occ.sustainability.plotformat$category <- factor(results.occ.sustainability.plotformat$category, levels = unique(results.occ.sustainability.plotformat$category))


results.occ.sustainability.plotformat <- results.occ.sustainability.plotformat[nrow(results.occ.sustainability.plotformat):1,]



vertical.line <- unlist(lapply(sust.categories, function(item) {
  
  max(grep(item, unique(results.occ.sustainability.plotformat$category), perl=T, ignore.case=T))
  
})) 
vertical.line <- vertical.line[-length(vertical.line)]
vertical.line  <- vertical.line+0.5


x.axis.label.size <- c(10)

y.axis.label.size <- c(10)
y.axis.title.size <- c(10)

p <- ggplot(results.occ.sustainability.plotformat, aes(factor(category), value), stat = "identity") +
  
  geom_boxplot(outlier.colour = "black") +
  
  
  ylab("average normalized occurrence")+
  xlab("") +
  
  #theme black and white
  theme_bw() +
  
  #eliminates background, gridlines, and chart border
  theme(plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_rect(colour = "black")
        ,axis.line = element_line(colour = "black")
        ,plot.title = element_blank()
        ,plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")
        
  ) +
  
  
  # Rotate x-axis labels
  theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.15, size=14)
        ,axis.title.x = element_text(size=14)
        
        #set y-axis tick sizes
        ,axis.text.y = element_text(size=14, angle=0, vjust=0.25, hjust=0)
        
        
  ) +
  
  
  
  #set the limits of the y axis
  scale_y_continuous(expand = c(0,0), limits = c(0,1)) + 
  
  #set distance of y axis label to the axis
  theme(axis.title.y = element_text(vjust=1.1, size=10)) +
  
  
  
  theme(aspect.ratio=4.5) +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(results.occ.sustainability.plotformat$category))) + #this generally works but the color bars that were introduced are not reversed
  
  #don´t show legend
  theme(legend.position = "none")

p



setwd(wd.final)

par(mar=c(0.5,0.5,0.5,0.5))
scale_figure <- c(10,8)


win.metafile("sustainability_aspects_occurrence_boxplot.emf", height=scale_figure[1] ,width=scale_figure[2])
p
dev.off()


postscript("sustainability_aspects_occurrence_boxplot.eps", height=scale_figure[1] ,width=scale_figure[2])
p
dev.off()
##~-----------------------------------
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##END <<<<<<<<<<<<< 
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<



##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##START <<<<<<<<<<<<< PLOT COOCCURRENCE MATRICES
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


#COOCCURRENCE IN WIDE FORMAT

setwd(wd.interim)
categories <- list.dirs(wd.wordlists, recursive=F, full.names = F)
categories.num <-length(categories)

cooccurrence.filename.tag <- c("__cat_coocc_mat")
files <- list.files(pattern = paste0(cooccurrence.filename.tag, "\\.csv"))

#fix the categories of the mean case as reference for other cases to be compared later
cooccurrence.case.reference <- grep("Lower_Saxony_regional_centers", files)

##+READ DATA-----------------
cooccurrence.case <- read.csv(files[cooccurrence.case.reference], header=T, check.names = T)

#if reading in of the csv generates columns with numbers (old rownumbers) these are deleted
delete.columns <- grep("(X$)|(X\\.)(\\d+)($)", colnames(cooccurrence.case), perl=T)

if (length(delete.columns) > 0) {
  row.names(cooccurrence.case) <- as.character(cooccurrence.case[, grep("^X$", colnames(cooccurrence.case))])
  cooccurrence.case <- cooccurrence.case[,-delete.columns]
  colnames(cooccurrence.case) <- gsub("^X", "",  colnames(cooccurrence.case))
  colnames(cooccurrence.case) <- gsub("\\.", "-",  colnames(cooccurrence.case))
}


if (identical(as.character( colnames(cooccurrence.case)), as.character( row.names(cooccurrence.case))) == FALSE) {
  warning("Matrix not symmetric. Following calculations will produce wrong results.")
}
##~-------------------------


##+RENAME CATEGORIES---------------------------------
row.names(cooccurrence.case) <- gsub("--","><",row.names(cooccurrence.case) )
row.names(cooccurrence.case) <- paste0("<",row.names(cooccurrence.case) )
row.names(cooccurrence.case) <- paste0(row.names(cooccurrence.case), ">" )
colnames(cooccurrence.case) <- row.names(cooccurrence.case)
##~-------------------


##~ ORDER CATEGORIES (ALIGN CATEGORIES WITH SAME METACATEGORY)----------------
ordered.levels.alphabetic <- order(colnames(cooccurrence.case))
cooccurrence.case <- cooccurrence.case[ordered.levels.alphabetic,ordered.levels.alphabetic]


order.interim <- data.frame(category = as.character(row.names(cooccurrence.case)),
                            value = as.numeric(rowSums(cooccurrence.case)) )

#aggregate values by meta-category
order.interim.agg <- order.interim

order.interim.agg[,1] <-  gsub("(<[\\d]>)(<[\\w]{3}>)(<[\\w]+>)(.*$)", "\\3",order.interim.agg[,1] , perl=T)
order.interim.agg <- aggregate(order.interim.agg[,2], by=list(category=order.interim.agg$category), FUN=mean)

#replace the original values for each category 
#by the aggregated sum of its meta category in the interim list
for (c in 1:nrow(order.interim.agg)) {
  replace <- order.interim.agg[c,]
  order.interim[grep(replace[,1], order.interim[,1]),2] <- replace[,2]
  
}

order.cluster.occ <- order(order.interim[,2], decreasing=T)

cooccurrence.case <- cooccurrence.case[order.cluster.occ ,order.cluster.occ]
##~--------------------


##+EXCLUDE CATEGORIES WITH VERY LOW OCCURRENCE AND APPLY SCALING TO MATRIX--------------------
x <- round(as.matrix(cooccurrence.case), d=3)

#for the unscaled plot
x_full <- x

#for the sustainabilty cooccurrence plot
x_sust <- x


oc_lower_1p <- which(as.numeric(diag(x)) < 0.01)
#rownames(x[oc_lower_1p,oc_lower_1p])
x <- x[-oc_lower_1p,-oc_lower_1p]



##+ SCALE BY MINIMUM VALUES OF DIAGONAL
x.diag <- as.numeric(diag(x))

for (d in seq(length(x.diag))) {
  
  x.diag.value <- cbind(x.diag, rep(as.numeric(x.diag[d]), length(x.diag)))
  
  x.diag.value <- apply(x.diag.value,1, min)
  
  x[,d] <- round(x[,d]/x.diag.value,d=3)
}

x <- ifelse(is.na(x), 0, x)
##~
##~----------------------------


##+ RENAME CATEGORIES, CLASSIFY; SELECT CATEGORIES ABOVE THRESHOLD VALUE FOR BEING PLOTTED-------------
x <- x[grep("<2>|<economy>|<industry>|<commerce>|<mobility_sector>|<local_administration_bodies>|<infrastructure>|<Food>|<Residents>", rownames(x), ignore.case = T), 
       -grep("<3>|<2><ENG>", colnames(x),ignore.case=T, perl=T)]

#exclude categories from y-axis
rows.exclude <- grep("<carrier>|<rental_of_houses>|<social_groups>|<general_reference><saving>|<building_parts_materials>|<Energy><unspecific_reference>|<Energy_Form><unspecific_reference>|<Economy><commerce><sector_unspecific>|<Infrastructure><disposal_unspecific>|<detailed_PPtechnology>|<alternative_wo_bike>|<tariffs_standing_orders>|<Economy><general_reference><employment_workplace>|<Infrastructure><unspecific>|<Infrastructure><supply_unspecific>|<Economy><general_reference><economic_viability>|<Economy><service><personal_services_crafting>", rownames(x), ignore.case = T)
if (length(rows.exclude) > 0) {
  x <- x[-rows.exclude,]
}


#exclude categories from x-axis
columns.exclude <- grep("<Spatial_scale><fed_state><ger>", colnames(x), ignore.case = T)
if (length(columns.exclude) > 0) {
  x <- x[,-columns.exclude]
}



##+ SELECT ROWS THAT ACHIEVED AVERAGE COOCURRENCE OF X%
columns.maxima <- apply(x, 2, sum)
average.sum.required.minimum <- nrow(x)*0.20
columns.maxima <- which(columns.maxima >= average.sum.required.minimum)
x <- x[,columns.maxima]
##~

##+ CLASSIFIY
classes <- c(0,0.25,0.5,0.75,1)
x <- ifelse(is.na(x), 0, x)
x <- ifelse(x <= classes[1], 0, x)
x <- ifelse((x > classes[1] & x < classes[2]), 0.05, x)
x <- ifelse((x >= classes[2] & x < classes[3]), 0.375,x)
x <- ifelse((x >= classes[3] & x < classes[4]), 0.625,x)
#x <- ifelse((x >= classes[4] & x < classes[5]), 0.7,x)
x <- ifelse((x >= classes[4]), 0.875,x)

columns.maxima <- apply(x, 2, function(item) length(which(item >=0.625)))
columns.maxima <- which(columns.maxima >1)

x <- x[,columns.maxima]
##~

setwd(wd.final)
writeLines(noquote(colnames(x)), "systems_with_at_least_one_L3_with_energy_system.txt")
##~-----------------------------


##+ SET ORDER OF CATEGORIES FOR PLOTTING AND RENAME CATEGORIES-------------------------
main.categories <- c("<resources><unsp", 
                     "<resources>(?!<unsp)",
                     "<conversion><unsp",
                     "<conversion>(?!<unsp)",
                     "<distribution><unsp", 
                     "<distribution>(?!<unsp)", 
                     "<sales_contracts><unsp",
                     "<sales_contracts>(?!<unsp)",
                     "<Technology_Option>",
                     "<Energy_Form>",
                     "<end_use><consumption>",
                     "<mobility>(?!<frei)",
                     "<mobility><frei",
                     "<building><unsp",
                     "<building>(?!<unsp)",
                     "<electric_application>",
                     "<local_administration_bodies>",
                     "<Mobility_Sector>",
                     "<Infrastructure>",
                     
                     "<Residents>",
                     "<Food>",
                     "<Economy><unspec",
                     "<Economy><service>",
                     "<commerce>",
                     "<industry><unsp",
                     "(<industry>)(?!<unsp)"
)


main.categories.order <- unlist(lapply(main.categories, function(item) {
  
  grep(item, rownames(x), perl=T, ignore.case = T)
  
  
}))

x <- x[rev(main.categories.order),order(colnames(x))]

row.names(x) <- gsub("<1><SOC>","",row.names(x))
row.names(x) <- gsub("<2><ENG>","",row.names(x))
##~------------------------------


##+ FORMAT MATRIX FOR PLOTTING (LONG FORMAT)--------------------
cooccurrence.case.plotformat <- t(round(x, d=3))

category.1 <- rownames(cooccurrence.case.plotformat)
cooccurrence.case.plotformat <- cbind.data.frame(category.1, cooccurrence.case.plotformat)
row.names(cooccurrence.case.plotformat) <- NULL

cooccurrence.case.plotformat.melted <- melt(cooccurrence.case.plotformat, id.vars=c("category.1"),
                                            #source columns
                                            measure.vars= colnames(cooccurrence.case.plotformat)[2:ncol(cooccurrence.case.plotformat)],
                                            
                                            #name of the destination column
                                            variable.name = "category.2", 
                                            value.name = "strength_of_link_scaling_option_1",
                                            na.rm = FALSE)


cooccurrence.case.plotformat <- cooccurrence.case.plotformat.melted

cooccurrence.levels <- c(0.000, seq(1000)/1000)

cooccurrence.case.plotformat$strength_of_link_scaling_option_1 <- factor( cooccurrence.case.plotformat$strength_of_link_scaling_option_1, levels = cooccurrence.levels[order(cooccurrence.levels)])

cooccurrence.case.plotformat$category.1 <- factor(cooccurrence.case.plotformat$category.1, levels = unique(as.character(cooccurrence.case.plotformat$category.1)))
cooccurrence.case.plotformat$category.2 <- factor(cooccurrence.case.plotformat$category.2,  levels = unique(as.character(cooccurrence.case.plotformat$category.2)))
##~--------------------


##+ CALCULATE ADDITIONAL PLOTTING OPTIONS (VERTICAL LINES)------------------------
main.categories.reduced <- c( "resources",         
                              
                              "<conversion>", 
                              "<distribution>", 
                              "<sales_contracts>",
                              "<Technology_Option>",
                              "<Energy_Form>", 
                              "<end_use><consumption>",
                              "<mobility>",
                              "<building>",
                              "<electric_application>",
                              "<local_administration_bodies>", 
                              "<Mobility_Sector>",
                              "<Infrastructure>",
                              
                              "<Residents>",
                              "<Food>",
                              "<Economy>"
                              
)


##+ VERTICAL LINES
line.position <- unlist(lapply(main.categories.reduced, function(item) {
  
  min(grep(item, as.character(unique(cooccurrence.case.plotformat$category.2)), perl=T, ignore.case = T))
  
  
}))
line.position<- line.position-1

#position of lines to be drawn between systems
system.levels <- unique(as.character(cooccurrence.case.plotformat$category.1)) 
system.levels <- unique(gsub("(<[\\d]>)(<[\\w]{3}>)(<[\\w]+>)(.*$)", "\\3", system.levels , perl=T))

line.position.v <- unlist(lapply(system.levels, function(item) {
  
  min(grep(item, as.character(unique(cooccurrence.case.plotformat$category.1)), perl=T, ignore.case = T))
  
  
}))
line.position.v <- line.position.v-1
##~



cooccurrence.case.plotformat$category.1 <- gsub("(^<[\\d]>)(<[\\w]+>)(<.*$)","\\3", as.character(cooccurrence.case.plotformat$category.1), perl=T)
cooccurrence.case.plotformat$category.1 <- factor(cooccurrence.case.plotformat$category.1, levels = unique(as.character(cooccurrence.case.plotformat$category.1)))

cooccurrence.case.plotformat$category.2 <- gsub("(^<[\\d]>)(<[\\w]+>)(<.*$)","\\3", as.character(cooccurrence.case.plotformat$category.2), perl=T)
cooccurrence.case.plotformat$category.2 <- factor(cooccurrence.case.plotformat$category.2, levels = unique(as.character(cooccurrence.case.plotformat$category.2)))



##+ MARK SELF-REFERENCE (COOCCURRENCE) OF CATEGORIES WITH NAs/CROSSES
energy_categories <- gsub("<End_Use>","", as.character(cooccurrence.case.plotformat$category.2))

energy_categories <- as.character(cooccurrence.case.plotformat$category.2)
subsystem_categories <- as.character(cooccurrence.case.plotformat$category.1)


#get categories which co-occur with themselves in the graph to mark
#the respective positions with a cross
self_cooc <- which(subsystem_categories == energy_categories)
#cooccurrence.case.plotformat[self_cooc,] 
#head(cooccurrence.case.plotformat)
cooccurrence.case.plotformat[self_cooc,"strength_of_link_scaling_option_1"] <- NA

na <- rep(FALSE, nrow(cooccurrence.case.plotformat))
cooccurrence.case.plotformat <- cbind(cooccurrence.case.plotformat, na)
cooccurrence.case.plotformat[self_cooc,"na"] <- TRUE

self_cooc_categories <- cooccurrence.case.plotformat[self_cooc,c(1,2)]

#full list of categories of x and y axis with numbering
cat_y <-   cbind(unique(as.character(cooccurrence.case.plotformat$category.2)),
                 1:length( unique(as.character(cooccurrence.case.plotformat$category.2))))

cat_x <-    cbind(unique(as.character(cooccurrence.case.plotformat$category.1)), 
                  1:length(unique(as.character(cooccurrence.case.plotformat$category.1))))


#leave only those categories which match with the self cooccurring categories
cat_x <- cat_x[!is.na(match(cat_x[,1], self_cooc_categories[,1])), ]
cat_y <- cat_y[!is.na(match(cat_y[,1], self_cooc_categories[,2])), ]

order_y <- unlist(lapply(cat_x[,1], function(item) {
  
  grep(item, cat_y[,1])
  
}))

cat_y <- cat_y[order_y,]


na_positions <- as.data.frame(cbind(
  as.integer(cat_x[,2])-0.5,
  as.integer(cat_y[,2])-0.5
))
##~
##~------------------------


##+ COOCCURRENCE PLOT ENERGY / SOCIAL SYSTEM--------------------
axis.label.size <- 8

p_scaled_cooc <- ggplot(NULL) +
  
  #raster with no distance to the axis
  geom_raster(data= cooccurrence.case.plotformat, aes(x = category.1, y = category.2, fill = strength_of_link_scaling_option_1), 
              hjust = 0, vjust = 0) +  
  
  
  xlab("")+
  ylab("")+
  
  #set colour scale for displaying values
  scale_fill_manual(values = c("white", "grey95", "grey75", "grey30", "black"),
                    labels = c("0", "L1","L2","L3","L4 (self-reference: white cross)"),
                    na.value="black") +
  
  #remove labels
  labs(x = element_blank(), y=element_blank()) +
  
  #set basic theme of the plot
  theme_bw() +
  
  theme(
    
    plot.background = element_blank(),
    
    plot.title = element_blank(),
    
    aspect.ratio = round(length(unique(as.character(cooccurrence.case.plotformat$category.2)))/length(unique(as.character(cooccurrence.case.plotformat$category.1)))),
    
    plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),
    
    #rotate x-axis label, and set distance of label/axis to zero
    axis.text.x = element_text(angle = 90, hjust = 0, vjust=-0.25, size=axis.label.size),
    axis.text.y = element_text(vjust = .8, size=axis.label.size, hjust = 0),
    
    #remove axis title
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    
    axis.ticks = element_line(size=.01),
    axis.ticks.length = unit(.1, "cm"),
    
    
    legend.position = c(-0.5,-0.1),
    legend.key.size = unit(0.25, "cm"),
    legend.title = element_text(size=axis.label.size),
    legend.direction = "vertical",
    legend.background = element_rect(colour = "grey30"),
    legend.key = element_rect(colour = "grey30"),
    legend.text = element_text(size=axis.label.size)
    
    
  ) +
  
  geom_vline(xintercept = line.position.v, colour = "black", linetype= "dashed", size=.01) +
  geom_hline(yintercept = line.position, colour = "black", linetype= "dashed", size=.01) +
  
  geom_point(data = na_positions, aes(x=V1, y=V2, shape=4), color="white", size=4) +
  scale_shape_identity()

p_scaled_cooc 
##~--------------------


##~ SAVE PLOT IN FILE----------------
opar <- par()

par(mar=c(0.1,0.1,0.1,0.1))

setwd(wd.final)

scale_figure <- c(10,6.8)

win.metafile("Figure_2_CoOc_L3_to_Energy_system_and_average_CoOc_0125_scalingOpt1.emf", height=scale_figure[1] ,width=scale_figure[2])
p_scaled_cooc
dev.off()


postscript("Figure_2_CoOc_L3_to_Energy_system_and_average_CoOc_0125_scalingOpt1.eps", height=scale_figure[1] ,width=scale_figure[2])
p_scaled_cooc
dev.off()

par(opar)
##~-----------------------------



##+ PLOT COOCCURRENCE MATRIX WITHOUT ANY SCALING AND CLASSIFICATION---------------------------
# explanations of code see above in plotting cooccurrence of energy system with social system categories

x_full  <- round(x_full, d=3)

x_full <- x_full[order(row.names(x_full)), order(colnames(x_full))]



cooccurrence.case.plotformat.full <- x_full

category.1 <- rownames(cooccurrence.case.plotformat.full)
cooccurrence.case.plotformat.full <- cbind.data.frame(category.1, cooccurrence.case.plotformat.full)
row.names(cooccurrence.case.plotformat.full) <- NULL

cooccurrence.case.plotformat.full.melted <- melt(cooccurrence.case.plotformat.full, id.vars=c("category.1"),
                                                 #source columns
                                                 measure.vars= colnames(cooccurrence.case.plotformat.full)[2:ncol(cooccurrence.case.plotformat.full)],
                                                 
                                                 #name of the destination column
                                                 variable.name = "category.2", 
                                                 value.name = "cooccurrence",
                                                 na.rm = FALSE)


cooccurrence.case.plotformat <- cooccurrence.case.plotformat.full.melted

cooccurrence.levels <- c(0.000, seq(1000)/1000)

cooccurrence.case.plotformat$strength_of_link_scaling_option_1 <- factor(cooccurrence.case.plotformat$strength_of_link_scaling_option_1, levels = cooccurrence.levels[order(cooccurrence.levels)])

cooccurrence.case.plotformat$category.1 <- factor(cooccurrence.case.plotformat$category.1, levels = unique(as.character(cooccurrence.case.plotformat$category.1)))
cooccurrence.case.plotformat$category.2 <- factor(cooccurrence.case.plotformat$category.2,  levels = unique(as.character(cooccurrence.case.plotformat$category.2)))


axis.label.size <- 3

p_full <- ggplot(NULL) +
  
  #raster with no distance to the axis
  geom_raster(data= cooccurrence.case.plotformat, aes(x = category.1, y = category.2, fill = cooccurrence), hjust = 0, vjust = 0) +  
  
  
  xlab("")+
  ylab("")+
  
  
  scale_fill_grey(start = 1, end = 0, na.value = "white") +   
  
  #remove labels
  labs(x = element_blank(), y=element_blank()) +
  
  #set basic theme of the plot
  theme_bw() +
  
  theme(
    axis.text.x = element_text(angle = 90, hjust = 0, vjust=-0.25, size=axis.label.size),
    # 
    axis.text.y = element_text(vjust = .8, hjust = 0, size=axis.label.size),
    
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    
    axis.ticks = element_line(size=.01),
    
    axis.ticks.length = unit(.1, "cm"),
    
    plot.background = element_rect(fill = NULL,colour = NA),
    plot.background = element_blank(),
    
    plot.title = element_blank(),
    
    aspect.ratio = 1,
    
    plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),
    legend.position = "none"#,
    
    
  ) +
  
  geom_vline(xintercept = 1:ncol(x_full), colour = "black", linetype= "dotted", size=.1) +
  geom_hline(yintercept = 1:nrow(x_full), colour = "black", linetype= "dotted", size=.1)


p_full


par(mar=c(0.2,0.2,0.2,0.2))

setwd(wd.final)

pdf("full_cooc_mat_scaled_by_total_max_cooc.pdf")
p_full
dev.off()

win.metafile("full_cooc_mat_scaled_by_total_max_cooc.emf")
p_full
dev.off()

postscript("full_cooc_mat_scaled_by_total_max_cooc.eps")
p_full
dev.off()
##~----------------------

##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##END <<<<<<<<<<<<<
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##START <<<<<<<<<<<<< COOCCURRENCE PLOT ENERGY / SUSTAINABILITY
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


##+EXCLUDE CATEGORIES WITH VERY LOW OCCURRENCE AND APPLY SCALING TO MATRIX-----------
oc_lower_1p_all <- which(as.numeric(diag(x_sust)) < 0.01)
oc_lower_1p_all_names <- rownames(x_sust)[oc_lower_1p_all]
keep <- grep("<3><SUS>|biofuels|storage|renewable|car_sharing|bike_pedestrian|energetic_building_refurbishment", rownames(x_sust))
oc_lower_1p <- setdiff(oc_lower_1p_all, keep)
x_sust <- x_sust[-oc_lower_1p,-oc_lower_1p]


# scale by relative occurrence of sustainability categories in sets of other categories
x_sust.diag <- as.matrix_sust(diag(x_sust))
x_sust <- apply(x_sust, 2, function(column) column/as.numeric(x_sust.diag))
##~------------------------------

##+ CLASSIFY--------------------
classes <- c(0,0.25,0.5,0.75,1)
x_sust <- ifelse(is.na(x_sust), 0, x_sust)
x_sust <- ifelse(x_sust <= classes[1], 0, x_sust)
x_sust <- ifelse((x_sust > classes[1] & x_sust < classes[2]), 0.05, x_sust)
x_sust <- ifelse((x_sust >= classes[2] & x_sust < classes[3]), 0.375,x_sust)
x_sust <- ifelse((x_sust >= classes[3] & x_sust < classes[4]), 0.625,x_sust)
#x_sust <- ifelse((x_sust >= classes[4] & x_sust < classes[5]), 0.7,x_sust)
x_sust <- ifelse((x_sust >= classes[4]), 0.875,x_sust)
##~--------------------

##+ SELECT AND RENAME CATEGORIES------------------------
x_sust <- x_sust[grep("<2>|<economy>|<industry>|<commerce>|<mobility_sector>|<local_administration_bodies>|<infrastructure>|<Food>|<Residents>", rownames(x_sust), ignore.case = T), 
                 grep("<3>|biofuels|renewable|pedestrian|insulation|car_sharing|public_transport|storage", colnames(x_sust),ignore.case=T, perl=T)]



x_sust <- x_sust[-unlist(lapply(oc_lower_1p_all_names, function(item) grep(item, rownames(x_sust)))), ]

x_sust <- x_sust[,-grep("<sustainability>|<Uncertainty>", colnames(x_sust), ignore.case = T)]

#exclude categories from y-axis
rows.exclude <- grep("<carrier>|<rental_of_houses>|<social_groups>|<general_reference><saving>|<building_parts_materials>|<Energy><unspecific_reference>|<Energy_Form><unspecific_reference>|<Economy><commerce><sector_unspecific>|<Infrastructure><disposal_unspecific>|<detailed_PPtechnology>|<alternative_wo_bike>|<tariffs_standing_orders>|<Economy><general_reference><employment_workplace>|<Infrastructure><unspecific>|<Infrastructure><supply_unspecific>|<Economy><general_reference><economic_viability>|<Economy><service><personal_services_crafting>", rownames(x), ignore.case = T)
if (length(rows.exclude) > 0) {
  x <- x[-rows.exclude,]
}

if (length(rows.exclude) > 0) {
  x_sust <- x_sust[-rows.exclude,]
}

colnames(x_sust) <- gsub("<2><ENG><Resources><renewable>" , 
                         "<3><SUS><Consistency><Resources><renewable>",
                         colnames(x_sust))

colnames(x_sust) <- gsub("<2><ENG><Conversion><renewable>", 
                         "<3><SUS><Consistency><Conversion><renewable>",
                         colnames(x_sust))

colnames(x_sust) <- gsub("<2><ENG><Sales_Contracts><renewable>" , 
                         "<3><SUS><Consistency><Sales_Contracts><renewable>",
                         colnames(x_sust))

colnames(x_sust) <- gsub("<2><ENG><End_Use><mobility><biofuels>", 
                         "<3><SUS><Consistency><End_Use><mobility><biofuels>",
                         colnames(x_sust))

colnames(x_sust) <- gsub("<2><ENG><Technology_Option><storage>", 
                         "<3><SUS><Efficiency><Technology_Option><storage>",
                         colnames(x_sust))

colnames(x_sust) <- gsub("<2><ENG><End_Use><building><insulation>" , 
                         "<3><SUS><Efficiency><End_Use><building><insulation>",
                         colnames(x_sust))

colnames(x_sust) <- gsub("<2><ENG><End_Use><mobility><car_sharing>" , 
                         "<3><SUS><Sufficiency><End_Use><mobility><car_sharing>",
                         colnames(x_sust))


colnames(x_sust) <- gsub("<2><ENG><End_Use><mobility><bike_pedestrian>", 
                         "<3><SUS><Sufficiency><End_Use><mobility><bike_pedestrian>",
                         colnames(x_sust))

colnames(x_sust) <- gsub("<2><ENG><End_Use><mobility><public_transport>", 
                         "<3><SUS><Sufficiency><End_Use><mobility><public_transport>",
                         colnames(x_sust))

##~------------------------------------


##+ REORDER CATEGORIES----------------------------------
main.categories <- c("<resources><unsp", 
                     "<resources>(?!<unsp)",
                     "<conversion><unsp",
                     "<conversion>(?!<unsp)",
                     "<distribution><unsp", 
                     "<distribution>(?!<unsp)", 
                     "<sales_contracts><unsp",
                     "<sales_contracts>(?!<unsp)",
                     "<Technology_Option>",
                     "<Energy_Form>",
                     "<end_use><consumption>",
                     "<mobility>(?!<frei)",
                     "<mobility><frei",
                     "<building><unsp",
                     "<building>(?!<unsp)",
                     "<electric_application>",
                     "<local_administration_bodies>",
                     "<Mobility_Sector>",
                     "<Infrastructure>",
                     
                     "<Residents>",
                     "<Food>",
                     "<Economy><unspec",
                     "<Economy><service>",
                     "<commerce>",
                     "<industry><unsp",
                     "(<industry>)(?!<unsp)"
)

main.categories.order <- unlist(lapply(main.categories, function(item) {
  
  grep(item, rownames(x_sust), perl=T, ignore.case = T)
  
  
}))


sust.categories <- c("<Sufficiency", "<Efficiency", "<Consistency")

sust.categories.order <- unlist(lapply(sust.categories, function(item) {
  
  grep(item, colnames(x_sust), perl=T, ignore.case = T)
  
  
}))


x_sust <- x_sust[rev(main.categories.order),sust.categories.order]


row.names(x_sust) <- gsub("<1><SOC>","",row.names(x_sust))
row.names(x_sust) <- gsub("<2><ENG>","",row.names(x_sust))
colnames(x_sust) <- gsub("<3><SUS>","", colnames(x_sust))
##~--------------------------------


##+ FORMAT MATRIX FOR PLOTTING (LONG FORMAT)--------------------
cooccurrence.case.plotformat.plotformat <- t(round(x_sust, d=3))

category.1 <- rownames(cooccurrence.case.plotformat)
cooccurrence.case.plotformat <- cbind.data.frame(category.1, cooccurrence.case.plotformat)
row.names(cooccurrence.case.plotformat) <- NULL

cooccurrence.case.plotformat.melted <- melt(cooccurrence.case.plotformat, id.vars=c("category.1"),
                                            #source columns
                                            measure.vars= colnames(cooccurrence.case.plotformat)[2:ncol(cooccurrence.case.plotformat)],
                                            
                                            #name of the destination column
                                            variable.name = "category.2", 
                                            value.name = "degree_of_representation_scaling_option_2",
                                            na.rm = FALSE)


cooccurrence.case.plotformat <- cooccurrence.case.plotformat.melted

cooccurrence.levels <- c(0.000, seq(1000)/1000)

cooccurrence.case.plotformat$degree_of_representation_scaling_option_2 <- factor(cooccurrence.case.plotformat$degree_of_representation_scaling_option_2, levels = cooccurrence.levels[order(cooccurrence.levels)])

cooccurrence.case.plotformat$category.1 <- factor(cooccurrence.case.plotformat$category.1, levels = unique(as.character(cooccurrence.case.plotformat$category.1)))
cooccurrence.case.plotformat$category.2 <- factor(cooccurrence.case.plotformat$category.2,  levels = unique(as.character(cooccurrence.case.plotformat$category.2)))
##~----------------------------------------                          

##+ CALCULATE ADDITIONAL PLOTTING OPTIONS (VERTICAL LINES)------------------------

##+ VERTICAL LINES
#position of lines to be drawn between systems
sust.levels <- unique(as.character(cooccurrence.case.plotformat$category.1)) 
sust.levels.unique <- c("sufficiency", "efficiency", "consistency")
line.position.v <- c("")
for (i in seq(length(sust.levels.unique))) {
  
  line.position.v <- c(line.position.v, max(grep(sust.levels.unique[i],sust.levels, ignore.case = T)))
  
}
line.position.v <- as.numeric(line.position.v[-c(1)])


main.categories.reduced <- c(                 "resources",
                                              "<conversion>", 
                                              "<distribution>", 
                                              "<sales_contracts>",
                                              "<Technology_Option>",
                                              "<Energy_Form>", 
                                              "<end_use><consumption>",
                                              "<mobility>",
                                              "<building>",
                                              "<electric_application>",
                                              "<local_administration_bodies>",
                                              "<Mobility_Sector>",
                                              "<Infrastructure>",
                                              
                                              "<Residents>",
                                              "<Food>",
                                              "<Economy>"
                                              
)





line.position <- unlist(lapply(main.categories.reduced, function(item) {
  
  min(grep(item, as.character(unique(cooccurrence.case.plotformat$category.2)), perl=T, ignore.case = T))
  
  
}))
line.position<- line.position-1
##~




##+ MARK SELF-REFERENCE (COOCCURRENCE) OF CATEGORIES WITH NAs/CROSSES
energy_categories <- as.character(cooccurrence.case.plotformat$category.2)
sust_categories <- as.character(cooccurrence.case.plotformat$category.1)

sust_categories <-   gsub("^<sufficiency>|^<consistency>|^<efficiency>,", "",sust_categories, ignore.case=T)


#get categories which co-occur with themselves in the graph to mark
#the respective positions with a cross

self_cooc <- which(sust_categories == energy_categories)
#cooccurrence.case.plotformat[self_cooc,] 
#head(cooccurrence.case.plotformat)
cooccurrence.case.plotformat[self_cooc,"degree_of_representation_scaling_option_2"] <- NA

na <- rep(FALSE, nrow(cooccurrence.case.plotformat))
cooccurrence.case.plotformat <- cbind(cooccurrence.case.plotformat, na)
cooccurrence.case.plotformat[self_cooc,"na"] <- TRUE


self_cooc_categories <- cooccurrence.case.plotformat[self_cooc,c(1,2)]

#full list of categories of x and y axis with numbering
cat_y <-   cbind(unique(as.character(cooccurrence.case.plotformat$category.2)),
                 1:length( unique(as.character(cooccurrence.case.plotformat$category.2))))

cat_x <-    cbind(unique(as.character(cooccurrence.case.plotformat$category.1)), 
                  1:length(unique(as.character(cooccurrence.case.plotformat$category.1))))


#leave only those categories which match with the self cooccurring categories
cat_x <- cat_x[!is.na(match(cat_x[,1], self_cooc_categories[,1])), ]
cat_y <- cat_y[!is.na(match(cat_y[,1], self_cooc_categories[,2])), ]

cat_x[,1] <-   gsub("^<sufficiency>|^<consistency>|^<efficiency>,", "",cat_x[,1], ignore.case=T)



order_y <- unlist(lapply(cat_x[,1], function(item) {
  
  grep(item, cat_y[,1])
  
}))

cat_y <- cat_y[order_y,]


na_positions <- as.data.frame(cbind(
  as.integer(cat_x[,2])-0.5,
  as.integer(cat_y[,2])-0.5
))
##~----------------------------------------



##+ COOCCURRENCE PLOT ENERGY / SUSTAINABILITY--------------------
axis.label.size <- 8

p_sust <- ggplot(NULL) +
  
  #raster with no distance to the axis
  geom_raster(data= cooccurrence.case.plotformat, aes(x = category.1, y = category.2, fill = degree_of_representation_scaling_option_2),hjust = 0, vjust = 0) +  
  
  scale_fill_manual(values = c("white", "grey95", "grey75", "grey30", "black"),
                    labels = c("0", "S1","S2","S3","S4 (self-reference: white cross)"),
                    na.value="black")  +
  
  #remove labels
  labs(x = element_blank(), y=element_blank()) +
  
  #set basic theme of the plot
  theme_bw() +
  
  theme(
    
    #rotate x-axis label, and set distance of label/axis to zero
    axis.text.x = element_text(angle = 90, hjust = 0, vjust=-0.25, size=axis.label.size),
    axis.text.y = element_text(vjust = .8, size=axis.label.size, hjust=0),
    
    #remove axis title
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    
    axis.ticks = element_line(size=.01),
    
    axis.ticks.length = unit(.1, "cm"),
    
    plot.background = element_rect(fill = NULL,colour = NA),
    
    plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),
    
    #length(unique(as.character(cooccurrence.case.plotformat$category.2)))/length(unique(as.character(cooccurrence.case.plotformat$category.1)))
    aspect.ratio = 2.737,
    
    legend.position = c(-0.6,-0.1),
    legend.key.size = unit(0.25, "cm"),
    legend.title = element_text(size=axis.label.size),
    legend.direction = "vertical",
    legend.background = element_rect(colour = "grey30"),
    legend.key = element_rect(colour = "grey30"),
    legend.text = element_text(size=axis.label.size)
    
  ) +
  
  
  geom_vline(xintercept = line.position.v, colour = "black", linetype= "dashed", size=.01) +
  geom_hline(yintercept = line.position, colour = "black", linetype= "dashed", size=.01) +
  
  geom_point(data = na_positions, aes(x=V1, y=V2, shape=4), color="white", size=4) +
  scale_shape_identity()

p_sust
##~-------------------------


##~ SAVE PLOT IN FILE----------------
par(mar=c(0.2,0.2,0.2,0.2))

setwd(wd.final)


# pdf("Figure_3_CoOc_Energy_and_Sustainability_scalingOpt2.pdf")
# p_sust
# dev.off()

win.metafile("Figure_3_CoOc_Energy_and_Sustainability_scalingOpt2.emf", height=10, width=6)
p_sust
dev.off()


postscript("Figure_3_CoOc_Energy_and_Sustainability_scalingOpt2.eps", height=10, width=6)
p_sust
dev.off()
##~-------------------------------

##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##END <<<<<<<<<<<<<
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<





################### THE FOLLOWING LINES MIGHT BE USED AT DIFFERENT POINTS OF THE WORKFLOW FOR DOCUMENTING SELECTED CATEGORIES

##+ WRITE SELECTED CATEGORIES INTO FILE---------------
categories.all <- colnames(cooccurrence.case)

categories.SOC <- categories.all[grep("<1>", categories.all)]
categories.ENG <- rev(rownames(x))
#categories.SUS <- categories.all[grep("<3>", categories.all)]

categories.SOC <- gsub("(^<[\\d]>)(<[\\w]{3}>)(.*$)", "\\3", categories.SOC, perl=T)
categories.ENG <- gsub("(^<[\\d]>)(<[\\w]{3}>)(.*$)", "\\3", categories.ENG, perl=T)
#categories.SUS <- gsub("(^<[\\d]>)(<[\\w]{3}>)(.*$)", "\\3", categories.SUS, perl=T)


categories.SOC <- gsub("(^<[\\w]+>)(<.*$)", "\\1~#~\\2", categories.SOC, perl=T)
categories.ENG <- gsub("(^<[\\w]+>)(<.*$)", "\\1~#~\\2", categories.ENG, perl=T)
#categories.SUS <- gsub("(^<[\\w]+>)(<.*$)", "\\1~#~\\2", categories.SUS, perl=T)

delimiter <- "~#~"
categories.SOC <- do.call(rbind,strsplit(categories.SOC, delimiter, perl=T))
categories.ENG <- do.call(rbind,strsplit(categories.ENG, delimiter, perl=T))
#categories.SUS <- do.call(rbind,strsplit(categories.SUS, delimiter, perl=T))


wd <- getwd()
setwd(wd.final)
write.csv(categories.SOC, paste("categories_SOC_nMeta_",
                                length(unique(categories.SOC[,1])),
                                "_nCat_",
                                length(categories.SOC[,2]),
                                ".csv"))

write.csv(categories.ENG, paste("categories_ENG_nMeta_",
                                length(unique(categories.ENG[,1])),
                                "_nCat_",
                                length(unique(categories.ENG[,2])),
                                ".csv"))

# write.csv(categories.SUS, paste("categories_SUS_nMeta_",
#                                 length(unique(categories.SUS[,1])),
#                                 "_nCat_",
#                                 length(unique(categories.SUS[,2])),
#                                 ".csv"))
# 
# setwd(wd)
##~---------------------

#####################

