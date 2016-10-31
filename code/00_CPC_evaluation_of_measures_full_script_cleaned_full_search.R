
#Set main working directory
wd.main <- c("M:/Science/Programmierung/Github/semantic_sustainability_assessment/")

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

#the internal R textlist structure which is used to handle the files is as follows
#(check e.g. str(textlist) or text <- textlist[[24]][[2]] after creating the list)
#name
#[[ENT]]             list ENTity (=document name)
#[[ENT]][[1-WT]]     Dummy sentence (before: Whole Text of the document including code at filehead)
#[[ENT]][[2-AEXP]]   Available EXtraction Pattern in the wholetext, matrix
#[[ENT]][[3-MEAS]]   separated single measures (including linebreaks marked with "~~collapse~~")
#[[ENT]][[4-UnWinM]] Unique Words in the single Measures, words which exist in the available wordlists have an E~X~T at the beginning


#SYNTAX NOTES
# : comments...

##+ DESCRIPTION TEXT : marks beginning of a short code block
##~ : marks the end of a short code block

##+ DESCRIPTION TEXT -------------- : marks the end of a longer code block (may be collapsed, e.g. in R Studio)
##~-------------- : marks the end of a longer code block (may be collapsed, e.g. in R Studio)

##+<<<<<<<<<<<<< description text  : marks the beginning of a very large code block
##~<<<<<<<<<<<<<   : marks the end of a very large code block

#variables are named in lowercase letters only, separating symbol is a dot, e.g. variable.new
#Functions start with an uppercase letter, separating symboal is a hyphen, e.g. Function_new()

#ADDITONAL NOTES
#warnings concerning incomplete final lines may be neglected

#**************************************************************************************************



##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##+<<<<<<<<<<<<< INITIAL DEFINITIONS (working directories, functions, etc.)
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

##+ SET WORKING DIRECTORIES------------------------------------
  #in case the final slash was "forgotten", the final slash is assumed to exist in the following
  if (substr(wd.main, nchar(wd.main), nchar(wd.main)) != "/") {
    wd.main <- paste0(wd.main, "/")
  }
  
  #subdirectories are set automatically on basis of main directory
  wd.source <- paste(wd.main, "data/text_files/", sep="")
  wd.interim <- paste(wd.main, "data/results_interim/", sep="")
  wd.final <- paste(wd.main, "data/results_final/", sep="")
  wd.wordlists <- paste(wd.main, "thesauri_wordlists/thesauri/", sep="")
  wd.stopwords <- paste(wd.main, "thesauri_wordlists/stopwords/", sep="")
  wd.encoding <- paste(wd.main, "encoding/", sep="")
  wd.code <- paste(wd.main, "code/", sep="")
  
  #directory for storing words that are not matched by the wordlists
  wd.notmatched <- paste(wd.main, "thesauri_wordlists/words_not_matched/", sep="")
    #initialize an empty txt.file if there is no file, yet
    if (c("words_not_matched.txt") %in% list.files(path = wd.notmatched) == FALSE) {
      writeLines(c(""), paste(wd.notmatched, "words_not_matched.txt", sep="") )
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
##+~

##~-------------------------------------------------------------------

##+ DEFINITION OF FUNCTIONS------------------------------------------------------------

##+ CHRACTERS TO BE REPLACED BY BASIC LATIN LETTERS
  special_german_characters <- c("ä", "Ä", "ö", "Ö", "ü", "Ü", "ß")
##~


##+ READING CHARACTER ENCODING LIST
  setwd(wd.encoding)
  options(encoding = "native.enc")
  winencodingtable <- read.table("tm_win1252_misinterpretation_encoding_table_UTF8.txt", 
                                 header = TRUE, 
                                 sep=";", 
                                 colClasses = "character",
                                 encoding="UTF-8")
  
  #manual corrections; misinterpreted characters for space are not read in correctly... 
  #(at least this was the case on the machine of the author)
  winencodingtable[121,] <- gsub("space", " ", winencodingtable[121,])
  winencodingtable[121,4] <- c("S~P~A~C~E")
##~


##+ READING LIST OF PUNCTUACTION CHARACTERS (supplements [[:punct:]])
  initialencoding <- getOption("encoding")
  options(encoding = "native.enc")
  
  suppressWarnings(punct <- readLines(paste(wd.encoding, "punct_meta_ANSI.txt", sep=""), encoding = "ANSI" ))
  punct <- strsplit(punct, "~~")
  
  #generate vector format from list
  left <- c(1:length(punct))
  
  left <- sapply(left, function(x) {
    punct[[x]][[1]]  
  })
  
  punct <- as.character(left)
  options(encoding = initialencoding)
##~


##+ FUNCTION - CORRECT_MISINT_CHARS(CHARACTER VECTOR)
# correction of characters that have been mmisinterpreted during reading of text files
#on basis of the encoding table loaded above
  Correct_misint_chars <- function(text) {
    
    for (i in (1:nrow(winencodingtable))) {
      
      #unicode encoding, e.g.: U+203A
      text <- gsub(as.character(winencodingtable[i,1]), as.character(winencodingtable[i,3]), text,  fixed = TRUE) 
      
      #windows encoding, e.g.: 0x9B
      text <- gsub(as.character(winencodingtable[i,2]), as.character(winencodingtable[i,3]), text,  fixed = TRUE)
      
      #windows misinterpreted character symbols 
      text <- gsub(as.character(winencodingtable[i,4]), as.character(winencodingtable[i,3]), text,  fixed = TRUE) 
     
      #UTF-8 encoding, e.g.: %E2%80%BA
      text <- gsub(as.character(winencodingtable[i,5]), as.character(winencodingtable[i,3]), text,  fixed = TRUE) 
      
    }
    
    
    #correct some unicode characters, see e.g. https://en.wikipedia.org/wiki/Typographic_ligature
    text <- gsub("<U+FB00>", "ff",  text, fixed=TRUE)
    text <- gsub("U+FB00", "ff",  text, fixed=TRUE)
    
    text <- gsub("<U+FB01>", "fi",  text, fixed=TRUE)
    text <- gsub("U+FB01", "fi",  text, fixed=TRUE)
    
    text <- gsub("<U+00DF>", special_german_characters[7],  text, fixed=TRUE)
    text <- gsub("U+00DF", special_german_characters[7],  text, fixed=TRUE)
    
    text <- gsub("<U+FB02>", "fl",  text, fixed=TRUE)
    text <- gsub("U+FB02", "fl",  text, fixed=TRUE)
    
    text <- gsub("<U+AB50>", "ui",  text, fixed=TRUE)
    text <- gsub("U+AB50", "ui",  text, fixed=TRUE)
    
    text <- gsub("<U+FB06>", "st",  text, fixed=TRUE)
    text <- gsub("U+FB06", "st",  text, fixed=TRUE)
    
    text <- gsub("<U+FFFD>", "st",  text, fixed=TRUE)
    text <- gsub("U+FFFD", "st",  text, fixed=TRUE)
    
    text <- gsub("<U+FB06>", "st",  text, fixed=TRUE)
    text <- gsub("U+FB06", "st",  text, fixed=TRUE)
    
    
    #special characters which had been misinterpreted and appear in form of a replacement character
    text <- gsub("<U+FB06>", "",  text, fixed=TRUE)
    text <- gsub("U+FB06", "",  text, fixed=TRUE)
    
    text <- gsub("<U+263A>", "",  text, fixed=TRUE)
    text <- gsub("U+263A", "",  text, fixed=TRUE)
    
    text <- gsub("<U+F0E0>", "",  text, fixed=TRUE)
    text <- gsub("U+F0E0", "",  text, fixed=TRUE)
    
    return(text)
  }
##~


##+ FUNCTION - CLEANTEXT(CHARACTER VECTOR)
#cleaning of a text from punctuation, etc.
Cleantext <- function(text) {
  
  #when character vectors are collapsed, ~~collapse~~ is used as a marker phrase 
  #in case the information of the break point is needed at some point to restore the original format
  text <- gsub("~~collapse~~", "", text)
  
  text <- gsub("[[:cntrl:]]", " ", text)
  text <- gsub("[[:blank:]]{2,}", " ", text)
  
  #in German compounds divided by "und" or "oder" might share a mutual final noun and are therefore
  #combined, since their meaning cannot only be defined on basis of their first noun
  #e.g.: Energie- und Umweltmanagement - management belongs to both energy and environment
  text <- gsub("(-[[:blank:]]und[[:blank:]])([[:upper:]])", "UND\\2", text, perl=TRUE)
  text <- gsub("(-[[:blank:]]oder[[:blank:]])([[:upper:]])", "ODER\\2", text, perl=TRUE)
  
  #for parsing the texts, some marker phrases were introduced 
  #that are deleted during the cleaing step, e.g. [[:blank:]]
  text <-  gsub("([[:)([A-Za-z]+)(:]])", " " , text, perl=TRUE)
  
  
  #replace all puncutation characters
  initialencoding <- getOption("encoding")
  options(encoding = "native.enc")

  for (i in 1:length(punct)) {
    
    text <- gsub(punct[i],"",text, fixed = TRUE)
  }
  options(encoding = initialencoding)
  
  text <- gsub("[[:punct:]]", "", text)
  
  
  #replace special_german_characters
  text <- gsub(special_german_characters[1], "ae", text)
  text <- gsub(special_german_characters[2], "Ae", text)
  text <- gsub(special_german_characters[3], "oe", text)
  text <- gsub(special_german_characters[4], "Oe", text) 
  text <- gsub(special_german_characters[5], "ue", text)
  text <- gsub(special_german_characters[6], "Ue", text)
  
  #delete part of the gender formatting of nouns
  text <- gsub("/in |_in |/innen |_innen ", "", text)
  
  #delete various marker phrases, numbers, etc.
  text <- gsub("CO[[:blank:]]*2", "COzwei", text)
  
  text <- gsub("[[:digit:]]", "", text)
  
  text <- gsub("[[:blank:]][[:alpha:]][[:blank:]]", " ", text)
 
  text <- gsub("([[:blank:]]|^)[[:alpha:]]([[:blank:]]|$)", " ", text)
  
  text <- gsub("[[:blank:]]{2,}", " ", text)
 
  text <- gsub("^[[:blank:]]+|[[:blank:]]+$", "", text)
  
  text <- gsub("<U+FFFD>", "", text, fixed=TRUE)
  
  
  #convert all words which contain at least one uppercase letter to uppercase
  #and then all uppercase words to lower case except for the first letter
  #the first letter stays as marker for an uppercase word
  #hence, a "normal" uppercase word is recognized better by the porter stemming algorithm 
  #(this step accounts for misspellings in the documents)
  text <- gsub("(\\b)([\\w]*)(?=[A-Z])([\\w]*)(\\b)", "\\1\\U\\2\\U\\3\\U\\4\\5", text, perl=TRUE)
  text <- gsub("(\\b)([A-Z])([\\w]+)(\\b)", "\\1\\2\\L\\3\\4", text, perl=TRUE)
  
  #in case the text is a vector with several entries, empty lines are deleted
  text <- text[text != ""]
  
  return (text)
  
}
##~


##+ FUNCTION - UNIQUEWORDS(CHARACTER VECTOR)
#create a character vector of unique words
Uniquewords <- function(text) {
  
  text <- paste(unlist(text, recursive = TRUE), collapse = " ")
  text <- unlist(strsplit(text, " "))
  text <- sort(unique(text))
  
  return (text)
  
}
##~


##+ FUNCTION - LOADSTOPWOIRDLIST.TXT.CSV(STOPWORDDIR_wfinalSlash)
#loads the stopwordlists in the stopwordlist folder and creates a character vector of unique stopwords
#txt files may have comments marked via | or # (e.g. like the stopwordlist of the Porter Algorithm: http://snowballstem.org/algorithms/german/stop.txt)
#csv files are assumed to have a tabular format with a HEADER inlcuiding each category of wordtype and NO comments
#the function Uniquewords(text) is required
Loadstopwordlist.txt.csv <- function(stopworddir_wfinalSlash) {
  
  stopwordfiles <- list.files(stopworddir_wfinalSlash)
  #initial the wordlist
  stopwordlist <-  c("Dummy")
  
  #for txt files
  txtfiles <- stopwordfiles[grep(".txt$", stopwordfiles)]
  
  for (i in 1:length(txtfiles)) {
    txtwordlist <- readLines(paste(stopworddir_wfinalSlash, txtfiles[i] ,sep=""))
    txtwordlist <- gsub("\\|.*$", "", txtwordlist) #in case | is used as comment character
    txtwordlist <- gsub("\\#.*$", "", txtwordlist) #in case # is used as comment character
    txtwordlist <- gsub("^[[:blank:]]+|[[:blank:]]+$", "", txtwordlist)
    txtwordlist <- txtwordlist[grep("[[:graph:]]", txtwordlist)]
    
    stopwordlist <- c(stopwordlist, txtwordlist)
  }
  
  
  #for .csv files
  csvfiles <- stopwordfiles[grep(".csv$", stopwordfiles)]
  
  for (i in 1:length(csvfiles)) {
    csvwordlist <- read.csv(paste(stopworddir_wfinalSlash, csvfiles[i] ,sep=""), header = TRUE, sep=";")
    csvwordlist <- Uniquewords(csvwordlist)
  
    csvwordlist <- gsub("^[[:blank:]]+|[[:blank:]]+$", "", csvwordlist)
    csvwordlist <- csvwordlist[grep("[[:graph:]]", csvwordlist)]
    
    stopwordlist <- c(stopwordlist, csvwordlist)
  }
  
  stopwordlist <- unique(stopwordlist)
  stopwordlist <- stopwordlist[grep("[[:graph:]]", stopwordlist)]
  
  return(stopwordlist)
}
##~


##FUNCTION - STEM_VECTOR(CHARACTER VECTOR)
#applies the porter stemming algorithm with some adaptions for better results in German
#keeps the original format of a vector in case of multiple entries (lines)
Stem_vector <- function(vector) {
  
  vector <- paste(vector, collapse=" ~c~ ")
  vector <- unlist(strsplit(vector, " "))
  
  #mark the uppercase words before stemming as some (like Umwelt) are stemmed to lower case
  #and have to be reconverted to upper case after stemming
  
  vector <- gsub(paste("(\\b)([A-Z|", special_german_characters[2], "|", special_german_characters[6],"|",special_german_characters[4],"])", sep=""), "X~\\2" , vector, perl=TRUE)
  vector <- wordStem(vector, language = "german")
  
  vector <- gsub("(X~)([\\w])", "\\U\\2" , vector, perl=TRUE)
  vector <- paste(vector, collapse=" ")
  vector <- unlist(strsplit(vector, " ~c~ "))
  
  return (vector)
  
}
##~

##~--------------------------------------------


time.elapsed <- rbind(time.elapsed, proc.time())
row.names(time.elapsed)[nrow(time.elapsed)] <- "initial_definitions"
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##~<<<<<<<<<<<<<<<<<<<
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##+<<<<<<<<<<<<<<<<<<< WRITE EMPTY CODE STRUCTURE FOR TEXT PROCESSING AT FILE HEADS
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##+ WRITE EMPTY CODE STRUCTURE...----------------------------------
empty.processing.code <- readLines(paste0(wd.code, "code_structure_for_setting_text_processing_markers.txt"), 
                                   encoding = "UTF-8")


files <- list.files(wd.source, pattern="*.txt")

for (i in files) {
  
  text <- readLines(i, encoding = "UTF-8")
  
  if (length(c(grep("!Analysis_Code_Start!",text), grep("!Analysis_Code_End!",text))) == 2) {
    
    #do nothing, code structure already available in text file
    
  } else if (length(c(grep("!Analysis_Code_Start!",text), grep("!Analysis_Code_End!",text))) == 0) {
    
    text <- c(empty.processing.code, " ", text)
    
    #to suppress any encoding issues and keep UTF-8 useBytes = TRUE is set
    writeLines(text,i, useBytes = TRUE)
  } else {
    
  warning("ERROR: text contains the wording !Analysis_Code_Start! or !Analysis_Code_End! which is used by the code as marker phrase Please delete or change this phrase in the text. Another reason for the error could be that the code at the file head was corrupted and is missing partly. Please check.")
  
  }
  
}
##~--------------------------------
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##~<<<<<<<<<<<<<<<<<<< 
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

####MANUAL ACTION  REQUIRED<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##+<<<<<<<<<<<<<<<<<<< IDENTIFY EXTRACTION PATTERNS FOR TEXT WINDOWS AND SPLIT TEXTS
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

##+ READ IN TEXTFILES---------------------------------------
setwd(wd.source)

#list of txt files in folder
files <- list.files(pattern="*.txt")

#initialize list to work with
textlist <- vector(mode = "list", length = length(files))

for (f in 1:length(textlist)) {
  text <- readLines(files[f], encoding = "UTF-8")
  filename <- files[f]
  names(textlist)[f] <- filename
  
  text <- list(text)  
  
  textlist[[f]] <- text
  names(textlist[[f]]) <- filename
}
##~----------------------------------------


##+ IDENTIFY EXTRACTION PATTERNS -------------------------
#on basis of manually defined repeating marker phrases for beginning/end of the windows
#suitable strings are defined manually on basis of number of occurences of potential extraction words 
#and total number of known windows (from manual screening of text)


for (t in 1:length(textlist)) {
  
  wholetext <- textlist[[t]][[1]]
  
  ##+ READ ANALYSIS CODE AT FILE HEAD
    codestart <- grep("!Analysis_Code_Start!",wholetext)
    codeend <- grep("!Analysis_Code_End!",wholetext)
    code <- wholetext[codestart:codeend]
    
    startpage <- code[grep("pagerange",code)+1]
    endpage <- code[grep("pagerange",code)+2]
    
    #read potential words for marking text windows
    #that were guessed by looking into the text and 
    #inserted manually in the analysis code at the file head)
    desiredpatternrange <- grep("desired_pattern_end",code)-grep("desired_pattern_start",code)-1
    desiredpattern <- code[grep("desired_pattern_start",code)+rep(1:desiredpatternrange)]  
  ##~
  
  
  ##+ CHECK AVAILABILITY OF PATTERNS IN TEXT 
    wholetext <- wholetext[-c(codestart:codeend)]  
    
    #select the desired page range
    measurestext <- wholetext[grep(startpage,wholetext):grep(endpage,wholetext)]
    
    #check the number of occurence of potential pattern extraction words 
    #and build a matrix: word : number of occurences  
    availablepattern <- c(sapply(1:length(desiredpattern), function(i) {
      
      sum(grepl(desiredpattern[i], measurestext))
      
    }))
    
    pattern <- cbind(desiredpattern, availablepattern)
    pattern <- list(pattern)
    names(pattern) <- paste("potential_pattern_", names(textlist[t]), sep="")
    textlist[[t]] <- c(textlist[[t]],pattern)
  ##~
  
  }

  ##+Display the availability of patterns
  #for post-processing of the pattern matching and extraction
  
  sapply(c(1:length(textlist)),  function(i) {
    textlist[[i]][2]
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
identifiedpatterns <-  readLines(paste(wd.interim,"identified_extraction_patterns.txt", sep=""))

#clean text from comments
identifiedpatterns <- gsub("#.*$", "", identifiedpatterns)
identifiedpatterns <- identifiedpatterns[identifiedpatterns != ""]

identifiedpatterns <- strsplit(identifiedpatterns, "\\|")
identifiedpatterns <- lapply(identifiedpatterns, function(x) gsub("[[:blank:]]+$|^[[:blank:]]+","", x))


for (i in 1:length(identifiedpatterns )) {#loop through the identifiedpatterns to write them in the fileheads
  
  #check which identified pattern refers to which textlist element
  #and if a suitable element exists at all
  #(e.g. in case the order of documents changed or a additional document are stored in the folder)

  
  extractionpattern <- eval(parse(text=identifiedpatterns[[i]][[1]]))

  patternidintextlist <- grep(identifiedpatterns[[i]][[2]], names(textlist), fixed = TRUE)

 
  if (sum(patternidintextlist) > 0) { #check if the filename of the stored pattern matches the one in the textlist, if yes, insert extraction pattern
  
    startextractstring <- textlist[[patternidintextlist]][[2]][[extractionpattern[1],1]]
 
    textlist[[patternidintextlist]][[1]] <- append(textlist[[patternidintextlist]][[1]], startextractstring, after = grep("startextract \\|", textlist[[patternidintextlist]][[1]]))
  
    endextractstring <- textlist[[patternidintextlist]][[2]][[extractionpattern[2],1]]

    textlist[[patternidintextlist]][[1]] <- append(textlist[[patternidintextlist]][[1]], endextractstring, after = grep("endextract \\|", textlist[[patternidintextlist]][[1]]))
    
    
  } else { #in case the filename of the extraction pattern is not matched in the textlist
    print(paste("Not matched in textlist:", identifiedpatterns[[i]][[2]]))
    notmatched <- TRUE
  }
}

##~-------------------------

##+ SPLIT TEXTS INTO TEXT WINDOWS-------------------

for (t in 1:length(textlist)) {
  
  wholetext <- textlist[[t]][[1]]
  
  #read analysis code at filehead
  #it has been changed manually in the texts and is therefore read in again
  codestart <- grep("!Analysis_Code_Start!",wholetext)
  codeend <- grep("!Analysis_Code_End!",wholetext)
  code <- wholetext[codestart:codeend]
  
  wholetext <- wholetext[-c(codestart:codeend)]
  
  #reading in the pages to be analyzed
  startpage <- code[grep("pagerange",code)+1]
  endpage <- code[grep("pagerange",code)+2]
  
  ##+ CHECK FOR MARKER WORDS F~A~L~S~E AND T~R~U~E
    #the words F~A~L~S~E and T~R~U~E are used in the code at some parts instead of the boolean TRUE/FALSE
    #as the latter might in some steps (e.g. by gsub) be converted to character and not fulfill their purpose
    #in order to avoid mistakes in processing the text, it may not contain these words, this is checked
    if (sum(grepl("F~A~L~S~E", wholetext)) > 0) {
      wholetext <- gsub("F~A~L~S~E", "FALSE", wholetext)
      print(paste("The string F~A~L~S~E was replaced by FALSE in: ", names(textlist)[t], sep=""))
    } 
    
    if (sum(grepl("T~R~U~E", wholetext)) > TRUE) {
      wholetext <- gsub("T~R~U~E", "TRUE", wholetext)
      print(paste("The string T~R~U~E was replaced by TRUE in: ", names(textlist)[t], sep=""))
    } 
  ##~
  
  
  ##+ CLEAN TEXT FROM FOOTNOTES; ETC:
  #manually identified single lines
    if (grep("single_line_deletion_end",code)-grep("single_line_deletion_start",code)-1 >= 1) {
      
      deletesinglelinesrange <- grep("single_line_deletion_end",code)-grep("single_line_deletion_start",code)-1
      deletesinglelines <- code[grep("single_line_deletion_start",code)+rep(1:deletesinglelinesrange)]
      
    } else {
      deletesinglelines <- "F~A~L~S~E"
    }
  ##~
  
  ##+ READ EXTRACTION PATTERN FROM ANALYSIS CODE AT FILE HEAD
    #if single string pattern (FROM X TO X TO X TO END) option is set in the first line of extraction pattern take this into account
    #or otherwise use two (or more) extraction words
    if (grepl("S~I~N~G~L~E~S~T~R~I~N~G~P~A~T~T~E~R~N:", code[(grep("startextract",code)+1)]) == TRUE) { 
      
      startextract <- gsub("S~I~N~G~L~E~S~T~R~I~N~G~P~A~T~T~E~R~N:", "", code[(grep("startextract",code)+1)] )
      endextract <- "F~A~L~S~E"
      
      singlestringpattern <- "T~R~U~E"
      
    } else {
      
      #defining the numbers of words and lines for extraction / deletion
      extractrange <- grep("endextract",code)-grep("startextract",code)-1
      
      #defining end/start words based on above ranges to be extracted or deleted - stored in vectors
      startextract <- code[grep("startextract",code)+rep(1:extractrange)]
      endextract <- code[grep("endextract",code)+rep(1:extractrange)]
      
      singlestringpattern <- "F~A~L~S~E"
    }
 
    #clean the text from words of the analysis code
    startextract <- gsub("!~~.*~~!", "", startextract) 
    endextract <- gsub("!~~.*~~!", "", endextract)
    deletesinglelines <- gsub("!~~.*~~!", "", deletesinglelines)
  ##~
  
  
  
  ##+ EXTRACT PAGE RANGE TO BE ANALYZED AND SPLIT INTO TEXT WINDOWS
  measuretext <- wholetext[grep(startpage,wholetext):grep(endpage,wholetext)]

  #find the lines to START / END extraction on basis of a search string / search cases
  #for checking the output connected to the lines, use this command: measuretext[startlines]
    startlines <- c(sapply(seq(length(startextract)), function(e) {
      
      grep(startextract[e], measuretext)
      
    }))

 
  ##+ FIND THE LINES FOR SPLITTING INTO TEXT WINDOWS 
    if (singlestringpattern == "T~R~U~E") {
      
      #all rows to be extracted from measuretext
      #set endlines for extraction from one element to the next and last element = last row
      endlines <- startlines
      
      #delete the first entry, hence extraction would start with startlines[1] and end with startlines[2]
      endlines <- endlines[-1]
      #however extraction should be only until stratlines[2]-1 therfore -1 on all rows 
      endlines <- endlines-1 
      
      #as the final element is missing now the end of the text is defined as last endextraction line
      endlines <- c(endlines, length(measuretext)) #add the last row as last entry     
      
    } else {
      
      endlines <- c(sapply(seq(length(endextract)), function(e) {
        
        grep(endextract[e], measuretext)
        
      }))
    } #endif
    
    #all rows to be extracted from measuretext
    extractall <- cbind(startlines, endlines)
  ##~
  
  
  ##+ SPLIT INTO TEXT WINDOWS
  #explanation of the code - see deletall block
  
    ##+ CLEAN TEXT FROM ANALYSIS CODE WORDS
    #the extraction lines are fixed now therefore the extraction patterns (as single words) can be cleaned from the text
    #for singleline pattern there is only startextract which can be deleted
    if (singlestringpattern == "T~R~U~E") {
      measuretext <- gsub( startextract ,"",measuretext)
    } else {
      measuretext <- gsub( startextract ,"",measuretext)
      measuretext <- gsub( endextract ,"",measuretext)
    }
    ##~
  
    ##+ DO SPLITTING
    #different way of splitting for single string patterns and multiple string patterns
    if (singlestringpattern == "T~R~U~E") {
      
      #build a vector with each element including a single measure - collapsed text from the rows to be extracted
      measuresextracted <- c(sapply(seq(nrow(extractall)), function(x) {
        rows <- (extractall[x,1]:extractall[x,2])
        measure <- measuretext[rows] #extracted text 
        #for the current analysis it makes sense to collapse the measure into plain text
        #it might also be stored as a list
        #in case the former structure shall be generated again the wording " ~~collapse~~ " is included at the collaped locations
        
            ##+DELETE FOOTNOTES
            #in case footnotes were defined for deletion
            if (deletesinglelines[1] == "F~A~L~S~E") {  #first if/else
              
              # print(paste("no deletelines, nothing deleted in: ", names(textlist[t])))
              
            } else if  (deletesinglelines[1] != "F~A~L~S~E") { #if there is something to delete, loop through the vector of lines containing the deletion marker string
              
              for (z in 1:length(deletesinglelines)) { #for loop through deletesinglelines
                
                if (sum(c(grep(deletesinglelines[z], measure))) == 0) {  # second first/ifelse  / in case matching for deletion fails and a integer(o) vector is returned, a safety net is simply not to delete
                  
                  # if (deletesinglelines[z] != "!~delete_line_~!") { #only print something if the deletline text is not a the dummy text 
                  #  print(paste("failed to match deleteline", deletesinglelines[z] , "nothing deleted in: ", names(textlist[t])))
                  # }
                  
                } else if (sum(c(grep(deletesinglelines[z], measure))) > 0) {
                  measure <- measure[-c(grep(deletesinglelines[z], measure))]
                } #end second if/else
                
              }
          
            } #end first if/else    
            ##~   
        
        paste(measure, collapse= " ~~collapse~~ ")
        
      }))
      
    } else {
      
      #check if number of start/endextractlines are equal
      if (length(endlines) != length(startlines)) {
        print(paste("ERROR - number of start/endextractlines not equal in", names(textlist[t])))
      stop}
      
      #build a vector with each element including a single measure - collapsed text from the rows to be extracted
      measuresextracted <- c(sapply(seq(nrow(extractall)), function(x) {
        rows <- (extractall[x,1]:extractall[x,2])
        measure <- measuretext[rows] #extracted text 
        #for the current analysis it makes sense to collapse the measure into plain text
        #it might also be stored as a list
        #in case the former structure shall be generated again the wording " ~~collapse~~ " is included at the collaped locations
        
        
            ##+DELETE FOOTNOTES
            if (deletesinglelines[1] == "F~A~L~S~E") {  #first if/else
              
              # print(paste("no deletelines, nothing deleted in: ", names(textlist[t])))
              
            } else if  (deletesinglelines[1] != "F~A~L~S~E") { #if there is something to delete, loop through the vector of lines containing the deletion marker string
              
              for (z in 1:length(deletesinglelines)) { #for loop through deletesinglelines
                
                if (sum(c(grep(deletesinglelines[z], measure))) == 0) {  # second first/ifelse  / in case matching for deletion fails and a integer(o) vector is returned, a safety net is simply not to delete
                  
                  # if (deletesinglelines[z] != "!~delete_line_~!") { #only print something if the deletline text is not a the dummy text 
                  #  print(paste("failed to match deleteline", deletesinglelines[z] , "nothing deleted in: ", names(textlist[t])))
                  # }
                  
                } else if (sum(c(grep(deletesinglelines[z], measure))) > 0) {
                  measure <- measure[-c(grep(deletesinglelines[z], measure))]
                } #end second if/else
                
              } #end for loop through deletesinglelines
              
            } #end first if/else    
            ##~
      
        
        paste(measure, collapse= " ~~collapse~~ ")
        
      }))
    } 
    ##~
  
  measuresextracted <- list(measuresextracted)
  names(measuresextracted) <- paste("measures_", names(textlist[t]), sep="")
  textlist[[t]] <- c(textlist[[t]], measuresextracted)    
  
  #in order to save memory and speed up the following calculations the wholetext is removed
  textlist[[t]][[1]] <- c("Full text removed in order to free memory and improve calculation performance. Please load again by readLines if the full text is needed.")
  
}
##~-------------------


##+ COMBINE LISTS OF TEXT WINDOWS FOR HANNOVER-----------
#the action plan came in several files which are combined here
  files.combine <- grep("GER__Hannover__", names(textlist))
  
  #only do something if there are more than one list entries with the same entity name
  if (length(files.combine ) > 1) { 
    
    #initial the combined list with the first element of elements to be combined
    case.combine <- textlist[files.combine[1]]
    #get the number of first list levels which serve as loop variable
    num.listelements <- length(case.combine[[1]])
    
    #loop through all elements to combine except the intial one
    add.cases <- files.combine[-c(1)]
    
    for (t in add.cases) {
      
      for (u in 1:num.listelements) {
        
        case.combine[[1]][[u]] <- c(case.combine[[1]][[u]], textlist[[t]][[u]])
        
      }
      
    }
    
    #reassign the names
    names(case.combine) <- "GER__Hannover__combined_files.txt"
    names(case.combine[[1]]) <- c("GER__Hannover__combined_files.txt",
                                  "pattern_GER__Hannover__combined_files.txt",
                                  "measures_GER__Hannover__combined_files.txt")
    
    #add the combined files list and delete the single files
    textlist <- c(textlist,case.combine)
    textlist <- textlist[-c(files.combine)]
  }
##~-------------

time.elapsed <- rbind(time.elapsed, proc.time())
row.names(time.elapsed)[nrow(time.elapsed)] <- "after_text_loading_and_splitting"
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##~<<<<<<<<<<<<<<<<<<<
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<




##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##+<<<<<<<<<<<<<<<<<<< READ AND PREPARE THESAURI AND STOPWORDLIST DATA
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


##+ PREPARE THESAURI WORDLISTS---------------------------
#read all txt files in folder and build a list of their names
#then do some preliminary cleaning (trim, remove comments, etc.)
setwd(wd.wordlists)

##+ READ DIRECTORIES WHERE THESAURI ARE STOREDE
  dir_current_root <- paste(getwd(), "/", sep="")
  dirs <- list.dirs()
  #the root dir is included as a single dot "." and is excluded
  #also leading dots are deleted from directory names and a final slash is added
  dirs <- dirs[grep("[[:alpha:]]", dirs)]
  dirs <- paste(gsub("./", "", dirs, fixed=TRUE),"/" , sep="")
##~


##+ READ THE THESAURI DATA AND CLEAN THE WORDLISTS  
#initialize
wordlists <- vector(mode = "list", length = length(dirs))

for (f in 1:length(dirs)) {

  ##+ READ
    setwd(paste(dir_current_root, dirs[f], sep=""))
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
    
    text <- Correct_misint_chars(text)
    text <- gsub("[[:blank:]]{2,}", " ", text)
    text <- unique(text)
  ##~
    
  
  ##+ STORE WORDLISTS IN R LIST
  
    #create wordlistnames on basis of names of the directory
    category <- gsub("[\\W]", "", dirs[f], perl=TRUE)
    category <- gsub("[[:blank:]]", "_", category)
    listname <- category
    
    #add a number to the wordlist in case any names are duplicates
    listname <- paste("c",f,"__",listname,sep="")
    
    #if the list only contains one single word (i.e. the category in the first line) this
    #causes problems in later steps, therefore dummy words are introduced
    if(length(text) <= 1) text <- c(text, c("DUMMYUPPER", "dummylower", "Dummymixed"))
    
    names(wordlists)[f] <- listname
    
    wordlists[[f]] <- text
  ##~
  
  
}
##~---------------------------------------



##+ PREPARE STOPWORDLIST-----------------------

##+ COMPILE STOPWORDS FROM FOLDER AND EXTRACTION PATTERN WORDS
  #all words which had been used to identify extraction patterns
  #they are assumed to be very general, hence they can be deleted for the current analysis)
  stopwordlist <- Loadstopwordlist.txt.csv(wd.stopwords)

  stopwordlist.2 <- c(sapply(seq(length(textlist)), function(i) {
    
    text <- textlist[[i]][[2]]
    
  }))
  
  #here unique might be used if n-grams shall be preserved, however, this does not make much sense for stopwords
  stopwordlist.2 <- Uniquewords(stopwordlist.2)
  
  #some words should not be removed later and some others (like the extraction patterns) are added
  stopwordlist.2 <-  stopwordlist.2[-c(grep(paste("Hannover|Finanzierung|Klima-Allianz|B",special_german_characters[5],"ro|Industrie|W",special_german_characters[1],"rme|Strom|F",special_german_characters[3],"rderprogramme|Kostenaspekte|Verbraucherverhalten|Erneuerbare|Industrie|Wohnen|Klima-Allianz|Wirtschaftlichkeit|Regionen|Wirtschftlichkeit", sep=""), 
                                            stopwordlist.2))]
  
  
  stopwordlist.2 <- c(stopwordlist.2, "~PAGE~RANGE~START~", 
                      "~PAGE~RANGE~END~", 
                      "END_EXTRACTION",
                      "START_EXTRACTION")
  
  
  stopwordlist <- c(stopwordlist, stopwordlist.2)
##~

  
##+ CLEAN AND STEM STOPWORDLIST
  stopwordlist <- Correct_misint_chars(stopwordlist)
  
  #in contrast to stopwordlist.2 the loaded stopwordlist might include n-grams which shall be preserved
  #therefore not Uniquewords but unique is applied
  stopwordlist <- unique(Cleantext(stopwordlist))
  
  #stem the stopwordlist
  #lower and uppercase are preserved by stem_vector
  x <- Stem_vector(stopwordlist)
  
  #graph does not include blanks, similar like x!=""
  x <- x[grep("[[:graph:]]", x)]
  
  x <- sort(unique(x))
  
  stopwordlist.stem <- x
  
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
    
    x <- text
    x <- unique(Cleantext(x))  #here not Uniquewords() is applied as suitable splitting has been done in the process of preparing the wordlists
    
    #for exceot for word referring to sustainability categories only nouns are considered
    if (grepl("_SUST_", names(wordlists)[[i]]) == FALSE) {
      
      x <- x[grep("[[:upper:]]", x)]
      
    }
    
    x <- unique(Stem_vector(x))
    
    x <- x[grep("[[:graph:]]", x)]
    
    x <- gsub("^[[:alnum:]]{1,2}$","" , x)
    
    #remove the stopwords which appeared at the beginning of a sentence and begin with upper case
    rows.delete <- which(tolower(x) %in% tolower(stopwordlist))
    if (length(rows.delete) > 0) {
      x <- x[-rows.delete]
    }
    
    
    x <- x[x!=""]  
    text <- x
    
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
    prefix.wordlists <- c("uniq.stem_")
    names(text) <- paste(prefix.wordlists, names(wordlists[i]), sep="")
    
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
  
  category <- rep(gsub(prefix.wordlists, "", names(wordlists[[i]][1])), length(wordlists[[i]][[1]]))
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
##~<<<<<<<<<<<<<<<<<<<
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<



##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##+<<<<<<<<<<<<<<<<<<< CLEAN AND STEM ALL TEXT WINDOWS
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
for (i in 1:length(textlist)) {
  
  text <- textlist[[i]][[3]]
  
  ##+ FLAT STEMMED VERSION OF THE WORDS IN THE TEXT WINDOWS
    measure.stem <- lapply(text, function(x) {
      
      x <- paste(unlist(x), collapse=" ")
      
      x <- Cleantext(x)
      
      x <- Stem_vector(x)
      
      x <- unlist(strsplit(x, " "))
      
      x <- setdiff(x, stopwords.not.in.ngrams)
      
      x <- paste(x, collapse=" ") 
      
      x
    })
    
    measure.stem <- unlist(measure.stem)
  ##~
  
  
  ##+ GENERATE MATRIX OF STEMMED AND UNSTEMMED UNIQUE WORDS OF TEXT WINDOWS
  #needed later for generation of list of untagged words (this is not an analytical step)
  #this may be done for the whole text at once, no need to separate between measures as above
  
    #generate base words
    base <- text
    x <- Uniquewords(Cleantext(base))
    
    x <- x[grep("[[:graph:]]", x)]
    
    x <- gsub("^[[:alnum:]]{1,2}$","" , x)
    
    #remove the stopwords which appeared at the beginning of a sentence and begin with upper case
    rows.delete <- which(tolower(x) %in% tolower(stopwordlist))
    
    if (length(rows.delete) > 0) {
      x <- x[-rows.delete]
    }
    
    base <- x[x!=""]
    
    #generate stemmed words and bind both together
    base.stem <- Stem_vector(base)
    measure.unique.baseANDstem  <- cbind(base, base.stem)
    
    #delete stopwords in stemmed format
    x <- base.stem
    rows.delete <- which(tolower(x) %in% tolower(stopwordlist))
    
    if (length(rows.delete) > 0) {
      measure.unique.baseANDstem <- measure.unique.baseANDstem[-rows.delete, ]
    }
  ##~
 
  #full original measure text is overwritten
  textlist[[i]][[3]] <- measure.stem
  names(textlist[[i]])[3] <- paste("measstem_", names(textlist[i]), sep="")
  measure.unique.baseANDstem <- list(measure.unique.baseANDstem) 
  textlist[[i]] <- c(textlist[[i]], measure.unique.baseANDstem) 
  names(textlist[[i]])[4] <- paste("measunibaseANDstem_", names(textlist[i]), sep="")

}

##~-----------------------

time.elapsed <- rbind(time.elapsed, proc.time())
row.names(time.elapsed)[nrow(time.elapsed)] <- "after_cleaning_stemming_measures"
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##~<<<<<<<<<<<<<<<<<<<
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<




##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##+<<<<<<<<<<<<<<<<<<<  SEARCH ALGORITHM - CREATE OCCURRENCE MATRIX FOR CATEGORIES IN TEXT WINDOWS PER DOCUMENT ON BASIS OF TAGGED WORDS
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


words.tag.num  <- nrow(wordcategorylist) 
words.tag <- as.character(wordcategorylist[,1])
occurrence.filename.tag <- c("__w_occ_mat__MR")

#sustainability vocabulary shall be matched also within words 
#therefore their row numbers are identified
#in order to adjust the matching command for these rows
sustainability.rows <- grep("_SUST_", wordcategorylist[,2])
sustainability.words <- as.character(wordcategorylist[sustainability.rows,1])


for (t in seq(length(textlist))) {
  
  measures <- textlist[[t]][[3]]
  
  #initialize matrix to count word occurences
  #one row for each tagged word / one column for each measure
  meas.num <- length(measures)
  initial <- rep(0, meas.num*words.tag.num)
  occurence.matrix <- matrix(initial, nrow = words.tag.num)
  colnames(occurence.matrix) <- paste("meas_",seq(meas.num) , sep="")
  
##+ COUNT OCCURRENCE OF WORDS IN TEXT WINDOWS---------------------
  for (m in seq(meas.num)) {
    
    text <- as.character(measures[m])
    
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
    
    measures[m] <- gsub("[[:blank:]]{2,}", " ", text)
  }
  
  textlist[[t]][[3]] <- measures
  
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
  #matched.words <- unique(evaluationmatrix[which(rowSums(occurence.matrix)>0),"word"])
  #1grams in a first gsub step and n-grams in a next vector step with interim markers
  #unmatched.words <- gsub("(~)(\\s)([\\w]+)(\\s)(/)","", measures, perl=TRUE)
  
  unmatched.words <- c()
  matched.words <- c()
  
  for (m in seq(meas.num)) { 
    
    text <- as.character(measures[m])
    
    #1grams
    matched.words <- c(matched.words, regmatches(text, gregexpr("(?=~)(.*?)(?=/)", text, perl=TRUE))) 
    
    rest <- regmatches(text, gregexpr("(?=~)(.*?)(?=/)", text, perl=TRUE), invert=T)
    rest <- paste(unlist(rest), collapse = " ")
    rest <- gsub("~|/", " ", rest)
    
    #ngrams
    matched.words <- c(matched.words,regmatches(rest, gregexpr("(?=:)(.*?)(?=;)", rest, perl=TRUE)))
    #this would also have worked without lookaround
    #regmatches(rest, gregexpr("(:)(.*?)(;)", rest, perl=TRUE))
    
    rest <- regmatches(rest, gregexpr("(?=:)(.*?)(?=;)", rest, perl=TRUE), invert=T)
    rest <- paste(unlist(rest), collapse = " ")
    rest <- gsub("[[:punct:]]+", "", rest)
    
    unmatched.words <- c(unmatched.words, unlist(strsplit(unlist(rest), " ")))
    
    unmatched.words <- unique(Cleantext(Uniquewords(unmatched.words)))
    
  }
  
  unmatched.words <- unique(Cleantext(Uniquewords(unmatched.words)))
  
  #in case the information is needed how often a single word was matched,
  #counting of ~ or : has to be performed before
  matched.words <- unique(Cleantext(Uniquewords(matched.words)))                        
  
  ##remove stopwords in upper and lower case
  rows.delete <- which(tolower(matched.words) %in% tolower(stopwordlist.stem))
  
  
  if (length(rows.delete) > 0) {
    matched.words <- matched.words[-rows.delete]
  }
  
  rows.delete <- which(tolower(unmatched.words) %in% tolower(stopwordlist.stem))
  if (length(rows.delete) > 0) {
    unmatched.words <- unmatched.words[-rows.delete]
  }
  
  matched.words <- matched.words[matched.words!=""]
  unmatched.words <- unmatched.words[unmatched.words!=""]
  
  matched.words.upper <- matched.words[grep("[[:upper:]]", matched.words)]
  unmatched.words.upper <- unmatched.words[grep("[[:upper:]]", unmatched.words)]
  
  #matched words are fewer than unmatched, thus, searching for those instead is faster
  rows.notmatched <- which(!(textlist[[t]][[4]][,"base.stem"] %in% matched.words))
  notmatched.words.base <- textlist[[t]][[4]][rows.notmatched,"base"]
  
  #the steps before only identify whole words which have been matched
  #on basis of positive entries in the evaluation matrix, for the sustainability vocabulary these entries
  #are only stems of words. This means that in the evaluationmatrix a word "optim" is counted as positive
  # in the case a word like (combined phrase without hyphen, e.g. in German) "optimizationmeasures" has been matched
  #by the word stem. However, this word in the text does not appear in the evaluation matrix.
  #Therefore, such words have to be deleted additionally from the notmatched list.
  
  
  rows.delete <- c()
  for (d in 1:length(sustainability.words)) {
    rows.delete <- c(rows.delete, grep(paste("(\\b)(\\w*)(",sustainability.words[d],")(\\w*)(\\b)", sep=""), notmatched.words.base, perl=TRUE, ignore.case = TRUE))
    
  }
  
  sustainability.words.matched.not.marked <-  unique(notmatched.words.base[rows.delete])
  
  if (length(rows.delete) > 0) {
    notmatched.words.base <- notmatched.words.base[-rows.delete]
  }
  
  notmatched.words.base <- unique(notmatched.words.base)
  notmatched.words.base <- notmatched.words.base[order(notmatched.words.base)]
  notmatched.words.base <- notmatched.words.base[grep("[[:upper:]]", notmatched.words.base)]
  
  #append the not matched words in the respective file
  write(notmatched.words.base, file=paste(wd.notmatched, "words_not_matched.txt", sep=""), append=TRUE)
  
##~---------------------------------
  
  
  #calculate initial match rate
  #this rate only considers the uppercase words and shows if any words have not been tagged 
  #that could have been when following the goal of tagging nouns
  match.rate <- round(length(matched.words)/
                        (length(matched.words)+length(unmatched.words)),
                      #as the value is used in the filename no dots should be included
                      digits = 2)*100 

  
  filename <- gsub(".txt",paste(occurrence.filename.tag, match.rate, ".csv", sep=""),names(textlist[t]))

  
  write.csv(evaluationmatrix, paste(wd.interim, filename, sep=""))
  
 
  time.elapsed <- rbind(time.elapsed, proc.time())
  case.name <- gsub("^.*GER_", "", names(textlist[t]))
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
                      "occurrence.boolean", "wholetext", "interim", "case.combine"))

rm(list = remove)

##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##~<<<<<<<<<<<<<<<<<<<
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<




##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##+<<<<<<<<<<<<<<<<<<< CALCULATE NUMBER OF TAGGED CONCEPTS AND MATCH RATE
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#this part of the code was added in a second step of coding and has not been integrated into above
#main code; it therefore contains several repitions and requires more time than needed
#for smaller amounts of data time effort is still ok
#but integration and optimization of code is necessary for larger data sets

#initialize match rate table
match.rate.table.colnames <- c("case", 
                               "number of measures", 
                               "average number of words per measures",
                               "average number of unique stemmed words per measure", 
                               
                               "average number of unique uppercase stemmed words",
                               "average number of tagged unique uppercase stemmed words"
                                )
match.rate.table <- matrix(nrow=length(textlist),ncol=length(match.rate.table.colnames))
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

all.words <- c()
for (i in 1:length(textlist)) {
  
  text <- textlist[[i]][[3]] 
  
  text <- lapply(text, function(x) {
    
    x <- paste(unlist(x), collapse=" ")
    
    x <- Cleantext(x)
    
    x <- Stem_vector(x)
    
    x <- unlist(strsplit(x, " "))
    
    x <- setdiff(x, c("~~collapse~~"))
    
    x <- paste(x, collapse=" ") 
    
    x
  })
  
  all.words <- c(all.words, text)
}

all.words.unique <- unique(unlist(strsplit(unlist(all.words), " ")))
all.words.unique <- setdiff(all.words.unique, stopwordlist.stem)
all.words.unique <- setdiff(all.words.unique, words.tagged)
all.words.unique  <- all.words.unique [grep("[[:upper:]]",all.words.unique )]



for (i in 1:length(textlist)) {
  
  
  text <- textlist[[i]][[3]]
  

  measure.stem <- lapply(text, function(x) {
    
    x <- paste(unlist(x), collapse=" ")
    
    x <- Cleantext(x)
    
    x <- Stem_vector(x)
    
    x <- unlist(strsplit(x, " "))
    
    x <- setdiff(x, c("~~collapse~~"))
    
    x <- paste(x, collapse=" ") 
    
    x
  })
  
  measure.stem <- unlist(measure.stem)

  
  
  words.per.measure.average <- round(mean(unlist(lapply(measure.stem, function(x) {
    
    x <- unlist(strsplit(x, " "))
    
    length(x)
    
    
  }))), d=0)
  
  
  words.unique.per.measure.average <- round(mean(unlist(lapply(measure.stem, function(x) {
    
    x <- unlist(strsplit(x, " "))
    
    x <- unique(tolower(x))
    
    length(x)
    
    
  }))), d=0)
  
  
  
  unique.uppercase.stemmed.per.measure <- round(mean(unlist(lapply(measure.stem, function(x) {
    
    total <- length(unique(tolower(unlist(strsplit(x, " ")))))
    
    
    x <- unique(unlist(strsplit(x, " ")))
  
        if (length(grep("[[:upper:]]", x))>0) {
      
      x <- x[-grep("[[:upper:]]",x)]
      
    }
    
    minusupper <- length(x)
    
    #number of nouns
    total-minusupper
    
  }))), d=0)
  
  
  
  nouns.unique.tag.rate.per.measure.average <- round(mean(unlist(lapply(measure.stem, function(x) {
    
    
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
  
  
  match.rate.table[i,"case"] <- names(textlist)[i]
  
  match.rate.table[i,"number of measures" ] <- length(measure.stem)
  
  match.rate.table[i,"average number of words per measures"] <- words.per.measure.average
  
  match.rate.table[i, "average number of unique stemmed words per measure"] <- words.unique.per.measure.average
  
  match.rate.table[i, "average number of unique uppercase stemmed words"] <-   unique.uppercase.stemmed.per.measure 
  
  match.rate.table[i, "average number of tagged unique uppercase stemmed words"] <-  nouns.unique.tag.rate.per.measure.average
  
  
}

setwd(wd.final)
write.csv(match.rate.table, paste(wd.final, "match_rate_table.csv", sep=""))

##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##~<<<<<<<<<<<<<<<<<<< 
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<



##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##+<<<<<<<<<<<<<<<<<<< GENERATE RELATIVE COOCCURRENCE MATRICES
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##count normalized co-occurrence of categories within each text
#(co-occurrence of category1/category2 within X text windows of a text)/(number of all text windows of a text)

library("plyr")

setwd(wd.interim)
occurrence.filename.tag <- c("__w_occ_mat__MR")
files <- list.files(pattern = paste("^.*", occurrence.filename.tag, "[[:digit:]]+.csv$", sep=""))
cooccurrence.filename.tag <- c("__cat_coocc_mat")

categories <- names(wordlists)
categories<- gsub("c11__CE__climate_protection_tools_measures_aspects", 
                  "c10__CE__climate_protection_general_and_strategy",  
                  categories)

categories <- unique(categories)
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
  
  #if reading in of the csv generates a column with numbers (old rownumbers) this column is deleted
  if(colnames(evaluationmatrix)[1] == "X") {
    evaluationmatrix <- evaluationmatrix[,-c(1)]
  }
  
  #count the number of measures of the case
  meas.totalnum <- ncol(evaluationmatrix)-2
  
  case.name <- gsub(paste("(^.*GER__)([A-Za-z]+)(__.*$)"), "\\2",files[t])
  
  #delete sustainability category words which had been assigned in a wrong way
  #spar does not count as c161_prohibition_limits....
  delete.rows <- intersect(grep("spar", evaluationmatrix[,"word"]), grep("c161", evaluationmatrix[,"category"]))
  if (length(delete.rows)>0) {
    evaluationmatrix <-  evaluationmatrix[-delete.rows,]   
  }
  
  
  delete.rows <- intersect(grep("Vermi", evaluationmatrix[,"word"]), grep("sufficiency_avoidance", evaluationmatrix[,"category"]))
  if (length(delete.rows)>0) {
    evaluationmatrix <-  evaluationmatrix[-delete.rows,]   
  }
  
  delete.rows <- intersect(grep("^Bau$|^Werk$", evaluationmatrix[,"word"]), grep("AFLV", evaluationmatrix[,"category"]))
  if (length(delete.rows)>0) {
    evaluationmatrix <-  evaluationmatrix[-delete.rows,]   
  }
  
  delete.rows <- intersect(grep("^Baeum$", evaluationmatrix[,"word"]), grep("intermediate_products", evaluationmatrix[,"category"]))
  if (length(delete.rows)>0) {
    evaluationmatrix <-  evaluationmatrix[-delete.rows,]   
  }
  
  delete.rows <- intersect(grep("^Haushalt$|^Kind$|^Verbind$|^Materiali$|^Zusammenstell$|^Raeum$|^Unterlag$|^Buero$|^Bueros$|^Lieg$|^Tier$|^Abnehm$", evaluationmatrix[,"word"]), grep("CF", evaluationmatrix[,"category"]))
  if (length(delete.rows)>0) {
    evaluationmatrix <-  evaluationmatrix[-delete.rows,]   
  }
  
  #combine the two climate protection categories
  evaluationmatrix[,"category"] <- gsub("c11__CE__climate_protection_tools_measures_aspects", 
                                        "c10__CE__climate_protection_general_and_strategy",  
                                        evaluationmatrix[,"category"])
  
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
      result.cooccurrence.case[,category] <- round((cooccurrence/meas.totalnum) ,digits=4)
      
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
      
      result.cooccurrence.case[,category] <- round((cooccurrence/meas.totalnum) ,digits=4)
      
      next(i)
      
      
    }
    
    #>>>in case there are more than one columns fulfilling the current category build rowSums to calculate cooccurrence
    
    cooccurrence <- rowSums(category.subset)
    
    result.cooccurrence.case[,category] <- round((cooccurrence/meas.totalnum) ,digits=4)
    
    
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
##~<<<<<<<<<<<<<<<<<<<
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<



##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##+<<<<<<<<<<<<<<<<<<< CALCULATE AVERAGE COOCCURRENCE MATRIX
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

##START build mean values----------------------------------
setwd(wd.interim)
files <- list.files(pattern = paste("^.*", cooccurrence.filename.tag, ".csv$", sep=""))


#generation of mean values by loading one file after another and extraction the cooccurrence column only
#-> hence wide format data
#this is added to a new mean value matrix which is finally used to calculate the mean value over all cases

for (t in seq(length(files))) {   
  
  cooccurrence.matrix.case <- read.csv(files[t]) 
  
  
  #if reading in of the csv generates a column with numbers (old rownumbers) this column is deleted
  if(colnames(cooccurrence.matrix.case)[1] == "X") {
    
    row.names(cooccurrence.matrix.case) <- as.character(cooccurrence.matrix.case[,1])
    cooccurrence.matrix.case <- cooccurrence.matrix.case[,-c(1)]
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
filename <- paste("GER__Lower_Saxony_regional_centers_mean", cooccurrence.filename.tag, ".csv", sep="")
filename <- paste(wd.interim, filename, sep="")
write.csv(cooccurrence.matrix.mean, filename)

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##~<<<<<<<<<<<<<<<<<<< 
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
