##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##START <<<<<<<<<<<<< LOAD DEFINITIONS OPTIONS FUNCTIONS (incl. directories, libraries, etc.)
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
wd.current <- getwd()

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
  
  for (i in 1:length(files.csv)) {
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
time.elapsed <- rbind(time.elapsed, proc.time())
row.names(time.elapsed)[nrow(time.elapsed)] <- "LOAD_DEFINITIONS_OPTIONS_FUNCTIONS"
time.elapsed
setwd(wd.current)
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##END <<<<<<<<<<<<<<<<<<< LOAD DEFINITIONS OPTIONS FUNCTIONS (incl. directories, libraries, etc.)
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
