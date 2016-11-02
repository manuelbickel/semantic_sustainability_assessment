##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##START <<<<<<<<<<<<<<<<<<< READ AND PREPARE THESAURI AND STOPWORDLIST DATA
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

wd.current <- getwd()
setwd(wd.wordlists)

##+ PREPARE THESAURI WORDLISTS---------------------------
#read all txt files in folder and build a list of their names
#then do some preliminary cleaning (trim, remove comments, etc.)

##+ READ THE THESAURI DATA AND CLEAN THE WORDLISTS  
#initialize
dirs <- list.dirs(wd.wordlists, recursive=F, full.names = F)
wordlists <- vector(mode = "list", length = length(dirs))

for (f in 1:length(dirs)) {
  
  ##+ READ
  setwd(paste0(wd.wordlists,dirs[f]))
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
stopwordlist.stem <- stopwordlist.stem[grep("[[:graph:]]", stopwordlist.stem)]

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
row.names(time.elapsed)[nrow(time.elapsed)] <- "PREPARE_THESAURI_AND_STOPWORDS"
time.elapsed
setwd(wd.current)
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##END READ AND PREPARE THESAURI AND STOPWORDLIST DATA<<<<<<<<<<<<<<<<<<<
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
