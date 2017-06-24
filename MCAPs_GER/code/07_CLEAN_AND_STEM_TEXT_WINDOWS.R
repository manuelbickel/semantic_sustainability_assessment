##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##START <<<<<<<<<<<<<<<<<<< CLEAN AND STEM ALL TEXT WINDOWS
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

wd.current <- getwd()


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
  names(workingdata.list[[i]])[3] <- paste("windows_stem_", names(workingdata.list[i]), sep="")
  measure.unique.baseANDstem <- list(measure.unique.baseANDstem) 
  workingdata.list[[i]] <- c(workingdata.list[[i]], measure.unique.baseANDstem) 
  names(workingdata.list[[i]])[4] <- paste("windows_base_and_stem_", names(workingdata.list[i]), sep="")
  
}

##~-----------------------

time.elapsed <- rbind(time.elapsed, proc.time())
row.names(time.elapsed)[nrow(time.elapsed)] <- "CLEAN_AND_STEM_TEXT_WINDOWS"
time.elapsed
setwd(wd.current)

##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##END <<<<<<<<<<<<<<<<<<< CLEAN AND STEM ALL TEXT WINDOWS
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
