##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##START <<<<<<<<<<<<<<<<<<< CALCULATE NUMBER OF TAGGED CONCEPTS AND MATCH RATE
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
wd.current <- getwd()
setwd(wd.final)

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



time.elapsed <- rbind(time.elapsed, proc.time())
row.names(time.elapsed)[nrow(time.elapsed)] <- "CALCULATE_MATCH_RATE_TABLE"
setwd(wd.current)

##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##END <<<<<<<<<<<<<<<<<<< CALCULATE NUMBER OF TAGGED CONCEPTS AND MATCH RATE
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
