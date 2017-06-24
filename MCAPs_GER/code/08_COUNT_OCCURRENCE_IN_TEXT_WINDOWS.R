##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##START <<<<<<<<<<<<<<<<<<<  SEARCH ALGORITHM - CREATE OCCURRENCE MATRIX FOR CATEGORIES IN TEXT WINDOWS PER DOCUMENT ON BASIS OF TAGGED WORDS
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

wd.current <- getwd()
setwd(wd.interim)

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
  #words.notmatched <- gsub("(~)(\\s)([\\w]+)(\\s)(/)","", text.windows.split, perl=TRUE)
  
  words.notmatched <- c()
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
    
    words.notmatched <- c(words.notmatched, unlist(strsplit(unlist(rest), " ")))
    
    words.notmatched <- unique(CleanVector(UniqueWords(words.notmatched)))
    
  }
  
  words.notmatched <- unique(CleanVector(UniqueWords(words.notmatched)))
  
  #in case the information is needed how often a single word was matched,
  #counting of ~ or : has to be performed before
  words.matched <- unique(CleanVector(UniqueWords(words.matched)))                        
  
  ##remove stopwords in upper and lower case
  rows.delete <- which(tolower(words.matched) %in% tolower(stopwordlist.stem))
  
  
  if (length(rows.delete) > 0) {
    words.matched <- words.matched[-rows.delete]
  }
  
  rows.delete <- which(tolower(words.notmatched) %in% tolower(stopwordlist.stem))
  if (length(rows.delete) > 0) {
    words.notmatched <- words.notmatched[-rows.delete]
  }
  
  words.matched <- words.matched[words.matched!=""]
  words.notmatched <- words.notmatched[words.notmatched!=""]
  
  words.matched.upper <- words.matched[grep("[[:upper:]]", words.matched)]
  words.notmatched.upper <- words.notmatched[grep("[[:upper:]]", words.notmatched)]
  
  #matched words are fewer than unmatched, thus, searching for those instead is faster
  rows.notmatched <- which(!(workingdata.list[[t]][[4]][,"base.stem"] %in% words.matched))
  words.notmatched.base <- workingdata.list[[t]][[4]][rows.notmatched,"base"]
  
  #the steps before only identify whole words which have been matched
  #on basis of positive entries in the evaluation matrix, for the sustainability vocabulary these entries
  #are only stems of words. This means that in the evaluationmatrix a word "optim" is counted as positive
  # in the case a word like (combined phrase without hyphen, e.g. in German) "optimizationmeasures" has been matched
  #by the word stem. However, this word in the text does not appear in the evaluation matrix.
  #Therefore, such words have to be deleted additionally from the notmatched list.
  
  
  rows.delete <- c()
  for (d in 1:length(sustainability.words)) {
    rows.delete <- c(rows.delete, grep(paste("(\\b)(\\w*)(",sustainability.words[d],")(\\w*)(\\b)", sep=""), words.notmatched.base, perl=TRUE, ignore.case = TRUE))
    
  }
  
  sustainability.words.matched.not.marked <-  unique(words.notmatched.base[rows.delete])
  
  if (length(rows.delete) > 0) {
    words.notmatched.base <- words.notmatched.base[-rows.delete]
  }
  
  words.notmatched.base <- unique(words.notmatched.base)
  words.notmatched.base <- words.notmatched.base[order(words.notmatched.base)]
  words.notmatched.base <- words.notmatched.base[grep("[[:upper:]]", words.notmatched.base)]
  
  #append the not matched words in the respective file
  write(words.notmatched.base, file=paste(wd.notmatched, file.words.not.matched, sep=""), append=TRUE)
  
  ##~---------------------------------
  
  
  #calculate initial match rate
  #this rate only considers the uppercase words and shows if any words have not been tagged 
  #that could have been when following the goal of tagging nouns
  match.rate <- round(length(words.matched)/
                        (length(words.matched)+length(words.notmatched)),
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


time.elapsed <- rbind(time.elapsed, proc.time())
row.names(time.elapsed)[nrow(time.elapsed)] <- "COUNT_OCCURRENCE_IN_TEXT_WINDOWS"
time.elapsed
setwd(wd.current)
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##END SEARCH ALGORITHM - CREATE OCCURRENCE MATRIX FOR CATEGORIES IN TEXT WINDOWS PER DOCUMENT ON BASIS OF TAGGED WORDS<<<<<<<<<<<<<<<<<<<
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
