##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##START <<<<<<<<<<<<<<<<<<< GENERATE NORMALIZED COOCCURRENCE MATRICES AND GET WORDS WITH HIGHEST OCCURRENCE
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##count normalized co-occurrence of categories within each text
#(co-occurrence of category1/category2 within X text windows of a text)/(number of all text windows of a text)
wd.current <- getwd()
setwd(wd.interim)

stopwords.additional <- c("dummy_stopword")

files <- list.files(pattern = paste("^.*", occurrence.filename.tag, "[[:digit:]]+.csv$", sep=""))

categories <- list.dirs(wd.wordlists, recursive=F, full.names = F)
categories <- categories[order(categories)]

categories.num <- length(categories)

#initialize
cooccurrence.matrix.initial <- matrix(rep(0, categories.num*categories.num), nrow = categories.num)
row.names(cooccurrence.matrix.initial) <- categories[order(categories)]
colnames(cooccurrence.matrix.initial) <- categories[order(categories)]

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
  
  case.name <- gsub(paste("(^.*GER)(_+)([A-Za-z]+)(_+)(.*$)"), "\\3",files[t], perl=T)
  
   #find specific combinations of words and categories
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
  cooccurrence.matrix.case <- cooccurrence.matrix.initial
  
  evaluationmatrix <- evaluationmatrix[order(evaluationmatrix[,1]),]
  
  #check if categories in matrix are the same as the loaded categories, they should be,
  #but due to naming mistakes etc. there might be errors
  if (identical(as.character(evaluationmatrix[,1]),row.names(cooccurrence.matrix.case)) == FALSE)  {
    warning("ERROR: categories of wordlists and evaluationmatrix are not identical, only the intersecting categories are used.
            This might lead to incompatibility of the result files of different files.
            Possible reasons might be mistakes in naming the categories or during reading in the wordlists.")
    
    categories.intersect <- intersect(as.character(evaluationmatrix[,1]),row.names(cooccurrence.matrix.case))
    categories.num <- length(categories.intersect)
    
    evaluationmatrix <- evaluationmatrix[which(evaluationmatrix[,1] == categories.intersect),]
    evaluationmatrix <- evaluationmatrix[order(evaluationmatrix[,1]),]
    
    cooccurrence.matrix.case <- cooccurrence.matrix.case[which(row.names(cooccurrence.matrix.case) == categories.intersect),]
    cooccurrence.matrix.case <- cooccurrence.matrix.case[order(cooccurrence.matrix.case[,1]),]
    
  }
  
  
  ####START loop through all categories and check cooccurrence---------------------------------
  for(i in seq(length(evaluationmatrix[,"category"]))) {
    
    #fix the column/category which shall be filled
    category <- colnames(cooccurrence.matrix.case)[i]
    
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
      cooccurrence.matrix.case[,category] <- round((cooccurrence/text.windows.num) ,digits=4)
      
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
      
      cooccurrence.matrix.case[,category] <- round((cooccurrence/text.windows.num) ,digits=4)
      
      next(i)
      
      
    }
    
    #>>>in case there are more than one columns fulfilling the current category build rowSums to calculate cooccurrence
    
    cooccurrence <- rowSums(category.subset)
    
    cooccurrence.matrix.case[,category] <- round((cooccurrence/text.windows.num) ,digits=4)
    
    
  }
  ####END loop through all categories and check cooccurrence---------------------------------
  
  filename <- gsub(paste(occurrence.filename.tag,".*$" ,sep=""),paste(cooccurrence.filename.tag, ".csv", sep=""),files[t])
  filename <- paste(wd.interim, filename, sep="")
  
  write.csv(cooccurrence.matrix.case, filename)
  
  
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


time.elapsed <- rbind(time.elapsed, proc.time())
row.names(time.elapsed)[nrow(time.elapsed)] <- "NORMALIZED_COOCCURRENCE_MATRICES_AND_TOP_WORDS"
setwd(wd.current)
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##END <<<<<<<<<<<<<<<<<<< GENERATE NORMALIZED COOCCURRENCE MATRICES AND GET WORDS WITH HIGHEST OCCURRENCE
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
