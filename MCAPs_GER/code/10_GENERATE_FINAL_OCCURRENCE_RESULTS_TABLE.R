##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##START <<<<<<<<<<<<<<<<<<< GENERATE FINAL OCCURRENCE RESULTS TABLE
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

wd.current <- getwd()

setwd(wd.interim)
##+ INITIALIZE VARIABLE NAMES ETC.--------------------------
#delete words
#in case stopwords which were only identified during an analytical step 
#and should be removed in addition to the previously defined stopwordlist
stopwords.additional <- c("dummy_stopword")

categories <- list.dirs(wd.wordlists, recursive=F, full.names = F)
categories.num <-length(categories)

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
  colnames(results.occ.final)[t] <- gsub(paste("(^.*GER)(_+)([A-Za-z]+)(_+)(.*$)"), "\\3",files[t], perl=T)
  
  
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
write.csv(results.occ.final, paste0(wd.final,file.occurrence.results))

##~-----------------------------------------

time.elapsed <- rbind(time.elapsed, proc.time())
row.names(time.elapsed)[nrow(time.elapsed)] <- "GENERATE_FINAL_OCCURRENCE_RESULTS_TABLE"
time.elapsed
setwd(wd.current)
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##END <<<<<<<<<<<<<<<<<<< GENERATE FINAL OCCURRENCE RESULTS TABLE
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<