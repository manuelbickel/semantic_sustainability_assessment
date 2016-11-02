##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##START <<<<<<<<<<<<<<<<<<< GENERATE AVERAGE COOCCURRENCE MATRIX
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
wd.current <- getwd()
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
filename <- paste0(filenamebeginning.cooccurrence.matrix.mean, cooccurrence.filename.tag, ".csv")
filename <- paste(wd.interim, filename, sep="")
write.csv(cooccurrence.matrix.mean, filename)
##~----------------------


time.elapsed <- rbind(time.elapsed, proc.time())
row.names(time.elapsed)[nrow(time.elapsed)] <- "GENERATE_AVERAGE_COOCCURRENCE_MATRIX"
setwd(wd.current)
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##END <<<<<<<<<<<<<<<<<<< GENERATE AVERAGE COOCCURRENCE MATRIX
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
