##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##START <<<<<<<<<<<<<<<<<<< WRITE EMPTY CODE STRUCTURE FOR TEXT PROCESSING AT FILE HEADS
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
wd.current <- getwd()
setwd(wd.source)
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
time.elapsed <- rbind(time.elapsed, proc.time())
row.names(time.elapsed)[nrow(time.elapsed)] <- "ADD_PROCESSING_CODE_AT_FILEHEAD"
time.elapsed
setwd(wd.current)
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##END WRITE EMPTY CODE STRUCTURE FOR TEXT PROCESSING AT FILE HEADS <<<<<<<<<<<<<<<<<<< 
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
