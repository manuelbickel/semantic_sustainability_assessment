#####Purpose of the code: write the same predefined text (code structure) 
#####at the filehead of textfiles - the text is used to set text processing
#####variables manually directly in the txt document


#####START - Settings and explanations-----------------------

setwd("M:/Science/Promotion/Working_Documents/R/niedersachsen_city_evaluation/CPC_evaluation_of_measures/interim")

#location and name of the code file
cfil <- "M:/Science/Promotion/Working_Documents/R/main_folder/analysis_code_to_extract_measures.txt"

#it is assumed that .txt files are read in, other formats have to be treated separately, if required

#####END - Settings and explanations-----------------------




#START - write the analysis code into beginning of each file-------------------------------

#read analysis code 
code <- readLines(cfil, encoding = "UTF-8")

#read all txt files in folder and write code into filehead
files <- list.files(pattern="*.txt")

invisible(for (i in files) {
  text <- readLines(i, encoding = "UTF-8")
  
  text <- c(code, text)
  
  writeLines(text,i, useBytes = TRUE) #to suppress any encoding issues and keep UTF-8 useBytes = TRUE is set
})
##END write the analysis code into beginning of each file------------------------------------


##START - Check if warnings are only of the uncritical type - incomplete final line-----------------
counter <- 0
invisible(sapply(names(warnings()), function(i) {
  
  if (grepl("incomplete final line found on", i)==TRUE) {
    counter <<- counter + 1
  }
 }))
print (paste(counter, "of", length(warnings()), "warnings are of the type: incomplete final line"), sep = " ")   
##END - Check if warnings are only of the uncritical type - incomplete final line-----------------

rm(list = ls()) #clear workspace
