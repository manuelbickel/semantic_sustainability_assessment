#COOCCURRENCE IN WIDE FORMAT
library("ggplot2")
library("reshape2")
library("plyr")


#delete words
delete.word.rows <- c("Beschreib", "Eur", "Lag", "lag", 
                      #"Investitionskost", "Kost", 
                      "Eur", "Kombination", 
                      # "Kosteneinschaetz",
                      "Zug", "Material", "Ausbau", "Bearbeit", "Bearbeitung",
                      #"Finanzier", 
                      "Einschaetz", "Rahm", "Lok", "Kontakt", "Einstellung", "Modell", "Konzept",
                      "Untersuch", "Land", "Geraet", "Darstell", "Statist", "Hilf", "Reihe", "Reih", "Einricht", "Bestandteil",
                      "Vorbereit", "Einstell", "Aufbereit", "Erstellung", "Erstell",
                      "Roll", "Wass", "Led", "Mitt", "Bestimm", "Ausricht", "Reis", "Staerk")


#"Energieverbrauch", "Energieprodukt"

#START evaluation system cooccurrence----------------------------------
setwd(wd.interim)
occurrence.filename.tag <- c("__w_occ_mat__MR")
files <- list.files(pattern = paste("^.*", occurrence.filename.tag, "[[:digit:]]+.csv$", sep=""))
cooccurrence.filename.tag <- c("__cat_coocc_mat")

##START count (percentaged) co-occurrence of categories within each text: (co-occurrence of category1/category2 within X measures of a text)/(number of all measures in a text)-------------------------

###START - do counting and create results matrix-------------------------

###as memory is an issue the cooccurrences for each category are calculated per file
###and then stored in a txt file to be called for later calculations

categories <- names(wordlists)

#categories <- gsub(".*conversion.*", "c00__EN__conversion",  categories)

#categories <- gsub(".*distribution.*", "c00__EN__distribution",  categories)

#categories <- gsub(".*EN__sales.*", "c00__EN__sales",  categories)
#categories <- gsub(".*spec__sales.*", "c00__EN__sales",  categories)

#categories <- gsub(".*spec__mobility_car.*", "c00__IMWW__mobility_car",  categories)
#categories <- gsub(".*spec__mobility_car.*", "c00__IMWW__mobility_car",  categories)
#categories <- gsub(".*mobility_biofuels.*", "c00__IMWW__mobility_car",  categories)
#categories <- gsub(".*mobility_plane.*", "c00__IMWW__mobility_plane",  categories)

#categories <- gsub(".*mobility_public.*", "c00__IMWW__mobility_foot_bike_train",  categories)
#categories <- gsub(".*mobility_renewable.*", "c00__IMWW__mobility_foot_bike_train",  categories)
#categories <- gsub(".*mobility_bike.*", "c00__IMWW__mobility_foot_bike_train",  categories)

#categories <- gsub(".*mobility_transport.*", "c00__IMWW__mobility_transport_of_goods",  categories)

#categories <- gsub(".*energy_resources.*", "c00__EN__resources",  categories)
#categories <- gsub(".*spec__resources.*", "c00__EN__resources",  categories)

#categories <- gsub(".*spec__building.*", "c00__BC__standards_and_rehabilitation",  categories)
#categories <- gsub(".*spec__climatisation.*", "c00__BC__climatisation",  categories)

categories<- gsub("c11__CE__climate_protection_tools_measures_aspects", 
                  "c10__CE__climate_protection_general_and_strategy",  
                                     categories)

categories <- unique(categories)


categories <- categories[order(categories)]

categories.num <- length(categories)

#initial
cooccurrence.matrix <- matrix(rep(0, categories.num*categories.num), nrow = categories.num)
row.names(cooccurrence.matrix) <- categories[order(categories)]
colnames(cooccurrence.matrix) <- categories[order(categories)]

#word occurrence
words.occurrence.topten <- as.data.frame(matrix(c("word", "category", "relative_occurrence", "document"), nrow=1))
colnames(words.occurrence.topten) <- c("word","category", "relative_occurrence", "document")


words.occurrence.topten <- data.frame(word = as.character(),
                                  category = as.character(),
                                  relative_occurrence = as.numeric(),
                                 document = as.character())
                              

for (t in seq(length(files))) {   
  
  #print(paste("t:", t, sep=""))
  
  evaluationmatrix <- read.csv(files[t]) 
  
  #if reading in of the csv generates a column with numbers (old rownumbers) this column is deleted
  if(colnames(evaluationmatrix)[1] == "X") {
    evaluationmatrix <- evaluationmatrix[,-c(1)]
  }

  #count the number of measures of the case
  meas.totalnum <- ncol(evaluationmatrix)-2
  
  #casename
  case.name <- gsub(paste("(^.*GER__)([A-Za-z]+)(__.*$)"), "\\2",files[t])
  
  #delete rows with words that occur in almost all measures (not representative)
  evaluationmatrix <- evaluationmatrix[-which(as.character(evaluationmatrix[,"word"]) %in% delete.word.rows),]
  #delete sustainability category words which had been assigned in a wrong way
  #spar does not count as c161_prohibition_limits....
  delete.rows <- intersect(grep("spar", evaluationmatrix[,"word"]), grep("c161", evaluationmatrix[,"category"]))
  
  if (length(delete.rows)>0) {
    evaluationmatrix <-  evaluationmatrix[-delete.rows,]   
  }


  delete.rows <- intersect(grep("Vermi", evaluationmatrix[,"word"]), grep("sufficiency_avoidance", evaluationmatrix[,"category"]))
  
  if (length(delete.rows)>0) {
    evaluationmatrix <-  evaluationmatrix[-delete.rows,]   
  }
  
   
  
  delete.rows <- intersect(grep("^Bau$|^Werk$", evaluationmatrix[,"word"]), grep("AFLV", evaluationmatrix[,"category"]))
  if (length(delete.rows)>0) {
    evaluationmatrix <-  evaluationmatrix[-delete.rows,]   
  }
  

  delete.rows <- intersect(grep("^Baeum$", evaluationmatrix[,"word"]), grep("intermediate_products", evaluationmatrix[,"category"]))
  if (length(delete.rows)>0) {
    evaluationmatrix <-  evaluationmatrix[-delete.rows,]   
  }

  delete.rows <- intersect(grep("^Haushalt$|^Kind$|^Verbind$|^Materiali$|^Zusammenstell$|^Raeum$|^Unterlag$|^Buero$|^Bueros$|^Lieg$|^Tier$|^Abnehm$", evaluationmatrix[,"word"]), grep("CF", evaluationmatrix[,"category"]))
  if (length(delete.rows)>0) {
    evaluationmatrix <-  evaluationmatrix[-delete.rows,]   
  }
  
  
 
  #combine the two climate protection categories
  evaluationmatrix[,"category"] <- gsub("c11__CE__climate_protection_tools_measures_aspects", 
                               "c10__CE__climate_protection_general_and_strategy",  
                               evaluationmatrix[,"category"])
  
 # wordlists[grep("c11|c10", wordlists)]#-----------------------------#########################
  
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
 
  
  #combine categories for the cooccurrence plot
#   evaluationmatrix[,1] <- gsub(".*conversion.*", "c00__EN__conversion",  evaluationmatrix[,1])
#   
#   evaluationmatrix[,1] <- gsub(".*distribution.*", "c00__EN__distribution",  evaluationmatrix[,1])
#   
#   evaluationmatrix[,1] <- gsub(".*EN__sales.*", "c00__EN__sales",  evaluationmatrix[,1])
#   evaluationmatrix[,1] <- gsub(".*spec__sales.*", "c00__EN__sales",  evaluationmatrix[,1])
#   
#   evaluationmatrix[,1] <- gsub(".*spec__mobility_car.*", "c00__IMWW__mobility_car",  evaluationmatrix[,1])
#   evaluationmatrix[,1] <- gsub(".*spec__mobility_car.*", "c00__IMWW__mobility_car",  evaluationmatrix[,1])
#   evaluationmatrix[,1] <- gsub(".*mobility_biofuels.*", "c00__IMWW__mobility_car",  evaluationmatrix[,1])
#   evaluationmatrix[,1] <- gsub(".*mobility_plane.*", "c00__IMWW__mobility_plane",  evaluationmatrix[,1])
#   
#   evaluationmatrix[,1] <- gsub(".*mobility_public.*", "c00__IMWW__mobility_foot_bike_train",  evaluationmatrix[,1])
#   evaluationmatrix[,1] <- gsub(".*mobility_renewable.*", "c00__IMWW__mobility_foot_bike_train",  evaluationmatrix[,1])
#   evaluationmatrix[,1] <- gsub(".*mobility_bike.*", "c00__IMWW__mobility_foot_bike_train",  evaluationmatrix[,1])
#   
#   evaluationmatrix[,1] <- gsub(".*mobility_transport.*", "c00__IMWW__mobility_transport_of_goods",  evaluationmatrix[,1])
#   
#   evaluationmatrix[,1] <- gsub(".*energy_resources.*", "c00__EN__resources",  evaluationmatrix[,1])
#   evaluationmatrix[,1] <- gsub(".*spec__resources.*", "c00__EN__resources",  evaluationmatrix[,1])
#   
#   evaluationmatrix[,1] <- gsub(".*spec__building.*", "c00__BC__standards_and_rehabilitation",  evaluationmatrix[,1])
#   evaluationmatrix[,1] <- gsub(".*spec__climatisation.*", "c00__BC__climatisation",  evaluationmatrix[,1])

  #aggregate the category lines
  evaluationmatrix <- aggregate(evaluationmatrix[2:ncol(evaluationmatrix)], by=list(category=evaluationmatrix$category), FUN=sum)

    #make matrix boolean
  evaluationmatrix[,c(2:ncol(evaluationmatrix))] <- ifelse(evaluationmatrix[,c(2:ncol(evaluationmatrix))]>0,1,0)
  
  #generate the same order as in the results matrix
  evaluationmatrix <- evaluationmatrix[order(evaluationmatrix[,1]),]
  
  #initialize results matrix for the case 
  result.cooccurrence.case <- cooccurrence.matrix
  
  evaluationmatrix <- evaluationmatrix[order(evaluationmatrix[,1]),]
  
  #check if categories in matrix are the same as the loaded categories, they should be,
  #but due to naming mistakes etc. there might be errors
  if (identical(as.character(evaluationmatrix[,1]),row.names(result.cooccurrence.case)) == FALSE)  {
    warning("ERROR: categories of wordlists and evaluationmatrix are not identical, only the intersecting categories are used.
            This might lead to incompatibility of the result files of different files.
            Possible reasons might be mistakes in naming the categories or during reading in the wordlists.")
    
    categories.intersect <- intersect(as.character(evaluationmatrix[,1]),row.names(result.cooccurrence.case))
    categories.num <- length(categories.intersect)
    
    evaluationmatrix <- evaluationmatrix[which(evaluationmatrix[,1] == categories.intersect),]
    evaluationmatrix <- evaluationmatrix[order(evaluationmatrix[,1]),]
    
    result.cooccurrence.case <- result.cooccurrence.case[which(row.names(result.cooccurrence.case) == categories.intersect),]
    result.cooccurrence.case <- result.cooccurrence.case[order(result.cooccurrence.case[,1]),]
    
  }
  
  
####START loop through all categories and check cooccurrence---------------------------------
  for(i in seq(length(evaluationmatrix[,"category"]))) {
    
  #  print(paste("i:", i, sep=""))
    #i=1
    
    #fix the column/category which shall be filled
    category <- colnames(result.cooccurrence.case)[i]
      
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
      result.cooccurrence.case[,category] <- round((cooccurrence/meas.totalnum) ,digits=4)
        
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
      
      result.cooccurrence.case[,category] <- round((cooccurrence/meas.totalnum) ,digits=4)
      
      next(i)
 
      
    }
  
  #>>>in case there are more than one columns fulfilling the current category build rowSums to calculate cooccurrence
    
    cooccurrence <- rowSums(category.subset)
    
    result.cooccurrence.case[,category] <- round((cooccurrence/meas.totalnum) ,digits=4)
    
    
  } #end i loop through the categories 
####END loop through all categories and check cooccurrence---------------------------------
  
  
#####START generate mean value by summing up the numeric part of the matrices for each case----------------------
  #in the last run division by the total number of cases

  
#####somehow the loop inside did produce mistakes by creating the mean, not solved yet, therefore mean is calculated in an extra loop  
#     if (t== 1) {
#     
#     result.cooccurrence.mean <- result.cooccurrence.case
#     
#   } else if (t < length(files)) {
#     
#     result.cooccurrence.mean[,c(2:ncol(result.cooccurrence.case))] <- result.cooccurrence.mean[,c(2:ncol(result.cooccurrence.case))] +
#                                                                       result.cooccurrence.case[,c(2:ncol(result.cooccurrence.case))]
#     
#   } else if (t == length(files)) {
#     
#     result.cooccurrence.mean[,c(2:ncol(result.cooccurrence.case))] <- result.cooccurrence.mean[,c(2:ncol(result.cooccurrence.case))] +
#                                                                       result.cooccurrence.case[,c(2:ncol(result.cooccurrence.case))]
#     
#     
#     result.cooccurrence.mean[,c(2:ncol(result.cooccurrence.case))] <- round(result.cooccurrence.mean[,c(2:ncol(result.cooccurrence.case))]/
#                                                                       t, digits=2)      
#     
#   }
  
  
#####START generate mean value by summing up the numeric part of the matrices for each case----------------------  


  filename <- gsub(paste(occurrence.filename.tag,".*$" ,sep=""),paste(cooccurrence.filename.tag, ".csv", sep=""),files[t])
  filename <- paste(wd.interim, filename, sep="")

  write.csv(result.cooccurrence.case, filename)
  
  
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


##START build mean values----------------------------------
setwd(wd.interim)
files <- list.files(pattern = paste("^.*", cooccurrence.filename.tag, ".csv$", sep=""))

exclude.cases <- c("Hannover96",
                   "Wilhelmshaven__CPC1_main_short"
)

exclude.cases <- grep(paste(exclude.cases, collapse="|"), files)

if (length(exclude.cases) > 0) {
  files <- files[-exclude.cases]
}



#generation of mean values by loading one file after another and extraction the cooccurrence column only
#-> hence wide format data
#this is added to a new mean value matrix which is finally used to calculate the mean value over all cases

for (t in seq(length(files))) {   
  
  cooccurrence.matrix.case <- read.csv(files[t]) 
  
  
  #if reading in of the csv generates a column with numbers (old rownumbers) this column is deleted
  if(colnames(cooccurrence.matrix.case)[1] == "X") {
    
    row.names(cooccurrence.matrix.case) <- as.character(cooccurrence.matrix.case[,1])
    cooccurrence.matrix.case <- cooccurrence.matrix.case[,-c(1)]
  }
  
#   #extract the casename <---------------not required
#   case <-  gsub("^.*GER__", "", files[t])
#   case <-  gsub("__.*$", "", case)
#   case <-  paste("case",t, "__",case, sep="")
 # colnames( cooccurrence.matrix.case)
 # row.names( cooccurrence.matrix.case)
  
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
filename <- paste("GER__Lower_Saxony_regional_centers_mean", cooccurrence.filename.tag, ".csv", sep="")
filename <- paste(wd.interim, filename, sep="")
write.csv(cooccurrence.matrix.mean, filename)

###END - do counting and create results matrix-------------------------


#--------------------------calculate median
# 
# 
# r=4
# c=4
# m1<- matrix(sample(0:1,r*c, replace=TRUE),r,c)
# m2<- matrix(sample(2:3,r*c, replace=TRUE),r,c)
# m3 <- matrix(sample(0:1,r*c, replace=TRUE),r,c)
# 
# #initial with first matrix
# combined.matrices <- split(m1, col(m1))
# elems <- names(combined.matrices)
# 
# #add further matrices
# add <- split(m2, col(m2))
# 
# 
# combined.matrices <- c(list(combined.matrices), list(add))
# combined.matrices <- sapply(elems, function(E) {
#   do.call(rbind,
#           lapply(combined.matrices, function(X) {
#             X[[E]]
#           }))
# }, simplify=FALSE)
# 
# 
# 
# test <- lapply(test, function(x) {
#   
#   apply(x, 2, median)
#   
# })
# 
# unlist(test)
# 
# unsplit(test, f=4 )
# 
# as.matrix(test)
# 
# apply(combined.matrices[[1]], 2, median)
# 
