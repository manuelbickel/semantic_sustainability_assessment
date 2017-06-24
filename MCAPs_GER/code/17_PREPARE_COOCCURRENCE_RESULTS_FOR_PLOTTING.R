##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##START <<<<<<<<<<<<< PREPARE DATA FOR COOCCURRENCE PLOTS
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

wd.current <- getwd()


#COOCCURRENCE IN WIDE FORMAT

setwd(wd.interim)
categories <- list.dirs(wd.wordlists, recursive=F, full.names = F)
categories.num <-length(categories)

files <- list.files(pattern = paste0(cooccurrence.filename.tag, "\\.csv"))

#fix the categories of the mean case as reference for other cases to be compared later
cooccurrence.case.reference <- grep(filenamebeginning.cooccurrence.matrix.mean, files)

##+READ DATA-----------------
cooccurrence.case <- read.csv(files[cooccurrence.case.reference], header=T, check.names = T)

#if reading in of the csv generates columns with numbers (old rownumbers) these are deleted
delete.columns <- grep("(X$)|(X\\.)(\\d+)($)", colnames(cooccurrence.case), perl=T)

if (length(delete.columns) > 0) {
  row.names(cooccurrence.case) <- as.character(cooccurrence.case[, grep("^X$", colnames(cooccurrence.case))])
  cooccurrence.case <- cooccurrence.case[,-delete.columns]
  colnames(cooccurrence.case) <- gsub("^X", "",  colnames(cooccurrence.case))
  colnames(cooccurrence.case) <- gsub("\\.", "-",  colnames(cooccurrence.case))
}


if (identical(as.character( colnames(cooccurrence.case)), as.character( row.names(cooccurrence.case))) == FALSE) {
  warning("Matrix not symmetric. Following calculations will produce wrong results.")
}
##~-------------------------


##+RENAME CATEGORIES---------------------------------
row.names(cooccurrence.case) <- gsub("--","><",row.names(cooccurrence.case) )
row.names(cooccurrence.case) <- paste0("<",row.names(cooccurrence.case) )
row.names(cooccurrence.case) <- paste0(row.names(cooccurrence.case), ">" )
colnames(cooccurrence.case) <- row.names(cooccurrence.case)
##~-------------------


##~ ORDER CATEGORIES (ALIGN CATEGORIES WITH SAME METACATEGORY)----------------
ordered.levels.alphabetic <- order(colnames(cooccurrence.case))
cooccurrence.case <- cooccurrence.case[ordered.levels.alphabetic,ordered.levels.alphabetic]


order.interim <- data.frame(category = as.character(row.names(cooccurrence.case)),
                            value = as.numeric(rowSums(cooccurrence.case)) )

#aggregate values by meta-category
order.interim.agg <- order.interim

order.interim.agg[,1] <-  gsub("(<[\\d]>)(<[\\w]{3}>)(<[\\w]+>)(.*$)", "\\3",order.interim.agg[,1] , perl=T)
order.interim.agg <- aggregate(order.interim.agg[,2], by=list(category=order.interim.agg$category), FUN=mean)

#replace the original values for each category 
#by the aggregated sum of its meta category in the interim list
for (c in 1:nrow(order.interim.agg)) {
  replace <- order.interim.agg[c,]
  order.interim[grep(replace[,1], order.interim[,1]),2] <- replace[,2]
  
}

order.cluster.occ <- order(order.interim[,2], decreasing=T)

cooccurrence.case <- cooccurrence.case[order.cluster.occ ,order.cluster.occ]

#for the energy/social system plot
x <- round(as.matrix(cooccurrence.case), d=3)

#for the unscaled plot
x_full <- x

#for the sustainabilty cooccurrence plot
x_sust <- x

##~--------------------
time.elapsed <- rbind(time.elapsed, proc.time())
row.names(time.elapsed)[nrow(time.elapsed)] <- "PREPARE_DATA_FOR_COOCCURRENCE_PLOTS"
time.elapsed
setwd(wd.current)
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##END <<<<<<<<<<<<< PREPARE DATA FOR COOCCURRENCE PLOTS
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
