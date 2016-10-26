

library("ggplot2")
library("reshape2")
warning("Please make sure that categories have been named and combined by hand within the script.")
library("plyr")
#library("cowplot")


#START - directories
wd.main <- c("M:/Science/Promotion/R_CPC_evaluation_of_measures/")

wd.source <- paste(wd.main, "text_files/", sep="")

wd.interim <- paste(wd.main, "results_interim/", sep="")

wd.final <- paste(wd.main, "results_final/", sep="")

wd.wordlists <- paste(wd.main, "wordlists/", sep="")

wd.stopwords <- paste(wd.main, "wordlists_stopwords/", sep="")

wd.encoding <- paste(wd.main, "encoding/", sep="")

wd.notmatched <- paste(wd.main, "wordlist_notmatched_words/", sep="")
#initialize an empty txt.file if there is no file, yet
if (c("wordlist_notmatched_words.txt") %in% list.files(path = wd.notmatched) == FALSE) {
  writeLines(c(""), paste(wd.notmatched, "wordlist_notmatched_words.txt", sep="") )
}





##START read word list names---------------------------------------
setwd(paste(wd.wordlists, "wordlists_for_analysis/", sep=""))
dir_current_root <- paste(getwd(), "/", sep="")
dirs <- list.dirs()
#the root dir is included as a single dot and is excluded, also leading dots are deleted and a final slash is added
dirs <- dirs[grep("[[:alpha:]]", dirs)]
dirs <- paste(gsub("./", "", dirs, fixed=TRUE),"/" , sep="")

#initialize list
wordlists <- vector(mode = "list", length = length(dirs))


for (f in 1:length(dirs)) {
  
  setwd(paste(dir_current_root, dirs[f], sep=""))
  
  category <- gsub("[\\W]", "", dirs[f], perl=TRUE)
  category <- gsub("[[:blank:]]", "_", category)
  
  listname <- category
  
  ###END create wordlistnames on basis of names of the directory--------------------  
  
  #add a number to the wordlist in case any names are duplicates
  listname <- paste("c",f,"__",listname,sep="")
  
  names(wordlists)[f] <- listname
  
}
##END read word list names---------------------------------------

# wd <- getwd()
# setwd(wd.final)
# 
# write.csv(noquote(gsub("c[[:digit:]]+__", "",names(wordlists))), quote=F, row.names=F, col.names=F, "names_wordlists.csv")
# setwd(wd)



#can also be done right before ploting with the result.matrix -> maybe easier....
#generate textlist selection for plotting 
#i.e. combine the hannover matrices to one
# names(textlist)
# hannover <- "x_GER_Hannover_2008_KSAP_Materialband_CPC1_main_appendix"
# for (i in 1:8) {
#  }



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



#from script to check which have the highest occurrence, the following table shows up,
#   #for a threshold of 80%
#   word             category
# 18044   Energieeinspar      c35__EN__saving
# 17268 Energieverbrauch c26__EN__consumption
# 17855           Energi     c33__EN__general
# 18031   Energieprodukt       c34__EN__sales


occurrence.filename.tag <- c("__w_occ_mat__MR")

#START evaluation energy chain----------------------------------

##START select the categories which shall be checked on occurrence from the available wordlistnames------------------
categories <- names(wordlists)
categories<- gsub("c11__CE__climate_protection_tools_measures_aspects", 
                  "c10__CE__climate_protection_general_and_strategy",  
                  categories)

categories.num <-length(categories)
##END select the categories which shall be checked on occurrence from the available wordlistnames------------------


##START count (percentaged) occurrence of categories within each text: (occurrence of category within X measures of a text)/(number of all measures in a text)-------------------------

###START - do counting and create results matrix-------------------------
setwd(wd.interim)
files <- list.files(pattern = paste("^.*", occurrence.filename.tag, "[[:digit:]]+.csv$", sep=""))

exclude.cases <- c("Hannover96",
                   "Wilhelmshaven__CPC1_main_short"
)

exclude.cases <- grep(paste(exclude.cases, collapse="|"), files)

if (length(exclude.cases) > 0) {
  files <- files[-exclude.cases]
}



#initial
result.energychain <- matrix(rep(0, categories.num*length(files)), nrow = categories.num)
row.names(result.energychain) <- categories
colnames(result.energychain) <- rep("dummy", ncol(result.energychain))


for (t in seq(length(files))) {
  
  #print(files[t])  
  evaluationmatrix <- read.csv(files[t], header = TRUE)
  
  
  #if reading in of the csv generates a column with numbers (old rownubers) this column is deleted
  if(colnames(evaluationmatrix)[1] == "X") {
    evaluationmatrix <- evaluationmatrix[,-c(1)]
    
  }
  
  meas.totalnum <- ncol(evaluationmatrix)-2
  
  #delete rows with words that occur in almost all measures (not representative)
  evaluationmatrix <- evaluationmatrix[-which(as.character(evaluationmatrix[,"word"]) %in% delete.word.rows),]
  
  
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
  
  
  
  #delete the words column
  evaluationmatrix <-   evaluationmatrix[,-c(1)]
  
  #aggregate the category lines
  
  
  
  
  
  evaluationmatrix <- aggregate(evaluationmatrix[2:ncol(evaluationmatrix)], by=list(category=evaluationmatrix$category), FUN=sum)
  
  
  #make matrix boolean
  evaluationmatrix[,c(2:ncol(evaluationmatrix))] <- ifelse(evaluationmatrix[,c(2:ncol(evaluationmatrix))]>0,1,0)
  
  
  #in future steps maybe also the year, etc. is extracted and used as name
  colnames(result.energychain)[t] <- gsub(paste("(^.*GER__)([A-Za-z]+)(__.*$)"), "\\2",files[t])
  
  
  for(i in seq(length(categories))) {
    
    #print(categories[i])
    #i=1
    category <- categories[i]
    
    #select only the lines which are connected to (contain) a word which refers to the respective category of the energy chain
    #if the category is a combined category it has to be split to be suitable for the which==XX inquiry (if this operation is done outside below line, an additional variable like category.unl has to be introduced, othwerwise the category name is wrong for finding it again in the resultmatrix)
    category.subset <- evaluationmatrix[which(evaluationmatrix[,"category"] %in% unlist(strsplit(category, "~~"))),]
    
    category.subset.counts <- category.subset[,c(2:ncol(category.subset))]
    
    
    count <- colSums(category.subset.counts)
    #only "boolean-counting"/"occurrence at all" applied, word frequency within a measure is not used for evaluation
    #therefore all numbers larger than 0 are converted to a 1
    count <- ifelse(count>0,1,0)
    result <- round(sum(count)/meas.totalnum, digits=4)
    
    result.energychain[category,t] <- result
  }
}


#add MEAN over all columns as last column 
result.energychain  <- cbind(result.energychain, round((rowSums(result.energychain)/ncol(result.energychain)), digits=4))
colnames(result.energychain)[ncol(result.energychain)] <- "mean"
###END - do counting and create results matrix-------------------------
#dimnames(result.energychain)




###START ORDER the rows according to the energy chain order--------------------------
#result.energychain <- result.energychain[rows.ordered ,]
###END ORDER the rows according to the energy chain order--------------------------


##START count (percentaged) occurrence of categories within each text: (occurrence of category within X measures of a text)/(number of all measures in a text)-------------------------


#END evaluation energy chain----------------------------------


wd <- getwd()
setwd(wd.final)
names.replace <- read.csv("categories_replace_names.csv", sep=";")
setwd(wd)

rownames(result.energychain) <- gsub("c[[:digit:]]+__", "",rownames(result.energychain) )

names.replace["new2"] <- sapply(names.replace["new2"], function(item) {
  
  item <- gsub("(^)", "<\\1", item, perl=T)
  item <- gsub("($)", "\\1>", item, perl=T)
  item <- gsub("#", "><", item, fixed=T)
  
  
})

#interim <- result.energychain
#result.energychain <- interim


for (i in 1:nrow(names.replace)) {
  
  rownames(result.energychain) <- gsub(paste("^",as.character(names.replace[i,"old"]),"$", sep=""),
                                       as.character(names.replace[i,"new2"]),
                                       rownames(result.energychain))
}

#new Economy, Infrastructure, deleted: <public_illumination>
interim <- result.energychain
result.energychain <- interim

#START select energy categories--------------------------
categories.select1 <- grep("<1>", rownames(result.energychain), ignore.case = T)
categories.select3 <- grep("<3>|biofuels|renewable|pedestrian|insulation|car_sharing|public_transport|storage", rownames(result.energychain), ignore.case = T)

result.energychain1 <- result.energychain[categories.select1,]
result.energychain3 <- result.energychain[categories.select3,]


categories.exclude <- grep("<fed_state><ger>", rownames(result.energychain1), ignore.case = T)
result.energychain1 <- result.energychain1[-categories.exclude,]



#-----------shift some 1SOC categories to 2ENG and simplify category names
row.names(result.energychain1) <- gsub("<1><SOC>","",row.names(result.energychain1))


#END select energy categories----------------------

result.energychain1 <- result.energychain1[order(rownames(result.energychain1)),]

# 
# 
# ##START write category names into file-------------------------------------------------
# categories.ENG <- rownames(result.energychain1)
# 
# categories.ENG <- gsub("(^<[\\d]>)(<[\\w]{3}>)(.*$)", "\\3", categories.ENG, perl=T)
# categories.ENG <- gsub("(^<[\\w]+>)(<.*$)", "\\1~#~\\2", categories.ENG, perl=T)
# 
# delimiter <- "~#~"
# categories.ENG <- do.call(rbind,strsplit(categories.ENG, delimiter, perl=T))
# 
# wd <- getwd()
# setwd(wd.final)
# 
# 
# write.csv(categories.ENG, paste("categories_ENG_nMeta_",
#                                 length(unique(categories.ENG[,1])),
#                                 "_nCat_",
#                                 length(categories.ENG[,2]),
#                                 ".csv"))
# ##END  write category names into file-------------------------------------------------


result.energychain1 <- t(result.energychain1)

result.energychain1 <- as.data.frame(result.energychain1)

result.energychain1 <- cbind(rownames(result.energychain1), result.energychain1)
colnames(result.energychain1)[1] <- "case"
rownames(result.energychain1) <- NULL

results.melt <- melt(result.energychain1, id.vars= c("case"))

plot.results <- results.melt

colnames(plot.results)[2] <- "category"

plot.results <- subset(plot.results, plot.results$case != "mean")

row.names(plot.results) <- 1:nrow(plot.results)

plot.results$category <- factor(plot.results$category, levels = unique(plot.results$category))


#interim2 <- plot.results
#plot.results <- interim2
plot.results <- plot.results[nrow(plot.results):1,]

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<???
vertical.line <- unlist(lapply(main.categories, function(item) {
  
  max(grep(item, unique(plot.results$category), perl=T, ignore.case=T))
  
})) 
vertical.line <- vertical.line[-length(vertical.line)]
vertical.line  <- vertical.line+0.5


#----------------------plot-----------------------
x.axis.label.size <- c(10)

y.axis.label.size <- c(10)
y.axis.title.size <- c(10)
annotate.label.size <-c(3)

#plot.results <- test
#plot.results <- plot.results[nrow(plot.results):1,]

p <- ggplot(plot.results, aes(factor(category), value), stat = "identity") +
  
  geom_boxplot(outlier.colour = "black") + #width: distance between bars
  
  # ylab("average normalized occurrence of category") +
  ylab("average normalized occurrence")+
  xlab("") +
  
  
  #theme black and white
  theme_bw() +
  
  #colour set for columns
  scale_fill_grey(start = 1, end = 0.4, na.value = "red") +
  
  
  #eliminates background, gridlines, and chart border
  theme(plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_rect(colour = "black")
        ,axis.line = element_line(colour = "black")
        ,plot.title = element_blank()
        ,plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")
        #,legend.position = "bottom"
        # ,legend.key.size = unit(2.5, "cm")
  ) +
  
  
  # Rotate x-axis labels
  theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.15, size=7)
        ,axis.title.x = element_text(size=7)
        
        #set y-axis tick sizes
        ,axis.text.y = element_text(size=7, angle=0, vjust=0.25, hjust=0)
        #,axis.title.y = element_text(size=5)
        
  ) +
  
  
  
  #set the limits of the y axis
  scale_y_continuous(expand = c(0,0), limits = c(0,1)) + 
  
  #set distance of y axis label to the axis
  theme(axis.title.y = element_text(vjust=1.1, size=10)) +
  
  
  #coord_fixed(ratio=15) +#15
  theme(aspect.ratio=4.5) +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(plot.results$category))) + #this generally works but the color bars that were introduced are not reversed

#use a legend theme which includes a black rectangle around the legend
theme(legend.background = element_rect(colour = "black")) +
  
  #remove crossing lines through the colouer fields in the legend
  guides(fill = guide_legend(override.aes = list(colour = NULL))) +
  
  #don´t show legend
  theme(legend.position = "none")


#----------------------plot-----------------------
p

#plot




##<<<<<<<<---Export - Save as Metafile
wd.sub <- "M:\\Science\\Promotion\\Working_Documents\\articles_texts\\1_article_interpretation_network_analysis_for_sustainability_analysis\\submission_1"
setwd(wd.sub)

par(mar=c(0.5,0.5,0.5,0.5))
scale_figure <- c(9,5.5)


win.metafile("ESM_S9_societal_subsystems_occurrence_boxplot.emf", height=scale_figure[1] ,width=scale_figure[2])
p
dev.off()


postscript("ESM_S9_societal_subsystems_occurrence_boxplot.eps", height=scale_figure[1] ,width=scale_figure[2])
p
dev.off()


#START Plot energychain - boxplot-------------------------------------





##########################thesaurus 3-------------------------#########################################







#-----------shift some 1SOC categories to 2ENG and simplify category names
#END select energy categories----------------------

result.energychain3 <- result.energychain3[order(rownames(result.energychain3)),]



rownames(result.energychain3) <- gsub("<2><ENG><Resources><renewable>" , 
                    "<3><SUS><Consistency><Resources><renewable>",
                    rownames(result.energychain3))

rownames(result.energychain3) <- gsub("<2><ENG><Conversion><renewable>", 
                    "<3><SUS><Consistency><Conversion><renewable>",
                    rownames(result.energychain3))

rownames(result.energychain3) <- gsub("<2><ENG><Sales_Contracts><renewable>" , 
                    "<3><SUS><Consistency><Sales_Contracts><renewable>",
                    rownames(result.energychain3))

rownames(result.energychain3) <- gsub("<2><ENG><End_Use><mobility><biofuels>", 
                    "<3><SUS><Consistency><End_Use><mobility><biofuels>",
                    rownames(result.energychain3))

rownames(result.energychain3) <- gsub("<2><ENG><Technology_Option><storage>", 
                    "<3><SUS><Efficiency><Technology_Option><storage>",
                    rownames(result.energychain3))

rownames(result.energychain3) <- gsub("<2><ENG><End_Use><building><insulation>" , 
                    "<3><SUS><Efficiency><End_Use><building><insulation>",
                    rownames(result.energychain3))

rownames(result.energychain3) <- gsub("<2><ENG><End_Use><mobility><car_sharing>" , 
                    "<3><SUS><Sufficiency><End_Use><mobility><car_sharing>",
                    rownames(result.energychain3))


rownames(result.energychain3) <- gsub("<2><ENG><End_Use><mobility><bike_pedestrian>", 
                    "<3><SUS><Sufficiency><End_Use><mobility><bike_pedestrian>",
                    rownames(result.energychain3))

rownames(result.energychain3) <- gsub("<2><ENG><End_Use><mobility><public_transport>", 
                    "<3><SUS><Sufficiency><End_Use><mobility><public_transport>",
                    rownames(result.energychain3))



categories.exclude <- grep("<Sustainability><unspecific_reference>|Uncertainty><Uncertainty_Risks_Accidents>", rownames(result.energychain3), ignore.case = T)
result.energychain3 <- result.energychain3[-categories.exclude,]



sust.categories <- c("<Sufficiency", "<Efficiency", "<Consistency")

sust.categories.order <- unlist(lapply(sust.categories, function(item) {
  
  grep(item, rownames(result.energychain3), perl=T, ignore.case = T)
  
  
}))

result.energychain3 <- result.energychain3[sust.categories.order,]

row.names(result.energychain3) <- gsub("<3><SUS>","",row.names(result.energychain3))

# 
# 
# ##START write category names into file-------------------------------------------------
# categories.ENG <- rownames(result.energychain3)
# 
# categories.ENG <- gsub("(^<[\\d]>)(<[\\w]{3}>)(.*$)", "\\3", categories.ENG, perl=T)
# categories.ENG <- gsub("(^<[\\w]+>)(<.*$)", "\\1~#~\\2", categories.ENG, perl=T)
# 
# delimiter <- "~#~"
# categories.ENG <- do.call(rbind,strsplit(categories.ENG, delimiter, perl=T))
# 
# wd <- getwd()
# setwd(wd.final)
# 
# 
# write.csv(categories.ENG, paste("categories_ENG_nMeta_",
#                                 length(unique(categories.ENG[,1])),
#                                 "_nCat_",
#                                 length(categories.ENG[,2]),
#                                 ".csv"))
# ##END  write category names into file-------------------------------------------------



result.energychain3 <- t(result.energychain3)

result.energychain3 <- as.data.frame(result.energychain3)

result.energychain3 <- cbind(rownames(result.energychain3), result.energychain3)
colnames(result.energychain3)[1] <- "case"
rownames(result.energychain3) <- NULL

results.melt <- melt(result.energychain3, id.vars= c("case"))

plot.results <- results.melt

colnames(plot.results)[2] <- "category"

plot.results <- subset(plot.results, plot.results$case != "mean")

row.names(plot.results) <- 1:nrow(plot.results)

plot.results$category <- factor(plot.results$category, levels = unique(plot.results$category))


#interim2 <- plot.results
#plot.results <- interim2
plot.results <- plot.results[nrow(plot.results):1,]

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<???
vertical.line <- unlist(lapply(main.categories, function(item) {
  
  max(grep(item, unique(plot.results$category), perl=T, ignore.case=T))
  
})) 
vertical.line <- vertical.line[-length(vertical.line)]
vertical.line  <- vertical.line+0.5


#----------------------plot-----------------------
x.axis.label.size <- c(10)

y.axis.label.size <- c(10)
y.axis.title.size <- c(10)
annotate.label.size <-c(3)

#plot.results <- test
#plot.results <- plot.results[nrow(plot.results):1,]

p <- ggplot(plot.results, aes(factor(category), value), stat = "identity") +
  
  geom_boxplot(outlier.colour = "black") + #width: distance between bars
  
  # ylab("average normalized occurrence of category") +
  ylab("average normalized occurrence")+
  xlab("") +
  
  
  #theme black and white
  theme_bw() +
  
  #colour set for columns
  scale_fill_grey(start = 1, end = 0.4, na.value = "red") +
  
  
  #eliminates background, gridlines, and chart border
  theme(plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_rect(colour = "black")
        ,axis.line = element_line(colour = "black")
        ,plot.title = element_blank()
        ,plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")
        #,legend.position = "bottom"
        # ,legend.key.size = unit(2.5, "cm")
  ) +
  
  
  # Rotate x-axis labels
  theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.15, size=14)
        ,axis.title.x = element_text(size=14)
        
        #set y-axis tick sizes
        ,axis.text.y = element_text(size=14, angle=0, vjust=0.25, hjust=0)
        #,axis.title.y = element_text(size=5)
        
  ) +
  
  
  
  #set the limits of the y axis
  scale_y_continuous(expand = c(0,0), limits = c(0,1)) + 
  
  #set distance of y axis label to the axis
  theme(axis.title.y = element_text(vjust=1.1, size=10)) +
  
  
  #coord_fixed(ratio=15) +#15
  theme(aspect.ratio=4.5) +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(plot.results$category))) + #this generally works but the color bars that were introduced are not reversed
  
  #use a legend theme which includes a black rectangle around the legend
  theme(legend.background = element_rect(colour = "black")) +
  
  #remove crossing lines through the colouer fields in the legend
  guides(fill = guide_legend(override.aes = list(colour = NULL))) +
  
  #don´t show legend
  theme(legend.position = "none")


#----------------------plot-----------------------
p

#plot




##<<<<<<<<---Export - Save as Metafile
wd.sub <- "M:\\Science\\Promotion\\Working_Documents\\articles_texts\\1_article_interpretation_network_analysis_for_sustainability_analysis\\submission_1"
setwd(wd.sub)

par(mar=c(0.5,0.5,0.5,0.5))
scale_figure <- c(10,8)


win.metafile("ESM_S10_sustainability_aspects_occurrence_boxplot.emf", height=scale_figure[1] ,width=scale_figure[2])
p
dev.off()


postscript("ESM_S10_sustainability_aspects_occurrence_boxplot.eps", height=scale_figure[1] ,width=scale_figure[2])
p
dev.off()


#START Plot energychain - boxplot-------------------------------------




