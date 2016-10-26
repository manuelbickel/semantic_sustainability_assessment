


library("ggplot2")
library("reshape2")
warning("Please make sure that categories have been named and combined by hand within the script.")
library("plyr")



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


delete.word.rows <- c("Beschreib", "Eur", "Lag", "lag", 
                      "Investitionskost", "Kost", "Eur", "Kombination", 
                      "Kosteneinschaetz", "Zug",
                      "Finanzier", "Einschaetz", "Rahm", "Lok", "Kontakt", 
                      "Untersuch", "Land", "Geraet", "Darstell", "Statist", "Hilf",
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



#START Plot energychain - boxplot-------------------------------------

#rownames(result.energychain) <- as.character(result.energychain[,1])
#result.energychain <- result.energychain[,-1]

wd <- getwd()
setwd(wd.final)
names.replace <- read.csv("categories_replace_names.csv", sep=";")
setwd(wd)

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

#result.energychain <- interim
for (i in 1:nrow(names.replace)) {
  
  rownames(result.energychain) <- gsub(paste("^",as.character(names.replace[i,"old"]),"$", sep=""),
                                       as.character(names.replace[i,"new2"]),
                                       rownames(result.energychain))
}




result.energychain <- t(result.energychain)

exclude.cols <- c("<fed_state><ger>")
exclude.cols <- unlist(lapply(exclude.cols, function(item) as.numeric(grep(item, colnames(result.energychain)))))

if (length(exclude.cols) > 0) {
  
  result.energychain <- result.energychain[,-exclude.cols]
  
}



result.energychain <- as.data.frame(result.energychain)


result.energychain <- cbind(rownames(result.energychain), result.energychain)
colnames(result.energychain)[1] <- "case"
rownames(result.energychain) <- NULL


results.melt <- melt(result.energychain, id.vars= c("case"))

plot.results <- results.melt

#plot.results[,2] <- gsub("^.*__", "", plot.results[,2])

#exclude mean value and energy general
#plot.results <- subset(plot.results, plot.results$case == "mean")
plot.results <- subset(plot.results, plot.results$case != "mean")


#store information which bars refer to "TOTAL" categories in order to colour them differently

#Reassign the levels in order to preserve the order as displayed in the data
row.names(plot.results) <- 1:nrow(plot.results)

plot.results$variable <- factor(plot.results$variable, levels = unique(plot.results$variable))

min25 <- plot.results[which(plot.results$value >= 0.25),]

min25 <- min25[order(min25["value"], decreasing = TRUE),]

wd <- getwd()
setwd(wd.final)
write.csv(min25, "categories_with_minimum_normalized_occurrence_of_25.csv")
setwd(wd)

temp <- gregexpr("[0-9]",plot.results$variable)
split <- as.numeric(unlist(regmatches(plot.results$variable, temp)))
#split <- gsub("1", "2", split)
#plot.results3 <- plot.results3[order(plot.results$variable),]

plot.results <- cbind(plot.results, split)

#set which thesauri shall be displayed################<-set value
par_org <- par()

#######################################plot all three thesauri in facets---------------
##START  ############################plot thesaurus 1##########---------------------

selected_thesauri <- 1
plot.results1 <- subset(plot.results, plot.results$split == selected_thesauri) 
plot.results1$variable <- factor(plot.results1$variable, levels = unique(plot.results1$variable))

selected_thesauri <- 2
plot.results2 <- subset(plot.results, plot.results$split == selected_thesauri) 
plot.results2$variable <- factor(plot.results2$variable, levels = unique(plot.results2$variable))

selected_thesauri <- 3
plot.results3 <- subset(plot.results, plot.results$split == selected_thesauri) 
plot.results3$variable <- factor(plot.results3$variable, levels = unique(plot.results3$variable))

common_aspect_ratio <- 10
#plot.results3$variable <- gsub("<3><SOC>|<2><ENG>","",plot.results3$variable)


##START  ############################plot thesaurus 1##########---------------------



###############################boxplot-----<1>-------------

x.axis.label.size <- c(10)

y.axis.label.size <- c(11)
y.axis.title.size <- c(11)
annotate.label.size <-c(3)


p <- ggplot(plot.results1, aes(factor(category), value), stat = "identity") +
  
  geom_boxplot(outlier.colour = "black") + #width: distance between bars
  
  # ylab("average normalized occurrence of variable") +
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
  theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.15, size=10)
        ,axis.title.x = element_text(size=10)
        
        #set y-axis tick sizes
        ,axis.text.y = element_text(size=10, angle=0, vjust=0.25, hjust=0)
        #,axis.title.y = element_text(size=5)
        
  ) +
  
  
  
  #set the limits of the y axis
  scale_y_continuous(expand = c(0,0), limits = c(0,1)) + 
  
  #set distance of y axis label to the axis
  theme(axis.title.y = element_text(vjust=1.1, size=11)) +
  
  
  #coord_fixed(ratio=15) +#15
  theme(aspect.ratio=3) +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(plot.results1$variable))) + #this generally works but the color bars that were introduced are not reversed
  #force start of the bars directly at the axis (without the distance) - NOT WORKING WITH YLIM
  #scale_y_continuous(expand = c(0,0)) +
  #scale_x_discrete(expand = c(0,0)) +
  
  # geom_vline(aes(xintercept = c(rep(vertical.line[1],  nrow(plot.results1)))), linetype= "dashed") +
#   geom_vline(aes(xintercept = c(rep(vertical.line[2], nrow(plot.results1)))), linetype= "dashed") +
#   geom_vline(aes(xintercept = c(rep(vertical.line[3], nrow(plot.results1)))), linetype= "dashed") +
#   geom_vline(aes(xintercept = c(rep(vertical.line[4], nrow(plot.results1)))), linetype= "dashed") +
#   geom_vline(aes(xintercept = c(rep(vertical.line[5], nrow(plot.results1)))), linetype= "dashed") +
#   geom_vline(aes(xintercept = c(rep(vertical.line[6], nrow(plot.results1)))), linetype= "dashed") +
#   geom_vline(aes(xintercept = c(rep(vertical.line[7], nrow(plot.results1)))), linetype= "dashed") +
#   geom_vline(aes(xintercept = c(rep(vertical.line[8], nrow(plot.results1)))), linetype= "dashed") +
#   geom_vline(aes(xintercept = c(rep(vertical.line[9], nrow(plot.results1)))), linetype= "dashed") +
#   geom_vline(aes(xintercept = c(rep(vertical.line[10], nrow(plot.results1)))), linetype= "dashed") +
#   geom_vline(aes(xintercept = c(rep(vertical.line[11], nrow(plot.results1)))), linetype= "dashed") +
#   geom_vline(aes(xintercept = c(rep(vertical.line[12], nrow(plot.results1)))), linetype= "dashed") +
#   geom_vline(aes(xintercept = c(rep(vertical.line[13], nrow(plot.results1)))), linetype= "dashed") +
#   geom_vline(aes(xintercept = c(rep(vertical.line[14], nrow(plot.results1)))), linetype= "dashed") +
#   geom_vline(aes(xintercept = c(rep(vertical.line[15], nrow(plot.results1)))), linetype= "dashed") +
#   geom_vline(aes(xintercept = c(rep(vertical.line[16], nrow(plot.results1)))), linetype= "dashed") +
#   # geom_vline(aes(xintercept = c(rep(vertical.line[17], nrow(plot.results1)))), linetype= "dashed") +
  #  geom_vline(aes(xintercept = c(rep(vertical.line[18], nrow(plot.results1)))), linetype= "dashed") +
  

#use a legend theme which includes a black rectangle around the legend
theme(legend.background = element_rect(colour = "black")) +
  
  #remove crossing lines through the colouer fields in the legend
  guides(fill = guide_legend(override.aes = list(colour = NULL))) +
  
  #don´t show legend
  theme(legend.position = "none")


#----------------------plot-----------------------
p1















############################boxplot----<1>-------------------

x.axis.label.size <- c(7)
y.axis.label.size <- c(5)
y.axis.title.size <- c(4)
annotate.label.size <-c(7)

#----------------------plot-----------------------

warning("adapt plot results number in coord fixed")
p1 <- ggplot(plot.results1, aes(factor(variable), value), stat = "identity") +

#c + geom_boxplot(outlier.colour = "black") + #width: distance between bars
 
  geom_boxplot(stat="identity", width= .8) + #width: distance between bars
  
  ylab("") +
 # ylab("average normalized occurrence of categories [%]") +
  xlab("") +
  
  
  #theme black and white
  theme_bw() +
  
  #colour set for columns
  scale_fill_grey(start = 1, end = 0.4, na.value = "red") +
  
  
  #eliminates background, gridlines, and chart border
  theme(plot.background =  element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_rect(colour = "black")
        ,axis.line = element_line(colour = "black")
        ,legend.position = "none"
        ,plot.margin = unit(c(0.25,0.25,0.25,0.25), "cm")
        ,plot.title = element_blank()
        #,aspect.ratio = common_aspect_ratio
    
        # ,legend.key.size = unit(2.5, "cm")
  ) +
  
  # Rotate x-axis labels
  theme(axis.text.x = element_text(angle = 90,   hjust = 1, vjust = 0.25, size=x.axis.label.size )) +
  
  coord_fixed(common_aspect_ratio) +
    
  #set y-axis tick sizes
  theme(axis.text.y = element_text(angle= 90,hjust = 0.5, size=y.axis.label.size[1])) +
  
  #set the limits of the y axis
  scale_y_continuous(expand = c(0,0), limits = c(0,1)) +
  
  #coord_fixed(ratio = 10*149/length(unique(plot.results1$variable))) +
  #coord_fixed(ratio = 15) +
  #set distance of y axis label to the axis
  theme(axis.title.y = element_text(size=y.axis.title.size)) +
  theme(axis.title.y = element_blank()) +  
  
  geom_hline(yintercept = 0.25, colour = "black", linetype= "dashed") +
  geom_hline(yintercept = 0.5, colour = "black", linetype= "dashed") +
  geom_hline(yintercept = 0.75, colour = "black", linetype= "dashed")
 p1
#----------------------plot-----------------------

par(mar=c(0.25,0.25,0.25,0.25))
##<<<<<<<<---Export - Save as Metafile
wd.sub <- "M:\\Science\\Promotion\\Working_Documents\\articles_texts\\1_article_interpretation_network_analysis_for_sustainability_analysis\\submission_1"
setwd(wd.sub)
#ggsave(file="energy_chain_occurrence_plot.pdf")
#ggsave(file="ESM_Figure_XY_thesaurus_1_SOC_mean_occurrence_barplot.eps",  dpi=800, units="cm")
#ggsave(file="ESM_Figure_XY_thesaurus_1_SOC_mean_occurrence_barplot.emf",  dpi=800, units="cm")


win.metafile("ESM_Figure_XY_thesaurus_1_SOC_mean_occurrence_barplot.emf")
p1
dev.off()


postscript("ESM_Figure_XY_thesaurus_1_SOC_mean_occurrence_barplot.eps")
p1
dev.off()

pdf("ESM_Figure_XY_thesaurus_1_SOC_mean_occurrence_barplot.pdf")
p1
dev.off()

#START Plot energychain - boxplot-------------------------------------

##END  ############################plot thesaurus 1##########----------------------------


##START  ############################plot thesaurus 2##########---------------------


x.axis.label.size <- c(7)
y.axis.label.size <- c(5)
y.axis.title.size <- c(8)
annotate.label.size <-c(7)

#----------------------plot-----------------------

warning("adapt plot results number in coord fixed")
p2 <- ggplot(plot.results2, aes(factor(variable), value), stat = "identity") +

  
  #c + geom_boxplot(outlier.colour = "black") + #width: distance between bars
  
  geom_bar(stat="identity", width= .8*(nrow(plot.results2)/nrow(plot.results1))) + #width: distance between bars
  
  ylab("") +
  # ylab("average normalized occurrence of categories [%]") +
  xlab("") +
  
  #theme black and white
  theme_bw() +
  
  #colour set for columns
  scale_fill_grey(start = 1, end = 0.4, na.value = "red") +
  
  
  #eliminates background, gridlines, and chart border
  theme(plot.background =  element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_rect(colour = "black")
        ,axis.line = element_line(colour = "black")
        ,legend.position = "none"
        ,plot.margin = unit(c(0.25,0.25,0.25,0.25), "cm")
        ,plot.title = element_blank()
       # ,aspect.ratio =  common_aspect_ratio/(nrow(plot.results2)/nrow(plot.results1))
        # ,legend.key.size = unit(2.5, "cm")
  ) +
  
  # Rotate x-axis labels
  theme(axis.text.x = element_text(angle = 90,   hjust = 1, vjust = 0.25, size=x.axis.label.size )) +
  
  coord_fixed(common_aspect_ratio*(nrow(plot.results2)/nrow(plot.results1))) +
  
  #set y-axis tick sizes
  theme(axis.text.y = element_text(angle= 90,hjust = 0.5, size=y.axis.label.size[1])) +
  
  #set the limits of the y axis
  scale_y_continuous(expand = c(0,0), limits = c(0,1)) +
  
  #coord_fixed(ratio = 10*149/length(unique(plot.results2$variable))) +
  

  #set distance of y axis label to the axis
  theme(axis.title.y = element_text(size=y.axis.title.size)) +
  theme(axis.title.y = element_blank()) +  
  
  geom_hline(yintercept = 0.25, colour = "black", linetype= "dashed") +
  geom_hline(yintercept = 0.5, colour = "black", linetype= "dashed") +
  geom_hline(yintercept = 0.75, colour = "black", linetype= "dashed")

p2

#----------------------plot-----------------------


##<<<<<<<<---Export - Save as Metafile
#ggsave(file="energy_chain_occurrence_plot.pdf")
setwd(wd.sub)
#ggsave(file="ESM_Figure_XY_thesaurus_2_ENG_mean_occurrence_barplot.eps",  dpi=800, units="cm")
#ggsave(file="ESM_Figure_XY_thesaurus_2_ENG_mean_occurrence_barplot.emf",  dpi=800, units="cm")

win.metafile("ESM_Figure_XY_thesaurus_2_ENG_mean_occurrence_barplot.emf")
p2
dev.off()


postscript("ESM_Figure_XY_thesaurus_2_ENG_mean_occurrence_barplot.eps")
p2
dev.off()

pdf("ESM_Figure_XY_thesaurus_2_ENG_mean_occurrence_barplot.pdf")
p2
dev.off()

#START Plot energychain - boxplot-------------------------------------

##END  ############################plot thesaurus 2##########----------------------------




##START  ############################plot thesaurus 3##########---------------------

x.axis.label.size <- c(7)
y.axis.label.size <- c(5)
y.axis.title.size <- c(8)
annotate.label.size <-c(7)


#----------------------plot-----------------------
warning("adapt plot results number in coord fixed")
p3 <- ggplot(plot.results3, aes(factor(variable), value), stat = "identity")+


#c + geom_boxplot(outlier.colour = "black") + #width: distance between bars

geom_bar(stat="identity", width= .8*(nrow(plot.results3)/nrow(plot.results1))) + #width: distance between bars
  
  ylab("") +
  # ylab("average normalized occurrence of categories [%]") +
  xlab("") +
  
  
  #theme black and white
  theme_bw() +
  
  #colour set for columns
  scale_fill_grey(start = 1, end = 0.4, na.value = "red") +
  
  
  #eliminates background, gridlines, and chart border
  theme(plot.background =  element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_rect(colour = "black")
        ,axis.line = element_line(colour = "black")
        ,legend.position = "none"
        ,plot.margin = unit(c(0.25,0.25,0.25,0.25), "cm")
        ,plot.title = element_blank()
        # ,legend.key.size = unit(2.5, "cm")
  ) +
  
  # Rotate x-axis labels
  theme(axis.text.x = element_text(angle = 90,   hjust = 1, vjust = 0.25, size=x.axis.label.size )) +
  

  #set y-axis tick sizes
  theme(axis.text.y = element_text(angle= 90,hjust = 0.5, size=y.axis.label.size[1])) +
  
  #set the limits of the y axis
  scale_y_continuous(expand = c(0,0), limits = c(0,1)) +
  
  coord_fixed(common_aspect_ratio*(nrow(plot.results3)/nrow(plot.results1))) +

  
  #set distance of y axis label to the axis
  theme(axis.title.y = element_text(size=y.axis.title.size)) +
  theme(axis.title.y = element_blank()) +  
  
  geom_hline(yintercept = 0.25, colour = "black", linetype= "dashed") +
  geom_hline(yintercept = 0.5, colour = "black", linetype= "dashed") +
  geom_hline(yintercept = 0.75, colour = "black", linetype= "dashed")

p3

#----------------------plot-----------------------


##<<<<<<<<---Export - Save as Metafile
setwd(wd.sub)
#ggsave(file="ESM_Figure_XY_thesaurus_2_ENG_mean_occurrence_barplot.eps",  dpi=800, units="cm")
#ggsave(file="ESM_Figure_XY_thesaurus_2_ENG_mean_occurrence_barplot.emf",  dpi=800, units="cm")

win.metafile("ESM_Figure_XY_thesaurus_3_SUS_mean_occurrence_barplot.emf")
p3
dev.off()


postscript("ESM_Figure_XY_thesaurus_3_SUS_mean_occurrence_barplot.eps")
p3
dev.off()

pdf("ESM_Figure_XY_thesaurus_3_SUS_mean_occurrence_barplot.pdf")
p3
dev.off()



##END  ############################plot thesaurus 3##########----------------------------



#START-----------------------facet wrap barplot-----------------------

c + geom_bar(stat = "identity", position = "dodge", colour = "black", width = 0.5, show_guide=TRUE) + #width: distance between bars
  
  #theme black and white
  theme_bw() +
  
  #colour set for columns
  scale_fill_grey(start = 0.75, end = 0, na.value = "red") +
  
  #scale_fill_hue(l=50, c=75) + #additional possibility of color adaption: h=c(10, 300)
  
  #eliminates background, gridlines, and chart border
  theme(plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_rect(colour = "black")
        ,axis.line = element_line(colour = "black")
        ,legend.position = "bottom"
        # ,legend.key.size = unit(2.5, "cm")
  ) +
  
  # Rotate x-axis labels
  theme(axis.text.x = element_blank(), axis.ticks=element_blank(), axis.title.x = element_blank()) +
  
  
  #set the limits of the y axis
  scale_y_continuous(expand = c(0,0), limits = c(0,0.8)) +
  
  #force start of the bars directly at the axis (witout the distance) - NOT WORKING WITH YLIM
  #scale_y_continuous(expand = c(0,0)) +
  #scale_x_discrete(expand = c(0,0)) +
  
  
  #use a legend theme which includes a black rectangle around the legend
  theme(legend.background = element_rect(colour = "black")) +
  
  #remove crossing lines through the colouer fields in the legend
  guides(fill = guide_legend(override.aes = list(colour = NULL))) + 
  
  
  #NOT READY ---> probably not facet wrapt but a loop might be needed to create 
  #several graphs and arrange them, then each graph can be controlled
  #separately and be treated as "normal single graph"
  #insert vertical lines between the main categories
  
  geom_vline(aes(xintercept = c(rep(5.5,  nrow(plot.results)))), linetype= "dashed") +
  geom_vline(aes(xintercept = c(rep(12.5, nrow(plot.results)))), linetype= "dashed") +
  geom_vline(aes(xintercept = c(rep(17.5, nrow(plot.results)))), linetype= "dashed") +
  geom_vline(aes(xintercept = c(rep(23.5, nrow(plot.results)))), linetype= "dashed") +
  geom_vline(aes(xintercept = c(rep(32.5, nrow(plot.results)))), linetype= "dashed") +
  geom_vline(aes(xintercept = c(rep(35.5, nrow(plot.results)))), linetype= "dashed") +
  geom_vline(aes(xintercept = c(rep(39.5, nrow(plot.results)))), linetype= "dashed") +
  
  
  #which(plot.results$ == 'm')
  #grep("^total_", plot.results$category)
  
  #geom_vline(data=plot.results, aes(xintercept = grep("^total_", plot.results$category))) +
  # geom_vline(aes(xintercept = c(which(plot.results$category == "total_resources")[1]#,
  #  which(plot.results$category == "total_conversion")[1],
  # which(plot.results$category == "total_distribution")[1],
  #  which(plot.results$category == "total_sales")[1],
  #  which(plot.results$category == "total_mobility")[1],
#  which(plot.results$category == "total_builtenvironment")[1]
#   ))) +



#Reverse the order within the legend
#guides(fill = guide_legend(reverse = TRUE)) + 

#should add lines around the color boxes in legend - does not work as desired 
#theme(legend.key = element_rect(colour = "black")) +

#create separate plots by a certain category - NOT WORKING WITH FACET_WRAP 
#facet_grid( ~maincategory, scales="free_x") +

#make a plot for each case (e.g. city)
facet_wrap(~variable,ncol=5)

#END----------------------facet wrap barplot-----------------------



#######################test
library(ggplot2)   # v2.1.0
library(gtable)    # v0.2.0
library(grid)
library(gridExtra) # v2.2.1

# Get the gtables
gC <- ggplotGrob(c)

gC$layout[grep("background", gC$layout$name),]$b <- 1


grid.newpage()
grid.arrange(gC) 
grid.draw(gC)
c
str(c)




gl <- lapply(list(p1,p2), ggplotGrob)
library(grid)
widths <- do.call(unit.pmax, lapply(gl, "[[", "widths"))
heights <- do.call(unit.pmax, lapply(gl, "[[", "heights"))
lg <- lapply(gl, function(g) {g$widths <- widths; g$heights <- heights; g})
grid.newpage()
grid.draw(lg[[1]])
grid.newpage()
grid.draw(lg[[2]])

