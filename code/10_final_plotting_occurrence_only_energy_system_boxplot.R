

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
categories.select <- grep("<2>|<economy>|<industry>|<commerce>|<mobility_sector>|<local_administration_bodies>|<infrastructure>|<Food>|<Residents>", rownames(result.energychain), ignore.case = T)

result.energychain <- result.energychain[categories.select,]
#rownames(result.energychain)
#<Sales_Contracts><unspecific_reference>|
#<consumption><unspecific>|
categories.exclude <- grep("<carrier>|<rental_of_houses>|<social_groups>|<general_reference><saving>|<building_parts_materials>|<Energy><unspecific_reference>|<Energy_Form><unspecific_reference>|<Economy><commerce><sector_unspecific>|<Infrastructure><disposal_unspecific>|<detailed_PPtechnology>|<alternative_wo_bike>|<tariffs_standing_orders>|<Economy><general_reference><employment_workplace>|<Infrastructure><unspecific>|<Infrastructure><supply_unspecific>|<Economy><general_reference><economic_viability>|<Economy><service><personal_services_crafting>", rownames(result.energychain), ignore.case = T)
result.energychain <- result.energychain[-categories.exclude,]

#main.cateogories <- unique(gsub("(^<.*?>)(<.*?>)(<.*?>)(<.*$)", "\\3", rownames(result.energychain),perl=T))

#-----------shift some 1SOC categories to 2ENG and simplify category names
row.names(result.energychain) <- gsub("<1><SOC>","",row.names(result.energychain))
row.names(result.energychain) <- gsub("<2><ENG>","",row.names(result.energychain))

main.categories <- c("<resources>", 
                     "<conversion>", 
                     "<distribution>", 
                     "<sales_contracts>",
                     "<Technology_Option>",
                     "<Energy_Form>", #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                     "<end_use><consumption>",
                     "<mobility>",
                     "<building>",
                     "<electric_application>",
                     "<local_administration_bodies>",
                     "<Mobility_Sector>",
                     "<Infrastructure>",
                 
                      "<Residents>",
                     "<Food>",
                     "<Economy><unspec",
                     "<Economy><service>",
                     "<commerce>",
                     "<industry>"
                     )

#END select energy categories----------------------


main.categories.order <- unlist(lapply(main.categories, function(item) {
  
  grep(item, rownames(result.energychain), ignore.case=T, perl=T)
  
  
}))


result.energychain <- result.energychain[main.categories.order,]


##START write category names into file-------------------------------------------------
categories.ENG <- rownames(result.energychain)

categories.ENG <- gsub("(^<[\\d]>)(<[\\w]{3}>)(.*$)", "\\3", categories.ENG, perl=T)
categories.ENG <- gsub("(^<[\\w]+>)(<.*$)", "\\1~#~\\2", categories.ENG, perl=T)

delimiter <- "~#~"
categories.ENG <- do.call(rbind,strsplit(categories.ENG, delimiter, perl=T))

wd <- getwd()
setwd(wd.final)


write.csv(categories.ENG, paste("categories_ENG_nMeta_",
                                length(unique(categories.ENG[,1])),
                                "_nCat_",
                                length(categories.ENG[,2]),
                                ".csv"))
##END  write category names into file-------------------------------------------------



##START - check median values of categories and which were lower than 1 percent---------------------

result.energychain.median <-  result.energychain[, setdiff(colnames(result.energychain), c("mean"))]

result.energychain.median <- as.data.frame(apply(result.energychain.median,1,function(x) median(x)))
colnames(result.energychain.median) <- "median"
mean(result.energychain.median[,"median"])


#results.energychain.percent

# results.energychain.percent <- cbind(results.energychain.percent, rownames(results.energychain.percent))
# 
# results.energychain.percent[which(results.energychain.percent[,1] == max(results.energychain.percent[,1])),]
# 
# stages <- data.frame( V1 = as.character(gsub("(^<[\\w]+>)(<.*)","\\1",results.energychain.percent[,2], perl=T)),
#                V2 = as.numeric(results.energychain.percent[,1]))
# 
# stages_2 <- split(stages, stages[,1])
# 
# lapply(stages_2, function(df) {
#   
# 
#  mean(df$V2)
#   
# })


mean(results.energychain.percent[,1])
median(results.energychain.percent[,1])
summary(results.energychain.percent[,1])

nrow(results.energychain.percent)


rownames(result.energychain[which(result.energychain[,"mean"] < 0.01),])



wd <- getwd()
setwd(wd.interim)
oc_lower_1p <- rownames(result.energychain[which(result.energychain[,"mean"] < 0.01),])
writeLines(noquote(oc_lower_1p), "categories_occurrence_lower_1_percent.txt")
setwd(wd.final)
write.csv(result.energychain.median, "categories_energy_occurrence_median.csv")
setwd(wd)
#interim <- result.energychain
##END - check median values of categories and which were lower than 1 percent---------------------



result.energychain <- t(result.energychain)

result.energychain <- as.data.frame(result.energychain)

result.energychain <- cbind(rownames(result.energychain), result.energychain)
colnames(result.energychain)[1] <- "case"
rownames(result.energychain) <- NULL

results.melt <- melt(result.energychain, id.vars= c("case"))

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



annotate.text <- unlist(lapply(main.categories, function(item) {
  
  max(grep(item, unique(plot.results$category), perl=T, ignore.case=T))
  
}))

annotate.text  <- c(0, annotate.text,length(unique(plot.results$category) ))

annotate.position <- vector(mode="numeric", length= length(annotate.text)-1 )
for (i in 1:(length(annotate.position))) {
  
  annotate.position[i] <- annotate.text[i]+(annotate.text[i+1]-annotate.text[i])/2
  
}
annotate.position <-  annotate.position+0.5

#further simplification of names of categories

#plot.results$category  <- gsub("(^<[\\w]+>)(<.*$)", "\\2", plot.results$category, perl=T)

#plot.results$category <- factor(plot.results$category, levels = unique(plot.results$category))

#----------------------plot-----------------------
x.axis.label.size <- c(10)

y.axis.label.size <- c(11)
y.axis.title.size <- c(11)
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
  scale_x_discrete(limits = rev(levels(plot.results$category))) + #this generally works but the color bars that were introduced are not reversed
  #force start of the bars directly at the axis (without the distance) - NOT WORKING WITH YLIM
  #scale_y_continuous(expand = c(0,0)) +
  #scale_x_discrete(expand = c(0,0)) +

 # geom_vline(aes(xintercept = c(rep(vertical.line[1],  nrow(plot.results)))), linetype= "dashed") +
  geom_vline(aes(xintercept = c(rep(vertical.line[2], nrow(plot.results)))), linetype= "dashed") +
  geom_vline(aes(xintercept = c(rep(vertical.line[3], nrow(plot.results)))), linetype= "dashed") +
  geom_vline(aes(xintercept = c(rep(vertical.line[4], nrow(plot.results)))), linetype= "dashed") +
  geom_vline(aes(xintercept = c(rep(vertical.line[5], nrow(plot.results)))), linetype= "dashed") +
  geom_vline(aes(xintercept = c(rep(vertical.line[6], nrow(plot.results)))), linetype= "dashed") +
  geom_vline(aes(xintercept = c(rep(vertical.line[7], nrow(plot.results)))), linetype= "dashed") +
  geom_vline(aes(xintercept = c(rep(vertical.line[8], nrow(plot.results)))), linetype= "dashed") +
  geom_vline(aes(xintercept = c(rep(vertical.line[9], nrow(plot.results)))), linetype= "dashed") +
  geom_vline(aes(xintercept = c(rep(vertical.line[10], nrow(plot.results)))), linetype= "dashed") +
  geom_vline(aes(xintercept = c(rep(vertical.line[11], nrow(plot.results)))), linetype= "dashed") +
  geom_vline(aes(xintercept = c(rep(vertical.line[12], nrow(plot.results)))), linetype= "dashed") +
  geom_vline(aes(xintercept = c(rep(vertical.line[13], nrow(plot.results)))), linetype= "dashed") +
  geom_vline(aes(xintercept = c(rep(vertical.line[14], nrow(plot.results)))), linetype= "dashed") +
  geom_vline(aes(xintercept = c(rep(vertical.line[15], nrow(plot.results)))), linetype= "dashed") +
  geom_vline(aes(xintercept = c(rep(vertical.line[16], nrow(plot.results)))), linetype= "dashed") +
 # geom_vline(aes(xintercept = c(rep(vertical.line[17], nrow(plot.results)))), linetype= "dashed") +
#  geom_vline(aes(xintercept = c(rep(vertical.line[18], nrow(plot.results)))), linetype= "dashed") +

#   annotate("rect", xmin = vertical.line[1], xmax = vertical.line[2], ymin = 0.95, ymax = 1, fill="grey90") +
#  # annotate("text", x = annotate.position[1], y = .95, label = "resources", size=annotate.label.size, angle=0) +
#   
#   annotate("rect", xmin = vertical.line[2], xmax = vertical.line[3], ymin = 0.95, ymax = 1, fill="grey80") +
# #  annotate("text", x = annotate.position[2], y = .95, label = "conversion", size=annotate.label.size, angle=0) +    
#   
#   annotate("rect", xmin = vertical.line[3], xmax = vertical.line[4], ymin = 0.95, ymax = 1,fill="grey70") +
#  # annotate("text", x = annotate.position[3], y = .95, label = "distribution", size=annotate.label.size, angle=0) +
#   
#   annotate("rect", xmin = vertical.line[4], xmax = vertical.line[5], ymin = 0.95, ymax = 1, fill="grey60") +
#  # annotate("text", x = annotate.position[4], y = .95, label = "sales_contracts", size=annotate.label.size, angle=0) +  
#   
#   annotate("rect", xmin = vertical.line[5], xmax = vertical.line[6], ymin = 0.95, ymax = 1, fill="grey50") +
# #  annotate("text", x = annotate.position[5], y = .95, label = "technology", size=annotate.label.size, angle=0) +
#   
#   annotate("rect", xmin = vertical.line[6], xmax = vertical.line[7], ymin = 0.95, ymax = 1, fill="grey40") +
#  # annotate("text", x = annotate.position[6], y = .95, label = "form", size=annotate.label.size, angle=0) +
# 
#   annotate("rect", xmin = vertical.line[7], xmax =  0, ymin = 0.95, ymax = 1, fill="grey30") +
#   # annotate("text", x = annotate.position[9], y = .95, label = "energy_consumption_and_end_use_sectors", size=annotate.label.size, angle=0) +
#   
#   
    
 # annotate("rect", xmin = vertical.line[6], xmax =  length(unique(plot.results$category))+1, ymin = 0.95, ymax = 1, fill="grey46") +
 # annotate("text", x = annotate.position[9], y = .95, label = "energy_consumption_and_end_use_sectors", size=annotate.label.size, angle=0) +
  
 
#   
#   annotate("text", x = annotate.position[7], y = .75, label = "end_use_energy_consumption", size=annotate.label.size, angle=90) +
#   
#   annotate("text", x = annotate.position[8], y = .75, label = "end_use_mobility", size=annotate.label.size, angle=90) +
#   
#   annotate("text", x = annotate.position[9], y = .75, label = "9end_use_buildings", size=annotate.label.size, angle=90) +
#   
#   annotate("text", x = annotate.position[10], y = .75, label = "10local_public_administration", size=annotate.label.size, angle=90) +
#   
#   annotate("text", x = annotate.position[11], y = .75, label = "11end_use_public_illumination", size=annotate.label.size, angle=90) +
#   
#   annotate("text", x = annotate.position[12], y = .75, label = "12end_use_food", size=annotate.label.size, angle=90) +
#   
#   annotate("text", x = annotate.position[13], y = .75, label = "13end_use_residents", size=annotate.label.size, angle=90) +
#   
#   annotate("text", x = annotate.position[14], y = .75, label = "14end_use_commerce", size=annotate.label.size, angle=90) +
#   
#   annotate("text", x = annotate.position[15], y = .75, label = "15end_use_economy", size=annotate.label.size, angle=90) +
#   
#   annotate("text", x = annotate.position[16], y = .75, label = "16end_use_economy", size=annotate.label.size, angle=90) +
  
  
  
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
scale_figure <- c(9,6.5)

win.metafile("Figure_1_energy_chain_occurrence_boxplot.emf", height=scale_figure[1] ,width=scale_figure[2])
p
dev.off()


postscript("Figure_1_energy_chain_occurrence_boxplot.eps", height=scale_figure[1] ,width=scale_figure[2])
p
dev.off()







#ggsave(file="energy_chain_occurrence_plot.pdf")
#ggsave(file="Figure_1_energy_chain_occurrence_boxplot.eps",  dpi=800, units="cm")
#ggsave(file="Figure_1_energy_chain_occurrence_boxplot.emf",  dpi=800, units="cm")



#START Plot energychain - boxplot-------------------------------------





