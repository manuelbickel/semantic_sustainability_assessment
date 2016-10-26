
library("ggplot2")
library("reshape2")
warning("Please make sure that categories have been named and combined by hand within the script.")
library("plyr")


#can also be done right before ploting with the result.matrix -> maybe easier....
#generate textlist selection for plotting 
#i.e. combine the hannover matrices to one
# names(textlist)
# hannover <- "x_GER_Hannover_2008_KSAP_Materialband_CPC1_main_appendix"
# for (i in 1:8) {
#  }





#START evaluation energy chain----------------------------------

##START select the categories which shall be checked on occurrence from the available wordlistnames------------------
categories <- names(wordlists)[grep("__energy_", names(wordlists))]

#build combined (main) categories manually and add them to the categories
#use following code for combining: "categoryX~~categoryY~~categoryZ",.....

#categories[grep("resources_foss_ELit", categories)]....
categories <- categories[-c(grep("detailed_PPtechnology", categories))]

#COMBINE ALL AVAILABLE CATEGORIES, every category needs an upper category
categories.combined <- c(paste(categories[grep("resources", 
                                               categories)], collapse = "~~"),
                         paste(categories[grep("conversion", 
                                               categories)], collapse = "~~"),
                         paste(categories[grep("distribution", 
                                               categories)], collapse = "~~"),
                         paste(categories[grep("sales", 
                                               categories)], collapse = "~~"),
                         paste(categories[grep("mobility", 
                                               categories)], collapse = "~~"),
                         paste(categories[grep("storage|fuell_cells",
                                               categories)], collapse = "~~"),
                         paste(categories[grep("household_and_office_appliances|built_environment|climatisation_active_heat_cold",
                                               categories)], collapse = "~~"),
                         paste(categories[grep("energy_carrier_unspecific|heat_cold_electricity_general|general|consumption|efficiency|saving|use_of_waste_energy",
                                               categories)], collapse = "~~")
)

###<<<<<<<<<<<<<<<<<<in order to check which categories have been forgotten to be assigned
#setdiff(categories, unique(unlist(strsplit(categories.combined, "~~"))))

categories <- c(categories, categories.combined)  

categories.num <-length(categories)
##END select the categories which shall be checked on occurrence from the available wordlistnames------------------


##START count (percentaged) occurrence of categories within each text: (occurrence of category within X measures of a text)/(number of all measures in a text)-------------------------

###START - do counting and create results matrix-------------------------
setwd(wd.interim)
files <- list.files(pattern = paste("^.*", occurrence.filename.tag, "[[:digit:]]+.csv$", sep=""))

#initial
result.energychain <- matrix(rep(0, categories.num*length(files)), nrow = categories.num)
row.names(result.energychain) <- categories
colnames(result.energychain) <- rep("dummy", ncol(result.energychain))


for (t in seq(length(files))) {
  
  print(files[t])  
  evaluationmatrix <- read.csv(files[t], header = TRUE)
  
  #if reading in of the csv generates a column with numbers (old rownubers) this column is deleted
  if(colnames(evaluationmatrix)[1] == "X") {
    evaluationmatrix <- evaluationmatrix[,-c(1)]
    
  }
  
  
  #delete the words column
  evaluationmatrix <-   evaluationmatrix[,-c(1)]
  
  #aggregate the category lines
  evaluationmatrix <- aggregate(evaluationmatrix[2:ncol(evaluationmatrix)], by=list(category=evaluationmatrix$category), FUN=sum)
  
  
  #check if all desired category names appear in the matrix, if not something with the naming went wrong
  #the all have to be present as they were read in from the wordlists previously
  if(length(intersect(evaluationmatrix[,"category"], 
                      categories[!(categories %in% categories.combined)])) != 
     length(categories[!(categories %in% categories.combined)])) {
    
    warning("ERROR: Not all of the selected categories are present in the evaluationmatrix.
            Possible reasons for this error might be mistakes in naming the categories or during reading in the wordlists.")
  }
  
  #make matrix boolean
  evaluationmatrix[,c(2:ncol(evaluationmatrix))] <- ifelse(evaluationmatrix[,c(2:ncol(evaluationmatrix))]>0,1,0)
  
  #print(paste("t:", t, sep=""))
  
  meas.totalnum <- ncol(evaluationmatrix)-1
  
  #in future steps maybe also the year, etc. is extracted and used as name
  colnames(result.energychain)[t] <- gsub(paste("(^.*GER__)([A-Za-z]+)(__.*$)"), "\\2",files[t])
  
  
  for(i in seq(length(categories))) {
    
    print(categories[i])
    #i=1
    category <- categories[i]
    
    #select only the lines which are connected to (contain) a word which refers to the respective category of the energy chain
    #if the category is a combined category it has to be split to be suitable for the which==XX inquiry (if this operation is done outside below line, an additional variable like category.unl has to be introduced, othwerwise the category name is wrong for finding it again in the resultmatrix)
    category.subset <- evaluationmatrix[which(evaluationmatrix[,"category"] %in% unlist(strsplit(category, "~~"))),]
    
    category.subset.counts <- category.subset[,c(2:ncol(category.subset))]
    
    #mode(category.subset.counts) <- "numeric"
    
    #Count procedure: if the column sum is 1 or higher the measure contains a selective word
    #if the subset contains only one row it is converted to an atomic vector and colSums will not work
    #    if (class(category.subset.counts) == "numeric") {
    #     
    #     count <- sum(category.subset.counts)
    #     
    #   } else {
    #     
    #     count <- colSums(category.subset.counts)
    #     
    #   }
    
    count <- colSums(category.subset.counts)
    #only "boolean-counting"/"occurrence at all" applied, word frequency within a measure is not used for evaluation
    #therefore all numbers larger than 0 are converted to a 1
    count <- ifelse(count>0,1,0)
    result <- round(sum(count), digits=2)
    
    result.energychain[category,t] <- result
  }
  }


#add MEAN over all columns as last column 
result.energychain  <- cbind(result.energychain, round((rowSums(result.energychain)/ncol(result.energychain)), digits=2))
colnames(result.energychain)[ncol(result.energychain)] <- "mean"
###END - do counting and create results matrix-------------------------
#dimnames(result.energychain)

###START ORDER the rows according to the energy chain order--------------------------

#this grep command finds the combined categories by the "~~" marker
#grep(".*resources.*~~", row.names(result.energychain ))
#this grep command finds the single categories

rows.ordered <- c(grep(".*resources.*~~", row.names(result.energychain )),
                  grep("(^c[\\d]+)(__energy)([\\w]+)(resources)((?!~~).)*$", row.names(result.energychain), perl=TRUE),
                  
                  grep(".*conversion.*~~", row.names(result.energychain )),
                  grep("(^c[\\d]+)(__energy)([\\w]+)(conversion)((?!~~).)*$", row.names(result.energychain), perl=TRUE),
                  
                  grep(".*distribution.*~~", row.names(result.energychain )),
                  grep("(^c[\\d]+)(__energy)([\\w]+)(distribution)((?!~~).)*$", row.names(result.energychain), perl=TRUE),
                  
                  grep(".*sales.*~~", row.names(result.energychain )),
                  grep("(^c[\\d]+)(__energy)([\\w]+)(sales)((?!~~).)*$", row.names(result.energychain), perl=TRUE),
                  
                  
                  grep(".*mobility.*~~", row.names(result.energychain )),
                  grep("(^c[\\d]+)(__energy)([\\w]+)(mobility)((?!~~).)*$", row.names(result.energychain), perl=TRUE),
                  
                  grep("(.*)(fuell_cells)(.*)(~~)", row.names(result.energychain ), perl=TRUE),
                  grep("(^c[\\d]+)(__energy)([\\w]+)(fuell_cells)((?!~~).)*$", row.names(result.energychain), perl=TRUE),
                  grep("(^c[\\d]+)(__energy)([\\w]+)(storage)((?!~~).)*$", row.names(result.energychain), perl=TRUE),
                  
                  
                  grep("(.*)(household_and_office_appliances)(.*)(~~)", row.names(result.energychain ), perl=TRUE),
                  grep("(^c[\\d]+)(__energy)([\\w]+)(built_environment)((?!~~).)*$", row.names(result.energychain), perl=TRUE),
                  grep("(^c[\\d]+)(__energy)([\\w]+)(climatisation_active_heat_cold)((?!~~).)*$", row.names(result.energychain), perl=TRUE),
                  grep("(^c[\\d]+)(__energy)([\\w]+)(household_and_office_appliances)((?!~~).)*$", row.names(result.energychain), perl=TRUE),
                  
                  
                  grep("(.*)(carrier_unspecific)(.*)(~~)", row.names(result.energychain ), perl=TRUE),
                  grep("(^c[\\d]+)(__energy)([\\w]+)(carrier_unspecific)((?!~~).)*$", row.names(result.energychain), perl=TRUE),
                  grep("(^c[\\d]+)(__energy)([\\w]+)(consumption)((?!~~).)*$", row.names(result.energychain), perl=TRUE),
                  grep("(^c[\\d]+)(__energy)([\\w]+)(heat_cold_electricity_general)((?!~~).)*$", row.names(result.energychain), perl=TRUE),
                  grep("(^c[\\d]+)(__energy)([\\w]+)(efficiency)((?!~~).)*$", row.names(result.energychain), perl=TRUE),
                  grep("(^c[\\d]+)(__energy)([\\w]+)(saving)((?!~~).)*$", row.names(result.energychain), perl=TRUE),
                  grep("(^c[\\d]+)(__energy)([\\w]+)(use_of_waste_energy)((?!~~).)*$", row.names(result.energychain), perl=TRUE),
                  grep("(^c[\\d]+)(__energy_general)$", row.names(result.energychain), perl=TRUE)
)

rows.unordered <- setdiff(seq(nrow(result.energychain)), rows.ordered)
rows.ordered <- c(rows.ordered, rows.unordered)

#dimnames(result.energychain)

result.energychain <- result.energychain[rows.ordered ,]
###END ORDER the rows according to the energy chain order--------------------------

##START count (percentaged) occurrence of categories within each text: (occurrence of category within X measures of a text)/(number of all measures in a text)-------------------------


#END evaluation energy chain----------------------------------


#something regarding the counting and summing up is wrong, row sum yields a smaller value in the combined categories
#than it should result from summing the summed up single categories
#SOLVED - summing up occurrences of single categories is larger than summing up general occurrence of these categories together
#e.g. to categories might intersect in one measure, then they are counted once in the "together sum" -hence one in total - but in the single sum they are counted as one each - hence two in total.
#e.g. compare the resources:
#result.energychain[grep("resources", row.names(result.energychain)),]
#some texts have 0 entries and others have 0.0000 entries maybe this is a source of error


#dimnames(result.energychain)
#setdiff(categories, unique(unlist(strsplit(maincategories, "~~"))))


#START re-format result matrix for plotting-------------------------------------

#first step: colnames are the combined categories - extract them by their marker ~~
maincategories <- categories[grep("~~", categories)]

##START loop through the categories to build a column (in melted form) for each category-------------------
for (i in seq(length(maincategories))) {
  print(i)
  print(maincategories[i])
  
  #select a maincategory MC from the results matrix and extract their subcategories SC, these MC+ all SCs are used in the loop 
  rows.combinedcategory <- c(maincategories[i], unlist(strsplit(maincategories[i], "~~")))
  
  #select from full results matrix only rows which contain the loop categories -> starting matrix to reformat
  result.energychain.combinedcategory <- result.energychain[row.names(result.energychain) %in% rows.combinedcategory,]
  
  #prepare the matrix for melting
  result.energychain.combinedcategory <- as.data.frame(result.energychain.combinedcategory)
  result.energychain.combinedcategory <- cbind(rownames(result.energychain.combinedcategory), result.energychain.combinedcategory, stringsAsFactors=FALSE)
  colnames(result.energychain.combinedcategory)[1] <- "category"
  rownames(result.energychain.combinedcategory) <- NULL
  
  #MCs in the results matrix represent total counts of all SCs, the name of the MC includes all the SC names
  #in order to serve as column name the MC name is replaced by its short name to serve 
  #(i )as "MCname_total" in the row and (ii) as MCname in the column
  
  #generate the short MC name
  maincategory.generic <- if(grepl("energy__spec__resources",  maincategories[i]) == TRUE) {
    
    c("_resources")
  } else if(grepl("energy__spec__sales",  maincategories[i]) == TRUE) {
    c("_sales") 
    
  } else if(grepl("energy__spec__mobility",  maincategories[i]) == TRUE) {
    c("_mobility")
    
  } else if(grepl("energy__spec__distribution",  maincategories[i]) == TRUE) {
    c("_distribution")
    
  } else if(grepl("energy__spec__conversion",  maincategories[i]) == TRUE) {
    c("_conversion")
    
  } else if(grepl("energy__spec__built_environment",  maincategories[i]) == TRUE) {
    c("_end_use")
    
  } else if(grepl("storage",  maincategories[i]) == TRUE) {
    c("_spec_technology")  
    
  } else if(grepl("carrier_unspecific",  maincategories[i]) == TRUE) {
    c("_energy_general")
    
    
  }#end if
  
  #replace the MC name by the short MC name_total (as this is what the row really is)  
  result.energychain.combinedcategory[,1] <- gsub(maincategories[i], paste("TOTAL",maincategory.generic,sep=""), result.energychain.combinedcategory[,1])
  print(result.energychain.combinedcategory[,1])
  melted <- melt(result.energychain.combinedcategory, id.vars=c("category"), measure.vars= colnames(result.energychain.combinedcategory)[2:ncol(result.energychain.combinedcategory)])
  
  
  #add the short name (freed from the leading _) to the melted data as additional melted id variable (which in wide format is an additional first column specifying the maincategory for the subcategories)
  melted <- cbind(rep(substr(maincategory.generic, 2, nchar(maincategory.generic)), nrow(melted)), melted)
  colnames(melted)[1] <- "maincategory"
  
  ###START initialize / add results to the melted data format----------------------------
  if (i == 1) {
    #initializing result
    results.melt <- melted
    
  } else {
    #additional results which are added
    results.melt <- rbind(results.melt, melted)
  } #endif
  ###END initialize / add results to the melted data format----------------------------
}
##END loop through the categories to build a column (in melted form) for each category--------


#clean the names of the cases (here: cities) for the plot
#ASSUMPTION plotname is separated from the rest of the text by a double hyphen "__"
#gsub("(^)(A-Za-z)(_)(.*$)" , "\\2", results.melt[,"variable"], perl=TRUE)


#END re-format result matrix for plotting-------------------------------------



#START Plot energychain-------------------------------------

plot.results <- results.melt

plot.results[,2] <- gsub("^.*__", "", plot.results[,2])

#exclude mean value and energy general
plot.results <- subset(plot.results, plot.results$variable != "mean")
plot.results <- subset(plot.results, plot.results$maincategory != "energy_general")

#unique(plot.results$maincategory)
#head(plot.results,50)

#names(plot.results)
#Reassign the levels in order to preserve the order as displayed in the data
plot.results$category <- factor(plot.results$category, levels = unique(plot.results$category))

#for cecking the data in wide format
#casttest <- dcast(plot.results, maincategory + category ~ variable, value.var = "value")

#plot.results <- plot.results[-which(plot.results[,"variable"] == c("Hannover")),]

c <- ggplot(plot.results, aes(category,value, fill=maincategory)) # fill = category?
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
  scale_y_continuous(expand = c(0,0), limits = c(0,200)) +
  
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



#NOT USED/TESTED:<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#draws x and y axis line
#theme(axis.line = element_line(color = 'black'))  

#coord_cartesian(xlim = NULL, ylim = NULL, wise = NULL)  

#For space between factor bars - DID NOT WORK IN MY EXAMPLE
#+ geom_bar(width = 0.8, position = position_dodge(width = 0.9))



##<<<<<<<<---Export - Save as Metafile

#END Plot energychain-------------------------------------




#START plot energy chain - only mean----------------------------

plot.results.mean <- results.melt

plot.results.mean[,2] <- gsub("^.*__", "", plot.results.mean[,2])

#select mean value and exclude energy_general category
plot.results.mean <- subset(plot.results.mean, plot.results.mean$variable == "mean")
plot.results.mean <- subset(plot.results.mean, plot.results.mean$maincategory != "energy_general")

#head(plot.results.mean,50)



#names(plot.results.mean)
#Reassign the levels in order to preserve the order as displayed in the data
plot.results.mean$category <- factor(plot.results.mean$category, levels = unique(plot.results.mean$category))

#for cecking the data in wide format
#casttest <- dcast(plot.results.mean, maincategory + category ~ variable, value.var = "value")

#plot.results.mean <- plot.results.mean[-which(plot.results.mean[,"variable"] == c("Hannover")),]


c <- ggplot(plot.results.mean, aes(category,value, fill=maincategory)) # fill = category?
c + geom_bar(stat = "identity", position = "dodge", colour = "black", width = 0.5, show_guide=TRUE) + #width: distance between bars
  
  #theme black and white
  theme_bw() +
  
  
  #colour set for columns
  scale_fill_grey(start = 0.8, end = 0, na.value = "red") +
  
  
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
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.25)) +
  
  #set the limits of the y axis
  scale_y_continuous(expand = c(0,0), limits = c(0,200)) +
  
  
  #force start of the bars directly at the axis (without the distance) - NOT WORKING WITH YLIM
  #scale_y_continuous(expand = c(0,0)) +
  #scale_x_discrete(expand = c(0,0)) +
  
  
  #use a legend theme which includes a black rectangle around the legend
  theme(legend.background = element_rect(colour = "black")) +
  
  
  geom_vline(aes(xintercept = c(rep(5.5,  nrow(plot.results.mean)))), linetype= "dashed") +
  geom_vline(aes(xintercept = c(rep(12.5, nrow(plot.results.mean)))), linetype= "dashed") +
  geom_vline(aes(xintercept = c(rep(17.5, nrow(plot.results.mean)))), linetype= "dashed") +
  geom_vline(aes(xintercept = c(rep(23.5, nrow(plot.results.mean)))), linetype= "dashed") +
  geom_vline(aes(xintercept = c(rep(32.5, nrow(plot.results.mean)))), linetype= "dashed") +
  geom_vline(aes(xintercept = c(rep(35.5, nrow(plot.results.mean)))), linetype= "dashed") +
  
  #remove crossing lines through the colouer fields in the legend
  guides(fill = guide_legend(override.aes = list(colour = NULL))) 





#NOT READY ---> probably not facet wrapt but a loop might be needed to create 
#several graphs and arrange them, then each graph can be controlled
#separately and be treated as "normal single graph"
#insert vertical lines between the main categories

# geom_vline(aes(xintercept = c(rep(10,5)))) +
# geom_vline(aes(xintercept = c(rep(4.5, 5)))) +
#geom_vline(aes(xintercept = c(rep(10,5)))) +
# geom_vline(aes(xintercept = eval(parse(text = paste("4.5", sep="" ))))) +


#which(plot.results.mean$ == 'm')
#grep("^total_", plot.results.mean$category)

#geom_vline(data=plot.results.mean, aes(xintercept = grep("^total_", plot.results.mean$category))) +
# geom_vline(aes(xintercept = c(which(plot.results.mean$category == "total_resources")[1]#,
#  which(plot.results.mean$category == "total_conversion")[1],
# which(plot.results.mean$category == "total_distribution")[1],
#  which(plot.results.mean$category == "total_sales")[1],
#  which(plot.results.mean$category == "total_mobility")[1],
#  which(plot.results.mean$category == "total_builtenvironment")[1]
#   ))) +



#Reverse the order within the legend
#guides(fill = guide_legend(reverse = TRUE)) + 

#should add lines around the color boxes in legend - does not work as desired 
#theme(legend.key = element_rect(colour = "black")) +

#create separate plots by a certain category - NOT WORKING WITH FACET_WRAP 
#facet_grid( ~maincategory, scales="free_x") +

#make a plot for each case (e.g. city) - not needed for single case plot
#facet_wrap(~variable,ncol=5)

#END plot energy chain - only mean----------------------------
