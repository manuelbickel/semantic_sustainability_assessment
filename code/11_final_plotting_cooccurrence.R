#COOCCURRENCE IN WIDE FORMAT
library("ggplot2")
library("reshape2")
library("plyr")
library("grid")




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


wd.errors <- paste(wd.main, "errors/", sep="")
#initialize an empty txt.file if there is no file, yet
if (c("error_record.txt") %in% list.files(path = wd.errors) == FALSE) {
  writeLines(c(""), paste(wd.errors, "error_record.txt", sep="") )
}
#END - directories------------------------------------


#START R-options------------------------------------
options("nwarnings" = 150)
#START R-options------------------------------------


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



scale.by.x.diag <- TRUE


setwd(wd.interim)
files <- list.files(pattern = "__cat_coocc_mat.csv")

#fix the categories of the mean case as reference for other cases to be compared later
cooccurrence.case.reference <- grep("Lower_Saxony_regional_centers", files)

#exclude single files which are included for expermental testing, etc.
files <- files[-grep("GER__Wilhelmshaven__CPC1_main_short", files)]
#files <- files[-grep("Hannover96", files)]

#####--------------Plots for Lower Saxony, mean--------------------------------------

cooccurrence.case <- read.csv(files[cooccurrence.case.reference], header=TRUE)

row.names(cooccurrence.case) <- cooccurrence.case[,1]
cooccurrence.case <- as.matrix(cooccurrence.case[,-c(1)])


if (identical(colnames(cooccurrence.case), row.names(cooccurrence.case)) == FALSE) {
  warning("Matrix not symmetric. Following calculations will produce wrong results.")
}




colnames(cooccurrence.case) <- gsub("c[[:digit:]]+__SUST__energy_electricity_sector", 
                                    "c000__EN__energy_electricity_sector", 
                                    colnames(cooccurrence.case))

colnames(cooccurrence.case) <- gsub("c[[:digit:]]+__SUST__energy_heat_sector", 
                                    "c000__EN__energy_heat_sector", 
                                    colnames(cooccurrence.case))


colnames(cooccurrence.case) <- gsub("(.*?)(EN__spec__)(.*$)", "EN__\\3", colnames(cooccurrence.case),perl=T)


row.names(cooccurrence.case) <- gsub("c[[:digit:]]+__SUST__energy_electricity_sector", 
                                    "c000__EN__energy_electricity_sector", 
                                    row.names(cooccurrence.case))

row.names(cooccurrence.case) <- gsub("c[[:digit:]]+__SUST__energy_heat_sector", 
                                    "c000__EN__energy_heat_sector", 
                                    row.names(cooccurrence.case))


row.names(cooccurrence.case) <- gsub("(.*?)(EN__spec__)(.*$)", "EN__\\3", row.names(cooccurrence.case),perl=T)



##START - exclusion of certain rows and columns---------------------------
exclusionterms <- c("_SUST_", 
                    "_fed_state", 
                    # "c19__", 
                    # "c20__", 
                    # "c24__",
                    #  "c28__", 
                    # "c26__",
                    "LEV__offshore",
                    "LEV__onshore",
                    "EN__saving",
                    "EN__general",
                    "CE__climate_protection_general",
                    "OCM__",
                    "__fishery",
                    "infrastructure_disposal_general",
                    "infrastructure_supply_general",
                    "infrastructure_general",
                    "commercial_sector_unspecific",
                    "buildings_unspecific",
                    "communication_objects",
                  #  "technology_general",
                    "resources_nucl",
                    "execution_of_law",
                  #  "tech_storage",
                  #  "tech_fuel_cells",
                    "PPtechnology",
                    "carrier_uns",
                    "energy_heat_cold", 
                    "detailed_PP",
                    "energy_general",
                    "energy_saving", 
                    "LEV__regions_areas_Germany_TBC",
                    "carrier_oil_gas",
                     "domesticated_animals",
                     "climate_protection_tools",
                    "population_general",
                    "IMWW__mobility_sector",
                    "technology_general",
                    "information_technology",
                    "consumer_goods",
                     "building_parts",
                   #  "heat_sector",
                   #  "electricity_sector",
                    "mobility_biofuels",
                    "mobility_hydrogen",
                    "mobility_plane",
                    "conversion_nucl",
                    "renewable_other_than_bike"
                     )


#categories[grep("resources_foss_ELit", categories)]....

for (i in seq(length(exclusionterms))) {
  
  delete.rows.columns <- grep(exclusionterms[i], row.names(cooccurrence.case))
                
  if (length(delete.rows.columns) > 0) {
    cooccurrence.case <- cooccurrence.case[-delete.rows.columns, -delete.rows.columns]
  }
}

##START - correct wrong spelling and simplify category names-------------------
#shorten the category names
colnames(cooccurrence.case) <- gsub("c[[:digit:]]+__", "", colnames(cooccurrence.case))

row.names(cooccurrence.case) <- colnames(cooccurrence.case)
#substr(...,1,35)
#gsub("^.*__", "", colnames(cooccurrence.case))

##END - exclusion of certain rows and columns---------------------------


##START order rows and columns by highest to lowest activity--------------------

#calculate the "activity sums" of the categories for rows/columns and order the rows/columns accordingly
#row.column.order <- order(rowSums(cooccurrence.case), decreasing = TRUE)
#cooccurrence.case <- cooccurrence.case[row.column.order,row.column.order]
##END order rows and columns by highest to lowest activity--------------------

#START----------alternative alphabetic order-------------------------------
ordered.levels.alphabetic <- order(colnames(cooccurrence.case))
cooccurrence.case <- cooccurrence.case[ordered.levels.alphabetic,ordered.levels.alphabetic]
#END---------alternative alphabetic order-----------------------------

order.interim <- data.frame(category = as.character(row.names(cooccurrence.case)),
                            value = as.numeric(rowSums(cooccurrence.case)) )

#row.names(order.interim) <- category = as.character(row.names(cooccurrence.case)),

order.interim.agg <- order.interim

order.interim.agg[,1] <- gsub("([A-Z]+)(__)(.*$)", "\\1",order.interim.agg[,1] , perl=T)
order.interim.agg <- aggregate(order.interim.agg[,2], by=list(category=order.interim.agg$category), FUN=mean)

for (c in 1:nrow(order.interim.agg)) {
  replace <- order.interim.agg[c,]
  order.interim[grep(replace[,1], order.interim[,1]),2] <- replace[,2]
  
}

order.cluster.occ <- order(order.interim[,2], decreasing=T)

cooccurrence.case <- cooccurrence.case[order.cluster.occ ,order.cluster.occ]


#START - format numeric part of matrix
#cooccurrence.case.matrix 
x <- cooccurrence.case


x  <- round(x, d=3)


#following scaling works...
##START----------scale x by maximum values of diagonal--------------------
#scale x according to maximum possible cooccurrence of categories for each cooccurrence box

if (scale.by.x.diag == TRUE) {
print("Plot is scaled by minimum diagonal values of the matrix.")
x.diag <- as.numeric(diag(x))

for (d in seq(length(x.diag))) {
  
  x.diag.value <- cbind(x.diag, rep(as.numeric(x.diag[d]), length(x.diag)))
  
  x.diag.value <- apply(x.diag.value,1, min)
  
  x[,d] <- round(x[,d]/x.diag.value,d=3)
  

}
}
##END----------scale x by maximum values of diagonal--------------------

# x <- ifelse(x==0 , 0,x)
# x <- ifelse(x>0 & x < .2, 1,x)
# x <- ifelse(x>=.2 & x < .4, 2,x)
# x <- ifelse(x>=.4 & x < .6, 3,x)
# x <- ifelse(x>=.6 & x < .8, 4,x)
# x <- ifelse(x>=.8 & x < 1, 5,x)
# x <- ifelse(x==1,6,x)


# #before using log, zero values are changed to NA
# #otherwise they end up as -Inf values
# x <- ifelse(x == 0 , NA, x)
# 
# #bin data into the main logarithmic classes
# x <- round(log(x))
# 
# 
# class.zero <- -(min(x, na.rm=TRUE)-1)
# #change back the NA values to build the lower end of the classes
# x <- ifelse(is.na(x) , -(class.zero), x)
# 
# #for easier interpretaion change to postive scale
# x <- x*(-1)


#reduce classes: highes interaction, high interaction, low interaction, no interaction
#classes.middle <- setdiff(unique(x), c(class.zero))

# 
classes <- c(0,0.25,0.5,0.75,1)
x <- ifelse(x <= classes[1], 0, x)
x <- ifelse((x > classes[1] & x < classes[2]), 0.05, x)
x <- ifelse((x >= classes[2] & x < classes[3]), 0.375,x)
x <- ifelse((x >= classes[3] & x < classes[4]), 0.625,x)
#x <- ifelse((x >= classes[4] & x < classes[5]), 0.7,x)
x <- ifelse((x >= classes[4]), 0.875,x)
#x <-ifelse(x == NA, 0, x)





# x <- ifelse(x <= (min(x)+1) , 1, x)
# x <- ifelse((x > (min(x)+1) & x <=4), 2, x)
# x <- ifelse((x > 4 & x <=6), 3,x)
# x <- ifelse((x > 6 & x <= 8), 4,x)
# x <- ifelse((x == 9), 5,x)
#  
# #include median in upper class
# x <- ifelse(x ==  median(classes.middle), 3, x)
# 
# x <- ifelse(x == class.zero , 4, x)

#set lower triangle to NA in order to allow better readability and clarity of the matrix
#x[lower.tri(x, diag = FALSE)] <- NA

#test <- x
#x <- test


##START-----------scale by diagional values------->>no significant change in results...deactivated----------



#scale each row and column by its value in the diagonal
# x.diag <- diag(x)
# x.diag <- x.diag[grep("EN__", names(x.diag))]
# 
# 
# for (d in length(x.diag)) {
#   
#   row <- which(row.names(x) %in% names(x.diag[d]))
#     
#   x[row,] <- t(round(x[row,]/as.numeric(x.diag[d]),d=3))
#   
# }

##END-----------scale by diagional values-----------------

cooccurrence.case <- round(x, d=3)

##START renumber rownames and define the order of levels-----------------
category.1 <- rownames(cooccurrence.case)
cooccurrence.case <- cbind.data.frame(category.1, cooccurrence.case)
cooccurrence.case <- cooccurrence.case[grep("EN__", row.names(cooccurrence.case)),-grep("EN__", colnames(cooccurrence.case))]


categories <- row.names(cooccurrence.case)
main.categories <- c("resources", 
                    # "carrier", 
                     "conversion",
                     #"sector",
                     "distribution",
                     "sales",
                     "mobility", 
                     "tech")


##START order categories according to order of main categories and create the combined main superior categories---------------
order.categories <- NULL
for(i in seq(length(main.categories))) {
  
  order.categories <- c(order.categories, grep(main.categories[i], categories))
  
}
order.categories <- c(order.categories, setdiff(1:length(categories), order.categories))

cooccurrence.case <- cooccurrence.case[order.categories,]

row.names(cooccurrence.case) <- NULL
#END renumber rownames and define the order of levels-----------------


##START - convert from wide to long format----------------------
cooccurrence.case.melted <- melt(cooccurrence.case, id.vars=c("category.1"),
                                #source columns
                                measure.vars= colnames(cooccurrence.case)[2:ncol(cooccurrence.case)],
                                 
                                 #name of the destination column
                                 variable.name = "category.2", 
                                 value.name = "cooccurrence",
                                 na.rm = FALSE)
##END - convert from wide to long format----------------------


##START decrease number of levels by building classes------------------
plot.results <- cooccurrence.case.melted

#cooccurrence.levels <- unique(plot.results$cooccurrence)
cooccurrence.levels <- c(0.000, seq(1000)/1000)



#cooccurrence.levels <- 0:6
plot.results$cooccurrence <- factor(plot.results$cooccurrence, levels = cooccurrence.levels[order(cooccurrence.levels)])

#ordered by upper systemic category (alphabetic)
# ordered.levels.alphabetic <- unique(as.character(plot.results$category.1))
# ordered.levels.alphabetic <- ordered.levels.alphabetic[order(ordered.levels.alphabetic)]
# plot.results$category.1 <- factor(plot.results$category.1, levels = ordered.levels.alphabetic)
# plot.results$category.2 <- factor(plot.results$category.2, levels = ordered.levels.alphabetic)
# 
# 
# #position of lines to be drawn between systems
# system.levels <- gsub("__.*", "", ordered.levels.alphabetic)
# system.levels.unique <- unique(system.levels)
# line.position <- c("")
# for (i in seq(length(system.levels.unique))) {
#   
#   line.position <- c(line.position, max(grep(system.levels.unique[i],system.levels)))
#   
# }
# line.position <- as.numeric(line.position[-c(1)])


#ordered.levels.by.cluster <- unique(as.character(plot.results$category.1))
plot.results$category.1 <- factor(plot.results$category.1, levels = unique(as.character(plot.results$category.1)))
plot.results$category.2 <- factor(plot.results$category.2, levels = unique(as.character(plot.results$category.2)))


#----------------------for full adjacency matrix----------------------------------------------------
#position of lines to be drawn between systems
# system.levels <- gsub("__.*", "", ordered.levels.by.cluster)
# system.levels.unique <- unique(system.levels)
# line.position <- c("")
# for (i in seq(length(system.levels.unique))) {
#   
#   line.position <- c(line.position, max(grep(system.levels.unique[i],system.levels)))
#   
# }
# line.position <- as.numeric(line.position[-c(1)])
#----------------------for full adjacency matrix----------------------------------------------------

#position of lines to be drawn between systems
system.levels <- gsub("__.*", "", unique(as.character(plot.results$category.2)))
system.levels.unique <- unique(system.levels)
line.position <- c("")
for (i in seq(length(system.levels.unique))) {
  
  line.position <- c(line.position, max(grep(system.levels.unique[i],system.levels)))
  
}
line.position <- as.numeric(line.position[-c(1)])


line.position.v <- c("")
for (i in seq(length(main.categories))) {
  
  line.position.v <- c(line.position.v, max(grep(main.categories[i],as.character(unique(plot.results$category.1)))))
  
}
line.position.v <- as.numeric(line.position.v[-c(1)])


##END - set all potential levels for plotting--------------------------



##START plot basic mean value-------------------------------
ggplot(NULL) +
  
  #raster with no distance to the axis
  geom_raster(data= plot.results, aes(x = category.1, y = category.2, fill = cooccurrence), hjust = 0, vjust = 0) +  
  
  #making sure that none of the categories for x/y axis are dropped if they have no entries
  #this step is not necessary but kept as a safety net
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(drop = FALSE) +
  
  #set colour scale for displaying values
  #scale_fill_grey(start = 1, end = 0, na.value = "white") +   #the following alternative creates a coloured plot, probably the setting of scale is wrong,yet:scale_colour_grey(start = 1, end = 0, na.value = "red") +
  #scale_fill_hue(l=50, c=100, h=c(0, 240)) +
  scale_fill_manual(values = c("white", "grey95", "grey75", "grey30", "black")) +
  
  #remove labels
  labs(x = element_blank(), y=element_blank()) +
  
  #set basic theme of the plot
  theme_bw() +
  
  theme(
    text = element_text(size=3),
    #  #rotate x-axis label, and set distance of label/axis to zero
    axis.text.x = element_text(angle = 270, hjust = 0, vjust=.8, size=8),
    axis.text.y = element_text(vjust = .8, size=8),
    
    #remove axis title
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    
    axis.ticks = element_line(size=.01),
    
    axis.ticks.length = unit(.1, "cm"),
    
   # plot.background = element_rect(fill = NULL,colour = NA),
   plot.background = element_blank(),
    # hide legend
   
   plot.title = element_blank(),
   
   plot.margin = unit(c(.5,0,0,0), "cm"),
    legend.position = "none"#,
    
  
    #set square aspect ratio
    #aspect.ratio = 1
    ) +
    
    coord_fixed(ratio = 1) +


           #,legend.position = "bottom"
        # ,legend.key.size = unit(2.5, "cm")
  ) +

#-----------------------------------------experimental graphical adaptions-----------

#draw lines for better readability / similar to panel grid but as overlay not in the background
#vertical and horizontal lines
# geom_vline(xintercept = line.position, colour = "black", linetype= "dashed", size=.2) +
# geom_hline(yintercept = line.position, colour = "black", linetype= "dashed", size=.2)# +


geom_vline(xintercept = line.position.v, colour = "black", linetype= "dashed", size=.01) +
geom_hline(yintercept = line.position, colour = "black", linetype= "dashed", size=.01)# +


#   
#   geom_rect(data = cbind.data.frame(xrect= 1:length(ordered.levels), yrect= 1:length(ordered.levels)), 
#             size=0.1, fill=NA, colour="grey42", linetype= "dashed",
#             aes(xmin = xrect, xmax = xrect + 1, ymin = yrect, ymax = yrect + 1)
#   )
# 


# ##ggplot linetypes
# 
# d=data.frame(lt=c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash", "1F", "F1", "4C88C488", "12345678"))
# ggplot() +
#   scale_x_continuous(name="", limits=c(0,1), breaks=NA) +
#   scale_y_discrete(name="linetype") +
#   scale_linetype_identity() +
#   geom_segment(data=d, mapping=aes(x=0, xend=1, y=lt, yend=lt, linetype=lt))

#-----------------------------------------experimental graphical adaptions-----------
##START plot basic mean value-------------------------------

##<<<<<<<<---Export - Save as Metafile
wd <- getwd()
setwd(wd.final)
opar <- par()      # make a copy of current settings
par(mar=c(0, 0, 5, 2.5))
#par()$mar
#height = par("din")[2]

if (scale.by.x.diag == TRUE) { 
  ggsave(file="cooccurrence_mean__scaled_by_min_diag_plot.emf", width=3, height=4, dpi=300, scale=3)
  ggsave(file="cooccurrence_mean_scaled_by_min_diag_plot.pdf", dpi=300, scale=3)
} else {
    
ggsave(file="cooccurrence_mean__plot.emf", width=3, height=4)
ggsave(file="cooccurrence_mean__plot.pdf")
}
#par(opar)          # restore original settings 

setwd(wd)






