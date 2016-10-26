#COOCCURRENCE IN WIDE FORMAT
library("ggplot2")
library("reshape2")
#library("plyr")
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




##START - correct wrong spelling and simplify category names-------------------
#shorten the category names
colnames(cooccurrence.case) <- gsub("c[[:digit:]]+__", "", colnames(cooccurrence.case))

row.names(cooccurrence.case) <- colnames(cooccurrence.case)
#gsub("^.*__", "", colnames(cooccurrence.case))
##START - correct wrong spelling and simplify category names-------------------


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

x <- cooccurrence.case


x  <- round(x, d=3)


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



x <- ifelse(is.na(x), 0, x)
#first selection of categories which achieve at least 50% of possibly realisable co-occurrence
columns.maxima <- apply(x, 2, sum)

#which rows achieved the medium possible value in at least half of the cases
half.of.maximum.possible.sum <- nrow(x)*0.25

columns.maxima <- which(columns.maxima >= half.of.maximum.possible.sum)

x <- x[,columns.maxima]



classes <- c(0,0.25,0.5,0.75,1)
x <- ifelse(is.na(x), 0, x)
x <- ifelse(x <= classes[1], 0, x)
x <- ifelse((x > classes[1] & x < classes[2]), 0.05, x)
x <- ifelse((x >= classes[2] & x < classes[3]), 0.375,x)
x <- ifelse((x >= classes[3] & x < classes[4]), 0.625,x)
#x <- ifelse((x >= classes[4] & x < classes[5]), 0.7,x)
x <- ifelse((x >= classes[4]), 0.875,x)

#select only energy chain and exclude certain columns/rows ---------


x <- x[grep("^EN__|industrial_production_intermediate_produ|heavy_industry|public_illumination|ETCS__economy_general|food_processing_consumption|commercial_sector_unspecific|industrial_sector_incl_company|residential_area|commerce_sales", row.names(x)),
       -grep("^EN__", colnames(x))]


rows.delete <- c( "EN__saving",
                    "EN__general",
                    #  "tech_storage",
                    #  "tech_fuel_cells",
                    "PPtechnology",
                    "carrier_uns",
                    "energy_heat_cold", 
                     "EN__general",
                    "EN__saving", 
                     "carrier_oil_gas",
                   #  "mobility_biofuels",
                   # "mobility_hydrogen",
                   # "mobility_plane",
                   # "conversion_nucl",
                    "renewable_other_than_bike"
                  
)

columns.delete <- c( "SUST__",
                     "LEV__fed",
                     "LEV__onshore",
                     "LEV__offshore",
                     "IMWW__mobility",
                     "climate_protection_tools"
                     
)


rows.delete <- unlist(sapply(rows.delete, 
                             function(d) grep(d, row.names(x))))

if (length(rows.delete) > 0) {
    x <- x[-rows.delete,]
}


columns.delete <- unlist(sapply(columns.delete, 
                             function(d) grep(d, colnames(x))))

if (length(columns.delete) > 0) {
  x <- x[,-columns.delete]
}

#select only energy chain and exclude certain columns/rows--------
#select only the  categories with at least one L3 to energy chain


columns.maxima <- apply(x, 2, function(item) length(which(item >=0.625)))
columns.maxima <- which(columns.maxima >1)

x <- x[,columns.maxima]

# #columns.average.maxima <- 
# columns.maxima <- apply(x, 2, sum)
# 
# #which rows achieved the medium possible value in at least half of the cases
# half.of.maximum.possible.sum <- nrow(x)*(1/2)/2
# 
# 
# columns.maxima <- which(columns.maxima >= half.of.maximum.possible.sum)
# 
# x <- x[,columns.maxima]  
  
  
# rows.maxima <- apply(x, 1, max)
# rows.maxima <- which(rows.maxima >=0.625)

#x <- x[,columns.maxima]


writeLines(noquote(colnames(x)), "systems_with_at_least_one_L3_with_energy_chain.txt")

##END-----------scale by diagional values-----------------

cooccurrence.case <- t(round(x, d=3))

##START renumber rownames and define the order of levels-----------------
categories <- colnames(cooccurrence.case)
main.categories <- c("resources", 
                     #"carrier", 
                     "conversion",
                     "sector$",
                     "distribution",
                     "sales",
                     "mobility", 
                     "tech"
                     )


##START order categories according to order of main categories and create the combined main superior categories---------------
order.categories <- NULL
for(i in seq(length(main.categories))) {
  
  order.categories <- c(order.categories, grep(main.categories[i], categories))
  
}
order.categories <- c(order.categories, setdiff(1:length(categories), order.categories))

cooccurrence.case <- cooccurrence.case[,rev(order.categories)]

#END renumber rownames and define the order of levels-----------------


category.1 <- rownames(cooccurrence.case)
cooccurrence.case <- cbind.data.frame(category.1, cooccurrence.case)
row.names(cooccurrence.case) <- NULL

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

cooccurrence.levels <- c(0.000, seq(1000)/1000)

plot.results$cooccurrence <- factor(plot.results$cooccurrence, levels = cooccurrence.levels[order(cooccurrence.levels)])
plot.results$category.1 <- factor(plot.results$category.1, levels = unique(as.character(plot.results$category.1)))
plot.results$category.2 <- factor(plot.results$category.2, levels = unique(as.character(plot.results$category.2)))


#position of lines to be drawn between systems
system.levels <- gsub("__.*", "", unique(as.character(plot.results$category.1)))
system.levels.unique <- unique(system.levels)
line.position.v <- c("")
for (i in seq(length(system.levels.unique))) {
  
  line.position.v <- c(line.position.v, max(grep(system.levels.unique[i],system.levels)))
  
}
line.position.v <- as.numeric(line.position.v[-c(1)])


line.position <- c("")
for (i in seq(length(main.categories))) {
  
  line.position <- c(line.position, max(grep(main.categories[i],as.character(unique(plot.results$category.2)))))
  
}
line.position <- as.numeric(line.position[-c(1)])
##END - set all potential levels for plotting--------------------------



##START plot basic mean value-------------------------------
axis.label.size <- 7

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
    axis.text.x = element_text(angle = 270, hjust = 0, vjust=.8, size=axis.label.size),
    axis.text.y = element_text(vjust = .8, size=axis.label.size),
    
    #remove axis title
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    
    axis.ticks = element_line(size=.01),
    
    axis.ticks.length = unit(.1, "cm"),
    
    plot.background = element_rect(fill = NULL,colour = NA),
    
    # hide legend
    legend.position = "none"#,
    
  ) +
  
  coord_fixed(ratio = 1) +
  
  

geom_vline(xintercept = line.position.v, colour = "black", linetype= "dashed", size=.01) +
geom_hline(yintercept = line.position, colour = "black", linetype= "dashed", size=.01)# +



##<<<<<<<<---Export - Save as Metafile
wd <- getwd()
setwd(wd.final)
ggsave(file="cooccurrence_mean__main_links_energy_socsubsyst_scaled_by_min_diag.emf")
setwd(wd)
##END plot basic mean value-------------------------------