#COOCCURRENCE IN WIDE FORMAT
library("ggplot2")
library("reshape2")
#library("plyr")
library("grid")



#START - directories
wd.main <- c("M:/Science/Promotion/R_CPC_evaluation_of_measures/")
#wd.main <- c("M:/Science/Promotion/R_CPC_evaluation_of_measures2_full_search/")


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
cooccurrence.case.reference <- grep("Lower_Saxony_regional_centers",
                                    files)

#exclude single files which are included for experimental testing, etc.
exclude.cases <- c("Hannover96",
                   "Wilhelmshaven__CPC1_main_short"
)

exclude.cases <- grep(paste(exclude.cases, collapse="|"), files)

if (length(exclude.cases) > 0) {
  files <- files[-exclude.cases]
}


#####--------------Plots for Lower Saxony, mean--------------------------------------

cooccurrence.case <- read.csv(files[cooccurrence.case.reference], header=TRUE)

row.names(cooccurrence.case) <- cooccurrence.case[,1]
cooccurrence.case <- as.matrix(cooccurrence.case[,-c(1)])


if (identical(colnames(cooccurrence.case), row.names(cooccurrence.case)) == FALSE) {
  warning("Matrix not symmetric. Following calculations will produce wrong results.")
}

##START - correct wrong spelling and simplify category names-------------------
colnames(cooccurrence.case) <- gsub("c[[:digit:]]+__", "", colnames(cooccurrence.case))

row.names(cooccurrence.case) <- colnames(cooccurrence.case)


wd <- getwd()
setwd(wd.final)
names.replace <- read.csv("categories_replace_names.csv", sep=";")
setwd(wd)

rownames(cooccurrence.case) <- gsub("c[[:digit:]]+__", "",rownames(cooccurrence.case) )

names.replace["new2"] <- sapply(names.replace["new2"], function(item) {
  
  item <- gsub("(^)", "<\\1", item, perl=T)
  item <- gsub("($)", "\\1>", item, perl=T)
  item <- gsub("#", "><", item, fixed=T)
  
  
})

#cooccurrence.case <- interim
for (i in 1:nrow(names.replace)) {
  
  rownames(cooccurrence.case) <- gsub(paste("^",as.character(names.replace[i,"old"]),"$", sep=""),
                                      as.character(names.replace[i,"new2"]),
                                      rownames(cooccurrence.case))
}


colnames(cooccurrence.case) <- row.names(cooccurrence.case)
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

# order.interim <- data.frame(category = as.character(row.names(cooccurrence.case)),
#                             value = as.numeric(rowSums(cooccurrence.case)) )
# 
# #row.names(order.interim) <- category = as.character(row.names(cooccurrence.case)),
# 
# #aggregate values by meta-category
# order.interim.agg <- order.interim
# 
# order.interim.agg[,1] <-  gsub("(<[\\d]>)(<[\\w]{3}>)(<[\\w]+>)(.*$)", "\\3",order.interim.agg[,1] , perl=T)
# order.interim.agg <- aggregate(order.interim.agg[,2], by=list(category=order.interim.agg$category), FUN=mean)
# 
# #replace the original values for each category 
# #by the aggregated sum of its meta category in the interim list
# for (c in 1:nrow(order.interim.agg)) {
#   replace <- order.interim.agg[c,]
#   order.interim[grep(replace[,1], order.interim[,1]),2] <- replace[,2]
#   
# }
# 
# order.cluster.occ <- order(order.interim[,2], decreasing=T)
# 
# cooccurrence.case <- cooccurrence.case[order.cluster.occ ,order.cluster.occ]


#START - format numeric part of matrix

x <- cooccurrence.case



x  <- round(x, d=3)

delete <- grep("<Sustainability><unspecific_reference>|Uncertainty><Uncertainty_Risks_Accidents>|<Spatial_scale><fed_state><ger>", colnames(x))


x <- x[-delete,-delete]


classes <- c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)

x <- ifelse(is.na(x), 0, x)
x <- ifelse(x <= classes[1], 0, x)
x <- ifelse((x > classes[1] & x < classes[2]), 0.05, x)
x <- ifelse((x >= classes[2] & x < classes[3]), 0.15,x)
x <- ifelse((x >= classes[3] & x < classes[4]), 0.25,x)
x <- ifelse((x >= classes[4] & x < classes[5]), 0.35,x)
x <- ifelse((x >= classes[5] & x < classes[6]), 0.45,x)
x <- ifelse((x >= classes[6] & x < classes[7]), 0.55,x)
x <- ifelse((x >= classes[7] & x < classes[8]), 0.65,x)
x <- ifelse((x >= classes[8] & x < classes[9]), 0.75,x)
x <- ifelse((x >= classes[9] & x < classes[10]), 0.85,x)
x <- ifelse((x >= classes[10] & x < classes[11]), 0.95,x)
x <- ifelse((x >= classes[11]), 1, x)

cooccurrence.case <- t(round(x, d=3))


##START - convert from wide to long format----------------------
category.1 <- rownames(cooccurrence.case)
cooccurrence.case <- cbind.data.frame(category.1, cooccurrence.case)
row.names(cooccurrence.case) <- NULL

cooccurrence.case.melted <- melt(cooccurrence.case, id.vars=c("category.1"),
                                 #source columns
                                 measure.vars= colnames(cooccurrence.case)[2:ncol(cooccurrence.case)],
                                 
                                 #name of the destination column
                                 variable.name = "category.2", 
                                 value.name = "average_normalized_cooccurrence_of_categories",
                                 na.rm = FALSE)
##END - convert from wide to long format----------------------


##START decrease number of levels by building classes------------------
plot.results <- cooccurrence.case.melted

cooccurrence.levels <- c(0.000, seq(1000)/1000)


plot.results$average_normalized_cooccurrence_of_categories <- factor(plot.results$average_normalized_cooccurrence_of_categories, levels = cooccurrence.levels[order(cooccurrence.levels)])

plot.results$category.1 <- factor(plot.results$category.1, levels = unique(as.character(plot.results$category.1)))
plot.results$category.2 <- factor(plot.results$category.2,  levels = unique(as.character(plot.results$category.2)))





##START plot basic mean value-------------------------------
axis.label.size <- 3

p_scaled_cooc <- ggplot(NULL) +
  
  #raster with no distance to the axis
  geom_raster(data= plot.results, aes(x = category.1, y = category.2, fill = average_normalized_cooccurrence_of_categories), 
              hjust = 0, vjust = 0) +  
  
  #making sure that none of the categories for x/y axis are dropped if they have no entries
  #this step is not necessary but kept as a safety net
  # scale_x_discrete(expand = c(0,0), drop = FALSE) +
  # scale_y_discrete(expand = c(0,0), drop = FALSE) +
  
  
  xlab("")+
  ylab("")+
  
  #set colour scale for displaying values

  #scale_fill_grey( start = 1, end=0, na.value = "white") +   #the following alternative creates a coloured plot, probably the setting of scale is wrong,yet:scale_colour_grey(start = 1, end = 0, na.value = "red") +
 # scale_fill_hue(l=50, c=100, h=c(0, 240)) +
   scale_fill_manual(values = c("white", 
                                "grey95",
                                "grey85",
                                "grey75", 
                                
                                "grey65",
                                "grey55",
                                "grey45", 
                               
                                "grey35",
                                "grey25",
                                "grey15", 
                                "grey5",
                                "black"
                                  ),
                     labels = c("0",
                                "[0;0.1)",
                                "[0.1;0.2)",
                                "[0.2;0.3)",
                                "[0.3;0.4)",
                                "[0.4;0.5)",
                                "[0.5;0.6)",
                                "[0.6;0.7)",
                                "[0.7;0.8)",
                                "[0.8;0.9)",
                                "[0.9;1)",
                                "1"),
                     na.value="transparent") +
 
  #remove labels
  labs(x = element_blank(), y=element_blank()) +
  
  #set basic theme of the plot
  theme_bw() +
  
  theme(
    #  text = element_text(size=3),
    #  #rotate x-axis label, and set distance of label/axis to zero
    axis.text.x = element_text(angle = 90, hjust = 0, vjust=-0.25, size=axis.label.size),
    axis.text.y = element_text(vjust = .8, size=axis.label.size, hjust = 0),
    
    #remove axis title
    
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    
    
    axis.ticks = element_line(size=.01),
    
    axis.ticks.length = unit(.1, "cm"),
    
    #  plot.background = element_rect(fill = NULL,colour = NA),
    plot.background = element_blank(),
    
    
    plot.title = element_blank(),
    
    
    aspect.ratio = round(length(unique(as.character(plot.results$category.2)))/length(unique(as.character(plot.results$category.1)))),
    
    plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),
    
    legend.position = c(-0.1,-0.1),
    legend.key.size = unit(0.1, "cm"),
    legend.title = element_text(size=axis.label.size),
   # legend.direction = "vertical",
    #legend.background = element_rect(colour = "grey30"),
    #legend.key = element_rect(colour = "grey30"),
    legend.text = element_text(size=axis.label.size)
    
    
  ) +
  
   coord_fixed(ratio=1)# +
  
#  geom_vline(xintercept = line.position.v, colour = "black", linetype= "dashed", size=.01) +
 # geom_hline(yintercept = line.position, colour = "black", linetype= "dashed", size=.01) +
  
  
 # geom_point(data = na_positions, aes(x=V1, y=V2, shape=4), color="white", size=4) +
#  scale_shape_identity()



p_scaled_cooc 



##<<<<<<<<---Export - Save as Metafile
opar <- par()

par(mar=c(0.1,0.1,0.1,0.1))
##<<<<<<<<---Export - Save as Metafile

##<<<<<<<<---Export - Save as Metafile
wd.sub <- "M:\\Science\\Promotion\\Working_Documents\\articles_texts\\1_article_interpretation_network_analysis_for_sustainability_analysis\\submission_1"
setwd(wd.sub)



pdf("full_cooc.pdf")
p_scaled_cooc
dev.off()



scale_figure <- c(10,6.8)

win.metafile("Figure_2_CoOc_L3_to_Energy_system_and_average_CoOc_0125_scalingOpt1.emf", height=scale_figure[1] ,width=scale_figure[2])
p_scaled_cooc
dev.off()


postscript("Figure_2_CoOc_L3_to_Energy_system_and_average_CoOc_0125_scalingOpt1.eps", height=scale_figure[1] ,width=scale_figure[2])
p_scaled_cooc
dev.off()

par(opar)





#---------------------------------------plot coocurrence wo scaling etc.....---------------------------


x_full  <- round(x_full, d=3)

x_full <- x_full[order(row.names(x_full)), order(colnames(x_full))]

#x_full <- ifelse(is.na(x_full), 0, x_full)

# classes <- c(0,0.25,0.5,0.75,1)
# x_full <- ifelse(is.na(x_full), 0, x_full)
# x_full <- ifelse(x_full <= classes[1], 0, x_full)
# x_full <- ifelse((x_full > classes[1] & x_full < classes[2]), 0.05, x_full)
# x_full <- ifelse((x_full >= classes[2] & x_full < classes[3]), 0.375,x_full)
# x_full <- ifelse((x_full >= classes[3] & x_full < classes[4]), 0.625,x_full)
# #x_full <- ifelse((x_full >= classes[4] & x_full < classes[5]), 0.7,x_full)
# x_full <- ifelse((x_full >= classes[4]), 0.875,x_full)

# 
# main.categories.order <- unlist(lapply(main.categories, function(item) {
#   
#   grep(item, rownames(x_full), perl=T, ignore.case = T)
#   
#   
# }))


#x_full <- x_full[rev(main.categories.order),order(colnames(x_full))]


#x_full <- t(round(x_full, d=3))

##START - convert from wide to long format----------------------
cooccurrence.case.full <- x_full

category.1 <- rownames(cooccurrence.case.full)
cooccurrence.case.full <- cbind.data.frame(category.1, cooccurrence.case.full)
row.names(cooccurrence.case.full) <- NULL

cooccurrence.case.full.melted <- melt(cooccurrence.case.full, id.vars=c("category.1"),
                                      #source columns
                                      measure.vars= colnames(cooccurrence.case.full)[2:ncol(cooccurrence.case.full)],
                                      
                                      #name of the destination column
                                      variable.name = "category.2", 
                                      value.name = "cooccurrence",
                                      na.rm = FALSE)
##END - convert from wide to long format----------------------


##START decrease number of levels by building classes------------------
plot.results <- cooccurrence.case.full.melted

cooccurrence.levels <- c(0.000, seq(1000)/1000)

plot.results$average_normalized_cooccurrence_of_categories <- factor(plot.results$average_normalized_cooccurrence_of_categories, levels = cooccurrence.levels[order(cooccurrence.levels)])

plot.results$category.1 <- factor(plot.results$category.1, levels = unique(as.character(plot.results$category.1)))
plot.results$category.2 <- factor(plot.results$category.2,  levels = unique(as.character(plot.results$category.2)))

##START plot basic mean value-------------------------------

axis.label.size <- 3

p_full <- ggplot(NULL) +
  
  #raster with no distance to the axis
  geom_raster(data= plot.results, aes(x = category.1, y = category.2, fill = cooccurrence), hjust = 0, vjust = 0) +  
  
  #making sure that none of the categories for x/y axis are dropped if they have no entries
  #this step is not necessary but kept as a safety net
  # scale_x_discrete(expand = c(0,0), drop = FALSE) +
  # scale_y_discrete(expand = c(0,0), drop = FALSE) +
  
  
  xlab("")+
  ylab("")+
  
  #set colour scale for displaying values
  scale_fill_grey(start = 1, end = 0, na.value = "white") +   #the following alternative creates a coloured plot, probably the setting of scale is wrong,yet:scale_colour_grey(start = 1, end = 0, na.value = "red") +
  #scale_fill_hue(l=50, c=100, h=c(0, 240)) +
  # scale_fill_manual(values = c("white", "grey95", "grey75", "grey30", "black")) +
  
  #remove labels
  labs(x = element_blank(), y=element_blank()) +
  
  #set basic theme of the plot
  theme_bw() +
  
  theme(
    axis.text.x = element_text(angle = 90, hjust = 0, vjust=-0.25, size=axis.label.size),
    # 
    axis.text.y = element_text(vjust = .8, hjust = 0, size=axis.label.size),
    
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    
    axis.ticks = element_line(size=.01),
    
    axis.ticks.length = unit(.1, "cm"),
    
    plot.background = element_rect(fill = NULL,colour = NA),
    plot.background = element_blank(),
    
    plot.title = element_blank(),
    
    aspect.ratio = 1,
    
    plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),
    legend.position = "none"#,
    
    
  ) +
  
  geom_vline(xintercept = 1:ncol(x_full), colour = "black", linetype= "dotted", size=.1) +
  geom_hline(yintercept = 1:nrow(x_full), colour = "black", linetype= "dotted", size=.1)


p_full


##<<<<<<<<---Export - Save as Metafile

par(mar=c(0.2,0.2,0.2,0.2))
##<<<<<<<<---Export - Save as Metafile

##<<<<<<<<---Export - Save as Metafile
wd.sub <- "M:\\Science\\Promotion\\Working_Documents\\articles_texts\\1_article_interpretation_network_analysis_for_sustainability_analysis\\submission_1"
setwd(wd.sub)


pdf("ESM_full_cooc_mat_scaled_by_total_max_cooc.pdf")
p_full
dev.off()

win.metafile("ESM_full_cooc_mat_scaled_by_total_max_cooc.emf")
p_full
dev.off()


postscript("ESM_full_cooc_mat_scaled_by_total_max_cooc.eps")
p_full
dev.off()






