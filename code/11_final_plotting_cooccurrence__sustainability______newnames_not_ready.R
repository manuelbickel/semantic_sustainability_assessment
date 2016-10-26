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
cooccurrence.case.reference <- grep("Lower_Saxony_regional_centers", files)

#exclude single files which are included for expermental testing, etc.
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

order.interim <- data.frame(category = as.character(row.names(cooccurrence.case)),
                            value = as.numeric(rowSums(cooccurrence.case)) )

#row.names(order.interim) <- category = as.character(row.names(cooccurrence.case)),

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


#START - format numeric part of matrix

x <- cooccurrence.case


x  <- round(x, d=3)


oc_lower_1p_all <- which(as.numeric(diag(x)) < 0.01)
oc_lower_1p_all_names <- rownames(x)[oc_lower_1p_all]
#exclude sustainability categories
keep <- grep("<3><SUS>|biofuels|storage|renewable|car_sharing|bike_pedestrian|energetic_building_refurbishment", rownames(x))
oc_lower_1p <- setdiff(oc_lower_1p_all, keep)

x <- x[-oc_lower_1p,-oc_lower_1p]


##START----------scale x by system occurrence--------------------
x.diag <- as.matrix(diag(x))

x <- apply(x, 2, function(column) column/as.numeric(x.diag))

##END----------scale x by diagonal--------------------


classes <- c(0,0.25,0.5,0.75,1)
x <- ifelse(is.na(x), 0, x)
x <- ifelse(x <= classes[1], 0, x)
x <- ifelse((x > classes[1] & x < classes[2]), 0.05, x)
x <- ifelse((x >= classes[2] & x < classes[3]), 0.375,x)
x <- ifelse((x >= classes[3] & x < classes[4]), 0.625,x)
#x <- ifelse((x >= classes[4] & x < classes[5]), 0.7,x)
x <- ifelse((x >= classes[4]), 0.875,x)

#select only energy chain and exclude certain columns/rows ---------
#x <- x[grep("^SUST__", row.names(x)),grep("^SUST__", colnames(x))]
#x <- interim

x <- x[grep("<2>|<economy>|<industry>|<commerce>|<mobility_sector>|<local_administration_bodies>|<infrastructure>|<Food>|<Residents>", rownames(x), ignore.case = T), 
       grep("<3>|biofuels|renewable|pedestrian|insulation|car_sharing|public_transport|storage", colnames(x),ignore.case=T, perl=T)]



x <- x[-unlist(lapply(oc_lower_1p_all_names, function(item) grep(item, rownames(x)))), ]

x <- x[,-grep("<sustainability>|<Uncertainty>", colnames(x), ignore.case = T)]



colnames(x) <- gsub("<2><ENG><Resources><renewable>" , 
                    "<3><SUS><Consistency><Resources><renewable>",
                    colnames(x))

colnames(x) <- gsub("<2><ENG><Conversion><renewable>", 
                    "<3><SUS><Consistency><Conversion><renewable>",
                    colnames(x))

colnames(x) <- gsub("<2><ENG><Sales_Contracts><renewable>" , 
                    "<3><SUS><Consistency><Sales_Contracts><renewable>",
                    colnames(x))

colnames(x) <- gsub("<2><ENG><End_Use><mobility><biofuels>", 
                    "<3><SUS><Consistency><End_Use><mobility><biofuels>",
                    colnames(x))

colnames(x) <- gsub("<2><ENG><Technology_Option><storage>", 
                    "<3><SUS><Efficiency><Technology_Option><storage>",
                    colnames(x))

colnames(x) <- gsub("<2><ENG><End_Use><building><insulation>" , 
                    "<3><SUS><Efficiency><End_Use><building><insulation>",
                    colnames(x))

colnames(x) <- gsub("<2><ENG><End_Use><mobility><car_sharing>" , 
                    "<3><SUS><Sufficiency><End_Use><mobility><car_sharing>",
                    colnames(x))


colnames(x) <- gsub("<2><ENG><End_Use><mobility><bike_pedestrian>", 
                    "<3><SUS><Sufficiency><End_Use><mobility><bike_pedestrian>",
                    colnames(x))

colnames(x) <- gsub("<2><ENG><End_Use><mobility><public_transport>", 
                    "<3><SUS><Sufficiency><End_Use><mobility><public_transport>",
                    colnames(x))




rows.delete <- c( 
  "<carrier>",
  "<rental_of_houses>",
  "<social_groups>",
  "<general_reference><saving>",
  "<building_parts_materials>",
  "<Energy><unspecific_reference>",
  "<Energy_Form><unspecific_reference>",
  "<Economy><commerce><sector_unspecific>",
  "<Infrastructure><disposal_unspecific>",
  "<detailed_PPtechnology>",
  "<alternative_wo_bike>",
  "<tariffs_standing_orders>",
  "<Economy><general_reference><employment_workplace>",
  "<Infrastructure><unspecific>",
  "<Infrastructure><supply_unspecific>",
  "<Economy><general_reference><economic_viability>",
  "<Economy><service><personal_services_crafting>"
  
)


 
rows.delete <- unlist(sapply(rows.delete, 
                             function(d) grep(d, row.names(x), ignore.case = T)))

if (length(rows.delete) > 0) {
  x <- x[-rows.delete,]
}

##END-----------scale by diagional values-----------------


##START renumber rownames and define the order of levels-----------------

main.categories <- c("<resources><unsp", 
                     "<resources>(?!<unsp)",
                     "<conversion><unsp",
                     "<conversion>(?!<unsp)",
                     "<distribution><unsp", 
                     "<distribution>(?!<unsp)", 
                     "<sales_contracts><unsp",
                     "<sales_contracts>(?!<unsp)",
                     "<Technology_Option>",
                     "<Energy_Form>", #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                     "<end_use><consumption>",
                     "<mobility>(?!<frei)",
                     "<mobility><frei",
                     "<building><unsp",
                     "<building>(?!<unsp)",
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



main.categories.order <- unlist(lapply(main.categories, function(item) {
  
  grep(item, rownames(x), perl=T, ignore.case = T)
  
  
}))



main.categories <- c("<resources><unsp", 
                     "<resources>(?!<unsp)",
                     "<conversion><unsp",
                     "<conversion>(?!<unsp)",
                     "<distribution><unsp", 
                     "<distribution>(?!<unsp)", 
                     "<sales_contracts><unsp",
                     "<sales_contracts>(?!<unsp)",
                     "<Technology_Option>",
                     "<Energy_Form>", #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                     "<end_use><consumption>",
                     "<mobility>(?!<frei)",
                     "<mobility><frei",
                     "<building><unsp",
                     "<building>(?!<unsp)",
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


sust.categories <- c("<Sufficiency", "<Efficiency", "<Consistency")

sust.categories.order <- unlist(lapply(sust.categories, function(item) {
  
  grep(item, colnames(x), perl=T, ignore.case = T)
  
  
}))


x <- x[rev(main.categories.order),sust.categories.order]


row.names(x) <- gsub("<1><SOC>","",row.names(x))
row.names(x) <- gsub("<2><ENG>","",row.names(x))

#colnames(x) <- gsub("(<3><SUS><[\\w]+>#)(.*$)","\\2", colnames(x), perl=T)

colnames(x) <- gsub("<3><SUS>","", colnames(x))


##START write categories into file----------------------------

categories.SUS <- colnames(x)
categories.SUS <- gsub("(^<[\\d]>)(<[\\w]{3}>)(.*$)", "\\3", categories.SUS, perl=T)
categories.SUS <- gsub("(^<[\\w]+>)(<.*$)", "\\1~#~\\2", categories.SUS, perl=T)

delimiter <- "~#~"
categories.SUS <- do.call(rbind,strsplit(categories.SUS, delimiter, perl=T))

#----------------------------------activate if names of categories have been changed
wd <- getwd()
setwd(wd.final)

write.csv(categories.SUS, paste("categories_SUS_nMeta_",
                                length(unique(categories.SUS[,1])),
                                "_nCat_",
                                length(categories.SUS[,2]),
                                ".csv"))

setwd(wd)
##START write categories into file----------------------------





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
                                 value.name = "degree_of_representation_scaling_option_2",
                                 na.rm = FALSE)
##END - convert from wide to long format----------------------


##START decrease number of levels by building classes------------------
plot.results <- cooccurrence.case.melted

cooccurrence.levels <- c(0.000, seq(1000)/1000)

plot.results$degree_of_representation_scaling_option_2 <- factor(plot.results$degree_of_representation_scaling_option_2, levels = cooccurrence.levels[order(cooccurrence.levels)])

plot.results$category.1 <- factor(plot.results$category.1, levels = unique(as.character(plot.results$category.1)))
plot.results$category.2 <- factor(plot.results$category.2,  levels = unique(as.character(plot.results$category.2)))
                          

#position of lines to be drawn between systems
sust.levels <- unique(as.character(plot.results$category.1)) 
sust.levels.unique <- c("sufficiency", "efficiency", "consistency")
line.position.v <- c("")
for (i in seq(length(sust.levels.unique))) {
  
  line.position.v <- c(line.position.v, max(grep(sust.levels.unique[i],sust.levels, ignore.case = T)))
  
}
line.position.v <- as.numeric(line.position.v[-c(1)])


main.categories.reduced <- c(                 "resources",
                                              "<conversion>", 
                                              "<distribution>", 
                                              "<sales_contracts>",
                                              "<Technology_Option>",
                                              "<Energy_Form>", 
                                              "<end_use><consumption>",
                                              "<mobility>",
                                              "<building>",
                                              "<electric_application>",
                                              "<local_administration_bodies>",
                                              "<Mobility_Sector>",
                                              "<Infrastructure>",
                                          
                                              "<Residents>",
                                              "<Food>",
                                              "<Economy>"
                                              
)





line.position <- unlist(lapply(main.categories.reduced, function(item) {
  
  min(grep(item, as.character(unique(plot.results$category.2)), perl=T, ignore.case = T))
  
  
}))
line.position<- line.position-1



##END - set all potential levels for plotting--------------------------


energy_categories <- as.character(plot.results$category.2)
sust_categories <- as.character(plot.results$category.1)

sust_categories <-   gsub("^<sufficiency>|^<consistency>|^<efficiency>,", "",sust_categories, ignore.case=T)


#get categories which co-occur with themselves in the graph to mark
#the respective positions with a cross

self_cooc <- which(sust_categories == energy_categories)
#plot.results[self_cooc,] 
#head(plot.results)
plot.results[self_cooc,"degree_of_representation_scaling_option_2"] <- NA

na <- rep(FALSE, nrow(plot.results))
plot.results <- cbind(plot.results, na)
plot.results[self_cooc,"na"] <- TRUE


self_cooc_categories <- plot.results[self_cooc,c(1,2)]

#full list of categories of x and y axis with numbering
cat_y <-   cbind(unique(as.character(plot.results$category.2)),
                 1:length( unique(as.character(plot.results$category.2))))

cat_x <-    cbind(unique(as.character(plot.results$category.1)), 
                  1:length(unique(as.character(plot.results$category.1))))


#leave only those categories which match with the self cooccurring categories
cat_x <- cat_x[!is.na(match(cat_x[,1], self_cooc_categories[,1])), ]
cat_y <- cat_y[!is.na(match(cat_y[,1], self_cooc_categories[,2])), ]

cat_x[,1] <-   gsub("^<sufficiency>|^<consistency>|^<efficiency>,", "",cat_x[,1], ignore.case=T)



order_y <- unlist(lapply(cat_x[,1], function(item) {
  
  grep(item, cat_y[,1])
  
}))

cat_y <- cat_y[order_y,]


na_positions <- as.data.frame(cbind(
  as.integer(cat_x[,2])-0.5,
  as.integer(cat_y[,2])-0.5
))




##START plot basic mean value-------------------------------
axis.label.size <- 8

p_sust <- ggplot(NULL) +
  
  #raster with no distance to the axis
  geom_raster(data= plot.results, aes(x = category.1, y = category.2, fill = degree_of_representation_scaling_option_2),hjust = 0, vjust = 0) +  
  
  #making sure that none of the categories for x/y axis are dropped if they have no entries
  #this step is not necessary but kept as a safety net
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(drop = FALSE) +
  
  #set colour scale for displaying values
  #scale_fill_grey(start = 1, end = 0, na.value = "white") +   #the following alternative creates a coloured plot, probably the setting of scale is wrong,yet:scale_colour_grey(start = 1, end = 0, na.value = "red") +
  #scale_fill_hue(l=50, c=100, h=c(0, 240)) +
  scale_fill_manual(values = c("white", "grey95", "grey75", "grey30", "black"),
                    labels = c("0", "S1","S2","S3","S4 (self-reference: white cross)"),
                    na.value="black")  +
  
  #remove labels
  labs(x = element_blank(), y=element_blank()) +
  
  #set basic theme of the plot
  theme_bw() +
  
  theme(
  
    #  #rotate x-axis label, and set distance of label/axis to zero
    axis.text.x = element_text(angle = 90, hjust = 0, vjust=-0.25, size=axis.label.size),
    axis.text.y = element_text(vjust = .8, size=axis.label.size, hjust=0),
    
    #remove axis title
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    
    axis.ticks = element_line(size=.01),
    
    axis.ticks.length = unit(.1, "cm"),
    
    plot.background = element_rect(fill = NULL,colour = NA),
    
    plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),
    
    #length(unique(as.character(plot.results$category.2)))/length(unique(as.character(plot.results$category.1)))
    aspect.ratio = 2.737,
    
    legend.position = c(-0.6,-0.1),
    legend.key.size = unit(0.25, "cm"),
    legend.title = element_text(size=axis.label.size),
    legend.direction = "vertical",
    legend.background = element_rect(colour = "grey30"),
    legend.key = element_rect(colour = "grey30"),
    legend.text = element_text(size=axis.label.size)
    
  ) +
  
 # coord_fixed(ratio = 1) +
 
  
  geom_vline(xintercept = line.position.v, colour = "black", linetype= "dashed", size=.01) +
  geom_hline(yintercept = line.position, colour = "black", linetype= "dashed", size=.01) +

  geom_point(data = na_positions, aes(x=V1, y=V2, shape=4), color="white", size=4) +
  scale_shape_identity()

p_sust

##<<<<<<<<---Export - Save as Metafile
#par(mar=c(0.2,0.2,0.2,0.2))
##<<<<<<<<---Export - Save as Metafile

##<<<<<<<<---Export - Save as Metafile
wd.sub <- "M:\\Science\\Promotion\\Working_Documents\\articles_texts\\1_article_interpretation_network_analysis_for_sustainability_analysis\\submission_1"
setwd(wd.sub)


# pdf("Figure_3_CoOc_Energy_and_Sustainability_scalingOpt2.pdf")
# p_sust
# dev.off()

win.metafile("Figure_3_CoOc_Energy_and_Sustainability_scalingOpt2.emf", height=10, width=6)
p_sust
dev.off()


postscript("Figure_3_CoOc_Energy_and_Sustainability_scalingOpt2.eps", height=10, width=6)
p_sust
dev.off()



