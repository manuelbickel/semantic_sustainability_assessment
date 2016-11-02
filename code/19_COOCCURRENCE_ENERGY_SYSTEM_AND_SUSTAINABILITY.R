##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##START <<<<<<<<<<<<< COOCCURRENCE PLOT ENERGY / SUSTAINABILITY
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

wd.current <- getwd()

##+EXCLUDE CATEGORIES WITH VERY LOW OCCURRENCE AND APPLY SCALING TO MATRIX-----------
oc_lower_1p_all <- which(as.numeric(diag(x_sust)) < 0.01)
oc_lower_1p_all_names <- rownames(x_sust)[oc_lower_1p_all]
keep <- grep("<3><SUS>|biofuels|storage|renewable|car_sharing|bike_pedestrian|energetic_building_refurbishment", rownames(x_sust))
oc_lower_1p <- setdiff(oc_lower_1p_all, keep)
x_sust <- x_sust[-oc_lower_1p,-oc_lower_1p]


# scale by relative occurrence of sustainability categories in sets of other categories
x_sust.diag <- as.numeric(diag(x_sust))
x_sust <- apply(x_sust, 2, function(column) column/as.numeric(x_sust.diag))
##~------------------------------

##+ CLASSIFY--------------------
classes <- c(0,0.25,0.5,0.75,1)
x_sust <- ifelse(is.na(x_sust), 0, x_sust)
x_sust <- ifelse(x_sust <= classes[1], 0, x_sust)
x_sust <- ifelse((x_sust > classes[1] & x_sust < classes[2]), 0.05, x_sust)
x_sust <- ifelse((x_sust >= classes[2] & x_sust < classes[3]), 0.375,x_sust)
x_sust <- ifelse((x_sust >= classes[3] & x_sust < classes[4]), 0.625,x_sust)
#x_sust <- ifelse((x_sust >= classes[4] & x_sust < classes[5]), 0.7,x_sust)
x_sust <- ifelse((x_sust >= classes[4]), 0.875,x_sust)
##~--------------------

##+ SELECT AND RENAME CATEGORIES------------------------
x_sust <- x_sust[grep("<2>|<economy>|<industry>|<commerce>|<mobility_sector>|<local_administration_bodies>|<infrastructure>|<Food>|<Residents>", rownames(x_sust), ignore.case = T), 
                 grep("<3>|biofuels|renewable|pedestrian|insulation|car_sharing|public_transport|storage", colnames(x_sust),ignore.case=T, perl=T)]



x_sust <- x_sust[-unlist(lapply(oc_lower_1p_all_names, function(item) grep(item, rownames(x_sust)))), ]

x_sust <- x_sust[,-grep("<sustainability>|<Uncertainty>", colnames(x_sust), ignore.case = T)]

#exclude categories from y-axis
rows.exclude <- grep("<carrier>|<rental_of_houses>|<social_groups>|<general_reference><saving>|<building_parts_materials>|<Energy><unspecific_reference>|<Energy_Form><unspecific_reference>|<Economy><commerce><sector_unspecific>|<Infrastructure><disposal_unspecific>|<detailed_PPtechnology>|<alternative_wo_bike>|<tariffs_standing_orders>|<Economy><general_reference><employment_workplace>|<Infrastructure><unspecific>|<Infrastructure><supply_unspecific>|<Economy><general_reference><economic_viability>|<Economy><service><personal_services_crafting>", rownames(x), ignore.case = T)
if (length(rows.exclude) > 0) {
  x <- x[-rows.exclude,]
}

if (length(rows.exclude) > 0) {
  x_sust <- x_sust[-rows.exclude,]
}

colnames(x_sust) <- gsub("<2><ENG><Resources><renewable>" , 
                         "<3><SUS><Consistency><Resources><renewable>",
                         colnames(x_sust))

colnames(x_sust) <- gsub("<2><ENG><Conversion><renewable>", 
                         "<3><SUS><Consistency><Conversion><renewable>",
                         colnames(x_sust))

colnames(x_sust) <- gsub("<2><ENG><Sales_Contracts><renewable>" , 
                         "<3><SUS><Consistency><Sales_Contracts><renewable>",
                         colnames(x_sust))

colnames(x_sust) <- gsub("<2><ENG><End_Use><mobility><biofuels>", 
                         "<3><SUS><Consistency><End_Use><mobility><biofuels>",
                         colnames(x_sust))

colnames(x_sust) <- gsub("<2><ENG><Technology_Option><storage>", 
                         "<3><SUS><Efficiency><Technology_Option><storage>",
                         colnames(x_sust))

colnames(x_sust) <- gsub("<2><ENG><End_Use><building><insulation>" , 
                         "<3><SUS><Efficiency><End_Use><building><insulation>",
                         colnames(x_sust))

colnames(x_sust) <- gsub("<2><ENG><End_Use><mobility><car_sharing>" , 
                         "<3><SUS><Sufficiency><End_Use><mobility><car_sharing>",
                         colnames(x_sust))


colnames(x_sust) <- gsub("<2><ENG><End_Use><mobility><bike_pedestrian>", 
                         "<3><SUS><Sufficiency><End_Use><mobility><bike_pedestrian>",
                         colnames(x_sust))

colnames(x_sust) <- gsub("<2><ENG><End_Use><mobility><public_transport>", 
                         "<3><SUS><Sufficiency><End_Use><mobility><public_transport>",
                         colnames(x_sust))

##~------------------------------------


##+ REORDER CATEGORIES----------------------------------
main.categories.energy.order <- unlist(lapply(main.categories.energy, function(item) {
  
  grep(item, rownames(x_sust), perl=T, ignore.case = T)
  
  
}))


sust.categories <- c("<Sufficiency", "<Efficiency", "<Consistency")

sust.categories.order <- unlist(lapply(sust.categories, function(item) {
  
  grep(item, colnames(x_sust), perl=T, ignore.case = T)
  
  
}))


x_sust <- x_sust[rev(main.categories.energy.order),sust.categories.order]


row.names(x_sust) <- gsub("<1><SOC>","",row.names(x_sust))
row.names(x_sust) <- gsub("<2><ENG>","",row.names(x_sust))
colnames(x_sust) <- gsub("<3><SUS>","", colnames(x_sust))
##~--------------------------------


##+ FORMAT MATRIX FOR PLOTTING (LONG FORMAT)--------------------
cooccurrence.case.plotformat <- t(round(x_sust, d=3))

category.1 <- rownames(cooccurrence.case.plotformat)
cooccurrence.case.plotformat <- cbind.data.frame(category.1, cooccurrence.case.plotformat)
row.names(cooccurrence.case.plotformat) <- NULL

cooccurrence.case.plotformat.melted <- melt(cooccurrence.case.plotformat, id.vars=c("category.1"),
                                            #source columns
                                            measure.vars= colnames(cooccurrence.case.plotformat)[2:ncol(cooccurrence.case.plotformat)],
                                            
                                            #name of the destination column
                                            variable.name = "category.2", 
                                            value.name = "degree_of_representation_scaling_option_2",
                                            na.rm = FALSE)


cooccurrence.case.plotformat <- cooccurrence.case.plotformat.melted

cooccurrence.levels <- c(0.000, seq(1000)/1000)

cooccurrence.case.plotformat$degree_of_representation_scaling_option_2 <- factor(cooccurrence.case.plotformat$degree_of_representation_scaling_option_2, levels = cooccurrence.levels[order(cooccurrence.levels)])

cooccurrence.case.plotformat$category.1 <- factor(cooccurrence.case.plotformat$category.1, levels = unique(as.character(cooccurrence.case.plotformat$category.1)))
cooccurrence.case.plotformat$category.2 <- factor(cooccurrence.case.plotformat$category.2,  levels = unique(as.character(cooccurrence.case.plotformat$category.2)))
##~----------------------------------------                          

##+ CALCULATE ADDITIONAL PLOTTING OPTIONS (VERTICAL LINES)------------------------

##+ VERTICAL LINES
#position of lines to be drawn between systems
sust.levels <- unique(as.character(cooccurrence.case.plotformat$category.1)) 
sust.levels.unique <- c("sufficiency", "efficiency", "consistency")
line.position.v <- c("")
for (i in seq(length(sust.levels.unique))) {
  
  line.position.v <- c(line.position.v, max(grep(sust.levels.unique[i],sust.levels, ignore.case = T)))
  
}
line.position.v <- as.numeric(line.position.v[-c(1)])


line.position <- unlist(lapply(main.categories.energy.reduced, function(item) {
  
  min(grep(item, as.character(unique(cooccurrence.case.plotformat$category.2)), perl=T, ignore.case = T))
  
  
}))
line.position<- line.position-1
##~




##+ MARK SELF-REFERENCE (COOCCURRENCE) OF CATEGORIES WITH NAs/CROSSES
energy_categories <- as.character(cooccurrence.case.plotformat$category.2)
sust_categories <- as.character(cooccurrence.case.plotformat$category.1)

sust_categories <-   gsub("^<sufficiency>|^<consistency>|^<efficiency>,", "",sust_categories, ignore.case=T)


#get categories which co-occur with themselves in the graph to mark
#the respective positions with a cross

self_cooc <- which(sust_categories == energy_categories)
#cooccurrence.case.plotformat[self_cooc,] 
#head(cooccurrence.case.plotformat)
cooccurrence.case.plotformat[self_cooc,"degree_of_representation_scaling_option_2"] <- NA

na <- rep(FALSE, nrow(cooccurrence.case.plotformat))
cooccurrence.case.plotformat <- cbind(cooccurrence.case.plotformat, na)
cooccurrence.case.plotformat[self_cooc,"na"] <- TRUE


self_cooc_categories <- cooccurrence.case.plotformat[self_cooc,c(1,2)]

#full list of categories of x and y axis with numbering
cat_y <-   cbind(unique(as.character(cooccurrence.case.plotformat$category.2)),
                 1:length( unique(as.character(cooccurrence.case.plotformat$category.2))))

cat_x <-    cbind(unique(as.character(cooccurrence.case.plotformat$category.1)), 
                  1:length(unique(as.character(cooccurrence.case.plotformat$category.1))))


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
##~----------------------------------------



##+ COOCCURRENCE PLOT ENERGY / SUSTAINABILITY--------------------
axis.label.size <- 8

p_sust <- ggplot(NULL) +
  
  #raster with no distance to the axis
  geom_raster(data= cooccurrence.case.plotformat, aes(x = category.1, y = category.2, fill = degree_of_representation_scaling_option_2),hjust = 0, vjust = 0) +  
  
  scale_fill_manual(values = c("white", "grey95", "grey75", "grey30", "black"),
                    labels = c("0", "S1","S2","S3","S4 (self-reference: white cross)"),
                    na.value="black")  +
  
  #remove labels
  labs(x = element_blank(), y=element_blank()) +
  
  #set basic theme of the plot
  theme_bw() +
  
  theme(
    
    #rotate x-axis label, and set distance of label/axis to zero
    axis.text.x = element_text(angle = 90, hjust = 0, vjust=-0.25, size=axis.label.size),
    axis.text.y = element_text(vjust = .8, size=axis.label.size, hjust=0),
    
    #remove axis title
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    
    axis.ticks = element_line(size=.01),
    
    axis.ticks.length = unit(.1, "cm"),
    
    plot.background = element_rect(fill = NULL,colour = NA),
    
    plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),
    
    #length(unique(as.character(cooccurrence.case.plotformat$category.2)))/length(unique(as.character(cooccurrence.case.plotformat$category.1)))
    aspect.ratio = 2.737,
    
    legend.position = c(-0.6,-0.1),
    legend.key.size = unit(0.25, "cm"),
    legend.title = element_text(size=axis.label.size),
    legend.direction = "vertical",
    legend.background = element_rect(colour = "grey30"),
    legend.key = element_rect(colour = "grey30"),
    legend.text = element_text(size=axis.label.size)
    
  ) +
  
  
  geom_vline(xintercept = line.position.v, colour = "black", linetype= "dashed", size=.01) +
  geom_hline(yintercept = line.position, colour = "black", linetype= "dashed", size=.01) +
  
  geom_point(data = na_positions, aes(x=V1, y=V2, shape=4), color="white", size=4) +
  scale_shape_identity()

p_sust
##~-------------------------


##~ SAVE PLOT IN FILE----------------
par(mar=c(0.2,0.2,0.2,0.2))

setwd(wd.final)


# pdf("Figure_3_CoOc_Energy_and_Sustainability_scalingOpt2.pdf")
# p_sust
# dev.off()

win.metafile("Figure_3_CoOc_Energy_and_Sustainability_scalingOpt2.emf", height=10, width=6)
p_sust
dev.off()


postscript("Figure_3_CoOc_Energy_and_Sustainability_scalingOpt2.eps", height=10, width=6)
p_sust
dev.off()
##~-------------------------------

time.elapsed <- rbind(time.elapsed, proc.time())
row.names(time.elapsed)[nrow(time.elapsed)] <- "COOCCURRENCE_ENERGY_SYSTEM_AND_SUSTAINABILITY"
setwd(wd.current)
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##END <<<<<<<<<<<<< COOCCURRENCE PLOT ENERGY / SUSTAINABILITY
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
