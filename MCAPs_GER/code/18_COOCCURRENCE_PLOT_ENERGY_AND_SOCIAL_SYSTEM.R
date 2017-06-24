##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##START <<<<<<<<<<<<< SCALED COOCCURRENCE PLOT ENERGY SYSTEM AND SOCIAL SYSTEM CATEOGIRES
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
wd.current <- getwd()

##+EXCLUDE CATEGORIES WITH VERY LOW OCCURRENCE AND APPLY SCALING TO MATRIX--------------------

oc_lower_1p <- which(as.numeric(diag(x)) < 0.01)
#rownames(x[oc_lower_1p,oc_lower_1p])
x <- x[-oc_lower_1p,-oc_lower_1p]



##+ SCALE BY MINIMUM VALUES OF DIAGONAL
x.diag <- as.numeric(diag(x))

for (d in seq(length(x.diag))) {
  
  x.diag.value <- cbind(x.diag, rep(as.numeric(x.diag[d]), length(x.diag)))
  
  x.diag.value <- apply(x.diag.value,1, min)
  
  x[,d] <- round(x[,d]/x.diag.value,d=3)
}

x <- ifelse(is.na(x), 0, x)
##~
##~----------------------------


##+ RENAME CATEGORIES, CLASSIFY; SELECT CATEGORIES ABOVE THRESHOLD VALUE FOR BEING PLOTTED-------------
x <- x[grep("<2>|<economy>|<industry>|<commerce>|<mobility_sector>|<local_administration_bodies>|<infrastructure>|<Food>|<Residents>", rownames(x), ignore.case = T), 
       -grep("<3>|<2><ENG>", colnames(x),ignore.case=T, perl=T)]

#exclude categories from y-axis
rows.exclude <- grep("<carrier>|<rental_of_houses>|<social_groups>|<general_reference><saving>|<building_parts_materials>|<Energy><unspecific_reference>|<Energy_Form><unspecific_reference>|<Economy><commerce><sector_unspecific>|<Infrastructure><disposal_unspecific>|<detailed_PPtechnology>|<alternative_wo_bike>|<tariffs_standing_orders>|<Economy><general_reference><employment_workplace>|<Infrastructure><unspecific>|<Infrastructure><supply_unspecific>|<Economy><general_reference><economic_viability>|<Economy><service><personal_services_crafting>", rownames(x), ignore.case = T)
if (length(rows.exclude) > 0) {
  x <- x[-rows.exclude,]
}


#exclude categories from x-axis
columns.exclude <- grep("<Spatial_scale><fed_state><ger>", colnames(x), ignore.case = T)
if (length(columns.exclude) > 0) {
  x <- x[,-columns.exclude]
}



##+ SELECT ROWS THAT ACHIEVED AVERAGE COOCURRENCE OF X%
columns.maxima <- apply(x, 2, sum)
average.sum.required.minimum <- nrow(x)*0.20
columns.maxima <- which(columns.maxima >= average.sum.required.minimum)
x <- x[,columns.maxima]
##~

##+ CLASSIFIY
classes <- c(0,0.25,0.5,0.75,1)
x <- ifelse(is.na(x), 0, x)
x <- ifelse(x <= classes[1], 0, x)
x <- ifelse((x > classes[1] & x < classes[2]), 0.05, x)
x <- ifelse((x >= classes[2] & x < classes[3]), 0.375,x)
x <- ifelse((x >= classes[3] & x < classes[4]), 0.625,x)
#x <- ifelse((x >= classes[4] & x < classes[5]), 0.7,x)
x <- ifelse((x >= classes[4]), 0.875,x)

columns.maxima <- apply(x, 2, function(item) length(which(item >=0.625)))
columns.maxima <- which(columns.maxima >1)

x <- x[,columns.maxima]
##~

setwd(wd.final)
writeLines(noquote(colnames(x)), "systems_with_at_least_one_L3_with_energy_system.txt")
##~-----------------------------


##+ SET ORDER OF CATEGORIES FOR PLOTTING AND RENAME CATEGORIES-------------------------
main.categories.energy.order <- unlist(lapply(main.categories.energy, function(item) {
  
  grep(item, rownames(x), perl=T, ignore.case = T)
  
  
}))

x <- x[rev(main.categories.energy.order),order(colnames(x))]

row.names(x) <- gsub("<1><SOC>","",row.names(x))
row.names(x) <- gsub("<2><ENG>","",row.names(x))
##~------------------------------


##+ FORMAT MATRIX FOR PLOTTING (LONG FORMAT)--------------------
cooccurrence.case.plotformat <- t(round(x, d=3))

category.1 <- rownames(cooccurrence.case.plotformat)
cooccurrence.case.plotformat <- cbind.data.frame(category.1, cooccurrence.case.plotformat)
row.names(cooccurrence.case.plotformat) <- NULL

cooccurrence.case.plotformat.melted <- melt(cooccurrence.case.plotformat, id.vars=c("category.1"),
                                            #source columns
                                            measure.vars= colnames(cooccurrence.case.plotformat)[2:ncol(cooccurrence.case.plotformat)],
                                            
                                            #name of the destination column
                                            variable.name = "category.2", 
                                            value.name = "strength_of_link_scaling_option_1",
                                            na.rm = FALSE)


cooccurrence.case.plotformat <- cooccurrence.case.plotformat.melted

cooccurrence.levels <- c(0.000, seq(1000)/1000)

cooccurrence.case.plotformat$strength_of_link_scaling_option_1 <- factor( cooccurrence.case.plotformat$strength_of_link_scaling_option_1, levels = cooccurrence.levels[order(cooccurrence.levels)])

cooccurrence.case.plotformat$category.1 <- factor(cooccurrence.case.plotformat$category.1, levels = unique(as.character(cooccurrence.case.plotformat$category.1)))
cooccurrence.case.plotformat$category.2 <- factor(cooccurrence.case.plotformat$category.2,  levels = unique(as.character(cooccurrence.case.plotformat$category.2)))
##~--------------------


##+ CALCULATE ADDITIONAL PLOTTING OPTIONS (VERTICAL LINES)------------------------

##+ VERTICAL LINES
line.position <- unlist(lapply(main.categories.energy.reduced, function(item) {
  
  min(grep(item, as.character(unique(cooccurrence.case.plotformat$category.2)), perl=T, ignore.case = T))
  
  
}))
line.position<- line.position-1

#position of lines to be drawn between systems
system.levels <- unique(as.character(cooccurrence.case.plotformat$category.1)) 
system.levels <- unique(gsub("(<[\\d]>)(<[\\w]{3}>)(<[\\w]+>)(.*$)", "\\3", system.levels , perl=T))

line.position.v <- unlist(lapply(system.levels, function(item) {
  
  min(grep(item, as.character(unique(cooccurrence.case.plotformat$category.1)), perl=T, ignore.case = T))
  
  
}))
line.position.v <- line.position.v-1
##~



cooccurrence.case.plotformat$category.1 <- gsub("(^<[\\d]>)(<[\\w]+>)(<.*$)","\\3", as.character(cooccurrence.case.plotformat$category.1), perl=T)
cooccurrence.case.plotformat$category.1 <- factor(cooccurrence.case.plotformat$category.1, levels = unique(as.character(cooccurrence.case.plotformat$category.1)))

cooccurrence.case.plotformat$category.2 <- gsub("(^<[\\d]>)(<[\\w]+>)(<.*$)","\\3", as.character(cooccurrence.case.plotformat$category.2), perl=T)
cooccurrence.case.plotformat$category.2 <- factor(cooccurrence.case.plotformat$category.2, levels = unique(as.character(cooccurrence.case.plotformat$category.2)))



##+ MARK SELF-REFERENCE (COOCCURRENCE) OF CATEGORIES WITH NAs/CROSSES
energy_categories <- gsub("<End_Use>","", as.character(cooccurrence.case.plotformat$category.2))

energy_categories <- as.character(cooccurrence.case.plotformat$category.2)
subsystem_categories <- as.character(cooccurrence.case.plotformat$category.1)


#get categories which co-occur with themselves in the graph to mark
#the respective positions with a cross
self_cooc <- which(subsystem_categories == energy_categories)
#cooccurrence.case.plotformat[self_cooc,] 
#head(cooccurrence.case.plotformat)
cooccurrence.case.plotformat[self_cooc,"strength_of_link_scaling_option_1"] <- NA

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

order_y <- unlist(lapply(cat_x[,1], function(item) {
  
  grep(item, cat_y[,1])
  
}))

cat_y <- cat_y[order_y,]


na_positions <- as.data.frame(cbind(
  as.integer(cat_x[,2])-0.5,
  as.integer(cat_y[,2])-0.5
))
##~
##~------------------------


##+ COOCCURRENCE PLOT ENERGY / SOCIAL SYSTEM--------------------
axis.label.size <- 8

p_scaled_cooc <- ggplot(NULL) +
  
  #raster with no distance to the axis
  geom_raster(data= cooccurrence.case.plotformat, aes(x = category.1, y = category.2, fill = strength_of_link_scaling_option_1), 
              hjust = 0, vjust = 0) +  
  
  
  xlab("")+
  ylab("")+
  
  #set colour scale for displaying values
  scale_fill_manual(values = c("white", "grey95", "grey75", "grey30", "black"),
                    labels = c("0", "L1","L2","L3","L4 (self-reference: white cross)"),
                    na.value="black") +
  
  #remove labels
  labs(x = element_blank(), y=element_blank()) +
  
  #set basic theme of the plot
  theme_bw() +
  
  theme(
    
    plot.background = element_blank(),
    
    plot.title = element_blank(),
    
    aspect.ratio = round(length(unique(as.character(cooccurrence.case.plotformat$category.2)))/length(unique(as.character(cooccurrence.case.plotformat$category.1)))),
    
    plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),
    
    #rotate x-axis label, and set distance of label/axis to zero
    axis.text.x = element_text(angle = 90, hjust = 0, vjust=-0.25, size=axis.label.size),
    axis.text.y = element_text(vjust = .8, size=axis.label.size, hjust = 0),
    
    #remove axis title
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    
    axis.ticks = element_line(size=.01),
    axis.ticks.length = unit(.1, "cm"),
    
    
    legend.position = c(-0.5,-0.1),
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

p_scaled_cooc 
##~--------------------


##~ SAVE PLOT IN FILE----------------
opar <- par()

par(mar=c(0.1,0.1,0.1,0.1))

setwd(wd.final)

scale_figure <- c(10,6.8)

win.metafile("Figure_2_CoOc_L3_to_Energy_system_and_average_CoOc_0125_scalingOpt1.emf", height=scale_figure[1] ,width=scale_figure[2])
p_scaled_cooc
dev.off()


postscript("Figure_2_CoOc_L3_to_Energy_system_and_average_CoOc_0125_scalingOpt1.eps", height=scale_figure[1] ,width=scale_figure[2])
p_scaled_cooc
dev.off()

par(opar)
##~-----------------------------

time.elapsed <- rbind(time.elapsed, proc.time())
row.names(time.elapsed)[nrow(time.elapsed)] <- "COOCCURRENCE_PLOT_ENERGY_AND_SOCIAL_SYSTEM"
time.elapsed
setwd(wd.current)
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##END <<<<<<<<<<<<< SCALED COOCCURRENCE PLOT ENERGY SYSTEM AND SOCIAL SYSTEM CATEOGIRES
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
