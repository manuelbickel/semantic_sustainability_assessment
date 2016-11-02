##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##START <<<<<<<<<<<<< COOCCURRENCE PLOT OF ALL CATEGORIES WIHTOUT ANY CALSSIFICATION OR SCALING
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

wd.current <- getwd()

##+ PLOT COOCCURRENCE MATRIX WITHOUT ANY SCALING AND CLASSIFICATION---------------------------
# explanations of code see above in plotting cooccurrence of energy system with social system categories

x_full  <- round(x_full, d=3)

x_full <- x_full[order(row.names(x_full)), order(colnames(x_full))]

cooccurrence.case.plotformat.full <- x_full

category.1 <- rownames(cooccurrence.case.plotformat.full)
cooccurrence.case.plotformat.full <- cbind.data.frame(category.1, cooccurrence.case.plotformat.full)
row.names(cooccurrence.case.plotformat.full) <- NULL

cooccurrence.case.plotformat.full.melted <- melt(cooccurrence.case.plotformat.full, id.vars=c("category.1"),
                                                 #source columns
                                                 measure.vars= colnames(cooccurrence.case.plotformat.full)[2:ncol(cooccurrence.case.plotformat.full)],
                                                 
                                                 #name of the destination column
                                                 variable.name = "category.2", 
                                                 value.name = "cooccurrence",
                                                 na.rm = FALSE)


cooccurrence.case.plotformat <- cooccurrence.case.plotformat.full.melted

cooccurrence.levels <- c(0.000, seq(1000)/1000)

cooccurrence.case.plotformat$strength_of_link_scaling_option_1 <- factor(cooccurrence.case.plotformat$strength_of_link_scaling_option_1, levels = cooccurrence.levels[order(cooccurrence.levels)])

cooccurrence.case.plotformat$category.1 <- factor(cooccurrence.case.plotformat$category.1, levels = unique(as.character(cooccurrence.case.plotformat$category.1)))
cooccurrence.case.plotformat$category.2 <- factor(cooccurrence.case.plotformat$category.2,  levels = unique(as.character(cooccurrence.case.plotformat$category.2)))


axis.label.size <- 3

p_full <- ggplot(NULL) +
  
  #raster with no distance to the axis
  geom_raster(data= cooccurrence.case.plotformat, aes(x = category.1, y = category.2, fill = cooccurrence), hjust = 0, vjust = 0) +  
  
  
  xlab("")+
  ylab("")+
  
  
  scale_fill_grey(start = 1, end = 0, na.value = "white") +   
  
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


par(mar=c(0.2,0.2,0.2,0.2))

setwd(wd.final)

pdf("full_cooc_mat_scaled_by_total_max_cooc.pdf")
p_full
dev.off()

win.metafile("full_cooc_mat_scaled_by_total_max_cooc.emf")
p_full
dev.off()

postscript("full_cooc_mat_scaled_by_total_max_cooc.eps")
p_full
dev.off()
##~----------------------

time.elapsed <- rbind(time.elapsed, proc.time())
row.names(time.elapsed)[nrow(time.elapsed)] <- "COOCCURRENCE_ALL_CATEGORIES_NO_SCALING_OR_CLASSIFICATION"
setwd(wd.current)
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##END <<<<<<<<<<<<< COOCCURRENCE PLOT OF ALL CATEGORIES WIHTOUT ANY CALSSIFICATION OR SCALING
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
