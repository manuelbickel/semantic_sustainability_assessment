##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##START <<<<<<<<<<<<< BOXPLOT THESAURUS ONE; SOCIAL SYSTEM
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
wd.current <- getwd()
##+ BOXPLOT THESAURUS ONE-----------------------------------------------
#details on the following code steps are given above section concerning the
#BOXPLOT ENERGY SYSTEM
#the following lines are analogous

results.occ.social.plotformat <- results.occ.social[order(rownames(results.occ.social)),]

results.occ.social.plotformat <- t(results.occ.social.plotformat)

results.occ.social.plotformat <- as.data.frame(results.occ.social.plotformat)

results.occ.social.plotformat <- cbind(rownames(results.occ.social.plotformat), results.occ.social.plotformat)
colnames(results.occ.social.plotformat)[1] <- "case"
rownames(results.occ.social.plotformat) <- NULL

results.occ.social.plotformat <- melt(results.occ.social.plotformat, id.vars= c("case"))

colnames(results.occ.social.plotformat)[2] <- "category"

results.occ.social.plotformat <- subset(results.occ.social.plotformat, results.occ.social.plotformat$case != "mean")

row.names(results.occ.social.plotformat) <- 1:nrow(results.occ.social.plotformat)

results.occ.social.plotformat$category <- factor(results.occ.social.plotformat$category, levels = unique(results.occ.social.plotformat$category))


results.occ.social.plotformat <- results.occ.social.plotformat[nrow(results.occ.social.plotformat):1,]


vertical.line <- unlist(lapply(main.categories.energy, function(item) {
  
  max(grep(item, unique(results.occ.social.plotformat$category), perl=T, ignore.case=T))
  
})) 
vertical.line <- vertical.line[-length(vertical.line)]
vertical.line  <- vertical.line+0.5


x.axis.label.size <- c(10)

y.axis.label.size <- c(10)
y.axis.title.size <- c(10)


p <- ggplot(results.occ.social.plotformat, aes(factor(category), value), stat = "identity") +
  
  geom_boxplot(outlier.colour = "black") + 
  
  
  ylab("average normalized occurrence")+
  xlab("") +
  
  
  #theme black and white
  theme_bw() +
  
  #eliminates background, gridlines, and chart border
  theme(plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_rect(colour = "black")
        ,axis.line = element_line(colour = "black")
        ,plot.title = element_blank()
        ,plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")
        
  ) +
  
  
  # Rotate x-axis labels
  theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.15, size=7)
        ,axis.title.x = element_text(size=7)
        
        #set y-axis tick sizes
        ,axis.text.y = element_text(size=7, angle=0, vjust=0.25, hjust=0)
        
        
  ) +
  
  #set the limits of the y axis
  scale_y_continuous(expand = c(0,0), limits = c(0,1)) + 
  
  #set distance of y axis label to the axis
  theme(axis.title.y = element_text(vjust=1.1, size=10)) +
  
  theme(aspect.ratio=4.5) +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(results.occ.social.plotformat$category))) + #this generally works but the color bars that were introduced are not reversed
  
  #don´t show legend
  theme(legend.position = "none")

p



setwd(wd.final)

par(mar=c(0.5,0.5,0.5,0.5))
scale_figure <- c(9,5.5)


win.metafile("ESM_S9_societal_subsystems_occurrence_boxplot.emf", height=scale_figure[1] ,width=scale_figure[2])
p
dev.off()


postscript("ESM_S9_societal_subsystems_occurrence_boxplot.eps", height=scale_figure[1] ,width=scale_figure[2])
p
dev.off()
##~-----------------------------------------------

time.elapsed <- rbind(time.elapsed, proc.time())
row.names(time.elapsed)[nrow(time.elapsed)] <- "BOXPLOT_OCCURRENCE_SOCIAL"
time.elapsed
setwd(wd.current)
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##END <<<<<<<<<<<<< BOXPLOT THESAURUS ONE; SOCIAL SYSTEM
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
