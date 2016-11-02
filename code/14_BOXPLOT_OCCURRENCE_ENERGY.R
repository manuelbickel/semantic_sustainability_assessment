##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##START <<<<<<<<<<<<< BOXPLOT OF OCCURRENCES OF CATEGORIES IN ENERGY SYSTEM THESAURUS
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
wd.current <- getwd()
##+ SET ORDER OF CATEGORIES FOR PLOTTING-------------------------
main.categories.energy.order <- unlist(lapply(main.categories.energy, function(item) {
  
  grep(item, rownames(results.occ.energy), ignore.case=T, perl=T)
  
}))

results.occ.energy <- results.occ.energy[main.categories.energy.order,]
##~------------------------------

##+ CHECK MEDIAN VALUES OF CATEGORIES------------------------
results.occ.energy.median <-  results.occ.energy[, setdiff(colnames(results.occ.energy), c("mean"))]
results.occ.energy.median <- as.data.frame(apply(results.occ.energy.median,1,function(x) median(x)))
colnames(results.occ.energy.median) <- "median"

rownames(results.occ.energy[which(results.occ.energy[,"mean"] < 0.01),])

wd <- getwd()
setwd(wd.interim)
oc_lower_1p <- rownames(results.occ.energy[which(results.occ.energy[,"mean"] < 0.01),])
writeLines(noquote(oc_lower_1p), "categories_occurrence_lower_1_percent.txt")
setwd(wd.final)
write.csv(results.occ.energy.median, "categories_energy_occurrence_median.csv")
setwd(wd)
##~----------------------------


##+ FORMAT RESULTS TO BE PLOTTED WITH GGPLOT (LONG FORMAT, ROWNAMES)---------------------------
results.occ.energy.plotformat <- t(results.occ.energy)

results.occ.energy.plotformat  <- as.data.frame(results.occ.energy.plotformat )

results.occ.energy.plotformat  <- cbind(rownames(results.occ.energy.plotformat ), results.occ.energy.plotformat )
colnames(results.occ.energy.plotformat )[1] <- "case"
rownames(results.occ.energy.plotformat ) <- NULL

results.occ.energy.plotformat <- melt(results.occ.energy.plotformat , id.vars= c("case"))

colnames(results.occ.energy.plotformat)[2] <- "category"

results.occ.energy.plotformat <- subset(results.occ.energy.plotformat, results.occ.energy.plotformat$case != "mean")

row.names(results.occ.energy.plotformat) <- 1:nrow(results.occ.energy.plotformat)

results.occ.energy.plotformat$category <- factor(results.occ.energy.plotformat$category, levels = unique(results.occ.energy.plotformat$category))


results.occ.energy.plotformat <- results.occ.energy.plotformat[nrow(results.occ.energy.plotformat):1,]
##~-------------------------



##+ CALCULATE ADDITIONAL PLOTTING PARAMETERS (POSITION OF VERTICAL LINES)---------------
vertical.line <- unlist(lapply(main.categories.energy.reduced, function(item) {
  
  max(grep(item, unique(results.occ.energy.plotformat$category), perl=T, ignore.case=T))
  
})) 

vertical.line  <- vertical.line+0.5

##~---------------


##~ CREATE BOXPLOT VIA GGPLOT AS GRAPHICAL OBJECT------------------------------
x.axis.label.size <- c(10)
y.axis.label.size <- c(11)
y.axis.title.size <- c(11)

p <- ggplot(results.occ.energy.plotformat, aes(factor(category), value), stat = "identity") +
  
  geom_boxplot(outlier.colour = "black") +
  
  ylab("average normalized occurrence")+
  xlab("") +
  
  
  #theme black and white
  theme_bw() +
  
  #don´t show legend
  theme(legend.position = "none") +
  
  #eliminates background, gridlines, chart border
  theme(plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_rect(colour = "black")
        ,axis.line = element_line(colour = "black")
        ,plot.title = element_blank()
        ,plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")
        
  ) +
  
  
  # Rotate x-axis labels
  theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.15, size=10)
        ,axis.title.x = element_text(size=10)
        
        #set y-axis tick sizes
        ,axis.text.y = element_text(size=10, angle=0, vjust=0.25, hjust=0)
        #,axis.title.y = element_text(size=5)
        
  ) +
  
  #set limits of the y axis
  scale_y_continuous(expand = c(0,0), limits = c(0,1)) + 
  
  #set distance of y axis label to the axis
  theme(axis.title.y = element_text(vjust=1.1, size=11)) +
  
  
  theme(aspect.ratio=3) +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(results.occ.energy.plotformat$category)))

#draw vertical lines, desired lines have to be turned off/on manually in loop
#here first entry is skipped
for (i in 2:length(vertical.line)) {
  geom_vline.loop <- paste0("geom_vline(aes(xintercept = c(rep(vertical.line[",i,"], nrow(results.occ.energy.plotformat)))), linetype= \"dashed\")")
  p <- p + eval(parse(text=geom_vline.loop))                          
}
p
##~---------------------------------------------------------------

##+ WRITE RESULT PLOTS INTO FILE------------------------
setwd(wd.final)

par(mar=c(0.5,0.5,0.5,0.5))
scale_figure <- c(9,6.5)

win.metafile("Figure_1_energy_system_occurrence_boxplot.emf", height=scale_figure[1] ,width=scale_figure[2])
p
dev.off()


postscript("Figure_1_energy_system_occurrence_boxplot.eps", height=scale_figure[1] ,width=scale_figure[2])
p
dev.off()
##~-----------------------------------------------


time.elapsed <- rbind(time.elapsed, proc.time())
row.names(time.elapsed)[nrow(time.elapsed)] <- "BOXPLOT_OCCURRENCE_ENERGY"
setwd(wd.current)

##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##END <<<<<<<<<<<<< BOXPLOT OF OCCURRENCES OF CATEGORIES IN ENERGY SYSTEM THESAURUS
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
