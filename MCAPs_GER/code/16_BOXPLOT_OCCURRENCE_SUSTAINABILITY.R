##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##START <<<<<<<<<<<<< BOXPLOT THESAURUS THREE , SUSTAINABILITY
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
wd.current <- getwd()

##+ BOXPLOT THESAURUS THREE--------------------------
#details on code see
#BOXPLOT ENERGY SYSTEM

results.occ.sustainability.plotformat <- results.occ.sustainability[order(rownames(results.occ.sustainability)),]


#rename categories that have been included from the second thesaurus
rownames(results.occ.sustainability.plotformat) <- gsub("<2><ENG><Resources><renewable>" , 
                                                        "<3><SUS><Consistency><Resources><renewable>",
                                                        rownames(results.occ.sustainability.plotformat))

rownames(results.occ.sustainability.plotformat) <- gsub("<2><ENG><Conversion><renewable>", 
                                                        "<3><SUS><Consistency><Conversion><renewable>",
                                                        rownames(results.occ.sustainability.plotformat))

rownames(results.occ.sustainability.plotformat) <- gsub("<2><ENG><Sales_Contracts><renewable>" , 
                                                        "<3><SUS><Consistency><Sales_Contracts><renewable>",
                                                        rownames(results.occ.sustainability.plotformat))

rownames(results.occ.sustainability.plotformat) <- gsub("<2><ENG><End_Use><mobility><biofuels>", 
                                                        "<3><SUS><Consistency><End_Use><mobility><biofuels>",
                                                        rownames(results.occ.sustainability.plotformat))

rownames(results.occ.sustainability.plotformat) <- gsub("<2><ENG><Technology_Option><storage>", 
                                                        "<3><SUS><Efficiency><Technology_Option><storage>",
                                                        rownames(results.occ.sustainability.plotformat))

rownames(results.occ.sustainability.plotformat) <- gsub("<2><ENG><End_Use><building><insulation>" , 
                                                        "<3><SUS><Efficiency><End_Use><building><insulation>",
                                                        rownames(results.occ.sustainability.plotformat))

rownames(results.occ.sustainability.plotformat) <- gsub("<2><ENG><End_Use><mobility><car_sharing>" , 
                                                        "<3><SUS><Sufficiency><End_Use><mobility><car_sharing>",
                                                        rownames(results.occ.sustainability.plotformat))


rownames(results.occ.sustainability.plotformat) <- gsub("<2><ENG><End_Use><mobility><bike_pedestrian>", 
                                                        "<3><SUS><Sufficiency><End_Use><mobility><bike_pedestrian>",
                                                        rownames(results.occ.sustainability.plotformat))

rownames(results.occ.sustainability.plotformat) <- gsub("<2><ENG><End_Use><mobility><public_transport>", 
                                                        "<3><SUS><Sufficiency><End_Use><mobility><public_transport>",
                                                        rownames(results.occ.sustainability.plotformat))



categories.exclude <- grep("<Sustainability><unspecific_reference>|Uncertainty><Uncertainty_Risks_Accidents>", rownames(results.occ.sustainability.plotformat), ignore.case = T)
results.occ.sustainability.plotformat <- results.occ.sustainability.plotformat[-categories.exclude,]



sust.categories <- c("<Sufficiency", "<Efficiency", "<Consistency")

sust.categories.order <- unlist(lapply(sust.categories, function(item) {
  
  grep(item, rownames(results.occ.sustainability.plotformat), perl=T, ignore.case = T)
  
  
}))

results.occ.sustainability.plotformat <- results.occ.sustainability.plotformat[sust.categories.order,]

row.names(results.occ.sustainability.plotformat) <- gsub("<3><SUS>","",row.names(results.occ.sustainability.plotformat))




results.occ.sustainability.plotformat <- t(results.occ.sustainability.plotformat)

results.occ.sustainability.plotformat <- as.data.frame(results.occ.sustainability.plotformat)

results.occ.sustainability.plotformat <- cbind(rownames(results.occ.sustainability.plotformat), results.occ.sustainability.plotformat)
colnames(results.occ.sustainability.plotformat)[1] <- "case"
rownames(results.occ.sustainability.plotformat) <- NULL

results.occ.sustainability.plotformat <- melt(results.occ.sustainability.plotformat, id.vars= c("case"))

colnames(results.occ.sustainability.plotformat)[2] <- "category"

results.occ.sustainability.plotformat <- subset(results.occ.sustainability.plotformat, results.occ.sustainability.plotformat$case != "mean")

row.names(results.occ.sustainability.plotformat) <- 1:nrow(results.occ.sustainability.plotformat)

results.occ.sustainability.plotformat$category <- factor(results.occ.sustainability.plotformat$category, levels = unique(results.occ.sustainability.plotformat$category))


results.occ.sustainability.plotformat <- results.occ.sustainability.plotformat[nrow(results.occ.sustainability.plotformat):1,]



vertical.line <- unlist(lapply(sust.categories, function(item) {
  
  max(grep(item, unique(results.occ.sustainability.plotformat$category), perl=T, ignore.case=T))
  
})) 
vertical.line <- vertical.line[-length(vertical.line)]
vertical.line  <- vertical.line+0.5


x.axis.label.size <- c(10)

y.axis.label.size <- c(10)
y.axis.title.size <- c(10)

p <- ggplot(results.occ.sustainability.plotformat, aes(factor(category), value), stat = "identity") +
  
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
  theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.15, size=14)
        ,axis.title.x = element_text(size=14)
        
        #set y-axis tick sizes
        ,axis.text.y = element_text(size=14, angle=0, vjust=0.25, hjust=0)
        
        
  ) +
  
  
  
  #set the limits of the y axis
  scale_y_continuous(expand = c(0,0), limits = c(0,1)) + 
  
  #set distance of y axis label to the axis
  theme(axis.title.y = element_text(vjust=1.1, size=10)) +
  
  
  
  theme(aspect.ratio=4.5) +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(results.occ.sustainability.plotformat$category))) + #this generally works but the color bars that were introduced are not reversed
  
  #don´t show legend
  theme(legend.position = "none")

p



setwd(wd.final)

par(mar=c(0.5,0.5,0.5,0.5))
scale_figure <- c(10,8)


win.metafile("sustainability_aspects_occurrence_boxplot.emf", height=scale_figure[1] ,width=scale_figure[2])
p
dev.off()


postscript("sustainability_aspects_occurrence_boxplot.eps", height=scale_figure[1] ,width=scale_figure[2])
p
dev.off()
##~-----------------------------------


time.elapsed <- rbind(time.elapsed, proc.time())
row.names(time.elapsed)[nrow(time.elapsed)] <- "BOXPLOT_OCCURRENCE_SUSTAINABILITY"
time.elapsed
setwd(wd.current)

##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##END <<<<<<<<<<<<< BOXPLOT THESAURUS THREE , SUSTAINABILITY
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
