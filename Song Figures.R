#Boxplots of all traits

songlength.location = ggplot(data = IndividualJuncos, aes(x = UrbanorNot, y = SongLength, fill = Location)) + labs(y = "Song Length (s)", x = "") + geom_boxplot() + scale_x_discrete(labels = c("Non-urban", "Urban"))
trillrate.location = ggplot(data = IndividualJuncos, aes(x = UrbanorNot, y = TrillRate, fill = Location)) + labs(y = "Trill Rate", x = "") + geom_boxplot() + scale_x_discrete(labels = c("Non-urban", "Urban"))
minF.location = ggplot(data = IndividualJuncos, aes(x = UrbanorNot, y = minF, fill = Location)) + labs(y = "Minimum Frequency (kHz)", x = "") + geom_boxplot() + scale_x_discrete(labels = c("Non-urban", "Urban"))
maxF.location = ggplot(data = IndividualJuncos, aes(x = UrbanorNot, y = maxF, fill = Location)) + labs(y = "Maximum Frequency (kHz)", x = "") + geom_boxplot() + scale_x_discrete(labels = c("Non-urban", "Urban"))
bandF.location = ggplot(data = IndividualJuncos, aes(x = UrbanorNot, y = bandwidthF, fill = Location)) + labs(y = "Bandwidth Frequency (kHz)", x = "") + geom_boxplot() + scale_x_discrete(labels = c("Non-urban", "Urban"))

fig.traits.location = ggarrange(songlength.location, trillrate.location, 
                                minF.location, maxF.location, bandF.location, 
                                ncol = 3, nrow = 2, labels = c("A", "B", "C", "D","E"), 
                                legend = "bottom", common.legend = TRUE)
fig.traits.location = annotate_figure(fig.traits.location,top = text_grob(
  "Song Traits by Location grouped by Non-Urban or Urban", 
  color = "black", face = "bold", size = 14))
fig.traits.location = annotate_figure(fig.traits.location, fig.lab = "Angeles n = 13 \n James n = 5 \n Oxy n = 28 \n Stunt n = 3 \n UCLA n = 57 \n UCSB n = 7 \n UCSD n = 17", 
                                  fig.lab.pos = "bottom.right", fig.lab.face = "italic", fig.lab.size = 10)
fig.traits.location


#UCSD Comparison Traits, Boxplots
SD.TrillRate = ggplot(data = justSD, aes(x = justSD$Location, y = justSD$TrillRate)) + labs(x = "", y = "Trill Rate") + geom_boxplot() + scale_x_discrete(labels = c("UCSD 2018-2020","UCSD 2006/2007"))
SD.minF = ggplot(data = justSD, aes(x = justSD$Location, y = justSD$minF)) + labs(x = "", y = "Minimum Frequency (kHz)") + geom_boxplot() + scale_x_discrete(labels = c("UCSD 2018-2020","UCSD 2006/2007"))
SD.maxF = ggplot(data = justSD, aes(x = justSD$Location, y = justSD$maxF)) + labs(x = "", y = "Maximum Frequency (kHz)") + geom_boxplot() + scale_x_discrete(labels = c("UCSD 2018-2020","UCSD 2006/2007"))
SD.bandF = ggplot(data = justSD, aes(x = justSD$Location, y = justSD$bandwidthF)) + labs(x = "", y = "Bandwidth Frequency (kHz)") + geom_boxplot() + scale_x_discrete(labels = c("UCSD 2018-2020","UCSD 2006/2007"))

figs.traits.SD = ggarrange(SD.TrillRate, SD.minF, 
                           SD.maxF, SD.bandF,
                                ncol = 2, nrow = 2, labels = c("A", "B", "C", "D"))
figs.traits.SD = annotate_figure(figs.traits.SD,top = text_grob(
  "Song Traits by UCSD 2018-2020 or UCSD 2006/2007", 
  color = "black", face = "bold", size = 14))
figs.traits.SD = annotate_figure(figs.traits.SD, fig.lab = "UCSD 2018-2020 n = 17 \n UCSD 2006/2007 n = 101", fig.lab.pos = "top.right", fig.lab.face = "italic")
figs.traits.SD

#All pops LDA
library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())

plotLDAAllPop.location = ggplot(data = LDJuncos, aes(x = LD1, y = LD2, colour = Location)) +
  stat_ellipse(aes(x=LD1, y=LD2,color=Location, group=Location),type = "norm") +
  theme() + geom_point() + theme_classic() + ggtitle("By Location")

plotLDAAllPop.location

plotLDAAllPop.UorN = ggplot(data = LDJuncos2, aes(x = UrbanorNot, 
                                                  y = LD1)) + geom_boxplot() + 
                        ggtitle("By Urban or Non-Urban") + xlab("") +
                        scale_x_discrete(labels = c("Non-Urban (n = 21)", "Urban (n = 109)"))
plotLDAAllPop.UorN

plotLDAAllPop.UorNwRep = ggplot(data = LDJuncos4, aes(x = UrbanorNot,
                                y = LD1)) + geom_boxplot() + 
                                ggtitle("With Repository") + xlab("") +
                                scale_x_discrete(labels = c("Non-Urban (n = 40)", "Urban (n = 109)"))
plotLDAAllPop.UorNwRep


fig.lda.allpops = ggarrange(plotLDAAllPop.location, ggarrange(plotLDAAllPop.UorN, 
                                                              plotLDAAllPop.UorNwRep, ncol = 2,
                                                              labels = c("B", "C")),
                                                               nrow = 2, labels = "A") 

fig.lda.allpops = annotate_figure(fig.lda.allpops,top = text_grob(
  "Linear Discriminant Analysis - All Populations", 
  color = "black", face = "bold", size = 14))
fig.lda.allpops = annotate_figure(fig.lda.allpops, fig.lab = "Angeles n = 13 \n James n = 5 \n Oxy n = 28 \n Stunt n = 3 \n UCLA n = 57 \n UCSB n = 7 \n UCSD n = 17", 
                                  fig.lab.pos = "top.right", fig.lab.face = "italic", fig.lab.size = 10)
fig.lda.allpops


#noSmall LDA
plotLDA.noSmall.location = ggplot(data = LDJuncos.noSmall, aes(x = LD1, y = LD2, colour = Location)) +
  stat_ellipse(aes(x=LD1, y=LD2,color=Location, group=Location),type = "norm") +
  theme() + geom_point() + theme_classic() + ggtitle("By Location")
plotLDA.noSmall.location

plotLDA.noSmall.UorN = ggplot(data = LDJuncos2.noSmall, aes(x = UrbanorNot, 
                              y = LD1)) + geom_boxplot() + 
                              ggtitle("By Urban or Non-Urban") + xlab("") +
                              scale_x_discrete(labels = c("Non-Urban (n = 13)", "Urban (n = 102)"))
plotLDA.noSmall.UorN

plotLDA.noSmall.UorNwRep = ggplot(data = LDJuncos4.noSmall, aes(x = UrbanorNot, y = LD1)) + 
                          geom_boxplot() + ggtitle("With Repository") + xlab("") +
                          scale_x_discrete(labels = c("Non-Urban (n = 13)", "Urban (n = 102)"))
plotLDA.noSmall.UorNwRep 

fig.lda.noSmall = ggarrange(plotLDA.noSmall.location, ggarrange(plotLDA.noSmall.UorN, 
                                                              plotLDA.noSmall.UorNwRep, ncol = 2,
                                                              labels = c("B", "C")),
                                                               nrow = 2, labels = "A") 

fig.lda.noSmall = annotate_figure(fig.lda.noSmall,top = text_grob("Linear Discriminant Analysis - Populations n>10", 
                              color = "black", face = "bold", size = 14))
fig.lda.noSmall = annotate_figure(fig.lda.noSmall, fig.lab = "Angeles n = 13 \n Oxy n = 28 \n UCLA n = 57 \n UCSD n = 17", 
                                  fig.lab.pos = "top.right", fig.lab.face = "italic", fig.lab.size = 10)
fig.lda.noSmall

#LDA SD
library(ggplot2)
fig.LDA.SD = ggplot(data = LD.SDJuncos, aes(x = Location, y = LD1)) + geom_boxplot() + xlab("") +
  scale_x_discrete(labels = c("UCSD 2018-2020 \n n = 17", "UCSD 2006/2007 \n n = 101"))
fig.LDA.SD = annotate_figure(fig.LDA.SD,top = text_grob("UCSD Comparison", 
                                                                  color = "black", size = 14))
fig.LDA.SD

library(ggpubr)
UCSD.compare = ggplot(data = LD.SD.all.Juncos, aes(x = Location, y = LD1)) + geom_boxplot() + 
  scale_x_discrete(labels = c("Angeles \n n = 13", "Oxy \n n = 28", "UCLA \n n = 57", "UCSD 2006/2007 \n n = 101")) + xlab("")
UCSD.compare = annotate_figure(UCSD.compare,top = text_grob("UCSD 2006/2007 to Los Angeles Sites", 
                                                        color = "black", size = 14))
UCSD.compare

fig.SD = ggarrange(fig.LDA.SD,UCSD.compare, ncol = 1,  nrow = 2, labels = c("A","B")) 
fig.SD = annotate_figure(fig.SD,top = text_grob("Linear Discriminant Analysis - UCSD 2006/2007", 
                                                        color = "black", face = "bold", size = 14))

fig.SD

