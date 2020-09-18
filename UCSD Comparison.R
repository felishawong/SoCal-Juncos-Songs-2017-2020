#LinearDiscriminantAnalysis

library("MASS")
lda.SD = lda(justSD$Location ~ justSD$TrillRate + justSD$minF
                 + justSD$maxF + justSD$bandwidthF)

lda.SD

#Coefficients of linear discriminants:
#  LD1
#justSD$TrillRate  -0.14778995
#justSD$minF        1.46435752
#justSD$maxF        0.71489097
#justSD$bandwidthF -0.02195636

LDA.SD.values = predict(lda.SD, justSD)
LDIndi.SD = as.data.frame(LDA.SD.values$x)
LDIndi.SD$Observation = 1:nrow(LDIndi.SD)
justSD$Observation = 1:nrow(justSD)

library(ggplot2)

LD.SDJuncos <- merge(justSD, LDIndi.SD, by = "Observation")

ggplot(data = LD.SDJuncos, aes(x = Location, y = LD1)) + geom_boxplot()


glm.SD <- glm(LD.SDJuncos$LD1~LD.SDJuncos$Location)
summary(glm.SD)
glm.SD
#AIC = 529
library(emmeans)
pairs(emmeans(glm.SD, "Location"))
#p-value = 0.0061*

resid.SD = as.data.frame(glm.SD$residuals)
View(resid.SD)
resid.SD$Observation = 1:nrow(resid.SD)
resid.SDJuncos <- merge(justSD, resid.SD, by = "Observation")
View(resid.SDJuncos)

ggplot(data = resid.SDJuncos, aes(x = Location, y = resid.SDJuncos$`glm.SD$residuals`,
                                        colour = Location)) + geom_point()

bartlett.test(resid.SDJuncos$`glm.SD$residuals`~ resid.SDJuncos$Location)
#p-value = 0.3799

#Manova

song.man.SD = manova(cbind(TrillRate, minF, maxF, bandwidthF) ~ Location, data = justSD)
summary(song.man.SD)
summary.aov(song.man.SD)

resid.SD.man = as.data.frame(song.man.SD$residuals)
View(resid.SD.man)
resid.SD.man$Observation = 1:nrow(resid.SD.man)
names(resid.SD.man)[names(resid.SD.man) == "TrillRate"] <- "TrillRate.man"
names(resid.SD.man)[names(resid.SD.man) == "minF"] <- "minF.man"
names(resid.SD.man)[names(resid.SD.man) == "maxF"] <- "maxF.man"
names(resid.SD.man)[names(resid.SD.man) == "bandwidthF"] <- "bandF.man"


resid.SDJuncos.man <- merge(justSD, resid.SD.man, by = "Observation")
View(resid.SDJuncos.man)

ggplot(data = resid.SDJuncos.man, aes(x = Location, y = resid.SDJuncos.man$TrillRate.man,
                                  colour = Location)) + geom_point()
ggplot(data = resid.SDJuncos.man, aes(x = Location, y = resid.SDJuncos.man$minF.man,
                                      colour = Location)) + geom_point()
ggplot(data = resid.SDJuncos.man, aes(x = Location, y = resid.SDJuncos.man$maxF.man,
                                      colour = Location)) + geom_point()
ggplot(data = resid.SDJuncos.man, aes(x = Location, y = resid.SDJuncos.man$bandF.man,
                                      colour = Location)) + geom_point()

bartlett.test(resid.SDJuncos.man$TrillRate.man~ resid.SDJuncos$Location)
bartlett.test(resid.SDJuncos.man$minF.man~ resid.SDJuncos$Location)
bartlett.test(resid.SDJuncos.man$maxF.man~ resid.SDJuncos$Location)
bartlett.test(resid.SDJuncos.man$bandF.man~ resid.SDJuncos$Location)

#TrillRate p-value = 0.01334
#minF p-value = 0.1762
#maxF p-value = 0.07356
#bandF p-value = 0.3744

#TrillRate Transform for MANOVA
justSD$TrillRate.transform = sign(justSD$TrillRate)*abs(justSD$TrillRate)^(1/3)
View(justSD)

resid.SD.man.TRtransform = manova(cbind(TrillRate.transform, minF, maxF, bandwidthF) 
                                  ~ Location, data = justSD)
summary(resid.SD.man.TRtransform)
summary.aov(resid.SD.man.TRtransform)

resid.SD.man.TRtransform = as.data.frame(resid.SD.man.TRtransform$residuals)
View(resid.SD.man.TRtransform)

resid.SD.man.TRtransform$Observation = 1:nrow(resid.SD.man.TRtransform)
names(resid.SD.man.TRtransform)[names(resid.SD.man.TRtransform) == "TrillRate"] <- "TrillRate.man.transform"
names(resid.SD.man.TRtransform)[names(resid.SD.man.TRtransform) == "minF"] <- "minF.man"
names(resid.SD.man.TRtransform)[names(resid.SD.man.TRtransform) == "maxF"] <- "maxF.man"
names(resid.SD.man.TRtransform)[names(resid.SD.man.TRtransform) == "bandwidthF"] <- "bandF.man"


resid.SDJuncos.man.TRtransform <- merge(justSD, resid.SD.man.TRtransform, by = "Observation")
View(resid.SDJuncos.man.TRtransform)
bartlett.test(resid.SDJuncos.man.TRtransform$TrillRate.transform.y ~ resid.SDJuncos.man.TRtransform$Location)
#p-value = 0.008696

#By Trait Analysis
justSD.trait = justSD

#TrillRate

install.packages("nlme")
library(nlme)

glm.SD.TR = gls(justSD.trait$TrillRate ~ justSD.trait$Location)
summary(glm.SD.TR)
pairs(emmeans(glm.SD.TR, "Location"))

resid.SD.TR = as.data.frame(glm.SD.TR$residuals)
resid.SD.TR$Observation = 1:nrow(resid.SD.TR)
justSD.trait$Observation = 1:nrow(justSD.trait)
resid.SDJuncos.TR <- merge(justSD.trait, resid.SD.TR, by = "Observation")
View(resid.SDJuncos)

ggplot(data = resid.SDJuncos.TR, aes(x = Location, y = resid.SDJuncos.TR$`glm.SD.TR$residuals`,
                                     colour = Location)) + geom_point()

bartlett.test(resid.SDJuncos.TR$`glm.SD.TR$residuals`~ resid.SDJuncos.TR$Location)
#p-value = 0.01334

#TrillRAte Transform

install.packages("SciViews")
install.packages("emmeans")
install.packages("car")
install.packages("glmm")
library(SciViews)
library(emmeans)
library(car)
library(glmm)
library(MASS)

justSD.trait$TRZ = justSD.trait$TrillRate
glm.SD.TR2 = rlm(justSD.trait$TRZ ~ justSD.trait$Location)
summary(glm.SD.TR2)
rob.pvals(glm.SD.TR2)
pairs(emmeans(glm.SD.TR2, "Location"))

resid.SD.TR2 = as.data.frame(glm.SD.TR2$residuals)
resid.SD.TR2$Observation = 1:nrow(resid.SD.TR2)
justSD.trait$Observation = 1:nrow(justSD.trait)
resid.SDJuncos.TR2 <- merge(justSD.trait, resid.SD.TR2, by = "Observation")

ggplot(data = resid.SDJuncos.TR2, aes(x = Location, y = resid.SDJuncos.TR2$`glm.SD.TR2$residuals`,
                                     colour = Location)) + geom_point()

bartlett.test(resid.SDJuncos.TR2$`glm.SD.TR2$residuals`~ resid.SDJuncos.TR2$Location)
#p-value = 0.01334

#minF
glm.SD.minF = glm(justSD.trait$minF ~ justSD.trait$Location)
summary(glm.SD.minF)
pairs(emmeans(glm.SD.minF, "Location"))

resid.SD.minF = as.data.frame(glm.SD.minF$residuals)
resid.SD.minF$Observation = 1:nrow(resid.SD.minF)
justSD.trait$Observation = 1:nrow(justSD.trait)
resid.SDJuncos.minF <- merge(justSD.trait, resid.SD.minF, by = "Observation")
View(resid.SDJuncos)

ggplot(data = resid.SDJuncos.minF, aes(x = Location, y = resid.SDJuncos.minF$`glm.SD.minF$residuals`,
                                     colour = Location)) + geom_point()

bartlett.test(resid.SDJuncos.minF$`glm.SD.minF$residuals`~ resid.SDJuncos.minF$Location)
#p-value = 0.1762

#maxF
glm.SD.maxF = glm(justSD.trait$maxF ~ justSD.trait$Location)
summary(glm.SD.maxF)
pairs(emmeans(glm.SD.maxF, "Location"))

resid.SD.maxF = as.data.frame(glm.SD.maxF$residuals)
resid.SD.maxF$Observation = 1:nrow(resid.SD.maxF)
justSD.trait$Observation = 1:nrow(justSD.trait)
resid.SDJuncos.maxF <- merge(justSD.trait, resid.SD.maxF, by = "Observation")
View(resid.SDJuncos)

ggplot(data = resid.SDJuncos.maxF, aes(x = Location, y = resid.SDJuncos.maxF$`glm.SD.maxF$residuals`,
                                       colour = Location)) + geom_point()

bartlett.test(resid.SDJuncos.maxF$`glm.SD.maxF$residuals`~ resid.SDJuncos.maxF$Location)
#p-value = 0.07356

#bandF
glm.SD.bandF = glm(justSD.trait$bandwidthF ~ justSD.trait$Location)
summary(glm.SD.bandF)
pairs(emmeans(glm.SD.bandF, "Location"))

resid.SD.bandF = as.data.frame(glm.SD.bandF$residuals)
resid.SD.bandF$Observation = 1:nrow(resid.SD.bandF)
justSD.trait$Observation = 1:nrow(justSD.trait)
resid.SDJuncos.bandF <- merge(justSD.trait, resid.SD.bandF, by = "Observation")

ggplot(data = resid.SDJuncos.bandF, aes(x = Location, y = resid.SDJuncos.bandF$`glm.SD.bandF$residuals`,
                                       colour = Location)) + geom_point()

bartlett.test(resid.SDJuncos.bandF$`glm.SD.bandF$residuals`~ resid.SDJuncos.bandF$Location)
#p-value = 0.3744

#UCSD2006/2007 compared to all others, LDA

lda.SD.all = lda(noRep$Location ~ noRep$TrillRate + noRep$minF
             + noRep$maxF + noRep$bandwidthF)

lda.SD.all

#Coefficients of linear discriminants:
#  LD1        LD2          LD3
#noRep$TrillRate  -0.07692573  0.2944108 -0.007407475
#noRep$minF       -1.18191191 -0.7719985 -1.016825206
#noRep$maxF       -1.00867626 -0.2603140  0.348975630
#noRep$bandwidthF -0.21201120  0.1341146  0.649868952

#Proportion of trace:
#  LD1    LD2    LD3 
#0.7926 0.1371 0.0703 

LDA.SD.all.values = predict(lda.SD.all, noRep)
LDIndi.SD.all = as.data.frame(LDA.SD.all.values$x)
LDIndi.SD.all$Observation = 1:nrow(LDIndi.SD.all)
noRep$Observation = 1:nrow(noRep)

LD.SD.all.Juncos <- merge(noRep, LDIndi.SD.all, by = "Observation")

LD.SD.all.Juncos

LD.SD.all.Juncos = subset(LD.SD.all.Juncos, Location == "UCSD 2006/2007" | Location == "ANGELES" | Location == "UCLA" | Location == "OXY")
LD.SD.all.Juncos$Location = as.character(LD.SD.all.Juncos$Location)
LD.SD.all.Juncos$Location = as.factor(LD.SD.all.Juncos$Location)
levels(LD.SD.all.Juncos$Location)

UCSD.compare = ggplot(data = LD.SD.all.Juncos, aes(x = Location, y = LD1)) + geom_boxplot() + 
  scale_x_discrete(labels = c("Angeles \n n = 13", "Oxy \n n = 28", "UCLA \n n = 57", "UCSD 2006/2007 \n n = 101")) + xlab("")
UCSD.compare = annotate_figure(UCSD.compare,top = text_grob("Linear Discriminant Analysis - UCSD 2006/2007 to Los Angeles Sites", 
                                                                  color = "black", face = "bold", size = 14))
UCSD.compare

glm.SD.all <- glm(LD.SD.all.Juncos$LD1~LD.SD.all.Juncos$Location)
summary(glm.SD.all)
#AIC = 855.58

pairs(emmeans(glm.SD.all, "Location"))

resid.SD.all = as.data.frame(glm.SD.all$residuals)
View(resid.SD.all)
resid.SD.all$Observation = 1:nrow(resid.SD.all)
resid.SD.all.Juncos <- merge(noRep, resid.SD.all, by = "Observation")
View(resid.SD.all.Juncos)

ggplot(data = resid.SD.all.Juncos, aes(x = Location, y = resid.SD.all.Juncos$`glm.SD.all$residuals`,
                                  colour = Location)) + geom_point()

bartlett.test(resid.SD.all.Juncos$`glm.SD.all$residuals`~ resid.SD.all.Juncos$Location)
#p-value = 0.1133

glm.trait.SD.all.TR = glm(noRep$TrillRate ~ noRep$Location)
pairs(emmeans(glm.trait.SD.all.TR, "Location"))

glm.trait.SD.all.minF = glm(noRep$minF ~ noRep$Location)
pairs(emmeans(glm.trait.SD.all.minF, "Location"))

glm.trait.SD.all.maxF = glm(noRep$maxF ~ noRep$Location)
pairs(emmeans(glm.trait.SD.all.maxF, "Location"))

glm.trait.SD.all.bandF = glm(noRep$bandwidthF ~ noRep$Location)
pairs(emmeans(glm.trait.SD.all.bandF, "Location"))


