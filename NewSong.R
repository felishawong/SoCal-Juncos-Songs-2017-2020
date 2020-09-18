#clean environment
rm(list=ls())
graphics.off()

#set working directory
setwd("~/Documents")

JuncoSongs = read.csv("JUNCOSJUNCOS.csv")
View(JuncoSongs)

UniqueIDS = unique(JuncoSongs[,1])

install.packages("lubridate")
library("lubridate")

IndividualJuncos = data.frame(BirdIDS = numeric(), LocationID = numeric(), Location = character(),
                              UrbanID = numeric(), UrbanorNot = character(), 
                              SongBoutNum = numeric(), SongLength = numeric(), 
                              TrillRate = numeric(), 
                              minF = numeric(), 
                              maxF = numeric(), bandwidthF = numeric(), 
                              CityorMountain = character(), Date = numeric(), Time = numeric(),
                              DistRoad = numeric(), 
                              Lat = numeric(), Long = numeric())
                              
for(index in 1:length(UniqueIDS))
{
  BirdID = UniqueIDS[index]
  Juncos = JuncoSongs[JuncoSongs$BirdID == BirdID,]
  
  Location = Juncos$Location[1]
  LocationID = Juncos$LocationID[1]
  
  UrbanID = Juncos$UrbanID[1]
  UrbanorNot = Juncos$UrbanorNot[1]
  CityorMountainLump = Juncos$CityorMountainLump[1]
  
  SongBoutNum = nrow(Juncos)
  
  SongLength = mean(Juncos$SongLength)
  TrillRate = mean(Juncos$TrillRate)
  minF = mean(Juncos$minF)/1000
  maxF = mean(Juncos$maxF)/1000
  bandwidthF = mean(Juncos$maxF)/1000 - mean(Juncos$minF)/1000
  
  Date = median(Juncos$Date.1)
  Time = mean(as.numeric(hm(Juncos$Time)))
  DistRoad = mean(Juncos$DistRoad)
  Lat = mean(Juncos$Latitude)
  Long = mean(Juncos$Longitude)
  
  row = data.frame(BirdID, LocationID, Location, UrbanID, UrbanorNot, 
                   SongBoutNum, SongLength, TrillRate, 
                   minF, maxF, bandwidthF, CityorMountainLump, Date, Time, DistRoad, Lat, Long)
  IndividualJuncos = rbind(IndividualJuncos, row)
  
}

UniqueLocation = unique(IndividualJuncos[,2])

LocationJuncos = data.frame(LocationID = numeric(), Location = character(), NumIndividuals = numeric(),
                            UrbanID = numeric(), UrbanorNot = character(),
                            SongBoutNum = numeric(),
                            SongLength = numeric(), 
                            TrillRate = numeric(), 
                            minF = numeric(), 
                            maxF = numeric(), bandF = numeric() 
                            )

for(indexLOC in 1:length(UniqueLocation))
{
  LocationIDLOC= UniqueLocation[indexLOC]
  #View(LocationIDLOC)
  JuncosLOC = IndividualJuncos[IndividualJuncos$LocationID == LocationIDLOC,]
  #View(JuncosLOC)
  
  LocationLOC = JuncosLOC$Location[1]
  #View(LocationLOC)
  NumIndividuals= nrow(JuncosLOC)
  #View(NumIndividuals)
  
  UrbanIDLOC = JuncosLOC$UrbanID[1]
  #View(UrbanIDLOC)
  UrbanorNotLOC = JuncosLOC$UrbanorNot[1]
  #View(UrbanorNotLOC)
  
  SongBoutNumLOC = mean(JuncosLOC$SongBoutNum)
  SongLengthLOC = mean(JuncosLOC$SongLength)
  TrillRateLOC = mean(JuncosLOC$TrillRate)
  minFLOC = mean(JuncosLOC$minF)
  maxFLOC = mean(JuncosLOC$maxF)
  bandFLOC = mean(JuncosLOC$bandwidthF)
  
  rowLOC = data.frame(LocationIDLOC, LocationLOC, NumIndividuals,
                      UrbanIDLOC, UrbanorNotLOC,
                      SongBoutNumLOC,
                      SongLengthLOC, 
                      TrillRateLOC, 
                      minFLOC, 
                      maxFLOC, bandFLOC)
  LocationJuncos = rbind(LocationJuncos, rowLOC)
  
}

View(LocationJuncos)
View(IndividualJuncos)
#dataset that excludes any population with a sample size of N<10 (UCSB, Stunt, James)

NoSmallPop = subset(IndividualJuncos, Location != "UCSB")
NoSmallPop = subset(NoSmallPop, Location != "STUNT")
NoSmallPop = subset(NoSmallPop, Location != "JAMES")
NoSmallPop$Location <- as.character(NoSmallPop$Location)
NoSmallPop$Location <- as.factor(NoSmallPop$Location)
levels(NoSmallPop$Location)

#datasets for each location

UCLAJuncos = IndividualJuncos[IndividualJuncos$LocationID == 1,]
UCSBJuncos = IndividualJuncos[IndividualJuncos$LocationID == 2,]
UCSDJuncos = IndividualJuncos[IndividualJuncos$LocationID == 3,]
OXYJuncos = IndividualJuncos[IndividualJuncos$LocationID == 4,]
STUNTJuncos = IndividualJuncos[IndividualJuncos$LocationID == 5,]
JAMESJuncos = IndividualJuncos[IndividualJuncos$LocationID == 6,]
ANGELESJuncos = IndividualJuncos[IndividualJuncos$LocationID == 7,]
REPOSITORYJuncos = IndividualJuncos[IndividualJuncos$LocationID == 8,]
oldUCSDJuncos = IndividualJuncos[IndividualJuncos$LocationID == 9,]

#Linear Discriminant Analysis of all locations without UCSD 2006/2007
library(MASS)

noOLD = subset(IndividualJuncos, Location != "UCSD 2006/2007")
noOLD$Location <- as.character(noOLD$Location)
noOLD$Location <- as.factor(noOLD$Location)
levels(noOLD$Location)
View(noOLD)

LDAJuncos = lda(noOLD$Location ~ noOLD$SongLength 
                + noOLD$TrillRate + noOLD$minF
                + noOLD$maxF + noOLD$bandwidthF)

LDAJuncos

#The differences across locations is mostly explained by song length.
#Coefficients of linear discriminants:
#LD1         LD2         LD3         LD4
#noOLD$SongLength -2.67091610  0.60626657 -0.11854163 -3.19172698
#noOLD$TrillRate  -0.09535273 -0.30553613  0.01313872  0.04918375
#noOLD$minF        0.08984755  0.49492571  1.39397633 -0.29618201
#noOLD$maxF       -0.94962351  0.44969091  0.90878841  0.74348566
#noOLD$bandwidthF -0.68687257 -0.06864584 -0.42732186  0.70328680

#Proportion of trace:
#  LD1    LD2    LD3    LD4 
#0.5480 0.2865 0.0886 0.0769 

#Adds column to noOLD titled observations with consecutive values
noOLD$Observation = 1:nrow(noOLD)

#Gives an LDA value for every individual bird
LDAJuncos.values = predict(LDAJuncos, noOLD)
LDIndi = as.data.frame(LDAJuncos.values$x)
LDIndi$Observation = 1:nrow(LDIndi)

#When using merge function, must declare a column that is in both dataframes to combine by
LDJuncos <- merge(noOLD, LDIndi, by = "Observation")

View(LDJuncos)
summary(LDJuncos)
library(ggplot2)

#Plots LD1 by LD2 for every bird, color coded by location
ggplot(data = LDJuncos, aes(x = LD1, y = LD2, colour = Location)) +
  stat_ellipse(aes(x=LD1, y=LD2,color=Location, group=Location),type = "norm") +
        theme() + geom_point() + theme_classic()

#When all the ovals are overlapping, they are similar!

library(emmeans)

#GLM with all factors. Lower the AIC score = the better the model, get rid of highest p-value
glm.LD <- glm(LDJuncos$LD1~LDJuncos$Location + LDJuncos$Date + LDJuncos$Time +
                LDJuncos$DistRoad + LDJuncos$Lat + LDJuncos$Long)
summary(glm.LD)

#AIC = 308.4*********************
#Longitude p-value = 0.0447
#James p-value = 0.0375
#UCLA p-value = 0.0485
#UCSB p-value = 0.0362

pairs(emmeans(glm.LD, "Location"))

glm.LD2 <- glm(LDJuncos$LD1~LDJuncos$Location + LDJuncos$Date +
                LDJuncos$DistRoad + LDJuncos$Lat + LDJuncos$Long)
summary(glm.LD2)
#AIC = 380.81

glm.LD3 <- glm(LDJuncos$LD1~LDJuncos$Location + LDJuncos$Date +
                 LDJuncos$Lat + LDJuncos$Long)
summary(glm.LD3)
#AIC = 379.4

glm.LD4 <- glm(LDJuncos$LD1~LDJuncos$Location + LDJuncos$Date +
                 LDJuncos$Long)
summary(glm.LD4)
#AIC = 377.91
#Longitude p-value = 0.0494

glm.LD5 <- glm(LDJuncos$LD1~LDJuncos$Location +
                 LDJuncos$Long)
summary(glm.LD5)
#AIC = 377.42

glm.LD6 <- glm(LDJuncos$LD1~LDJuncos$Location)
summary(glm.LD6)
#AIC = 432.62
#James p-value = 0.0324

Resid = as.data.frame(glm.LD$residuals)
View(Resid)
Resid$BirdID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,24,25,26,27,28,29,30,31,
                 32,33,34,35,36,38,39,40,41,42,44,45,46,48,49,50,55,57,58,59,60,61,62,63,64,65,
                 66,68,69,70,71,72,73,74,77,78,79,80,81,82,83,85,86,87,88,89,90,91,92,93,94,95,
                 96,97,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,116,117,118,
                 119,120,131,132,133,153,154)
                 
ResidJuncos <- merge(noOLD, Resid, by = "BirdID")
View(ResidJuncos)


ggplot(data = ResidJuncos, aes(x = Location, y = ResidJuncos$`glm.LD$residuals`, colour = Location)) + geom_point()

bartlett.test(ResidJuncos$`glm.LD$residuals`~ResidJuncos$Location)
#p-value = 0.5723
#There is equal variances; no differences in variance.

#UrbanvsNotUrban
noOLD2 = noOLD
View(noOLD2)

LDAJuncos2 = lda(noOLD2$UrbanorNot ~ noOLD2$SongLength 
                + noOLD2$TrillRate + noOLD2$minF
                + noOLD2$maxF + noOLD2$bandwidthF)

View(LDAJuncos2)
LDAJuncos2

LDAJuncos2.values = predict(LDAJuncos2, noOLD2)
LDIndi2 = as.data.frame(LDAJuncos2.values$x)
LDIndi2$Observation = 1:nrow(LDIndi2)

#When using merge function, must declare a column that is in both dataframes to combine by
LDJuncos2 <- merge(noOLD2, LDIndi2, by = "Observation")

ggplot(data = LDJuncos2, aes(x = UrbanorNot, y = LD1)) + geom_boxplot()

#Coefficients of linear discriminants:
#LD1
#noOLD2$SongLength -2.21520977
#noOLD2$TrillRate  -0.04215192
#noOLD2$minF        0.03537047
#noOLD2$maxF        1.13661183
#noOLD2$bandwidthF  0.71696159

glm2.LD <- glm(LDJuncos2$LD1~LDJuncos2$UrbanorNot + LDJuncos2$Date + LDJuncos2$Time +
                LDJuncos2$DistRoad + LDJuncos2$Lat + LDJuncos2$Long)
summary(glm2.LD)
#AIC = 325.61

glm2.LD2 <- glm(LDJuncos2$LD1~LDJuncos2$UrbanorNot + LDJuncos2$Time +
                 LDJuncos2$DistRoad + LDJuncos2$Lat + LDJuncos2$Long)
summary(glm2.LD2)
#AIC = 323.62

glm2.LD3 <- glm(LDJuncos2$LD1~LDJuncos2$UrbanorNot + LDJuncos2$Time +
                 LDJuncos2$Lat + LDJuncos2$Long)
summary(glm2.LD3)
#AIC = 321.64

glm2.LD4 <- glm(LDJuncos2$LD1~LDJuncos2$UrbanorNot + LDJuncos2$Time +
                  LDJuncos2$Lat)
summary(glm2.LD4)
glm2.LD4
#AIC = 319.79**************************

pairs(emmeans(glm2.LD4, "UrbanorNot"))

glm2.LD5 <- glm(LDJuncos2$LD1~LDJuncos2$UrbanorNot + LDJuncos2$Time)
summary(glm2.LD5)
#AIC = 374.12

glm2.LD6 <- glm(LDJuncos2$LD1~LDJuncos2$UrbanorNot)
summary(glm2.LD6)
#AIC = 426.83

pairs(emmeans(glm2.LD6, "UrbanorNot"))


ResidU = as.data.frame(glm2.LD4$residuals)
ResidU$BirdID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,24,25,26,27,28,29,30,31,
                  32,33,34,35,36,38,39,40,41,42,44,45,46,48,49,50,55,57,58,59,60,61,62,63,64,65,
                  66,68,69,70,71,72,73,74,77,78,79,80,81,82,83,85,86,87,88,89,90,91,92,93,94,95,
                  96,97,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,116,117,118,
                  119,120,131,132,133,153,154)
View(ResidU)
ResidUJuncos <- merge(noOLD2, ResidU, by = "BirdID")
View(ResidUJuncos)

ggplot(data = ResidUJuncos, aes(x = UrbanorNot, y = ResidUJuncos$`glm2.LD4$residuals`,
                                colour = UrbanorNot)) + geom_point()

bartlett.test(ResidUJuncos$`glm2.LD4$residuals`~ ResidUJuncos$UrbanorNot)
#p-value = 0.6485

#CityorMountainLump
noOLD3 = noOLD
View(noOLD3)

LDAJuncos3 = lda(noOLD3$CityorMountainLump ~ noOLD3$SongLength 
                 + noOLD3$TrillRate + noOLD3$minF
                 + noOLD3$maxF + noOLD3$bandwidthF)

View(LDAJuncos3)
LDAJuncos3

LDAJuncos3.values = predict(LDAJuncos3, noOLD3)
LDIndi3 = as.data.frame(LDAJuncos3.values$x)
LDIndi3$Observation = 1:nrow(LDIndi3)

#When using merge function, must declare a column that is in both dataframes to combine by
LDJuncos3 <- merge(noOLD3, LDIndi3, by = "Observation")
View(LDJuncos3)

ggplot(data = LDJuncos3, aes(x = LD1, y = LD2, colour = CityorMountainLump)) +
  stat_ellipse(aes(x=LD1, y=LD2,color=CityorMountainLump, group=CityorMountainLump),type = "norm") +
  theme() + geom_point() + theme_classic()

#Coefficients of linear discriminants:
#  LD1        LD2        LD3         LD4
#noOLD3$SongLength  1.9438604 -0.7977167 -3.1721708 -1.74322931
##noOLD3$TrillRate   0.2380409 -0.1319125  0.1606510  0.07544193
#noOLD3$minF       -0.5506185 -0.2753086  0.2450886 -1.38207783
#noOLD3$maxF        0.4229659  0.8384492  0.6055988 -1.07312518
#noOLD3$bandwidthF  0.6766800  0.7544818  0.2257207  0.28155403

#Proportion of trace:
#  LD1    LD2    LD3    LD4 
#0.6778 0.1757 0.0995 0.0470 

glm3.LD <- glm(LDJuncos3$LD1~LDJuncos3$CityorMountainLump + LDJuncos3$Date + LDJuncos3$Time +
                 LDJuncos3$DistRoad + LDJuncos3$Lat + LDJuncos3$Long)
summary(glm3.LD)
#AIC = 317.61**********
pairs(emmeans(glm3.LD, "CityorMountainLump"))

glm3.LD2 <- glm(LDJuncos3$LD1~LDJuncos3$CityorMountainLump + LDJuncos3$Date +
                 LDJuncos3$DistRoad + LDJuncos3$Lat + LDJuncos3$Long)
summary(glm3.LD2)
#AIC = 368.8
#Longitude p-value = 0.04011
#Intercept p-value = 0.02793
#UCLA p-value = 0.03525
#UCSB p-value = 0.00978

glm3.LD3 <- glm(LDJuncos3$LD1~LDJuncos3$CityorMountainLump +
                  LDJuncos3$DistRoad + LDJuncos3$Lat + LDJuncos3$Long)
summary(glm3.LD3)
#AIC = 368.28
#Longitude p-value - 0.04904
#Intercept p-value = 0.04145
#UCLA p-value = 0.03821
#UCSB p-value = 0.00939

glm3.LD4 <- glm(LDJuncos3$LD1~LDJuncos3$CityorMountainLump +
                   LDJuncos3$Lat + LDJuncos3$Long)
summary(glm3.LD4)
#AIC = 367.95
#Longitude p-value - 0.03129
#Intercept p-value = 0.01448
#UCLA p-value = 0.04191
#UCSB p-value = 0.00673

glm3.LD5 <- glm(LDJuncos3$LD1~LDJuncos3$CityorMountainLump +
                  LDJuncos3$Long)
summary(glm3.LD5)
#AIC = 368.05
#Intercept p-value = 0.0444
#UCSB p-value = 0.0196
#UCSD p-value = 0.0210
#Longitude p-value = 0.0439

glm3.LD6 <- glm(LDJuncos3$LD1~LDJuncos3$CityorMountainLump)
summary(glm3.LD6)
#AIC = 429.76
#Longitude p-value = 0.0346

ResidCITY = as.data.frame(glm3.LD$residuals)
ResidCITY$BirdID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,24,25,26,27,28,29,30,31,
                     32,33,34,35,36,38,39,40,41,42,44,45,46,48,49,50,55,57,58,59,60,61,62,63,64,65,
                     66,68,69,70,71,72,73,74,77,78,79,80,81,82,83,85,86,87,88,89,90,91,92,93,94,95,
                     96,97,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,116,117,118,
                     119,120,131,132,133,153,154)
View(ResidCITY)

ResidCITYJuncos <- merge(noOLD3, ResidCITY, by = "BirdID")
View(ResidCITYJuncos)

ggplot(data = ResidCITYJuncos, aes(x = CityorMountainLump, y = ResidCITYJuncos$`glm3.LD$residuals`,
                                colour = CityorMountainLump)) + geom_point()

bartlett.test(ResidCITYJuncos$`glm3.LD$residuals`~ResidCITYJuncos$CityorMountainLump)
#p-value = 0.3351

#UCSD old to all other populations, can't use song length by Location
noSL.location = IndividualJuncos[,-7]

LDAnoSL.location = lda(noSL$Location ~ noSL$TrillRate + noSL$minF + noSL$maxF + noSL$bandwidthF)
LDAnoSL.location

#LD1        LD2          LD3
#noSL$TrillRate  -0.07868612  0.2993698 -0.005668903
#noSL$minF       -1.09393396 -0.7593690 -1.032759298
#noSL$maxF       -1.01955835 -0.2806219  0.317312809
#noSL$bandwidthF -0.23243974  0.1342000  0.666174646

#Proportion of trace:
#  LD1    LD2    LD3 
#0.7886 0.1393 0.0721 

noSL.location$Observation = 1:nrow(noSL.location)

LDAnoSL.location.values = predict(LDAnoSL.location, noSL)
LDComb.location = as.data.frame(LDAnoSL.location.values$x)
LDComb.location$Observation = 1:nrow(LDComb.location)

LDJuncosnoSL.location <- merge(noSL.location, LDComb.location, by = "Observation")

ggplot(data = LDJuncosnoSL.location, aes(x = LD1, y = LD2, colour = Location)) +
  stat_ellipse(aes(x=LD1, y=LD2,color=Location, group=Location),type = "norm") +
  theme() + geom_point() + theme_classic()


glmSD.LD6 <- glm(LDJuncosnoSL.location$LD1~LDJuncosnoSL.location$Location)
summary(glmSD.LD6)
#AIC = 910.48
#UCSD 2006/2007 p-value = 0.000352

location.SD = as.data.frame(pairs(emmeans(glmSD.LD6, "Location")))
location.SD=location.SD[,-2]
location.SD=location.SD[,-2]
location.SD=location.SD[,-2]
location.SD=location.SD[,-2]
View(location.SD)

ResidSD = as.data.frame(glmSD.LD6$residuals)
ResidSD$BirdID = noSL$BirdID
View(ResidSD)

ResidSDJuncos <- merge(noSL, ResidSD)
View(ResidSDJuncos)

ggplot(data = ResidSDJuncos, aes(x = Location, y = ResidSDJuncos$`glmSD.LD6$residuals`,
                                   colour = Location)) + geom_point()

bartlett.test(ResidSDJuncos$`glmSD.LD6$residuals`~ResidSDJuncos$Location)
#p-value = 0.07814

#UCSD old to all other populations, can't use song length by UrbanorNot
noSL.UorN = IndividualJuncos[,-7]

LDAnoSL.UorN = lda(noSL$UrbanorNot ~ noSL$TrillRate + noSL$minF + noSL$maxF + noSL$bandwidthF)

LDAnoSL.UorN

#Coefficients of linear discriminants:
#  LD1
#noSL$TrillRate  0.01609527
#noSL$minF       0.96777336
#noSL$maxF       1.03609725
#noSL$bandwidthF 0.32763359

noSL.UorN$Observation = 1:nrow(noSL.UorN)

LDAnoSL.UorN.values = predict(LDAnoSL.UorN, noSL.UorN)
LDComb.UorN = as.data.frame(LDAnoSL.UorN.values$x)
LDComb.UorN$Observation = 1:nrow(LDComb.UorN)

LDJuncosnoSL.UorN<- merge(noSL.UorN, LDComb.UorN, by = "Observation")

ggplot(data = LDJuncosnoSL.UorN, aes(x = UrbanorNot, y = LD1)) + geom_boxplot()

glmSD.LD.UorN <- glm(LDJuncosnoSL.UorN$LD1~LDJuncosnoSL.UorN$UrbanorNot)
summary(glmSD.LD.UorN)

pairs(emmeans(glmSD.LD.UorN, "UrbanorNot"))


ResidSD = as.data.frame(glmSD.LD6$residuals)
ResidSD$BirdID = noSL$BirdID
View(ResidSD)

ResidSDJuncos <- merge(noSL, ResidSD)
View(ResidSDJuncos)

ggplot(data = ResidSDJuncos, aes(x = Location, y = ResidSDJuncos$`glmSD.LD6$residuals`,
                                 colour = Location)) + geom_point()

bartlett.test(ResidSDJuncos$`glmSD.LD6$residuals`~ResidSDJuncos$Location)
#p-value = 0.07814

#noSmallPopulations, no old SD by Location

noSmallOld = NoSmallPop
noSmallOld = subset(noSmallOld, Location != "UCSD 2006/2007")
noSmallOld$Location <- as.character(noSmallOld$Location)
noSmallOld$Location <- as.factor(noSmallOld$Location)
levels(noSmallOld$Location)
View(noSmallOld)

LDAnoSmallnoSD = lda(noSmallOld$Location ~ noSmallOld$SongLength +
                       noSmallOld$TrillRate + noSmallOld$minF
                 + noSmallOld$maxF + noSmallOld$bandwidthF)

View(LDAnoSmallnoSD)
LDAnoSmallnoSD

#Coefficients of linear discriminants:
#  LD1         LD2         LD3         LD4
#noSmallOld$SongLength -2.2429639 -3.69221133 -0.78964424 -0.11355500
#noSmallOld$TrillRate  -0.2274854  0.19923925 -0.07065567  0.09566032
#noSmallOld$minF        0.1557751 -0.19689218 -0.58738052 -1.36702949
#noSmallOld$maxF       -0.5535178  0.04537587  0.72374546 -1.27204643
#noSmallOld$bandwidthF -0.4992631  0.18168192  0.94612697  0.17134143

#Proportion of trace:
#  LD1    LD2    LD3    LD4 
# 0.6193 0.2246 0.1010 0.0551 

LDAnoSmallnoSD.values = predict(LDAnoSmallnoSD, noSmallOld)
LDnoSmallnoOldIndi = as.data.frame(LDAnoSmallnoSD.values$x)
LDnoSmallnoOldIndi$Observation = 1:nrow(LDnoSmallnoOldIndi)
View(LDnoSmallnoOldIndi)
noSmallOld$Observation = 1:nrow(noSmallOld)

LDnoSmallOld <- merge(noSmallOld, LDnoSmallnoOldIndi, by = "Observation")
View(LDnoSmallOld)

ggplot(data = LDnoSmallOld, aes(x = LD1, y = LD2, colour = Location)) +
  stat_ellipse(aes(x=LD1, y=LD2,color=Location, group=Location),type = "norm") +
  theme() + geom_point() + theme_classic()


glm.LDnoSmallOld <- glm(LDnoSmallOld$LD1~LDnoSmallOld$Location + LDnoSmallOld$Date + LDnoSmallOld$Time +
                          LDnoSmallOld$DistRoad + LDnoSmallOld$Lat + LDnoSmallOld$Long)
summary(glm.LDnoSmallOld)
#AIC = 274.78***************
pairs(emmeans(glm.LDnoSmallOld, "Location"))

glm.LDnoSmallOld2 <- glm(LDnoSmallOld$LD1~LDnoSmallOld$Location + LDnoSmallOld$Date  +
                           LDnoSmallOld$DistRoad + LDnoSmallOld$Lat + LDnoSmallOld$Long)
summary(glm.LDnoSmallOld2)
#AIC = 333.08

glm.LDnoSmallOld3 <- glm(LDnoSmallOld$LD1~LDnoSmallOld$Location + LDnoSmallOld$Date  +
                           LDnoSmallOld$DistRoad + LDnoSmallOld$Long)
summary(glm.LDnoSmallOld3)
#AIC = 331.97

glm.LDnoSmallOld4 <- glm(LDnoSmallOld$LD1~LDnoSmallOld$Location + LDnoSmallOld$Date  +
                           LDnoSmallOld$Long)
summary(glm.LDnoSmallOld4)
#AIC = 332.02
#Longitude p-value = 0.0320

glm.LDnoSmallOld5 <- glm(LDnoSmallOld$LD1~LDnoSmallOld$Location  +
                           LDnoSmallOld$Long)
summary(glm.LDnoSmallOld5)
#AIC = 333.36

glm.LDnoSmallOld6 <- glm(LDnoSmallOld$LD1~LDnoSmallOld$Location)
summary(glm.LDnoSmallOld6)
#AIC = 387.18

ResidnoSmallOld = as.data.frame(glm.LDnoSmallOld$residuals)
View(ResidnoSmallOld)
ResidnoSmallOld$BirdID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
                           21,22,24,25,26,27,28,29,30,31,32,33,34,35,36,38,39,40,41,42,
                           44,45,46,48,49,50,55,57,58,59,60,61,68,69,70,71,
                           72,73,74,77,80,81,82,83,84,85,86,87,88,89,90,91,
                           92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,
                           110,111,112,131,132,133,154)


ResidnoSmallOldJuncos <- merge(noSmallOld, ResidnoSmallOld, by = "BirdID")
View(ResidnoSmallOldJuncos)

ggplot(data = ResidnoSmallOldJuncos, aes(x = Location, y = ResidnoSmallOldJuncos$`glm.LDnoSmallOld$residuals`,
                                   colour = Location)) + geom_point()

bartlett.test(ResidnoSmallOldJuncos$`glm.LDnoSmallOld$residuals`~ResidnoSmallOldJuncos$Location)
#p-value = 0.4807

#noSmallOld by UrbanorNot

noSmallOld2 = noSmallOld
View(noSmallOld2)

LDAnoSmallnoSD2 = lda(noSmallOld2$UrbanorNot ~ noSmallOld2$SongLength +
                       noSmallOld2$TrillRate + noSmallOld2$minF
                     + noSmallOld2$maxF + noSmallOld2$bandwidthF)

View(LDAnoSmallnoSD2)
LDAnoSmallnoSD2

#Coefficients of linear discriminants:
#  LD1
#noSmallOld2$SongLength -3.6474539
#noSmallOld2$TrillRate   0.1063236
#noSmallOld2$minF       -0.3438208
#noSmallOld2$maxF        0.5321629
#noSmallOld2$bandwidthF  0.6275133

LDAnoSmallnoSD2.values = predict(LDAnoSmallnoSD2, noSmallOld2)
LDnoSmallnoOldIndi2 = as.data.frame(LDAnoSmallnoSD2.values$x)
LDnoSmallnoOldIndi2$Observation = 1:nrow(LDnoSmallnoOldIndi2)
LDnoSmallOld2 = merge(noSmallOld2, LDnoSmallnoOldIndi2, by = "Observation")

ggplot(data = LDnoSmallOld2, aes(x = UrbanorNot, y = LD1)) + geom_boxplot()

glm2.LDnoSmallOld <- glm(LDnoSmallOld2$LD1~LDnoSmallOld2$UrbanorNot + LDnoSmallOld2$Date + LDnoSmallOld2$Time +
                  LDnoSmallOld2$DistRoad + LDnoSmallOld2$Lat + LDnoSmallOld2$Long)
summary(glm2.LDnoSmallOld)
#AIC = 270.53*******
pairs(emmeans(glm2.LDnoSmallOld, "UrbanorNot"))

glm2.LDnoSmallOld2 <- glm(LDnoSmallOld2$LD1~LDnoSmallOld2$UrbanorNot + LDnoSmallOld2$Date +
                           LDnoSmallOld2$DistRoad + LDnoSmallOld2$Lat + LDnoSmallOld2$Long)
summary(glm2.LDnoSmallOld2)
#AIC = 336.38

glm2.LDnoSmallOld3 <- glm(LDnoSmallOld2$LD1~LDnoSmallOld2$UrbanorNot + 
                            LDnoSmallOld2$DistRoad + LDnoSmallOld2$Lat + LDnoSmallOld2$Long)
summary(glm2.LDnoSmallOld3)
#AIC = 335.89

glm2.LDnoSmallOld4 <- glm(LDnoSmallOld2$LD1~LDnoSmallOld2$UrbanorNot + 
                          LDnoSmallOld2$Lat + LDnoSmallOld2$Long)
summary(glm2.LDnoSmallOld4)
#AIC = 334.98

glm2.LDnoSmallOld5 <- glm(LDnoSmallOld2$LD1~LDnoSmallOld2$UrbanorNot + 
                            LDnoSmallOld2$Lat)
summary(glm2.LDnoSmallOld5)
#AIC = 333.66

glm2.LDnoSmallOld6 <- glm(LDnoSmallOld2$LD1~LDnoSmallOld2$UrbanorNot)
summary(glm2.LDnoSmallOld6)
#AIC = 384.26

pairs(emmeans(glm2.LDnoSmallOld6, "UrbanorNot"))


glm2.LDnoSmallOld

ResidnoSmallOld2 = as.data.frame(glm2.LDnoSmallOld$residuals)
View(ResidnoSmallOld2)
ResidnoSmallOld2$BirdID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
                           21,22,24,25,26,27,28,29,30,31,32,33,34,35,36,38,39,40,41,42,
                           44,45,46,48,49,50,55,57,58,59,60,61,68,69,70,71,
                           72,73,74,77,80,81,82,83,84,85,86,87,88,89,90,91,
                           92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,
                           110,111,112,131,132,133,154)


ResidnoSmallOldJuncos2 <- merge(noSmallOld2, ResidnoSmallOld2, by = "BirdID")
View(ResidnoSmallOldJuncos2)

ggplot(data = ResidnoSmallOldJuncos2, aes(x = UrbanorNot, y = ResidnoSmallOldJuncos2$`glm2.LDnoSmallOld$residuals`,
                                         colour = UrbanorNot)) + geom_point()

bartlett.test(ResidnoSmallOldJuncos2$`glm2.LDnoSmallOld$residuals`~ResidnoSmallOldJuncos2$UrbanorNot)
#p-value = 0.3582

#noSmallOld by CityorMountainLump

noSmallOld3 = noSmallOld

View(noSmallOld3)
noSmallOld3$CityorMountainLump <- as.character(noSmallOld3$CityorMountainLump)
noSmallOld3$CityorMountainLump <- as.factor(noSmallOld3$CityorMountainLump)
levels(noSmallOld3$CityorMountainLump)
LDAnoSmallnoSD3 = lda(noSmallOld3$CityorMountainLump ~ noSmallOld3$SongLength +
                        noSmallOld3$TrillRate + noSmallOld3$minF
                      + noSmallOld3$maxF + noSmallOld3$bandwidthF)

View(LDAnoSmallnoSD3)
LDAnoSmallnoSD3

#Coefficients of linear discriminants:
#LD1         LD2         LD3
#noSmallOld3$SongLength  2.2816034 -3.69057052  0.72142892
#noSmallOld3$TrillRate   0.2273574  0.20573320  0.09904806
#noSmallOld3$minF       -0.2717219 -0.27600534  0.23105144
#noSmallOld3$maxF        0.4839059  0.01769252 -1.00335746
#noSmallOld3$bandwidthF  0.5425857  0.22455790 -0.86930929

#Proportion of trace:
#  LD1    LD2    LD3 
#0.6468 0.2467 0.1065 

LDAnoSmallnoSD3.values = predict(LDAnoSmallnoSD3, noSmallOld3)
LDnoSmallnoOldIndi3 = as.data.frame(LDAnoSmallnoSD3.values$x)
LDnoSmallnoOldIndi3$Observation = 1:nrow(LDnoSmallnoOldIndi3)
LDnoSmallOld3 = merge(noSmallOld3, LDnoSmallnoOldIndi3, by = "Observation")

ggplot(data = LDnoSmallOld3, aes(x = LD1, y = LD2, colour = CityorMountainLump)) +
  stat_ellipse(aes(x=LD1, y=LD2,color=CityorMountainLump, group=CityorMountainLump),type = "norm") +
  theme() + geom_point() + theme_classic()


glm3.LDnoSmallOld <- glm(LDnoSmallOld3$LD1~LDnoSmallOld3$CityorMountainLump + LDnoSmallOld3$Date + LDnoSmallOld3$Time +
                           LDnoSmallOld3$DistRoad + LDnoSmallOld3$Lat + LDnoSmallOld3$Long)
summary(glm3.LDnoSmallOld)
#AIC = 275.41******
pairs(emmeans(glm3.LDnoSmallOld, "CityorMountainLump"))

glm3.LDnoSmallOld2 <- glm(LDnoSmallOld3$LD1~LDnoSmallOld3$CityorMountainLump + LDnoSmallOld3$Date +
                           LDnoSmallOld3$DistRoad + LDnoSmallOld3$Lat + LDnoSmallOld3$Long)
summary(glm3.LDnoSmallOld2)
#AIC = 332.04

glm3.LDnoSmallOld3 <- glm(LDnoSmallOld3$LD1~LDnoSmallOld3$CityorMountainLump + LDnoSmallOld3$Date +
                            LDnoSmallOld3$DistRoad + LDnoSmallOld3$Long)
summary(glm3.LDnoSmallOld3)
#AIC = 331.23

glm3.LDnoSmallOld4 <- glm(LDnoSmallOld3$LD1~LDnoSmallOld3$CityorMountainLump + LDnoSmallOld3$Date +
                            LDnoSmallOld3$Long)
summary(glm3.LDnoSmallOld4)
#AIC = 331.32

glm3.LDnoSmallOld5 <- glm(LDnoSmallOld3$LD1~LDnoSmallOld3$CityorMountainLump + 
                            LDnoSmallOld3$Long)
summary(glm3.LDnoSmallOld5)
#AIC = 331.53

glm3.LDnoSmallOld6 <- glm(LDnoSmallOld3$LD1~LDnoSmallOld3$CityorMountainLump)
summary(glm3.LDnoSmallOld6)
#AIC = 386.21

ResidnoSmallOld3 = as.data.frame(glm3.LDnoSmallOld$residuals)
View(ResidnoSmallOld3)
ResidnoSmallOld3$BirdID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
                            21,22,24,25,26,27,28,29,30,31,32,33,34,35,36,38,39,40,41,42,
                            44,45,46,48,49,50,55,57,58,59,60,61,68,69,70,71,
                            72,73,74,77,80,81,82,83,84,85,86,87,88,89,90,91,
                            92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,
                            110,111,112,131,132,133,154)


ResidnoSmallOldJuncos3 <- merge(noSmallOld3, ResidnoSmallOld3, by = "BirdID")
View(ResidnoSmallOldJuncos3)

ggplot(data = ResidnoSmallOldJuncos3, aes(x = CityorMountainLump, y = ResidnoSmallOldJuncos3$`glm3.LDnoSmallOld$residuals`,
                                          colour = CityorMountainLump)) + geom_point()

bartlett.test(ResidnoSmallOldJuncos3$`glm3.LDnoSmallOld$residuals`~ResidnoSmallOldJuncos3$CityorMountainLump)
#p-value = 0.4252

#GLM for all the character traits by location, city lump, urban or not, paired emmeans,

#GLM SongLength by Location - all populations
allPops = IndividualJuncos
SL.glm.Location = glm(IndividualJuncos$SongLength~IndividualJuncos$Location+ IndividualJuncos$Date + 
                     IndividualJuncos$Time + IndividualJuncos$DistRoad + IndividualJuncos$Lat + IndividualJuncos$Long)

summary(SL.glm.Location)
#AIC = -12.405

SL.glm.Location2 = glm(IndividualJuncos$SongLength~IndividualJuncos$Location+ IndividualJuncos$Date + 
                     IndividualJuncos$Time + IndividualJuncos$Lat + IndividualJuncos$Long)

summary(SL.glm.Location2)
#AIC = -14.391

SL.glm.Location3 = glm(IndividualJuncos$SongLength~IndividualJuncos$Location + 
                      IndividualJuncos$Time + IndividualJuncos$Lat + IndividualJuncos$Long)

summary(SL.glm.Location3)
#AIC = -16.216***************

SL.glm.Location4 = glm(IndividualJuncos$SongLength~IndividualJuncos$Location + 
                      IndividualJuncos$Lat + IndividualJuncos$Long)

summary(SL.glm.Location4)
#AIC = 8.2973

SL.glm.Location5 = glm(IndividualJuncos$SongLength~IndividualJuncos$Location + 
                      IndividualJuncos$Long)

summary(SL.glm.Location5)
#AIC = 6.7572
#Longitude p-value = 0.0185

SL.glm.Location6 = glm(IndividualJuncos$SongLength~IndividualJuncos$Location)
summary(SL.glm.Location6)
#AIC = 7.1843

pairs(emmeans(SL.glm.Location3, "Location"))

ResidSLLocation = as.data.frame(SL.glm.Location3$residuals)
View(ResidSLLocation)
ResidSLLocation$BirdID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,24,25,26,27,28,29,30,31,
                              32,33,34,35,36,38,39,40,41,42,44,45,46,48,49,50,55,57,58,59,60,61,62,63,64,65,
                              66,68,69,70,71,72,73,74,77,78,79,80,81,82,83,85,86,87,88,89,90,91,92,93,94,95,
                              96,97,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,116,117,118,
                              119,120,131,132,133,153,154)


ResidSLLocationJuncos <- merge(allPops, ResidSLLocation, by = "BirdID")
View(ResidSLLocationJuncos)

ggplot(data = ResidSLLocationJuncos, aes(x = Location, y = ResidSLLocationJuncos$`SL.glm.Location3$residuals`,
                                          colour = Location)) + geom_point()

bartlett.test(ResidSLLocationJuncos$`SL.glm.Location3$residuals`~ResidSLLocationJuncos$Location)
#p-value = 0.07315

#GLM TrillRate by Location - all populations
allPops2 = IndividualJuncos
allPops2 = subset(allPops2, Location!="UCSD 2006/2007")
allPops2$Location=as.character(allPops2$Location)
allPops2$Location=as.factor(allPops2$Location)
levels(allPops2$Location)


TR.glm.Location = glm(allPops2$TrillRate~allPops2$Location+ allPops2$Date + 
                        allPops2$Time + allPops2$DistRoad + allPops2$Lat + allPops2$Long)

summary(TR.glm.Location)
#AIC = 598.51******************
#Date p-value = 0.0335
#DistRoad p-value = 0.0458


TR.glm.Location2 = glm(allPops2$TrillRate~allPops2$Location+ allPops2$Date + 
                        allPops2$DistRoad + allPops2$Lat + allPops2$Long)

summary(TR.glm.Location2)
#AIC = 690.11
#Date p-value = 0.0235

TR.glm.Location3 = glm(allPops2$TrillRate~allPops2$Location+ allPops2$Date + 
                         allPops2$DistRoad + allPops2$Long)

summary(TR.glm.Location3)
#AIC = 688.27
#Date p-value = 0.0218

TR.glm.Location4 = glm(allPops2$TrillRate~allPops2$Location+ allPops2$Date + 
                         allPops2$DistRoad)

summary(TR.glm.Location4)
#AIC = 686.73
#Date p-value = 0.0259
#Dist Road p-value = 0.0369

TR.glm.Location5 = glm(allPops2$TrillRate~allPops2$Location+ allPops2$Date)
summary(TR.glm.Location5)
#AIC = 788.41
#UCSD p-value = 0.0445

TR.glm.Location6 = glm(allPops2$TrillRate~allPops2$Location)
summary(TR.glm.Location6)
#AIC = 790.27

pairs(emmeans(TR.glm.Location, "Location"))

ResidTRLocation = as.data.frame(TR.glm.Location$residuals)
ResidTRLocation$BirdID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,24,25,26,27,28,29,30,31,
                           32,33,34,35,36,38,39,40,41,42,44,45,46,48,49,50,55,57,58,59,60,61,62,63,64,65,
                           66,68,69,70,71,72,73,74,77,78,79,80,81,82,83,85,86,87,88,89,90,91,92,93,94,95,
                           96,97,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,116,117,118,
                           119,120,131,132,133,153,154)


ResidTRLocationJuncos <- merge(allPops2, ResidTRLocation, by = "BirdID")

ggplot(data = ResidTRLocationJuncos, aes(x = Location, y = ResidTRLocationJuncos$`TR.glm.Location$residuals`,
                                         colour = Location)) + geom_point()

bartlett.test(ResidTRLocationJuncos$`TR.glm.Location$residuals`~ResidTRLocationJuncos$Location)
#p-value = 0.01384*

#GLM minF by Location - all populations
allPops3 = IndividualJuncos
minF.glm.Location = glm(allPops3$minF~allPops3$Location+ allPops3$Date + 
                          allPops3$Time + allPops3$DistRoad + allPops3$Lat + allPops3$Long)

summary(minF.glm.Location)
#AIC = 153.31
#Date p-value = 0.0342

minF.glm.Location2 = glm(allPops3$minF~allPops3$Location+ allPops3$Date + 
                          allPops3$Time + allPops3$DistRoad + allPops3$Lat)

summary(minF.glm.Location2)
#AIC = 151.38
#Date p-value = 0.0322

minF.glm.Location3 = glm(allPops3$minF~allPops3$Location+ allPops3$Date +
                           allPops3$DistRoad + allPops3$Lat)

summary(minF.glm.Location3)
#AIC = 190.11
#Date p-value = 0.0076

minF.glm.Location4 = glm(allPops3$minF~allPops3$Location+ allPops3$Date +
                          allPops3$Lat)

summary(minF.glm.Location4)
#AIC = 189.05

minF.glm.Location5 = glm(allPops3$minF~allPops3$Location+ allPops3$Date)
summary(minF.glm.Location5)
#AIC = 219.67

minF.glm.Location6 = glm(allPops3$minF~allPops3$Location)
summary(minF.glm.Location6)
#AIC = 471.21

pairs(emmeans(minF.glm.Location2, "Location"))

minF.ResidLocation = as.data.frame(minF.glm.Location2$residuals)
View(minF.ResidLocation)
minF.ResidLocation$BirdID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,24,25,26,27,28,29,30,31,
                           32,33,34,35,36,38,39,40,41,42,44,45,46,48,49,50,55,57,58,59,60,61,62,63,64,65,
                           66,68,69,70,71,72,73,74,77,78,79,80,81,82,83,85,86,87,88,89,90,91,92,93,94,95,
                           96,97,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,116,117,118,
                           119,120,131,132,133,153,154)


minF.ResidLocationJuncos <- merge(allPops3, minF.ResidLocation, by = "BirdID")

ggplot(data = minF.ResidLocationJuncos, aes(x = Location, y = minF.ResidLocationJuncos$`minF.glm.Location2$residuals`,
                                         colour = Location)) + geom_point()

bartlett.test(minF.ResidLocationJuncos$`minF.glm.Location2$residuals`~minF.ResidLocationJuncos$Location)
#p-value = 0.09514

#GLM maxF by Location - all populations
allPops4 = IndividualJuncos
maxF.glm.Location = glm(allPops4$maxF~allPops4$Location+ allPops4$Date + 
                          allPops4$Time + allPops4$DistRoad + allPops4$Lat + allPops4$Long)

summary(maxF.glm.Location)
#AIC = 154.38******

maxF.glm.Location2 = glm(allPops4$maxF~allPops4$Location+ allPops4$Date + 
                         allPops4$DistRoad + allPops4$Lat + allPops4$Long)

summary(maxF.glm.Location2)
#AIC = 184.09
#Date p-value = 0.0445

maxF.glm.Location3 = glm(allPops4$maxF~allPops4$Location+ allPops4$Date + 
                           allPops4$DistRoad + allPops4$Lat)

summary(maxF.glm.Location3)
#AIC = 182.15
#Date p-value = 0.043

maxF.glm.Location4 = glm(allPops4$maxF~allPops4$Location+ allPops4$Date + 
                           allPops4$DistRoad)

summary(maxF.glm.Location4)
#AIC = 180.45
#Date p-value = 0.0485

maxF.glm.Location5 = glm(allPops4$maxF~allPops4$Location+ allPops4$Date)
summary(maxF.glm.Location5)
#AIC = 204.32

maxF.glm.Location6 = glm(allPops4$maxF~allPops4$Location)
summary(maxF.glm.Location6)
#AIC = 620.23

pairs(emmeans(maxF.glm.Location, "Location"))

maxF.ResidLocation = as.data.frame(maxF.glm.Location$residuals)
View(maxF.ResidLocation)
maxF.ResidLocation$BirdID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,24,25,26,27,28,29,30,31,
                              32,33,34,35,36,38,39,40,41,42,44,45,46,48,49,50,55,57,58,59,60,61,62,63,64,65,
                              66,68,69,70,71,72,73,74,77,78,79,80,81,82,83,85,86,87,88,89,90,91,92,93,94,95,
                              96,97,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,116,117,118,
                              119,120,131,132,133,153,154)


maxF.ResidLocationJuncos <- merge(allPops4, maxF.ResidLocation, by = "BirdID")

ggplot(data = maxF.ResidLocationJuncos, aes(x = Location, y = maxF.ResidLocationJuncos$`maxF.glm.Location$residuals`,
                                            colour = Location)) + geom_point()

bartlett.test(maxF.ResidLocationJuncos$`maxF.glm.Location$residuals`~maxF.ResidLocationJuncos$Location)
#p-value = 0.1546

#GLM bandF by Location - all populations
allPops5 = IndividualJuncos
bandF.glm.Location = glm(allPops5$bandwidthF~allPops5$Location+ allPops5$Date + 
                           allPops5$Time + allPops5$DistRoad + allPops5$Lat + allPops5$Long)

summary(bandF.glm.Location)
#AIC = 206.62

bandF.glm.Location2 = glm(allPops5$bandwidthF~allPops5$Location+ allPops5$Date + 
                           allPops5$Time + allPops5$Lat + allPops5$Long)

summary(bandF.glm.Location2)
#AIC = 204.97

bandF.glm.Location3 = glm(allPops5$bandwidthF~allPops5$Location+ allPops5$Date + 
                            allPops5$Time+ allPops5$Long)

summary(bandF.glm.Location3)
#AIC = 203.37************

bandF.glm.Location4 = glm(allPops5$bandwidthF~allPops5$Location+ allPops5$Date + 
                            allPops5$Long)

summary(bandF.glm.Location4)
#AIC = 237.94

bandF.glm.Location5 = glm(allPops5$bandwidthF~allPops5$Location+ allPops5$Date)
summary(bandF.glm.Location5)
#AIC = 269.45

bandF.glm.Location6 = glm(allPops5$bandwidthF~allPops5$Location)
summary(bandF.glm.Location6)
#AIC = 736.95

pairs(emmeans(bandF.glm.Location3, "Location"))

bandF.ResidLocation = as.data.frame(bandF.glm.Location3$residuals)
View(bandF.glm.Location3)
bandF.ResidLocation$BirdID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,24,25,26,27,28,29,30,31,
                              32,33,34,35,36,38,39,40,41,42,44,45,46,48,49,50,55,57,58,59,60,61,62,63,64,65,
                              66,68,69,70,71,72,73,74,77,78,79,80,81,82,83,85,86,87,88,89,90,91,92,93,94,95,
                              96,97,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,116,117,118,
                              119,120,131,132,133,153,154)


bandF.ResidLocationJuncos <- merge(allPops5, bandF.ResidLocation, by = "BirdID")

ggplot(data = bandF.ResidLocationJuncos, aes(x = Location, y = bandF.ResidLocationJuncos$`bandF.glm.Location3$residuals`,
                                            colour = Location)) + geom_point()

bartlett.test(bandF.ResidLocationJuncos$`bandF.glm.Location3$residuals`~bandF.ResidLocationJuncos$Location)
#p-value = 0.1153

#GLM SongLength by UrbanorNot - all populations
all2Pops = IndividualJuncos


SL.glm.UorN = glm(all2Pops$SongLength~all2Pops$UrbanorNot+ all2Pops$Date + 
                    all2Pops$Time + all2Pops$DistRoad + all2Pops$Lat + all2Pops$Long)

summary(SL.glm.UorN)
#AIC = -15.127

SL.glm.UorN2 = glm(all2Pops$SongLength~all2Pops$UrbanorNot+ all2Pops$Date + 
                    all2Pops$Time + all2Pops$DistRoad + all2Pops$Long)

summary(SL.glm.UorN2)
#AIC = -17.126*****

SL.glm.UorN3 = glm(all2Pops$SongLength~all2Pops$UrbanorNot+ all2Pops$Date + 
                     all2Pops$DistRoad + all2Pops$Long)

summary(SL.glm.UorN3)
#AIC = 7.2237

SL.glm.UorN4 = glm(all2Pops$SongLength~all2Pops$UrbanorNot+ all2Pops$Date + 
                     all2Pops$Long)

summary(SL.glm.UorN4)
#AIC = 5.2354

SL.glm.UorN5 = glm(all2Pops$SongLength~all2Pops$UrbanorNot+ 
                     all2Pops$Long)

summary(SL.glm.UorN5)
#AIC = 3.8167

SL.glm.UorN6 = glm(all2Pops$SongLength~all2Pops$UrbanorNot)
summary(SL.glm.UorN6)
#AIC = 4.1781

pairs(emmeans(SL.glm.UorN2, "UrbanorNot"))

pairs(emmeans(SL.glm.UorN6, "UrbanorNot"))

SL.resid.UorN = as.data.frame(SL.glm.UorN2$residuals)
View(SL.resid.UorN)
SL.resid.UorN$BirdID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,24,25,26,27,28,29,30,31,
                           32,33,34,35,36,38,39,40,41,42,44,45,46,48,49,50,55,57,58,59,60,61,62,63,64,65,
                           66,68,69,70,71,72,73,74,77,78,79,80,81,82,83,85,86,87,88,89,90,91,92,93,94,95,
                           96,97,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,116,117,118,
                           119,120,131,132,133,153,154)


SL.resid.UorNJuncos <- merge(all2Pops, SL.resid.UorN, by = "BirdID")
View(SL.resid.UorNJuncos)

ggplot(data = SL.resid.UorNJuncos, aes(x = UrbanorNot, y = SL.resid.UorNJuncos$`SL.glm.UorN2$residuals`,
                                         colour = UrbanorNot)) + geom_point()

bartlett.test(SL.resid.UorNJuncos$`SL.glm.UorN2$residuals`~SL.resid.UorNJuncos$UrbanorNot)
#p-value = 0.4019

#GLM TrillRate by UrbanorNot - all populations
all2Pops2 = IndividualJuncos
all2Pops2 = subset(all2Pops2, Location!="UCSD 2006/2007")
all2Pops2$Location=as.character(all2Pops2$Location)
all2Pops2$Location=as.factor(all2Pops2$Location)
levels(all2Pops2$Location)

TR.glm.UorN = glm(all2Pops2$TrillRate~all2Pops2$UrbanorNot+ all2Pops2$Date + 
                    all2Pops2$Time + all2Pops2$DistRoad + all2Pops2$Lat + all2Pops2$Long)

summary(TR.glm.UorN)
#AIC = 591.06
#Date p-value = 0.0357
#DistRoad p-value = 0.0399

TR.glm.UorN2 = glm(all2Pops2$TrillRate~all2Pops2$UrbanorNot+ all2Pops2$Date + 
                    all2Pops2$Time + all2Pops2$DistRoad + all2Pops2$Lat)

summary(TR.glm.UorN2)
#AIC = 589.13***
#Date p-value = 0.03605
#DistRoad p-value = 0.04022
#Latitude p-value = 0.01071

TR.glm.UorN3 = glm(all2Pops2$TrillRate~all2Pops2$UrbanorNot+ all2Pops2$Date + 
                      all2Pops2$DistRoad + all2Pops2$Lat)

summary(TR.glm.UorN3)
#AIC = 681.68
#Date p-value = 0.04665

TR.glm.UorN4 = glm(all2Pops2$TrillRate~all2Pops2$UrbanorNot+ all2Pops2$Date + 
                     all2Pops2$DistRoad)

summary(TR.glm.UorN4)
#AIC = 683.44
#Date p-value = 0.046

TR.glm.UorN5 = glm(all2Pops2$TrillRate~all2Pops2$UrbanorNot+ all2Pops2$Date)
summary(TR.glm.UorN5)
#AIC = 785.27

TR.glm.UorN6 = glm(all2Pops2$TrillRate~all2Pops2$UrbanorNot)
summary(TR.glm.UorN6)
#AIC = 787.14

pairs(emmeans(TR.glm.UorN2, "UrbanorNot"))

#w/repository
pairs(emmeans(TR.glm.UorN6, "UrbanorNot"))

#w/o UCSD2006/2007, w/ repository
TR.glm.UorN7 = glm(noOLD$TrillRate~noOLD$UrbanorNot)
summary(TR.glm.UorN7)
View(noOLD)
pairs(emmeans(TR.glm.UorN7, "UrbanorNot"))


TR.resid.UorN = as.data.frame(TR.glm.UorN2$residuals)
TR.resid.UorN$BirdID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,24,25,26,27,28,29,30,31,
                           32,33,34,35,36,38,39,40,41,42,44,45,46,48,49,50,55,57,58,59,60,61,62,63,64,65,
                           66,68,69,70,71,72,73,74,77,78,79,80,81,82,83,85,86,87,88,89,90,91,92,93,94,95,
                           96,97,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,116,117,118,
                           119,120,131,132,133,153,154)


TR.resid.UorNJuncos <- merge(all2Pops2, TR.resid.UorN, by = "BirdID")

ggplot(data = TR.resid.UorNJuncos, aes(x = UrbanorNot, y = TR.resid.UorNJuncos$`TR.glm.UorN2$residuals`,
                                         colour = UrbanorNot)) + geom_point()

bartlett.test(TR.resid.UorNJuncos$`TR.glm.UorN2$residuals`~TR.resid.UorNJuncos$UrbanorNot)
#p-value = 0.737

#GLM minF by UrbanorNot - all populations
all2Pops3 = IndividualJuncos
minF.glm.UorN = glm(all2Pops3$minF~all2Pops3$UrbanorNot+ all2Pops3$Date + 
                      all2Pops3$Time + all2Pops3$DistRoad + all2Pops3$Lat + all2Pops3$Long)

summary(minF.glm.UorN)
#AIC = 146.07
#Date p-value = 0.0238

minF.glm.UorN2 = glm(all2Pops3$minF~all2Pops3$UrbanorNot+ all2Pops3$Date + 
                      all2Pops3$Time + all2Pops3$Lat + all2Pops3$Long)

summary(minF.glm.UorN2)
#AIC = 144.56
#Date p-value = 0.0301

minF.glm.UorN3 = glm(all2Pops3$minF~all2Pops3$UrbanorNot+ all2Pops3$Date + 
                       all2Pops3$Time + all2Pops3$Long)

summary(minF.glm.UorN3)
#AIC = 143.06
#Date p-value = 0.0352

minF.glm.UorN4 = glm(all2Pops3$minF~all2Pops3$UrbanorNot+ all2Pops3$Date + 
                       all2Pops3$Long)

summary(minF.glm.UorN4)
#AIC = 179.18
#Date p-value = 0.00944

minF.glm.UorN5 = glm(all2Pops3$minF~all2Pops3$UrbanorNot+ all2Pops3$Date)
summary(minF.glm.UorN5)
#AIC = 211.44

minF.glm.UorN6 = glm(all2Pops3$minF~all2Pops3$UrbanorNot)
summary(minF.glm.UorN6)
#AIC = 475.59

pairs(emmeans(minF.glm.UorN3, "UrbanorNot"))

#w/repository w/oUCSD 2006/2007
minF.glm.UorN7 = glm(noOLD$minF~noOLD$UrbanorNot)
pairs(emmeans(minF.glm.UorN7, "UrbanorNot"))

#w/reposistory and UCSD 2006/2007

pairs(emmeans(minF.glm.UorN6, "UrbanorNot"))

minF.resid.UorN = as.data.frame(minF.glm.UorN3$residuals)
View(minF.resid.UorN)
minF.resid.UorN$BirdID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,24,25,26,27,28,29,30,31,
                              32,33,34,35,36,38,39,40,41,42,44,45,46,48,49,50,55,57,58,59,60,61,62,63,64,65,
                              66,68,69,70,71,72,73,74,77,78,79,80,81,82,83,85,86,87,88,89,90,91,92,93,94,95,
                              96,97,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,116,117,118,
                              119,120,131,132,133,153,154)


minF.resid.UorNJuncos <- merge(all2Pops3, minF.resid.UorN, by = "BirdID")

ggplot(data = minF.resid.UorNJuncos, aes(x = UrbanorNot, y = minF.resid.UorNJuncos$`minF.glm.UorN3$residuals`,
                                            colour = UrbanorNot)) + geom_point()

bartlett.test(minF.resid.UorNJuncos$`minF.glm.UorN3$residuals`~minF.resid.UorNJuncos$UrbanorNot)
#p-value = 0.2138

#GLM maxF by Location - all populations
all2Pops4 = IndividualJuncos
maxF.glm.UorN = glm(all2Pops4$maxF~all2Pops4$UrbanorNot+ all2Pops4$Date + 
                          all2Pops4$Time + all2Pops4$DistRoad + all2Pops4$Lat + all2Pops4$Long)

summary(maxF.glm.UorN)
#AIC = 153.41***

maxF.glm.UorN2 = glm(all2Pops4$maxF~all2Pops4$UrbanorNot+ all2Pops4$Date + 
                      all2Pops4$DistRoad + all2Pops4$Lat + all2Pops4$Long)

summary(maxF.glm.UorN2)
#AIC = 181.38

maxF.glm.UorN3 = glm(all2Pops4$maxF~all2Pops4$UrbanorNot+ all2Pops4$Date + 
                       all2Pops4$DistRoad + all2Pops4$Lat)

summary(maxF.glm.UorN3)
#AIC = 180.93

maxF.glm.UorN4 = glm(all2Pops4$maxF~all2Pops4$UrbanorNot+ all2Pops4$Date + 
                       all2Pops4$DistRoad)

summary(maxF.glm.UorN4)
#AIC = 179.35

maxF.glm.UorN5 = glm(all2Pops4$maxF~all2Pops4$UrbanorNot+
                       all2Pops4$DistRoad)

summary(maxF.glm.UorN5)
#AIC = 179.16

maxF.glm.UorN5 = glm(all2Pops4$maxF~all2Pops4$UrbanorNot)
summary(maxF.glm.UorN5)
#AIC = 632.01

pairs(emmeans(maxF.glm.UorN, "UrbanorNot"))

#w/repository, w/o UCSD 2006/2007
maxF.glm.UorN6 = glm(noOLD$maxF~noOLD$UrbanorNot)
summary(maxF.glm.UorN6)
pairs(emmeans(maxF.glm.UorN6, "UrbanorNot"))

#w/repository and 2006/2007
pairs(emmeans(maxF.glm.UorN5, "UrbanorNot"))
summary(maxF.glm.UorN5)

maxF.resid.UorN = as.data.frame(maxF.glm.UorN$residuals)
View(maxF.resid.UorN)
maxF.resid.UorN$BirdID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,24,25,26,27,28,29,30,31,
                              32,33,34,35,36,38,39,40,41,42,44,45,46,48,49,50,55,57,58,59,60,61,62,63,64,65,
                              66,68,69,70,71,72,73,74,77,78,79,80,81,82,83,85,86,87,88,89,90,91,92,93,94,95,
                              96,97,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,116,117,118,
                              119,120,131,132,133,153,154)


maxF.resid.UorNJuncos <- merge(all2Pops4, maxF.resid.UorN, by = "BirdID")

ggplot(data = maxF.resid.UorNJuncos, aes(x = UrbanorNot, y = maxF.resid.UorNJuncos$`maxF.glm.UorN$residuals`,
                                            colour = UrbanorNot)) + geom_point()

bartlett.test(maxF.resid.UorNJuncos$`maxF.glm.UorN$residuals`~maxF.resid.UorNJuncos$UrbanorNot)
#p-value = 0.5013

#GLM bandF by Location - all populations
all2Pops5 = IndividualJuncos
bandF.glm.UorN = glm(all2Pops5$bandwidthF~all2Pops5$UrbanorNot+ all2Pops5$Date + 
                       all2Pops5$Time + all2Pops5$DistRoad + all2Pops5$Lat + all2Pops5$Long)

summary(bandF.glm.UorN)
#AIC = 205.85

bandF.glm.UorN2 = glm(all2Pops5$bandwidthF~all2Pops5$UrbanorNot+ all2Pops5$Date + 
                       all2Pops5$Time + all2Pops5$DistRoad + all2Pops5$Lat)

summary(bandF.glm.UorN2)
#AIC = 203.97

bandF.glm.UorN3 = glm(all2Pops5$bandwidthF~all2Pops5$UrbanorNot+ all2Pops5$Date + 
                        all2Pops5$Time + all2Pops5$Lat)

summary(bandF.glm.UorN3)
#AIC = 202.16****

bandF.glm.UorN4 = glm(all2Pops5$bandwidthF~all2Pops5$UrbanorNot+ all2Pops5$Date
                        + all2Pops5$Lat)

summary(bandF.glm.UorN4)
#AIC = 233

bandF.glm.UorN5 = glm(all2Pops5$bandwidthF~all2Pops5$UrbanorNot+ all2Pops5$Date)
summary(bandF.glm.UorN5)
#AIC = 264.64

bandF.glm.UorN6 = glm(all2Pops5$bandwidthF~all2Pops5$UrbanorNot)
summary(bandF.glm.UorN6)
#AIC = 729.23

pairs(emmeans(bandF.glm.UorN3, "UrbanorNot"))


bandF.resid.UorN = as.data.frame(bandF.glm.UorN3$residuals)
View(bandF.resid.UorN)
bandF.resid.UorN$BirdID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,24,25,26,27,28,29,30,31,
                               32,33,34,35,36,38,39,40,41,42,44,45,46,48,49,50,55,57,58,59,60,61,62,63,64,65,
                               66,68,69,70,71,72,73,74,77,78,79,80,81,82,83,85,86,87,88,89,90,91,92,93,94,95,
                               96,97,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,116,117,118,
                               119,120,131,132,133,153,154)


bandF.resid.UorNJuncos <- merge(all2Pops5, bandF.resid.UorN, by = "BirdID")

ggplot(data = bandF.resid.UorNJuncos, aes(x = UrbanorNot, y = bandF.resid.UorNJuncos$`bandF.glm.UorN3$residuals`,
                                             colour = UrbanorNot)) + geom_point()

bartlett.test(bandF.resid.UorNJuncos$`bandF.glm.UorN3$residuals`~bandF.resid.UorNJuncos$UrbanorNot)
#p-value = 0.425

#w/repository w/o UCSD 2006/2007
bandF.glm.UorN7 = glm(noOLD$bandwidthF~noOLD$UrbanorNot)
pairs(emmeans(bandF.glm.UorN7, "UrbanorNot"))

#w/repository w/ UCSD 2006/2007
pairs(emmeans(bandF.glm.UorN6, "UrbanorNot"))


#GLM SongLength by CityorMountainLump - all populations
all3Pops = IndividualJuncos
SL.glm.CITY = glm(all3Pops$SongLength~all3Pops$CityorMountainLump+ all3Pops$Date + 
                    all3Pops$Time + all3Pops$DistRoad + all3Pops$Lat + all3Pops$Long)

summary(SL.glm.CITY)
#AIC = -14.709

SL.glm.CITY2 = glm(all3Pops$SongLength~all3Pops$CityorMountainLump+ all3Pops$Date + 
                    all3Pops$Time + all3Pops$Lat + all3Pops$Long)

summary(SL.glm.CITY2)
#AIC = -16.697

SL.glm.CITY3 = glm(all3Pops$SongLength~all3Pops$CityorMountainLump+ all3Pops$Date + 
                     all3Pops$Time + all3Pops$Lat)

summary(SL.glm.CITY3)
#AIC = -18.675

SL.glm.CITY4 = glm(all3Pops$SongLength~all3Pops$CityorMountainLump + 
                     all3Pops$Time + all3Pops$Lat)

summary(SL.glm.CITY4)
#AIC = -20.59***
#UCSD p-value = 0.0441

SL.glm.CITY5 = glm(all3Pops$SongLength~all3Pops$CityorMountainLump + 
                     all3Pops$Lat)

summary(SL.glm.CITY5)
#AIC = 4.9408
#Latitude p-value = 0.0161

SL.glm.CITY6 = glm(all3Pops$SongLength~all3Pops$CityorMountainLump)
summary(SL.glm.CITY6)
#AIC = 5.7667

pairs(emmeans(SL.glm.CITY4, "CityorMountainLump"))

SL.resid.CITY = as.data.frame(SL.glm.CITY4$residuals)
View(SL.resid.CITY)
SL.resid.CITY$BirdID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,24,25,26,27,28,29,30,31,
                         32,33,34,35,36,38,39,40,41,42,44,45,46,48,49,50,55,57,58,59,60,61,62,63,64,65,
                         66,68,69,70,71,72,73,74,77,78,79,80,81,82,83,85,86,87,88,89,90,91,92,93,94,95,
                         96,97,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,116,117,118,
                         119,120,131,132,133,153,154)


SL.resid.CITYJuncos <- merge(all3Pops, SL.resid.CITY, by = "BirdID")
View(SL.resid.CITYJuncos)

ggplot(data = SL.resid.CITYJuncos, aes(x = CityorMountainLump, y = SL.resid.CITYJuncos$`SL.glm.CITY4$residuals`,
                                       colour = CityorMountainLump)) + geom_point()

bartlett.test(SL.resid.CITYJuncos$`SL.glm.CITY4$residuals`~SL.resid.CITYJuncos$CityorMountainLump)
#p-value = 0.03319*

#GLM TrillRate by CityorMountainLump - all populations
all3Pops2 = IndividualJuncos
TR.glm.CITY = glm(all3Pops2$TrillRate~all3Pops2$CityorMountainLump+ all3Pops2$Date + 
                    all3Pops2$Time + all3Pops2$DistRoad + all3Pops2$Lat + all3Pops2$Long)

summary(TR.glm.CITY)
#AIC = 595.46***
#Date p-value = 0.0278
#DistRoad p-value = 0.0425

TR.glm.CITY2 = glm(all3Pops2$TrillRate~all3Pops2$CityorMountainLump+ all3Pops2$Date + 
                    all3Pops2$DistRoad + all3Pops2$Lat + all3Pops2$Long)

summary(TR.glm.CITY2)
#AIC = 686.4
#Date p-value = 0.02

TR.glm.CITY3 = glm(all3Pops2$TrillRate~all3Pops2$CityorMountainLump+ all3Pops2$Date + 
                     all3Pops2$DistRoad + all3Pops2$Lat)

summary(TR.glm.CITY3)
#AIC = 684.4
#Date p-value = 0.0191

TR.glm.CITY4 = glm(all3Pops2$TrillRate~all3Pops2$CityorMountainLump+ all3Pops2$Date + 
                     all3Pops2$DistRoad)

summary(TR.glm.CITY4)
#AIC = 682.41
#Date p-value = 0.0174
#DistRoad p-value = 0.0412

TR.glm.CITY5 = glm(all3Pops2$TrillRate~all3Pops2$CityorMountainLump+ all3Pops2$Date)
summary(TR.glm.CITY5)
#AIC = 784.57
#Date p-value = 0.031

TR.glm.CITY6 = glm(all3Pops2$TrillRate~all3Pops2$CityorMountainLump)
summary(TR.glm.CITY6)
#AIC = 1680.3
#UCLA p-value = 0.0195

pairs(emmeans(TR.glm.CITY, "CityorMountainLump"))

TR.resid.CITY = as.data.frame(TR.glm.CITY$residuals)
TR.resid.CITY$BirdID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,24,25,26,27,28,29,30,31,
                         32,33,34,35,36,38,39,40,41,42,44,45,46,48,49,50,55,57,58,59,60,61,62,63,64,65,
                         66,68,69,70,71,72,73,74,77,78,79,80,81,82,83,85,86,87,88,89,90,91,92,93,94,95,
                         96,97,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,116,117,118,
                         119,120,131,132,133,153,154)


TR.resid.CITYJuncos <- merge(all3Pops2, TR.resid.CITY, by = "BirdID")

ggplot(data = TR.resid.CITYJuncos, aes(x = CityorMountainLump, y = TR.resid.CITYJuncos$`TR.glm.CITY$residuals`,
                                       colour = CityorMountainLump)) + geom_point()

bartlett.test(TR.resid.CITYJuncos$`TR.glm.CITY$residuals`~TR.resid.CITYJuncos$CityorMountainLump)
#p-value = 0.1885

#GLM minF by CityorMountainLump - all populations
all3Pops3 = IndividualJuncos
minF.glm.CITY = glm(all3Pops3$minF~all3Pops3$CityorMountainLump+ all3Pops3$Date + 
                      all3Pops3$Time + all3Pops3$DistRoad + all3Pops3$Lat + all3Pops3$Long)

summary(minF.glm.CITY)
#AIC = 150.37***
#Date p-value = 0.04

minF.glm.CITY2 = glm(all3Pops3$minF~all3Pops3$CityorMountainLump+ all3Pops3$Date + 
                       all3Pops3$DistRoad + all3Pops3$Lat + all3Pops3$Long)

summary(minF.glm.CITY2)
#AIC = 185.37
#Date p-value = 0.00687

minF.glm.CITY3 = glm(all3Pops3$minF~all3Pops3$CityorMountainLump+ all3Pops3$Date + 
                      all3Pops3$DistRoad + all3Pops3$Lat)

summary(minF.glm.CITY3)
#AIC = 184.24
#Date p-value = 0.00545

minF.glm.CITY4 = glm(all3Pops3$minF~all3Pops3$CityorMountainLump+ all3Pops3$Date + 
                        all3Pops3$Lat)

summary(minF.glm.CITY4)
#AIC = 183.15
#Date p-value = 0.0078

minF.glm.CITY5 = glm(all3Pops3$minF~all3Pops3$CityorMountainLump+ all3Pops3$Date)
summary(minF.glm.CITY5)
#AIC = 214.81

minF.glm.CITY6= glm(all3Pops3$minF~all3Pops3$CityorMountainLump)
summary(minF.glm.CITY6)
#AIC = 466.26


pairs(emmeans(minF.glm.CITY, "CityorMountainLump"))

minF.resid.CITY = as.data.frame(minF.glm.CITY$residuals)
View(minF.resid.CITY)
minF.resid.CITY$BirdID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,24,25,26,27,28,29,30,31,
                           32,33,34,35,36,38,39,40,41,42,44,45,46,48,49,50,55,57,58,59,60,61,62,63,64,65,
                           66,68,69,70,71,72,73,74,77,78,79,80,81,82,83,85,86,87,88,89,90,91,92,93,94,95,
                           96,97,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,116,117,118,
                           119,120,131,132,133,153,154)


minF.resid.CITYJuncos <- merge(all3Pops3, minF.resid.CITY, by = "BirdID")

ggplot(data = minF.resid.CITYJuncos, aes(x = CityorMountainLump, y = minF.resid.CITYJuncos$`minF.glm.CITY$residuals`,
                                         colour = CityorMountainLump)) + geom_point()

bartlett.test(minF.resid.CITYJuncos$`minF.glm.CITY$residuals`~minF.resid.CITYJuncos$CityorMountainLump)
#p-value = 0.1477

#GLM maxF by CityorMountainLump - all populations
all3Pops4 = IndividualJuncos
maxF.glm.CITY = glm(all3Pops4$maxF~all3Pops4$CityorMountainLump+ all3Pops4$Date + 
                      all3Pops4$Time + all3Pops4$DistRoad + all3Pops4$Lat + all3Pops4$Long)

summary(maxF.glm.CITY)
#AIC = 152.66***

maxF.glm.CITY2 = glm(all3Pops4$maxF~all3Pops4$CityorMountainLump+ all3Pops4$Date + 
                       all3Pops4$DistRoad + all3Pops4$Lat + all3Pops4$Long)

summary(maxF.glm.CITY2)
#AIC = 183.63

maxF.glm.CITY3 = glm(all3Pops4$maxF~all3Pops4$CityorMountainLump+ all3Pops4$Date + 
                       all3Pops4$DistRoad + all3Pops4$Long)

summary(maxF.glm.CITY3)
#AIC = 181.9
#UCSD p-value = 0.0391

maxF.glm.CITY4 = glm(all3Pops4$maxF~all3Pops4$CityorMountainLump+ all3Pops4$Date + 
                       all3Pops4$DistRoad)

summary(maxF.glm.CITY4)
#AIC = 182.01

maxF.glm.CITY5 = glm(all3Pops4$maxF~all3Pops4$CityorMountainLump+
                       all3Pops4$DistRoad)

summary(maxF.glm.CITY5)
#AIC = 182.73

maxF.glm.CITY6 = glm(all3Pops4$maxF~all3Pops4$CityorMountainLump)
summary(maxF.glm.CITY6)
#AIC = 617.52

pairs(emmeans(maxF.glm.CITY, "CityorMountainLump"))

maxF.resid.CITY = as.data.frame(maxF.glm.CITY$residuals)
View(maxF.resid.CITY)
maxF.resid.CITY$BirdID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,24,25,26,27,28,29,30,31,
                           32,33,34,35,36,38,39,40,41,42,44,45,46,48,49,50,55,57,58,59,60,61,62,63,64,65,
                           66,68,69,70,71,72,73,74,77,78,79,80,81,82,83,85,86,87,88,89,90,91,92,93,94,95,
                           96,97,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,116,117,118,
                           119,120,131,132,133,153,154)


maxF.resid.CITYJuncos <- merge(all3Pops4, maxF.resid.CITY, by = "BirdID")

ggplot(data = maxF.resid.CITYJuncos, aes(x = CityorMountainLump, y = maxF.resid.CITYJuncos$`maxF.glm.CITY$residuals`,
                                         colour = CityorMountainLump)) + geom_point()

bartlett.test(maxF.resid.CITYJuncos$`maxF.glm.CITY$residuals`~maxF.resid.CITYJuncos$CityorMountainLump)
#p-value = 0.5584

#GLM bandF by CityorMountainLump - all populations
all3Pops5 = IndividualJuncos
bandF.glm.CITY = glm(all3Pops5$bandwidthF~all3Pops5$CityorMountainLump+ all3Pops5$Date + 
                       all3Pops5$Time + all3Pops5$DistRoad + all3Pops5$Lat + all3Pops5$Long)

summary(bandF.glm.CITY)
#AIC = 204.69

bandF.glm.CITY2 = glm(all3Pops5$bandwidthF~all3Pops5$CityorMountainLump+ all3Pops5$Date + 
                       all3Pops5$Time + all3Pops5$DistRoad + all3Pops5$Lat)

summary(bandF.glm.CITY2)
#AIC = 202.98
#UCSD p-value = 0.0364

bandF.glm.CITY3 = glm(all3Pops5$bandwidthF~all3Pops5$CityorMountainLump+ all3Pops5$Date + 
                        all3Pops5$Time + all3Pops5$Lat)

summary(bandF.glm.CITY3)
#AIC = 201.25***
#UCSD p-value = 0.0412

bandF.glm.CITY4 = glm(all3Pops5$bandwidthF~all3Pops5$CityorMountainLump+ all3Pops5$Date + 
                       all3Pops5$Lat)

summary(bandF.glm.CITY4)
#AIC = 235.95

bandF.glm.CITY5 = glm(all3Pops5$bandwidthF~all3Pops5$CityorMountainLump+ all3Pops5$Date)
summary(bandF.glm.CITY5)
#AIC = 266.76

bandF.glm.CITY6 = glm(all3Pops5$bandwidthF~all3Pops5$CityorMountainLump)
summary(bandF.glm.CITY6)
#AIC = 733.17

pairs(emmeans(bandF.glm.CITY3, "CityorMountainLump"))

bandF.resid.CITY= as.data.frame(bandF.glm.CITY$residuals)
View(bandF.resid.CITY)
bandF.resid.CITY$BirdID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,24,25,26,27,28,29,30,31,
                            32,33,34,35,36,38,39,40,41,42,44,45,46,48,49,50,55,57,58,59,60,61,62,63,64,65,
                            66,68,69,70,71,72,73,74,77,78,79,80,81,82,83,85,86,87,88,89,90,91,92,93,94,95,
                            96,97,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,116,117,118,
                            119,120,131,132,133,153,154)


bandF.resid.CITYJuncos <- merge(all3Pops5, bandF.resid.CITY, by = "BirdID")

ggplot(data = bandF.resid.CITYJuncos, aes(x = CityorMountainLump, y = bandF.resid.CITYJuncos$`bandF.glm.CITY$residuals`,
                                          colour = CityorMountainLump)) + geom_point()

bartlett.test(bandF.resid.CITYJuncos$`bandF.glm.CITY$residuals`~bandF.resid.CITYJuncos$CityorMountainLump)
#p-value = 0.3443

#GLM SongLength by Location - no Small Populations
noSmallPops = noSmallRep

SL2.glm.Location = glm(noSmallPops$SongLength~noSmallPops$Location+ noSmallPops$Date + 
                        noSmallPops$Time + noSmallPops$DistRoad + noSmallPops$Lat + noSmallPops$Long)

summary(SL2.glm.Location)
#AIC = -29.134

SL2.glm.Location2 = glm(noSmallPops$SongLength~noSmallPops$Location+ noSmallPops$Date + 
                          noSmallPops$DistRoad + noSmallPops$Lat + noSmallPops$Long)

summary(SL2.glm.Location2)
#AIC = -31.107

SL2.glm.Location3 = glm(noSmallPops$SongLength~noSmallPops$Location+ noSmallPops$Date + 
                          noSmallPops$Lat + noSmallPops$Long)

summary(SL2.glm.Location3)
#AIC = -33.063

SL2.glm.Location4 = glm(noSmallPops$SongLength~noSmallPops$Location+ noSmallPops$Date + 
                          noSmallPops$Long)

summary(SL2.glm.Location4)
#AIC = -34.958

SL2.glm.Location5 = glm(noSmallPops$SongLength~noSmallPops$Location + 
                           noSmallPops$Long)

summary(SL2.glm.Location5)
#AIC = -36.447
#longitude p-value = 0.0137

SL2.glm.Location6 = glm(noSmallPops$SongLength~noSmallPops$Location)
summary(SL2.glm.Location6)
#AIC = -37.969

pairs(emmeans(SL2.glm.Location6, "Location"))

SL2.resid.location = as.data.frame(SL2.glm.Location6$residuals)
View(SL2.resid.location)
SL2.resid.location$BirdID = 1:nrow(SL2.resid.location)


SL2.resid.locationJuncos <- merge(noSmallPops, SL2.resid.location, by = "BirdID")
View(SL2.resid.locationJuncos)

ggplot(data = SL2.resid.locationJuncos, aes(x = Location, y = SL2.resid.locationJuncos$`SL2.glm.Location6$residuals`,
                                         colour = Location)) + geom_point()

bartlett.test(SL2.resid.locationJuncos$`SL2.glm.Location6$residuals`~SL2.resid.locationJuncos$Location)
#p-value = 0.6058

#GLM TrillRate by Location - no small populations
noSmallPops2=noSmallRep


TR2.glm.Location = glm(noSmallPops2$TrillRate~noSmallPops2$Location+ noSmallPops2$Date + 
                         noSmallPops2$Time + noSmallPops2$DistRoad + noSmallPops2$Lat + noSmallPops2$Long)

summary(TR2.glm.Location)
#AIC = 525.54***
#Date p-value = 0.0451

TR2.glm.Location2 = glm(noSmallPops2$TrillRate~noSmallPops2$Location+ noSmallPops2$Date + 
                        noSmallPops2$DistRoad + noSmallPops2$Lat + noSmallPops2$Long)

summary(TR2.glm.Location2)
#AIC = 524.19
#date p-value = 0.0326

TR2.glm.Location3 = glm(noSmallPops2$TrillRate~noSmallPops2$Location+ noSmallPops2$Date + 
                          noSmallPops2$DistRoad + noSmallPops2$Long)

summary(TR2.glm.Location3)
#AIC = 523.02
#Date p-value = 0.0302

TR2.glm.Location4 = glm(noSmallPops2$TrillRate~noSmallPops2$Location+ noSmallPops2$Date + 
                          noSmallPops2$DistRoad)

summary(TR2.glm.Location4)
#AIC = 521.69
#Date p-value = 0.0357
#Date p-value = 0.0483

TR2.glm.Location5 = glm(noSmallPops2$TrillRate~noSmallPops2$Location+ noSmallPops2$Date)
summary(TR2.glm.Location5)
#AIC = 613.17

TR2.glm.Location6 = glm(noSmallPops2$TrillRate~noSmallPops2$Location)
summary(TR2.glm.Location6)
#AIC = 614.37

pairs(emmeans(TR2.glm.Location4, "Location"))

TR2.resid.location = as.data.frame(TR2.glm.Location4$residuals)
View(TR2.resid.location)
TR2.resid.location$BirdID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
                              21,22,24,25,26,27,28,29,30,31,32,33,34,35,36,38,39,40,41,42,
                              44,45,46,48,49,50,55,57,58,59,60,61,68,69,70,71,
                              72,73,74,77,80,81,82,83,84,85,86,87,88,89,90,91,
                              92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,
                              110,111,112,131,132,133,154)


TR2.resid.locationJuncos <- merge(noSmallPops2, TR2.resid.location, by = "BirdID")

ggplot(data = TR2.resid.locationJuncos, aes(x = Location, y = TR2.resid.locationJuncos$`TR2.glm.Location4$residuals`,
                                         colour = Location)) + geom_point()

bartlett.test(TR2.resid.locationJuncos$`TR2.glm.Location4$residuals`~TR2.resid.locationJuncos$Location)
#p-value = 0.1224

#minF noSmallPopulations
noSmallPops3=noSmallRep
minF2.glm.Location = glm(noSmallPops3$minF~noSmallPops3$Location+ noSmallPops3$Date + 
                           noSmallPops3$Time + noSmallPops3$DistRoad + noSmallPops3$Lat + noSmallPops3$Long)

summary(minF2.glm.Location)
#AIC = 130.19

minF2.glm.Location2 = glm(noSmallPops3$minF~noSmallPops3$Location+ noSmallPops3$Date + 
                           noSmallPops3$Time + noSmallPops3$DistRoad + noSmallPops3$Lat)

summary(minF2.glm.Location2)
#AIC = 128.55

minF2.glm.Location3 = glm(noSmallPops3$minF~noSmallPops3$Location+ noSmallPops3$Date + 
                            noSmallPops3$DistRoad + noSmallPops3$Lat)

summary(minF2.glm.Location3)
#AIC = 127.23

minF2.glm.Location4 = glm(noSmallPops3$minF~noSmallPops3$Location+ noSmallPops3$Date + 
                            noSmallPops3$Lat)

summary(minF2.glm.Location4)
#AIC = 126.02

minF2.glm.Location5 = glm(noSmallPops3$minF~noSmallPops3$Location+ noSmallPops3$Date)
summary(minF2.glm.Location5)
#AIC = 154.92

minF2.glm.Location6 = glm(noSmallPops3$minF~noSmallPops3$Location)
summary(minF2.glm.Location6)
#AIC = 154.9

pairs(emmeans(minF2.glm.Location4, "Location"))

minF2.resid.location = as.data.frame(minF2.glm.Location4$residuals)
View(minF2.resid.location)
minF2.resid.location$BirdID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
                              21,22,24,25,26,27,28,29,30,31,32,33,34,35,36,38,39,40,41,42,
                              44,45,46,48,49,50,55,57,58,59,60,61,68,69,70,71,
                              72,73,74,77,80,81,82,83,84,85,86,87,88,89,90,91,
                              92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,
                              110,111,112,131,132,133,154)


minF2.resid.locationJuncos <- merge(noSmallPops3, minF2.resid.location, by = "BirdID")

ggplot(data = minF2.resid.locationJuncos, aes(x = Location, y = minF2.resid.locationJuncos$`minF2.glm.Location4$residuals`,
                                            colour = Location)) + geom_point()

bartlett.test(minF2.resid.locationJuncos$`minF2.glm.Location4$residuals`~minF2.resid.locationJuncos$Location)
#p-value = 0.02896

#maxF noSmallPopulations Location
noSmallPops4=noSmallRep
maxF2.glm.Location = glm(noSmallPops4$maxF~noSmallPops4$Location+ noSmallPops4$Date + 
                           noSmallPops4$Time + noSmallPops4$DistRoad + noSmallPops4$Lat + noSmallPops4$Long)

summary(maxF2.glm.Location)
#AIC = 134.13

maxF2.glm.Location2 = glm(noSmallPops4$maxF~noSmallPops4$Location+ noSmallPops4$Date + 
                           noSmallPops4$DistRoad + noSmallPops4$Lat + noSmallPops4$Long)

summary(maxF2.glm.Location2)
#AIC = 132.13

maxF2.glm.Location3 = glm(noSmallPops4$maxF~noSmallPops4$Location+ noSmallPops4$Date + 
                             noSmallPops4$Lat + noSmallPops4$Long)

summary(maxF2.glm.Location3)
#AIC = 130.33

maxF2.glm.Location4 = glm(noSmallPops4$maxF~noSmallPops4$Location+ 
                            noSmallPops4$Lat + noSmallPops4$Long)

summary(maxF2.glm.Location4)
#AIC = 130.16

maxF2.glm.Location5 = glm(noSmallPops4$maxF~noSmallPops4$Location+ 
                            noSmallPops4$Long)
summary(maxF2.glm.Location5)
#AIC = 130.26

maxF2.glm.Location6 = glm(noSmallPops4$maxF~noSmallPops4$Location)
summary(maxF2.glm.Location6)
#AIC = 148.94

pairs(emmeans(maxF2.glm.Location4, "Location"))

maxF2.resid.location = as.data.frame(maxF2.glm.Location4$residuals)
View(maxF2.resid.location)
maxF2.resid.location$BirdID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
                                21,22,24,25,26,27,28,29,30,31,32,33,34,35,36,38,39,40,41,42,
                                44,45,46,48,49,50,55,57,58,59,60,61,68,69,70,71,
                                72,73,74,77,80,81,82,83,84,85,86,87,88,89,90,91,
                                92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,
                                110,111,112,131,132,133,154)


maxF2.resid.locationJuncos <- merge(noSmallPops4, maxF2.resid.location, by = "BirdID")

ggplot(data = maxF2.resid.locationJuncos, aes(x = Location, y = maxF2.resid.locationJuncos$`maxF2.glm.Location4$residuals`,
                                              colour = Location)) + geom_point()

bartlett.test(maxF2.resid.locationJuncos$`maxF2.glm.Location4$residuals`~maxF2.resid.locationJuncos$Location)
#p-value = 0.05467

#bandF noSmallPopulations Location
noSmallPops5=noSmallRep
bandF2.glm.Location = glm(noSmallPops5$bandwidthF~noSmallPops5$Location+ noSmallPops5$Date + 
                            noSmallPops5$Time + noSmallPops5$DistRoad + noSmallPops5$Lat + noSmallPops5$Long)

summary(bandF2.glm.Location)
#AIC = 175.54

bandF2.glm.Location2 = glm(noSmallPops5$bandwidthF~noSmallPops5$Location+ noSmallPops5$Date + 
                            noSmallPops5$Time + noSmallPops5$Lat + noSmallPops5$Long)

summary(bandF2.glm.Location2)
#AIC = 173.63

bandF2.glm.Location3 = glm(noSmallPops5$bandwidthF~noSmallPops5$Location+ noSmallPops5$Date + 
                             noSmallPops5$Time + noSmallPops5$Long)

summary(bandF2.glm.Location3)
#AIC = 171.78

bandF2.glm.Location4 = glm(noSmallPops5$bandwidthF~noSmallPops5$Location + 
                             noSmallPops5$Time + noSmallPops5$Long)

summary(bandF2.glm.Location4)
#AIC = 169.97

bandF2.glm.Location5 = glm(noSmallPops5$bandwidthF~noSmallPops5$Location + 
                            noSmallPops5$Long)
summary(bandF2.glm.Location5)
#AIC = 168.44

bandF2.glm.Location6 = glm(noSmallPops5$bandwidthF~noSmallPops5$Location)
summary(bandF2.glm.Location6)
#AIC = 199.08

pairs(emmeans(bandF2.glm.Location5, "Location"))

bandF2.resid.location = as.data.frame(bandF2.glm.Location5$residuals)
View(bandF2.resid.location)
bandF2.resid.location$BirdID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
                                21,22,24,25,26,27,28,29,30,31,32,33,34,35,36,38,39,40,41,42,
                                44,45,46,48,49,50,55,57,58,59,60,61,68,69,70,71,
                                72,73,74,77,80,81,82,83,84,85,86,87,88,89,90,91,
                                92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,
                                110,111,112,131,132,133,154)


bandF2.resid.locationJuncos <- merge(noSmallPops5, bandF2.resid.location, by = "BirdID")

ggplot(data = bandF2.resid.locationJuncos, aes(x = Location, y = bandF2.resid.locationJuncos$`bandF2.glm.Location5$residuals`,
                                              colour = Location)) + geom_point()

bartlett.test(bandF2.resid.locationJuncos$`bandF2.glm.Location5$residuals`~bandF2.resid.locationJuncos$Location)
#p-value = 0.05135

#GLM SongLength by UrbanorNot - no Small Populations
noSmall2Pops = noSmallRep
View(noSmall2Pops)

SL2.glm.UorN = glm(noSmall2Pops$SongLength~noSmall2Pops$UrbanorNot+ noSmall2Pops$Date + 
                          noSmall2Pops$Time + noSmall2Pops$DistRoad + noSmall2Pops$Lat + noSmall2Pops$Long)

summary(SL2.glm.UorN)
#AIC = -30.397

SL2.glm.UorN2 = glm(noSmall2Pops$SongLength~noSmall2Pops$UrbanorNot+ noSmall2Pops$Date + 
                          noSmall2Pops$DistRoad + noSmall2Pops$Lat + noSmall2Pops$Long)

summary(SL2.glm.UorN2)
#AIC = -32.387

SL2.glm.UorN3 = glm(noSmall2Pops$SongLength~noSmall2Pops$UrbanorNot+ noSmall2Pops$Date + 
                           noSmall2Pops$Lat + noSmall2Pops$Long)

summary(SL2.glm.UorN3)
#AIC = -34.366

SL2.glm.UorN4 = glm(noSmall2Pops$SongLength~noSmall2Pops$UrbanorNot + 
                           noSmall2Pops$Lat + noSmall2Pops$Long)

summary(SL2.glm.UorN4)
#AIC = -35.81

SL2.glm.UorN5= glm(noSmall2Pops$SongLength~noSmall2Pops$UrbanorNot + 
                           noSmall2Pops$Long)

summary(SL2.glm.UorN5)
#AIC = -34.716

SL2.glm.UorN6 = glm(noSmall2Pops$SongLength~noSmall2Pops$UrbanorNot)
summary(SL2.glm.UorN6)
#AIC = -36.855

pairs(emmeans(SL2.glm.UorN6, "UrbanorNot"))

#w repository
SL2.glm.UorN7 = glm(noSmall$SongLength~noSmall$UrbanorNot)
summary(SL2.glm.UorN7)
pairs(emmeans(SL2.glm.UorN7, "UrbanorNot"))

SL2.resid.UorN = as.data.frame(SL2.glm.UorN7$residuals)
View(SL2.resid.UorN)
SL2.resid.UorN$Observation = 1:nrow(SL2.resid.UorN)
noSmall2Pops$Observation = 1:nrow(noSmall2Pops)

View(noSmall2Pops)
SL2.resid.UorNJuncos <- merge(noSmall, SL2.resid.UorN, by = "Observation")

View(SL2.resid.UorNJuncos)

ggplot(data = SL2.resid.UorNJuncos, aes(x = UrbanorNot, y = SL2.resid.UorNJuncos$`SL2.glm.UorN7$residuals`,
                                            colour = UrbanorNot)) + geom_point()

bartlett.test(SL2.resid.UorNJuncos$`SL2.glm.UorN7$residuals`~SL2.resid.UorNJuncos$UrbanorNot)
#p-value = 0.8832

#GLM TrillRate by UrbanorNot - no Small Populations
noSmall2Pops2 = noSmallRep
noSmall2Pops2 = subset(noSmall2Pops2, Location!="UCSD 2006/2007")
noSmall2Pops2$Location=as.character(noSmall2Pops2$Location)
noSmall2Pops2$Location=as.factor(noSmall2Pops2$Location)
levels(noSmall2Pops2$Location)

TR2.glm.UorN = glm(noSmall2Pops2$TrillRate~noSmall2Pops2$UrbanorNot+ noSmall2Pops2$Date + 
                     noSmall2Pops2$Time + noSmall2Pops2$DistRoad + noSmall2Pops2$Lat + noSmall2Pops2$Long)

summary(TR2.glm.UorN)
#AIC = 523.29

TR2.glm.UorN2 = glm(noSmall2Pops2$TrillRate~noSmall2Pops2$UrbanorNot+ noSmall2Pops2$Date + 
                     noSmall2Pops2$Time + noSmall2Pops2$DistRoad + noSmall2Pops2$Lat)

summary(TR2.glm.UorN2)
#AIC = 521.31

TR2.glm.UorN3 = glm(noSmall2Pops2$TrillRate~noSmall2Pops2$UrbanorNot+ noSmall2Pops2$Date + 
                      noSmall2Pops2$DistRoad + noSmall2Pops2$Lat)

summary(TR2.glm.UorN3)
#AIC = 519.94

TR2.glm.UorN4 = glm(noSmall2Pops2$TrillRate~noSmall2Pops2$UrbanorNot + noSmall2Pops2$Date +
                     noSmall2Pops2$Lat)

summary(TR2.glm.UorN4)
#AIC = 522.22

TR2.glm.UorN5 = glm(noSmall2Pops2$TrillRate~noSmall2Pops2$UrbanorNot + 
                       noSmall2Pops2$Lat)

summary(TR2.glm.UorN5)
#AIC = 522.7

TR2.glm.UorN6 = glm(noSmall2Pops2$TrillRate~noSmall2Pops2$UrbanorNot)
summary(TR2.glm.UorN6)
#AIC = 615.53

TR2.glm.UorN7 = glm(noSmall$TrillRate~noSmall$UrbanorNot)
summary(TR2.glm.UorN7)

View(noSmall2Pops)
pairs(emmeans(TR2.glm.UorN3, "UrbanorNot"))

#w/ repository
pairs(emmeans(TR2.glm.UorN7, "UrbanorNot"))

TR2.resid.UorN = as.data.frame(TR2.glm.UorN7$residuals)
View(TR2.resid.UorN)
#TR2.resid.UorN$BirdID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
#                          21,22,24,25,26,27,28,29,30,31,32,33,34,35,36,38,39,40,41,42,
#                          44,45,46,48,49,50,55,57,58,59,60,61,68,69,70,71,
#                          72,73,74,77,80,81,82,83,84,85,86,87,88,89,90,91,
#                          92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,
#                          110,111,112,131,132,133,154)
TR2.resid.UorN$Observation = 1:nrow(noSmall)

TR2.resid.UorNJuncos <- merge(noSmall, TR2.resid.UorN, by = "Observation")
View(TR2.resid.UorNJuncos)

ggplot(data = TR2.resid.UorNJuncos, aes(x = UrbanorNot, y = TR2.resid.UorNJuncos$`TR2.glm.UorN7$residuals`,
                                        colour = UrbanorNot)) + geom_point()

bartlett.test(TR2.resid.UorNJuncos$`TR2.glm.UorN7$residuals`~TR2.resid.UorNJuncos$UrbanorNot)
#p-value = 0.624

#GLM minF by UrbanorNot - no Small Populations
noSmall2Pops3 = noSmallRep
noSmall2Pops3 = subset(noSmall2Pops3, Location!="UCSD 2006/2007")
noSmall2Pops3$Location=as.character(noSmall2Pops3$Location)
noSmall2Pops3$Location=as.factor(noSmall2Pops3$Location)
levels(noSmall2Pops3$Location)


minF2.glm.UorN = glm(noSmall2Pops3$minF~noSmall2Pops3$UrbanorNot+ noSmall2Pops3$Date + 
                     noSmall2Pops3$Time + noSmall2Pops3$DistRoad + noSmall2Pops3$Lat + noSmall2Pops3$Long)

summary(minF2.glm.UorN)
#AIC = 128.6

minF2.glm.UorN2 = glm(noSmall2Pops3$minF~noSmall2Pops3$UrbanorNot+ noSmall2Pops3$Date + 
                       noSmall2Pops3$Time + noSmall2Pops3$DistRoad + noSmall2Pops3$Lat)

summary(minF2.glm.UorN2)
#AIC = 126.72

minF2.glm.UorN3 = glm(noSmall2Pops3$minF~noSmall2Pops3$UrbanorNot+ noSmall2Pops3$Date + 
                        noSmall2Pops3$Time + noSmall2Pops3$DistRoad)

summary(minF2.glm.UorN3)
#AIC = 124.79

minF2.glm.UorN4 = glm(noSmall2Pops3$minF~noSmall2Pops3$UrbanorNot+ noSmall2Pops3$Date + 
                        noSmall2Pops3$Time)

summary(minF2.glm.UorN4)
#AIC = 153.02

minF2.glm.UorN5 = glm(noSmall2Pops3$minF~noSmall2Pops3$UrbanorNot+ noSmall2Pops3$Date)
summary(minF2.glm.UorN5)
#AIC = 151.07

minF2.glm.UorN6= glm(noSmall2Pops3$minF~noSmall2Pops3$UrbanorNot)
summary(minF2.glm.UorN6)
#AIC = 151.18
View(noSmall2Pops3)

pairs(emmeans(minF2.glm.UorN3, "UrbanorNot"))

#w/repository
minF2.glm.UorN7= glm(noSmall$minF~noSmall$UrbanorNot)
summary(minF2.glm.UorN7)
pairs(emmeans(minF2.glm.UorN6, "UrbanorNot"))


minF2.resid.UorN = as.data.frame(minF2.glm.UorN7$residuals)
View(minF2.resid.UorN)
minF2.resid.UorN$Observation = 1:nrow(noSmall)
noSmall$Observation = 1:nrow(noSmall)

minF2.resid.UorNJuncos <- merge(noSmall, minF2.resid.UorN, by = "Observation")
View(minF2.resid.UorNJuncos)

ggplot(data = minF2.resid.UorNJuncos, aes(x = UrbanorNot, y = minF2.resid.UorNJuncos$`minF2.glm.UorN7$residuals`,
                                        colour = UrbanorNot)) + geom_point()

bartlett.test(minF2.resid.UorNJuncos$`minF2.glm.UorN7$residuals`~minF2.resid.UorNJuncos$UrbanorNot)
#p-value = 0.8522
#w/ repository p-value = 0.1743

#GLM maxF by UrbanorNot - no Small Populations
noSmall2Pops4 = noSmallRep
noSmall2Pops4 = subset(noSmall2Pops4, Location!="UCSD 2006/2007")
noSmall2Pops4$Location=as.character(noSmall2Pops4$Location)
noSmall2Pops4$Location=as.factor(noSmall2Pops4$Location)
levels(noSmall2Pops4$Location)

maxF2.glm.UorN = glm(noSmall2Pops4$maxF~noSmall2Pops4$UrbanorNot+ noSmall2Pops4$Date + 
                       noSmall2Pops4$Time + noSmall2Pops4$DistRoad + noSmall2Pops4$Lat + noSmall2Pops4$Long)

summary(maxF2.glm.UorN)
#AIC = 132.72

maxF2.glm.UorN2 = glm(noSmall2Pops4$maxF~noSmall2Pops4$UrbanorNot+ noSmall2Pops4$Date + 
                       noSmall2Pops4$DistRoad + noSmall2Pops4$Lat + noSmall2Pops4$Long)

summary(maxF2.glm.UorN2)
#AIC = 130.73

maxF2.glm.UorN3 = glm(noSmall2Pops4$maxF~noSmall2Pops4$UrbanorNot+ noSmall2Pops4$Date + 
                        noSmall2Pops4$Long + noSmall2Pops4$Lat)

summary(maxF2.glm.UorN3)
#AIC = 129.02

maxF2.glm.UorN4 = glm(noSmall2Pops4$maxF~noSmall2Pops4$UrbanorNot+ noSmall2Pops4$Date + 
                        noSmall2Pops4$Long)

summary(maxF2.glm.UorN4)
#AIC = 127.88

maxF2.glm.UorN5 = glm(noSmall2Pops4$maxF~noSmall2Pops4$UrbanorNot+ noSmall2Pops4$Long)
summary(maxF2.glm.UorN5)
#AIC = 127.42

maxF2.glm.UorN6 = glm(noSmall2Pops4$maxF~noSmall2Pops4$UrbanorNot)
summary(maxF2.glm.UorN6)
#AIC = 146.34

pairs(emmeans(maxF2.glm.UorN5, "UrbanorNot"))

#w/repository
maxF2.glm.UorN7 = glm(noSmall$maxF~noSmall$UrbanorNot)
summary(maxF2.glm.UorN7)
pairs(emmeans(maxF2.glm.UorN7, "UrbanorNot"))

maxF2.resid.UorN = as.data.frame(maxF2.glm.UorN7$residuals)
View(maxF2.resid.UorN)
maxF2.resid.UorN$Observation = 1:nrow(noSmall)

maxF2.resid.UorNJuncos <- merge(noSmall, maxF2.resid.UorN, by = "Observation")
View(maxF2.resid.UorNJuncos)

ggplot(data = maxF2.resid.UorNJuncos, aes(x = UrbanorNot, y = maxF2.resid.UorNJuncos$`maxF2.glm.UorN7$residuals`,
                                          colour = UrbanorNot)) + geom_point()

bartlett.test(maxF2.resid.UorNJuncos$`maxF2.glm.UorN7$residuals`~maxF2.resid.UorNJuncos$UrbanorNot)
#p-value = 0.5207
#w/ repository p-value = 0.5607

#GLM bandF by UrbanorNot - no Small Populations
noSmall2Pops5 = noSmallRep
noSmall2Pops5 = subset(noSmall2Pops5, Location!="UCSD 2006/2007")
noSmall2Pops5$Location=as.character(noSmall2Pops5$Location)
noSmall2Pops5$Location=as.factor(noSmall2Pops5$Location)
levels(noSmall2Pops5$Location)

bandF2.glm.UorN = glm(noSmall2Pops5$bandwidthF~noSmall2Pops5$UrbanorNot+ noSmall2Pops5$Date + 
                       noSmall2Pops5$Time + noSmall2Pops5$DistRoad + noSmall2Pops5$Lat + noSmall2Pops5$Long)

summary(bandF2.glm.UorN)
#AIC = 174.34

bandF2.glm.UorN2 = glm(noSmall2Pops5$bandwidthF~noSmall2Pops5$UrbanorNot+ noSmall2Pops5$Date + 
                        noSmall2Pops5$Time + noSmall2Pops5$Lat + noSmall2Pops5$Long)

summary(bandF2.glm.UorN2)
#AIC = 172.4

bandF2.glm.UorN3 = glm(noSmall2Pops5$bandwidthF~noSmall2Pops5$UrbanorNot + 
                         noSmall2Pops5$Time + noSmall2Pops5$Lat + noSmall2Pops5$Long)

summary(bandF2.glm.UorN3)
#AIC = 170.63

bandF2.glm.UorN4 = glm(noSmall2Pops5$bandwidthF~noSmall2Pops5$UrbanorNot + 
                         noSmall2Pops5$Lat + noSmall2Pops5$Long)

summary(bandF2.glm.UorN4)
#AIC = 169.16

bandF2.glm.UorN5 = glm(noSmall2Pops5$bandwidthF~noSmall2Pops5$UrbanorNot + 
                         noSmall2Pops5$Long)

summary(bandF2.glm.UorN5)
#AIC = 168.09

bandF2.glm.UorN6 = glm(noSmall2Pops5$bandwidthF~noSmall2Pops5$UrbanorNot)
summary(bandF2.glm.UorN6)
#AIC = 196.76

pairs(emmeans(bandF2.glm.UorN5, "UrbanorNot"))

#w/ repository
bandF2.glm.UorN7 = glm(noSmall$bandwidthF~noSmall$UrbanorNot)
summary(bandF2.glm.UorN7)
pairs(emmeans(bandF2.glm.UorN7, "UrbanorNot"))


bandF2.resid.UorN = as.data.frame(bandF2.glm.UorN7$residuals)
View(bandF2.resid.UorN)
#bandF2.resid.UorN$BirdID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
#                            21,22,24,25,26,27,28,29,30,31,32,33,34,35,36,38,39,40,41,42,
#                            44,45,46,48,49,50,55,57,58,59,60,61,68,69,70,71,
#                            72,73,74,77,80,81,82,83,84,85,86,87,88,89,90,91,
#                            92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,
#                             110,111,112,131,132,133,154)

bandF2.resid.UorN$Observation = 1:nrow(noSmall)
noSmall$Observation = 1:nrow(noSmall)

bandF2.resid.UorNJuncos <- merge(noSmall, bandF2.resid.UorN, by = "Observation")
View(bandF2.resid.UorNJuncos)

ggplot(data = bandF2.resid.UorNJuncos, aes(x = UrbanorNot, y = bandF2.resid.UorNJuncos$`bandF2.glm.UorN7$residuals`,
                                          colour = UrbanorNot)) + geom_point()

bartlett.test(bandF2.resid.UorNJuncos$`bandF2.glm.UorN7$residuals`~bandF2.resid.UorNJuncos$UrbanorNot)
#p-value = 0.9113
#w/ repository p-value = 0.3728

#GLM SongLength by CityorMountainLump - no Small Populations
noSmall3Pops = NoSmallPop

SL3.glm.CITY = glm(noSmall3Pops$SongLength~noSmall3Pops$CityorMountainLump+ noSmall3Pops$Date + 
                     noSmall3Pops$Time + noSmall3Pops$DistRoad + noSmall3Pops$Lat + noSmall3Pops$Long)

summary(SL3.glm.CITY)
#AIC = -29.134

SL3.glm.CITY2 = glm(noSmall3Pops$SongLength~noSmall3Pops$CityorMountainLump+ noSmall3Pops$Date + 
                     noSmall3Pops$Time + noSmall3Pops$Lat + noSmall3Pops$Long)

summary(SL3.glm.CITY2)
#AIC = -31.087

SL3.glm.CITY3 = glm(noSmall3Pops$SongLength~noSmall3Pops$CityorMountainLump+ noSmall3Pops$Date + 
                      noSmall3Pops$Lat + noSmall3Pops$Long)

summary(SL3.glm.CITY3)
#AIC = -5.5895

SL3.glm.CITY4 = glm(noSmall3Pops$SongLength~noSmall3Pops$CityorMountainLump + 
                      noSmall3Pops$Lat + noSmall3Pops$Long)

summary(SL3.glm.CITY4)
#AIC = -7.5473

SL3.glm.CITY5 = glm(noSmall3Pops$SongLength~noSmall3Pops$CityorMountainLump + 
                      noSmall3Pops$Long)

summary(SL3.glm.CITY5)
#AIC = -9.0588
#Longitude = 0.0122

SL3.glm.CITY6 = glm(noSmall3Pops$SongLength~noSmall3Pops$CityorMountainLump)
summary(SL3.glm.CITY6)
#AIC = -7.32


pairs(emmeans(SL3.glm.CITY2, "CityorMountainLump"))

SL3.resid.CITY = as.data.frame(SL3.glm.CITY2$residuals)
View(SL3.resid.CITY)
SL3.resid.CITY$BirdID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
                          21,22,24,25,26,27,28,29,30,31,32,33,34,35,36,38,39,40,41,42,
                          44,45,46,48,49,50,55,57,58,59,60,61,68,69,70,71,
                          72,73,74,77,80,81,82,83,84,85,86,87,88,89,90,91,
                          92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,
                          110,111,112,131,132,133,154)


SL3.resid.CITYJuncos <- merge(noSmall3Pops, SL3.resid.CITY, by = "BirdID")
View(SL3.resid.CITYJuncos)

ggplot(data = SL3.resid.CITYJuncos, aes(x = CityorMountainLump, y = SL3.resid.CITYJuncos$`SL3.glm.CITY2$residuals`,
                                        colour = CityorMountainLump)) + geom_point()

bartlett.test(SL3.resid.CITYJuncos$`SL3.glm.CITY2$residuals`~SL3.resid.CITYJuncos$CityorMountainLump)
#p-value = 0.1084

#Trill Rate City or Mountain Lump - small pop
noSmall3Pops2 = NoSmallPop

TR3.glm.CITY = glm(noSmall3Pops2$TrillRate~noSmall3Pops2$CityorMountainLump+ noSmall3Pops2$Date + 
                     noSmall3Pops2$Time + noSmall3Pops2$DistRoad + noSmall3Pops2$Lat + noSmall3Pops2$Long)

summary(TR3.glm.CITY)
#AIC = 525.54***
#Date p-value = 0.0451

TR3.glm.CITY2 = glm(noSmall3Pops2$TrillRate~noSmall3Pops2$CityorMountainLump+ noSmall3Pops2$Date + 
                     noSmall3Pops2$DistRoad + noSmall3Pops2$Lat + noSmall3Pops2$Long)

summary(TR3.glm.CITY2)
#AIC = 616.62

TR3.glm.CITY3 = glm(noSmall3Pops2$TrillRate~noSmall3Pops2$CityorMountainLump+ noSmall3Pops2$Date + 
                      noSmall3Pops2$DistRoad + noSmall3Pops2$Long)

summary(TR3.glm.CITY3)
#AIC = 614.81
#Date p-value = 0.0377

TR3.glm.CITY4 = glm(noSmall3Pops2$TrillRate~noSmall3Pops2$CityorMountainLump+ noSmall3Pops2$Date + 
                      noSmall3Pops2$DistRoad)

summary(TR3.glm.CITY4)
#AIC = 613.05
#Date p-value = 0.0414
#DistRoad p-value = 0.0366

TR3.glm.CITY5 = glm(noSmall3Pops2$TrillRate~noSmall3Pops2$CityorMountainLump + 
                      noSmall3Pops2$DistRoad)

summary(TR3.glm.CITY5)
#AIC = 615.45

TR3.glm.CITY6 = glm(noSmall3Pops2$TrillRate~noSmall3Pops2$CityorMountainLump)
summary(TR3.glm.CITY6)
#AIC = 1598.8

pairs(emmeans(TR3.glm.CITY, "CityorMountainLump"))

TR3.resid.CITY = as.data.frame(TR3.glm.CITY$residuals)
View(TR3.resid.CITY)
TR3.resid.CITY$BirdID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
                          21,22,24,25,26,27,28,29,30,31,32,33,34,35,36,38,39,40,41,42,
                          44,45,46,48,49,50,55,57,58,59,60,61,68,69,70,71,
                          72,73,74,77,80,81,82,83,84,85,86,87,88,89,90,91,
                          92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,
                          110,111,112,131,132,133,154)


TR3.resid.CITYJuncos <- merge(noSmall3Pops, TR3.resid.CITY, by = "BirdID")
View(TR3.resid.CITYJuncos)

ggplot(data = TR3.resid.CITYJuncos, aes(x = CityorMountainLump, y = TR3.resid.CITYJuncos$`TR3.glm.CITY$residuals`,
                                        colour = CityorMountainLump)) + geom_point()

bartlett.test(TR3.resid.CITYJuncos$`TR3.glm.CITY$residuals`~TR3.resid.CITYJuncos$CityorMountainLump)
#p-value = 0.007438

#minF City or Mountain Lump - small pop
noSmall3Pops3 = NoSmallPop

minF3.glm.CITY = glm(noSmall3Pops3$minF~noSmall3Pops3$CityorMountainLump+ noSmall3Pops3$Date + 
                     noSmall3Pops3$Time + noSmall3Pops3$DistRoad + noSmall3Pops3$Lat + noSmall3Pops3$Long)

summary(minF3.glm.CITY)
#AIC = 130.19

minF3.glm.CITY2= glm(noSmall3Pops3$minF~noSmall3Pops3$CityorMountainLump+ noSmall3Pops3$Date + 
                       noSmall3Pops3$Time + noSmall3Pops3$DistRoad + noSmall3Pops3$Lat)

summary(minF3.glm.CITY2)
#AIC = 128.55
#Date p-value = 0.0447

minF3.glm.CITY3= glm(noSmall3Pops3$minF~noSmall3Pops3$CityorMountainLump+ noSmall3Pops3$Date + 
                       noSmall3Pops3$DistRoad + noSmall3Pops3$Lat)

summary(minF3.glm.CITY3)
#AIC = 165.71
#Date p-value = 0.00911

minF3.glm.CITY4 = glm(noSmall3Pops3$minF~noSmall3Pops3$CityorMountainLump+ noSmall3Pops3$Date + 
                        noSmall3Pops3$Lat)

summary(minF3.glm.CITY4)
#AIC = 164.5
#Date p-value = 0.0118

minF3.glm.CITY5 = glm(noSmall3Pops3$minF~noSmall3Pops3$CityorMountainLump+ noSmall3Pops3$Date)
summary(minF3.glm.CITY5)
#AIC = 194.03

minF3.glm.CITY6 = glm(noSmall3Pops3$minF~noSmall3Pops3$CityorMountainLump)
summary(minF3.glm.CITY6)
#AIC = 445.1

pairs(emmeans(minF3.glm.CITY2, "CityorMountainLump"))

min3.resid.CITY = as.data.frame(minF3.glm.CITY2$residuals)
View(min3.resid.CITY)
min3.resid.CITY$BirdID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
                          21,22,24,25,26,27,28,29,30,31,32,33,34,35,36,38,39,40,41,42,
                          44,45,46,48,49,50,55,57,58,59,60,61,68,69,70,71,
                          72,73,74,77,80,81,82,83,84,85,86,87,88,89,90,91,
                          92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,
                          110,111,112,131,132,133,154)


min3.resid.CITYJuncos <- merge(noSmall3Pops3, min3.resid.CITY, by = "BirdID")
View(min3.resid.CITYJuncos)

ggplot(data = min3.resid.CITYJuncos, aes(x = CityorMountainLump, y = min3.resid.CITYJuncos$`minF3.glm.CITY2$residuals`,
                                        colour = CityorMountainLump)) + geom_point()

bartlett.test(min3.resid.CITYJuncos$`minF3.glm.CITY2$residuals`~min3.resid.CITYJuncos$CityorMountainLump)
#p-value = 0.06603

#maxF City or Mountain Lump - small pop
noSmall3Pops4 = NoSmallPop

maxF3.glm.CITY = glm(noSmall3Pops4$maxF~noSmall3Pops4$CityorMountainLump+ noSmall3Pops4$Date + 
                       noSmall3Pops4$Time + noSmall3Pops4$DistRoad + noSmall3Pops4$Lat + noSmall3Pops4$Long)

summary(maxF3.glm.CITY)
#AIC = 134.13

maxF3.glm.CITY2 = glm(noSmall3Pops4$maxF~noSmall3Pops4$CityorMountainLump+ noSmall3Pops4$Date + 
                       noSmall3Pops4$DistRoad + noSmall3Pops4$Lat + noSmall3Pops4$Long)

summary(maxF3.glm.CITY2)
#AIC = 160.8
#Date p-value = 0.0166

maxF3.glm.CITY3 = glm(noSmall3Pops4$maxF~noSmall3Pops4$CityorMountainLump+ noSmall3Pops4$Date + 
                        noSmall3Pops4$DistRoad + noSmall3Pops4$Lat)
summary(maxF3.glm.CITY3)
#AIC = 158.84
#Date p-value =0.0159

maxF3.glm.CITY4 = glm(noSmall3Pops4$maxF~noSmall3Pops4$CityorMountainLump+ noSmall3Pops4$Date + 
                        noSmall3Pops4$DistRoad)
summary(maxF3.glm.CITY4)
#AIC = 157.19
#Date p-value = 0.0182

maxF3.glm.CITY5 = glm(noSmall3Pops4$maxF~noSmall3Pops4$CityorMountainLump+ noSmall3Pops4$Date)
summary(maxF3.glm.CITY5)
#AIC = 180

maxF3.glm.CITY6 = glm(noSmall3Pops4$maxF~noSmall3Pops4$CityorMountainLump)
summary(maxF3.glm.CITY6)
#AIC = 592.43

pairs(emmeans(maxF3.glm.CITY, "CityorMountainLump"))

max3.resid.CITY = as.data.frame(maxF3.glm.CITY$residuals)
View(max3.resid.CITY)
max3.resid.CITY$BirdID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
                           21,22,24,25,26,27,28,29,30,31,32,33,34,35,36,38,39,40,41,42,
                           44,45,46,48,49,50,55,57,58,59,60,61,68,69,70,71,
                           72,73,74,77,80,81,82,83,84,85,86,87,88,89,90,91,
                           92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,
                           110,111,112,131,132,133,154)


max3.resid.CITYJuncos <- merge(noSmall3Pops4, max3.resid.CITY, by = "BirdID")
View(max3.resid.CITYJuncos)

ggplot(data = max3.resid.CITYJuncos, aes(x = CityorMountainLump, y = max3.resid.CITYJuncos$`maxF3.glm.CITY$residuals`,
                                         colour = CityorMountainLump)) + geom_point()

bartlett.test(max3.resid.CITYJuncos$`maxF3.glm.CITY$residuals`~max3.resid.CITYJuncos$CityorMountainLump)
#p-value = 0.04631*

#bandF City or Mountain Lump - small pop
noSmall3Pops5 = NoSmallPop

bandF3.glm.CITY = glm(noSmall3Pops5$bandwidthF~noSmall3Pops5$CityorMountainLump+ noSmall3Pops5$Date + 
                       noSmall3Pops5$Time + noSmall3Pops5$DistRoad + noSmall3Pops5$Lat + noSmall3Pops5$Long)

summary(bandF3.glm.CITY)
#AIC = 175.54

bandF3.glm.CITY2 = glm(noSmall3Pops5$bandwidthF~noSmall3Pops5$CityorMountainLump+ noSmall3Pops5$Date + 
                        noSmall3Pops5$Time + noSmall3Pops5$Lat + noSmall3Pops5$Long)

summary(bandF3.glm.CITY2)
#AIC = 173.63

bandF3.glm.CITY3= glm(noSmall3Pops5$bandwidthF~noSmall3Pops5$CityorMountainLump+ noSmall3Pops5$Date + 
                         noSmall3Pops5$Time + noSmall3Pops5$Long)

summary(bandF3.glm.CITY3)
#AIC = 171.78

bandF3.glm.CITY4 = glm(noSmall3Pops5$bandwidthF~noSmall3Pops5$CityorMountainLump + 
                        noSmall3Pops5$Time + noSmall3Pops5$Long)

summary(bandF3.glm.CITY4)
#AIC = 169.97

bandF3.glm.CITY5 = glm(noSmall3Pops5$bandwidthF~noSmall3Pops5$CityorMountainLump + 
                         noSmall3Pops5$Long)

summary(bandF3.glm.CITY5)
#AIC = 202.25

bandF3.glm.CITY6 = glm(noSmall3Pops5$bandwidthF~noSmall3Pops5$CityorMountainLump)
summary(bandF3.glm.CITY6)
#AIC = 699.46

pairs(emmeans(bandF3.glm.CITY4, "CityorMountainLump"))

bandF3.resid.CITY = as.data.frame(bandF3.glm.CITY4$residuals)
View(bandF3.resid.CITY)
bandF3.resid.CITY$BirdID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
                           21,22,24,25,26,27,28,29,30,31,32,33,34,35,36,38,39,40,41,42,
                           44,45,46,48,49,50,55,57,58,59,60,61,68,69,70,71,
                           72,73,74,77,80,81,82,83,84,85,86,87,88,89,90,91,
                           92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,
                           110,111,112,131,132,133,154)


bandF3.resid.CITYJuncos <- merge(noSmall3Pops5, bandF3.resid.CITY, by = "BirdID")
View(bandF3.resid.CITYJuncos)

ggplot(data = bandF3.resid.CITYJuncos, aes(x = CityorMountainLump, y = bandF3.resid.CITYJuncos$`bandF3.glm.CITY4$residuals`,
                                         colour = CityorMountainLump)) + geom_point()

bartlett.test(bandF3.resid.CITYJuncos$`bandF3.glm.CITY4$residuals`~bandF3.resid.CITYJuncos$CityorMountainLump)
#p-value = 0.03609*

#gridarray for boxplots, no color

#run PCA
?prcomp

JuncosPCA = prcomp(~IndividualJuncos$SongLength + IndividualJuncos$TrillRate 
                   + IndividualJuncos$minF + IndividualJuncos$maxF 
                   + IndividualJuncos$bandwidthF, scale = TRUE)
summary(JuncosPCA)
plot(JuncosPCA)
JuncosPCA

View(JuncosPCA$x)
JuncosPC <- merge(IndividualJuncos, JuncosPCA$x)
library("emmeans")
glm.PC <- glm(JuncosPC$PC1~JuncosPC$Location)
pairs(emmeans(glm.PC,"Location"))
#From the PCA, we find no differences between locations among traits.
JucosPC$Location <- as.character(JuncosPC$Location)
autoplot(JuncosPCA, data = JuncosPC, colour = "Location", jitter = all)
ggplot(JuncosPC, aes(x = PC1, y = PC2, color = Location))+geom_point()

noOLDPC <- JuncosPC[JuncosPC$Location != "UCSD 2006/2007",]
ggplot(noOLDPC, aes(x = PC1, y = PC2, color = Location))+geom_point()


#Stats consulting center said to run the models, test the residuals for equal variance, not outcome.
#DONT MEET ASSUMPTIONS
#SONGLENGTH
#MAXF 
#BandF

#Songlength
#trillrate
#maxf
#bandf

#SongLength
bartlett.test(IndividualJuncos$SongLength~IndividualJuncos$Location)
bartlett.test(IndividualJuncos$SongLength~IndividualJuncos$UrbanorNot)
bartlett.test(IndividualJuncos$SongLength~IndividualJuncos$CityorMountainLump)

leveneTest(IndividualJuncos$SongLength~IndividualJuncos$Location)
leveneTest(IndividualJuncos$SongLength~IndividualJuncos$UrbanorNot)
leveneTest(IndividualJuncos$SongLength~IndividualJuncos$CityorMountainLump)

#SongLength Location: p-value 0.0006375
#SongLength UrbanNon: p-value 0.001066
#SongLength CityorMountain: p-value 0.0001624

#TrillRate
bartlett.test(IndividualJuncos$TrillRate~IndividualJuncos$Location)
bartlett.test(IndividualJuncos$TrillRate~IndividualJuncos$UrbanorNot)
bartlett.test(IndividualJuncos$TrillRate~IndividualJuncos$CityorMountainLump)

#TrillRate Location: p-value 0.1855
#TrillRate UrbanNon: p-value 0.174
#TrillRate CityorMountain: p-value 0.08475

#minF
bartlett.test(IndividualJuncos$minF~IndividualJuncos$Location)
bartlett.test(IndividualJuncos$minF~IndividualJuncos$UrbanorNot)
bartlett.test(IndividualJuncos$minF~IndividualJuncos$CityorMountainLump)

#minF Location: p-value 0.1398
#minF UrbanNon: p-value 0.8835
#minF CityorMountain: p-value 0.2065

#maxF
bartlett.test(IndividualJuncos$maxF~IndividualJuncos$Location)
bartlett.test(IndividualJuncos$maxF~IndividualJuncos$UrbanorNot)
bartlett.test(IndividualJuncos$maxF~IndividualJuncos$CityorMountainLump)

#maxF Location: p-value 0.00002561
#maxF UrbanNon: p-value 0.05395
#maxF CityorMountain: p-value 0.000002319

#bandF
bartlett.test(IndividualJuncos$bandwidthF~IndividualJuncos$Location)
bartlett.test(IndividualJuncos$bandwidthF~IndividualJuncos$UrbanorNot)
bartlett.test(IndividualJuncos$bandwidthF~IndividualJuncos$CityorMountainLump)

#bandwidthF Location: p-value 0.00006171
#bandwidthF UrbanNon: p-value 0.01207
#bandwidthF CityorMountain: p-value 0.00001106

#Redo Bartlett test without small populations

#SongLength
bartlett.test(NoSmallPop$SongLength~NoSmallPop$Location)
bartlett.test(NoSmallPop$SongLength~NoSmallPop$UrbanorNot)
bartlett.test(NoSmallPop$SongLength~NoSmallPop$CityorMountainLump)

#SongLength Location: p-value 0.0002096
#SongLength UrbanNon: p-value 0.0004075
#SongLength CityorMountain: p-value 0.0002676

#TrillRate
bartlett.test(NoSmallPop$TrillRate~NoSmallPop$Location)
bartlett.test(NoSmallPop$TrillRate~NoSmallPop$UrbanorNot)
bartlett.test(NoSmallPop$TrillRate~NoSmallPop$CityorMountainLump)

#TrillRate Location: p-value 0.0465
#TrillRate UrbanNon: p-value 0.06311
#TrillRate CityorMountain: p-value 0.02383

#minF
bartlett.test(NoSmallPop$minF~NoSmallPop$Location)
bartlett.test(NoSmallPop$minF~NoSmallPop$UrbanorNot)
bartlett.test(NoSmallPop$minF~NoSmallPop$CityorMountainLump)

#minF Location: p-value 0.09399
#minF UrbanNon: p-value 0.4294
#minF CityorMountain: p-value 0.1245

#maxF
bartlett.test(NoSmallPop$maxF~NoSmallPop$Location)
bartlett.test(NoSmallPop$maxF~NoSmallPop$UrbanorNot)
bartlett.test(NoSmallPop$maxF~NoSmallPop$CityorMountainLump)

#maxF Location: p-value 0.000004887
#maxF UrbanNon: p-value 0.03355
#maxF CityorMountain: p-value 0.000002094

#bandF
bartlett.test(NoSmallPop$bandwidthF~NoSmallPop$Location)
bartlett.test(NoSmallPop$bandwidthF~NoSmallPop$UrbanorNot)
bartlett.test(NoSmallPop$bandwidthF~NoSmallPop$CityorMountainLump)

#bandwidthF Location: p-value 0.000002967
#bandwidthF UrbanNon: p-value 0.003299
#bandwidthF CityorMountain: p-value 0.000001194

#adds variance to the Location Juncos table
LocationJuncos$varSongLength <- c(var(UCLAJuncos$SongLength),var(UCSBJuncos$SongLength),
                                  var(UCSDJuncos$SongLength),var(OXYJuncos$SongLength), 
                                  var(STUNTJuncos$SongLength), var(JAMESJuncos$SongLength), 
                                  var(ANGELESJuncos$SongLength), var(REPOSITORYJuncos$SongLength), 
                                  var(oldUCSDJuncos$SongLength))

LocationJuncos$varTrillRate <- c(var(UCLAJuncos$TrillRate),var(UCSBJuncos$TrillRate),
                                 var(UCSDJuncos$TrillRate),var(OXYJuncos$TrillRate), 
                                 var(STUNTJuncos$TrillRate), var(JAMESJuncos$TrillRate), 
                                 var(ANGELESJuncos$TrillRate), var(REPOSITORYJuncos$TrillRate), 
                                 var(oldUCSDJuncos$TrillRate))

LocationJuncos$varminF <- c(var(UCLAJuncos$minF),var(UCSBJuncos$minF),
                            var(UCSDJuncos$minF),var(OXYJuncos$minF), 
                            var(STUNTJuncos$minF), var(JAMESJuncos$minF), 
                            var(ANGELESJuncos$minF), var(REPOSITORYJuncos$minF), 
                            var(oldUCSDJuncos$minF))

LocationJuncos$varmaxF <- c(var(UCLAJuncos$maxF),var(UCSBJuncos$maxF),
                            var(UCSDJuncos$maxF),var(OXYJuncos$maxF), 
                            var(STUNTJuncos$maxF), var(JAMESJuncos$maxF), 
                            var(ANGELESJuncos$maxF), var(REPOSITORYJuncos$maxF), 
                            var(oldUCSDJuncos$maxF))

LocationJuncos$varbandF <- c(var(UCLAJuncos$bandwidthF),var(UCSBJuncos$bandwidthF),
                             var(UCSDJuncos$bandwidthF),var(OXYJuncos$bandwidthF), 
                             var(STUNTJuncos$bandwidthF), var(JAMESJuncos$bandwidthF), 
                             var(ANGELESJuncos$bandwidthF), var(REPOSITORYJuncos$bandwidthF), 
                             var(oldUCSDJuncos$bandwidthF))

#graphing variance
plot(LocationJuncos$varSongLength~LocationJuncos$LocationLOC)
plot(LocationJuncos$varTrillRate~LocationJuncos$LocationLOC)
plot(LocationJuncos$varminF~LocationJuncos$LocationLOC)
plot(LocationJuncos$varmaxF~LocationJuncos$LocationLOC)
plot(LocationJuncos$varbandF~LocationJuncos$LocationLOC)

#Because variances are not equal, it does not depend on sample size.
#We cannot just remove the low sample sizes. We know it's not biasing the test.





