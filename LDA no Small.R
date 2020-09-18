#Linear Discriminant Analysis - Location, all populations
LDAJuncos.noSmall = lda(noSmallRep$Location ~ noSmallRep$SongLength 
                + noSmallRep$TrillRate + noSmallRep$minF
                + noSmallRep$maxF + noSmallRep$bandwidthF)

LDAJuncos.noSmall

#Coefficients of linear discriminants:
#  LD1         LD2        LD3
#noSmallRep$SongLength  3.04288402  3.62906730  0.8046155
#noSmallRep$TrillRate   0.18209390 -0.22525889  0.1067192
#noSmallRep$minF       -0.08305071 -0.06729029 -0.6938332
#noSmallRep$maxF        0.73006365 -0.02219692 -1.4402777
#noSmallRep$bandwidthF  0.52862272  0.03147212 -0.4587933

#Proportion of trace:
#  LD1    LD2    LD3 
#0.6788 0.2465 0.0747 

noSmallRep$Observation = 1:nrow(noSmallRep)
View(noSmallRep)

LDAJuncos.noSmall.values = predict(LDAJuncos.noSmall, noSmallRep)
View(LDIndi.noSmall)
LDIndi.noSmall = as.data.frame(LDAJuncos.noSmall.values$x)
LDIndi.noSmall$Observation = 1:nrow(LDIndi.noSmall)

LDJuncos.noSmall <- merge(noSmallRep, LDIndi.noSmall, by = "Observation")
View(LDJuncos.noSmall)

plotLDA.noSmall.location = ggplot(data = LDJuncos.noSmall, aes(x = LD1, y = LD2, colour = Location)) +
  stat_ellipse(aes(x=LD1, y=LD2,color=Location, group=Location),type = "norm") +
  theme() + geom_point() + theme_classic()
plotLDA.noSmall.location

glm.LD.noSmall <- glm(LDJuncos.noSmall$LD1~LDJuncos.noSmall$Location + LDJuncos.noSmall$Date + LDJuncos.noSmall$Time +
                LDJuncos.noSmall$DistRoad + LDJuncos.noSmall$Lat + LDJuncos.noSmall$Long)
summary(glm.LD.noSmall)
#AIC = 278.24

glm.LD.noSmall2 <- glm(LDJuncos.noSmall$LD1~LDJuncos.noSmall$Location + LDJuncos.noSmall$Date +
                        LDJuncos.noSmall$DistRoad + LDJuncos.noSmall$Lat + LDJuncos.noSmall$Long)
summary(glm.LD.noSmall2)
#AIC = 276.37

glm.LD.noSmall3 <- glm(LDJuncos.noSmall$LD1~LDJuncos.noSmall$Location +
                         LDJuncos.noSmall$DistRoad + LDJuncos.noSmall$Lat + LDJuncos.noSmall$Long)
summary(glm.LD.noSmall3)
#AIC = 275.7

glm.LD.noSmall4 <- glm(LDJuncos.noSmall$LD1~LDJuncos.noSmall$Location +
                          LDJuncos.noSmall$Lat + LDJuncos.noSmall$Long)
summary(c)
#AIC = 274.61

glm.LD.noSmall5 <- glm(LDJuncos.noSmall$LD1~LDJuncos.noSmall$Location +
                        LDJuncos.noSmall$Long)
summary(glm.LD.noSmall5)
#AIC = 274.7

glm.LD.noSmall6 <- glm(LDJuncos.noSmall$LD1~LDJuncos.noSmall$Location)
summary(glm.LD.noSmall6)
#AIC = 332.28
View(LDJuncos.noSmall)

pairs(emmeans(glm.LD.noSmall4, "Location"))

Resid.noSmall = as.data.frame(glm.LD.noSmall4$residuals)
View(Resid.noSmall)
Resid.noSmall$BirdID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,
                              20,21,22,24,25,26,27,28,29,30,31,32,33,34,35,36,38,39,40,
                              41
                              ,42,44,45,46,48,49,50,55,57,58,59,60,61,
                              68,69,70,71,72,73,74,77,78,79,80,81,
                              82,83,85,86,87,88,89,90,91,92,93,94,95,96,97,
                              99,100,101,102,103,104,105,106,107,108,109,
                              110,111,112,131,132,133,154)

ResidJuncos.noSmall <- merge(LDJuncos.noSmall, Resid.noSmall, by = "BirdID")
View(ResidJuncos.noSmall)


ggplot(data = ResidJuncos.noSmall, aes(x = Location, y = ResidJuncos.noSmall$`glm.LD.noSmall4$residuals`, colour = Location)) + geom_point()

bartlett.test(ResidJuncos.noSmall$`glm.LD.noSmall4$residuals`~ResidJuncos.noSmall$Location)
#p-value = 0.6009

#UrbanvsNotUrban

LDAJuncos2.noSmall = lda(noSmallRep$UrbanorNot ~ noSmallRep$SongLength 
                 + noSmallRep$TrillRate + noSmallRep$minF
                 + noSmallRep$maxF + noSmallRep$bandwidthF)

View(LDAJuncos2.noSmall)
LDAJuncos2.noSmall

#Coefficients of linear discriminants:
#LD1
#noSmallRep$SongLength -2.5648909
#noSmallRep$TrillRate   0.1525034
#noSmallRep$minF        0.4491207
#noSmallRep$maxF        1.0511131
#noSmallRep$bandwidthF  0.3758792

LDAJuncos2.noSmall.values = predict(LDAJuncos2.noSmall, noSmallRep)
LDIndi2.noSmall = as.data.frame(LDAJuncos2.noSmall.values$x)
LDIndi2.noSmall$Observation = 1:nrow(LDIndi2.noSmall)

LDJuncos2.noSmall <- merge(noSmallRep, LDIndi2.noSmall, by = "Observation")

plotLDA.noSmall.UorN = ggplot(data = LDJuncos2.noSmall, aes(x = UrbanorNot, y = LD1)) + geom_boxplot()
plotLDA.noSmall.UorN

glm2.LD.noSmall <- glm(LDJuncos2.noSmall$LD1~LDJuncos2.noSmall$UrbanorNot + LDJuncos2.noSmall$Date + LDJuncos2.noSmall$Time +
                         LDJuncos2.noSmall$DistRoad + LDJuncos2.noSmall$Lat + LDJuncos2.noSmall$Long)
summary(glm2.LD.noSmall)
#AIC = 283.32

glm2.LD.noSmall2 <- glm(LDJuncos2.noSmall$LD1~LDJuncos2.noSmall$UrbanorNot + LDJuncos2.noSmall$Date + LDJuncos2.noSmall$Time +
                         LDJuncos2.noSmall$DistRoad + LDJuncos2.noSmall$Lat)
summary(glm2.LD.noSmall2)
#AIC = 281.39

glm2.LD.noSmall3 <- glm(LDJuncos2.noSmall$LD1~LDJuncos2.noSmall$UrbanorNot + LDJuncos2.noSmall$Date +
                          LDJuncos2.noSmall$DistRoad + LDJuncos2.noSmall$Lat)
summary(glm2.LD.noSmall3)
#AIC = 279.52

glm2.LD.noSmall4 <- glm(LDJuncos2.noSmall$LD1~LDJuncos2.noSmall$UrbanorNot + LDJuncos2.noSmall$Date +
                         LDJuncos2.noSmall$Lat)
summary(glm2.LD.noSmall4)
#AIC = 279.84

glm2.LD.noSmall5 <- glm(LDJuncos2.noSmall$LD1~LDJuncos2.noSmall$UrbanorNot + LDJuncos2.noSmall$Date)
summary(glm2.LD.noSmall5)
#AIC = 329.49

glm2.LD.noSmall5 <- glm(LDJuncos2.noSmall$LD1~LDJuncos2.noSmall$UrbanorNot)
summary(glm2.LD.noSmall5)
#AIC = 330.34

pairs(emmeans(glm2.LD.noSmall3, "UrbanorNot"))


ResidU.noSmall = as.data.frame(glm2.LD.noSmall3$residuals)
ResidU.noSmall$BirdID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,
                          20,21,22,24,25,26,27,28,29,30,31,32,33,34,35,36,38,39,40,
                          41
                          ,42,44,45,46,48,49,50,55,57,58,59,60,61,
                          68,69,70,71,72,73,74,77,78,79,80,81,
                          82,83,85,86,87,88,89,90,91,92,93,94,95,96,97,
                          99,100,101,102,103,104,105,106,107,108,109,
                          110,111,112,131,132,133,154)
ResidUJuncos.noSmall <- merge(noSmallRep, ResidU.noSmall, by = "BirdID")
View(ResidUJuncos.noSmall)

ggplot(data = ResidUJuncos.noSmall, aes(x = UrbanorNot, y = ResidUJuncos.noSmall$`glm2.LD.noSmall3$residuals`,
                                colour = UrbanorNot)) + geom_point()

bartlett.test(ResidUJuncos.noSmall$`glm2.LD.noSmall3$residuals`~ ResidUJuncos.noSmall$UrbanorNot)
#p-value = 0.5608

#w/ repository
LDAJuncos4.noSmall = lda(noSmall$UrbanorNot ~ noSmall$SongLength 
                 + noSmall$TrillRate + noSmall$minF
                 + noSmall$maxF + noSmall$bandwidthF)

View(LDAJuncos4.noSmall)
LDAJuncos4.noSmall

#Coefficients of linear discriminants:
#  LD1
#noSmall$SongLength -3.6474539
#noSmall$TrillRate   0.1063236
#noSmall$minF       -0.3438208
#noSmall$maxF        0.5321629
#noSmall$bandwidthF  0.6275133

LDAJuncos4.values.noSmall = predict(LDAJuncos4.noSmall, noSmall)
LDIndi4.noSmall = as.data.frame(LDAJuncos4.values.noSmall$x)
LDIndi4.noSmall$Observation = 1:nrow(LDIndi4.noSmall)
noSmall$Observation = 1:nrow(noSmall)

LDJuncos4.noSmall <- merge(noSmall, LDIndi4.noSmall, by = "Observation")
View(LDJuncos4.noSmall)

plotLDA.noSmall.UorNwRep = ggplot(data = LDJuncos4.noSmall, aes(x = UrbanorNot, y = LD1)) + geom_boxplot()
plotLDA.noSmall.UorNwRep

glm4.LD.noSmall <- glm(LDJuncos4.noSmall$LD1~LDJuncos4.noSmall$UrbanorNot + LDJuncos4.noSmall$Date + LDJuncos4.noSmall$Time +
                 LDJuncos4.noSmall$DistRoad + LDJuncos4.noSmall$Lat + LDJuncos4.noSmall$Long)
summary(glm4.LD.noSmall)
#AIC = 270.53

glm4.LD.noSmall2 <- glm(LDJuncos4.noSmall$LD1~LDJuncos4.noSmall$UrbanorNot + LDJuncos4.noSmall$Date +
                         LDJuncos4.noSmall$DistRoad + LDJuncos4.noSmall$Lat + LDJuncos4.noSmall$Long)
summary(glm4.LD.noSmall2)
#AIC = 336.38

glm4.LD.noSmall3 <- glm(LDJuncos4.noSmall$LD1~LDJuncos4.noSmall$UrbanorNot + LDJuncos4.noSmall$Date +
                          LDJuncos4.noSmall$DistRoad + LDJuncos4.noSmall$Lat)
summary(glm4.LD.noSmall3)
#AIC = 334.75

glm4.LD.noSmall4 <- glm(LDJuncos4.noSmall$LD1~LDJuncos4.noSmall$UrbanorNot + 
                          LDJuncos4.noSmall$DistRoad + LDJuncos4.noSmall$Lat)
summary(glm4.LD.noSmall4)
#AIC = 334.43

glm4.LD.noSmall5 <- glm(LDJuncos4.noSmall$LD1~LDJuncos4.noSmall$UrbanorNot + 
                          LDJuncos4.noSmall$Lat)
summary(glm4.LD.noSmall5)
#AIC = 333.66

glm4.LD.noSmall6 <- glm(LDJuncos4.noSmall$LD1~LDJuncos4.noSmall$UrbanorNot)
summary(glm4.LD.noSmall6)
#AIC = 384.26

pairs(emmeans(glm4.LD.noSmall, "UrbanorNot"))


ResidU2.noSmall = as.data.frame(glm4.LD.noSmall$residuals)
ResidU2.noSmall$BirdID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,
                           20,21,22,24,25,26,27,28,29,30,31,32,33,34,35,36,38,39,40,
                           41
                           ,42,44,45,46,48,49,50,55,57,58,59,60,61,
                           68,69,70,71,72,73,74,77,78,79,80,81,
                           82,83,85,86,87,88,89,90,91,92,93,94,95,96,97,
                           99,100,101,102,103,104,105,106,107,108,109,
                           110,111,112,131,132,133,154)
View(ResidU2.noSmall)
ResidUJuncos2.noSmall <- merge(noSmall, ResidU2.noSmall, by = "BirdID")
View(ResidUJuncos2.noSmall)

ggplot(data = ResidUJuncos2.noSmall, aes(x = UrbanorNot, y = ResidUJuncos2.noSmall$`glm4.LD.noSmall$residuals`,
                                 colour = UrbanorNot)) + geom_point()

bartlett.test(ResidUJuncos2.noSmall$`glm4.LD.noSmall$residuals`~ ResidUJuncos2.noSmall$UrbanorNot)
#p-value = 0.3582

#CityorMountainLump
LDAJuncos3.noSmall = lda(noSmallRep$CityorMountainLump ~ noSmallRep$SongLength 
                 + noSmallRep$TrillRate + noSmallRep$minF
                 + noSmallRep$maxF + noSmallRep$bandwidthF)

View(LDAJuncos3.noSmall)
LDAJuncos3.noSmall

#Coefficients of linear discriminants:
#LD1         LD2        LD3
#noSmallRep$SongLength  3.04288402  3.62906730  0.8046155
#noSmallRep$TrillRate   0.18209390 -0.22525889  0.1067192
#noSmallRep$minF       -0.08305071 -0.06729029 -0.6938332
#noSmallRep$maxF        0.73006365 -0.02219692 -1.4402777
#noSmallRep$bandwidthF  0.52862272  0.03147212 -0.4587933

#Proportion of trace:
#  LD1    LD2    LD3 
#0.6788 0.2465 0.0747 

LDAJuncos3.values.noSmall = predict(LDAJuncos3.noSmall, noSmallRep)
LDIndi3.noSmall = as.data.frame(LDAJuncos3.values.noSmall$x)
LDIndi3.noSmall$Observation = 1:nrow(LDIndi3.noSmall)

LDJuncos3.noSmall <- merge(noSmallRep, LDIndi3.noSmall, by = "Observation")
View(LDJuncos3.noSmall)

ggplot(data = LDJuncos3.noSmall, aes(x = LD1, y = LD2, colour = CityorMountainLump)) +
  stat_ellipse(aes(x=LD1, y=LD2,color=CityorMountainLump, group=CityorMountainLump),type = "norm") +
  theme() + geom_point() + theme_classic()

glm3.LD.noSmall <- glm(LDJuncos3.noSmall$LD1~LDJuncos3.noSmall$CityorMountainLump + LDJuncos3.noSmall$Date + LDJuncos3.noSmall$Time +
                 LDJuncos3.noSmall$DistRoad + LDJuncos3.noSmall$Lat + LDJuncos3.noSmall$Long)
summary(glm3.LD.noSmall)
#AIC = 278.24

glm3.LD.noSmall2 <- glm(LDJuncos3.noSmall$LD1~LDJuncos3.noSmall$CityorMountainLump + LDJuncos3.noSmall$Date +
                         LDJuncos3.noSmall$DistRoad + LDJuncos3.noSmall$Lat + LDJuncos3.noSmall$Long)
summary(glm3.LD.noSmall2)
#AIC = 276.37

glm3.LD.noSmall3 <- glm(LDJuncos3.noSmall$LD1~LDJuncos3.noSmall$CityorMountainLump +
                          LDJuncos3.noSmall$DistRoad + LDJuncos3.noSmall$Lat + LDJuncos3.noSmall$Long)
summary(glm3.LD.noSmall3)
#AIC = 275.7

glm3.LD.noSmall4 <- glm(LDJuncos3.noSmall$LD1~LDJuncos3.noSmall$CityorMountainLump +
                          LDJuncos3.noSmall$Lat + LDJuncos3.noSmall$Long)
summary(glm3.LD.noSmall4)
#AIC = 274.61

glm3.LD.noSmall5 <- glm(LDJuncos3.noSmall$LD1~LDJuncos3.noSmall$CityorMountainLump +
                        LDJuncos3.noSmall$Long)
summary(glm3.LD.noSmall5)
#AIC = 274.7

glm3.LD.noSmall6 <- glm(LDJuncos3.noSmall$LD1~LDJuncos3.noSmall$CityorMountainLump)
summary(glm3.LD.noSmall6)
#AIC = 332.28

pairs(emmeans(glm3.LD.noSmall4, "CityorMountainLump"))

ResidCITY.noSmall = as.data.frame(glm3.LD.noSmall4$residuals)
ResidCITY.noSmall$BirdID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,
                             20,21,22,24,25,26,27,28,29,30,31,32,33,34,35,36,38,39,40,
                             41
                             ,42,44,45,46,48,49,50,55,57,58,59,60,61,
                             68,69,70,71,72,73,74,77,78,79,80,81,
                             82,83,85,86,87,88,89,90,91,92,93,94,95,96,97,
                             99,100,101,102,103,104,105,106,107,108,109,
                             110,111,112,131,132,133,154)
View(ResidCITY.noSmall)

ResidCITYJuncos.noSmall <- merge(noSmallRep, ResidCITY.noSmall, by = "BirdID")
View(ResidCITYJuncos.noSmall)

ggplot(data = ResidCITYJuncos.noSmall, aes(x = CityorMountainLump, y = ResidCITYJuncos.noSmall$`glm3.LD.noSmall4$residuals`,
                                   colour = CityorMountainLump)) + geom_point()

bartlett.test(ResidCITYJuncos.noSmall$`glm3.LD.noSmall4$residuals`~ResidCITYJuncos.noSmall$CityorMountainLump)
#p-value = 0.6009



