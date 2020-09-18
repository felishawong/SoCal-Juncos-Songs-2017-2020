#Linear Discriminant Analysis - Location, all populations
library(MASS)
LDAJuncos = lda(noOldRep$Location ~ noOldRep$SongLength 
                + noOldRep$TrillRate + noOldRep$minF
                + noOldRep$maxF + noOldRep$bandwidthF)

LDAJuncos
?pairs()
?glm()
?emmeans()

#Coefficients of linear discriminants:
#  LD1        LD2         LD3         LD4
#noOldRep$SongLength -3.19189859  0.5997185 -1.23488197  3.24769004
#noOldRep$TrillRate  -0.06817688 -0.3015659  0.02958298 -0.04234938
#noOldRep$minF       -0.01639382  0.5120853  1.31188536  0.84593871
#noOldRep$maxF       -1.06218549  0.3607330  1.15578836 -0.37190241
#noOldRep$bandwidthF -0.64237512 -0.1146711 -0.15132968 -0.78454892

#Proportion of trace:
#  LD1    LD2    LD3    LD4 
#0.6046 0.2655 0.0961 0.0338 

View(noOldRep)

noOldRep$Observation = 1:nrow(noOldRep)

LDAJuncos.values = predict(LDAJuncos, noOldRep)
LDIndi = as.data.frame(LDAJuncos.values$x)
LDIndi$Observation = 1:nrow(LDIndi)

LDJuncos <- merge(noOldRep, LDIndi, by = "Observation")

library(ggplot2)
plotLDAAllPop.location = ggplot(data = LDJuncos, aes(x = LD1, y = LD2, colour = Location)) +
  stat_ellipse(aes(x=LD1, y=LD2,color=Location, group=Location),type = "norm") +
  theme() + geom_point() + theme_classic() + ggtitle("LDA - All Populations by Location")
?stat_ellipse()
plotLDAAllPop.location

glm.LD <- glm(LDJuncos$LD1~LDJuncos$Location + LDJuncos$Date + LDJuncos$Time +
                LDJuncos$DistRoad + LDJuncos$Lat + LDJuncos$Long)
summary(glm.LD)
#AIC = 324.67

glm.LD2 <- glm(LDJuncos$LD1~LDJuncos$Location + LDJuncos$Date + LDJuncos$Time +
                LDJuncos$Lat + LDJuncos$Long)
summary(glm.LD2)
#AIC = 322.74

glm.LD3 <- glm(LDJuncos$LD1~LDJuncos$Location + LDJuncos$Date +
                 LDJuncos$Lat + LDJuncos$Long)
summary(glm.LD3)
#AIC = 320.85

glm.LD4 <- glm(LDJuncos$LD1~LDJuncos$Location +
                 LDJuncos$Lat + LDJuncos$Long)
summary(glm.LD4)
#AIC = 318.95

glm.LD5 <- glm(LDJuncos$LD1~LDJuncos$Location +
                 LDJuncos$Long)
summary(glm.LD5)
#AIC = 319.66

glm.LD6 <- glm(LDJuncos$LD1~LDJuncos$Location)
summary(glm.LD6)
#AIC = 377.73

library(emmeans)
pairs(emmeans(glm.LD4, "Location"))

Resid = as.data.frame(glm.LD4$residuals)
View(Resid)
Resid$BirdID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,24,25,26,27,28,29,30,31,
                 32,33,34,35,36,38,39,40,41,42,44,45,46,48,49,50,55,57,58,59,60,61,62,63,64,65,
                 66,68,69,70,71,72,73,74,77,78,79,80,81,82,83,85,86,87,88,89,90,91,92,93,94,95,
                 96,97,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,116,117,118,
                 119,120,131,132,133,153,154)

ResidJuncos <- merge(noOldRep, Resid, by = "BirdID")
View(ResidJuncos)


ggplot(data = ResidJuncos, aes(x = Location, y = ResidJuncos$`glm.LD4$residuals`, colour = Location)) + geom_point()

bartlett.test(ResidJuncos$`glm.LD4$residuals`~ResidJuncos$Location)
#p-value = 0.3919

#UrbanvsNotUrban

LDAJuncos2 = lda(noOldRep$UrbanorNot ~ noOldRep$SongLength 
                 + noOldRep$TrillRate + noOldRep$minF
                 + noOldRep$maxF + noOldRep$bandwidthF)

View(LDAJuncos2)
LDAJuncos2

#Coefficients of linear discriminants:
#LD1
#noOldRep$SongLength  0.18985930
#noOldRep$TrillRate  -0.04820538
#noOldRep$minF        0.65591592
#noOldRep$maxF        1.57985039
#noOldRep$bandwidthF  0.54591839

LDAJuncos2.values = predict(LDAJuncos2, noOldRep)
LDIndi2 = as.data.frame(LDAJuncos2.values$x)
LDIndi2$Observation = 1:nrow(LDIndi2)

LDJuncos2 <- merge(noOldRep, LDIndi2, by = "Observation")

plotLDAAllPop.UorN = ggplot(data = LDJuncos2, aes(x = UrbanorNot, y = LD1)) + geom_boxplot() + ggtitle("LDA - All Populations by Urban or Natural")
plotLDAAllPop.UorN

glm2.LD <- glm(LDJuncos2$LD1~LDJuncos2$UrbanorNot + LDJuncos2$Date + LDJuncos2$Time +
                 LDJuncos2$DistRoad + LDJuncos2$Lat + LDJuncos2$Long)
summary(glm2.LD)
#AIC = 328.31

glm2.LD2 <- glm(LDJuncos2$LD1~LDJuncos2$UrbanorNot + LDJuncos2$Time +
                  LDJuncos2$DistRoad + LDJuncos2$Lat + LDJuncos2$Long)
summary(glm2.LD2)
#AIC = 326.65

glm2.LD3 <- glm(LDJuncos2$LD1~LDJuncos2$UrbanorNot + LDJuncos2$DistRoad +
                  LDJuncos2$Lat + LDJuncos2$Long)
summary(glm2.LD3)
#AIC = 324.76

glm2.LD4 <- glm(LDJuncos2$LD1~LDJuncos2$UrbanorNot + LDJuncos2$Long +
                  LDJuncos2$Lat)
summary(glm2.LD4)
#AIC = 323.28

glm2.LD5 <- glm(LDJuncos2$LD1~LDJuncos2$UrbanorNot + LDJuncos2$Lat)
summary(glm2.LD5)
#AIC = 321.91

glm2.LD6 <- glm(LDJuncos2$LD1~LDJuncos2$UrbanorNot)
summary(glm2.LD6)
#AIC = 372.91

pairs(emmeans(glm2.LD5, "UrbanorNot"))


ResidU = as.data.frame(glm2.LD5$residuals)
ResidU$BirdID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,24,25,26,27,28,29,30,31,
                  32,33,34,35,36,38,39,40,41,42,44,45,46,48,49,50,55,57,58,59,60,61,62,63,64,65,
                  66,68,69,70,71,72,73,74,77,78,79,80,81,82,83,85,86,87,88,89,90,91,92,93,94,95,
                  96,97,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,116,117,118,
                  119,120,131,132,133,153,154)
View(ResidU)
ResidUJuncos <- merge(noOldRep, ResidU, by = "BirdID")
View(ResidUJuncos)

ggplot(data = ResidUJuncos, aes(x = UrbanorNot, y = ResidUJuncos$`glm2.LD5$residuals`,
                                colour = UrbanorNot)) + geom_point()

bartlett.test(ResidUJuncos$`glm2.LD5$residuals`~ ResidUJuncos$UrbanorNot)
#p-value = 0.1988

#w/ repository
LDAJuncos4 = lda(noOld$UrbanorNot ~ noOld$SongLength 
                 + noOld$TrillRate + noOld$minF
                 + noOld$maxF + noOld$bandwidthF)

View(LDAJuncos4)
LDAJuncos4

#Coefficients of linear discriminants:
#LD1
#noOld$SongLength -2.21520977
#noOld$TrillRate  -0.04215192
#noOld$minF        0.03537047
#noOld$maxF        1.13661183
#noOld$bandwidthF  0.71696159

LDAJuncos4.values = predict(LDAJuncos4, noOld)
LDIndi4 = as.data.frame(LDAJuncos4.values$x)
LDIndi4$Observation = 1:nrow(LDIndi4)
noOld$Observation = 1:nrow(noOld)

LDJuncos4 <- merge(noOld, LDIndi4, by = "Observation")
View(LDJuncos4)

plotLDAAllPop.UorNwRep = ggplot(data = LDJuncos4, aes(x = UrbanorNot, y = LD1)) + geom_boxplot() + ggtitle("LDA - All Populations by Urban or Natural (With Repository)")
plotLDAAllPop.UorNwRep

glm4.LD <- glm(LDJuncos4$LD1~LDJuncos4$UrbanorNot + LDJuncos4$Date + LDJuncos4$Time +
                 LDJuncos4$DistRoad + LDJuncos4$Lat + LDJuncos4$Long)
summary(glm4.LD)
#AIC = 325.61

glm4.LD2 <- glm(LDJuncos4$LD1~LDJuncos4$UrbanorNot + LDJuncos4$Time +
                 LDJuncos4$DistRoad + LDJuncos4$Lat + LDJuncos4$Long)
summary(glm4.LD2)
#AIC = 323.62

glm4.LD3 <- glm(LDJuncos4$LD1~LDJuncos4$UrbanorNot + LDJuncos4$Time +
                  LDJuncos4$Lat + LDJuncos4$Long)
summary(glm4.LD3)
#AIC = 321.64

glm4.LD4 <- glm(LDJuncos4$LD1~LDJuncos4$UrbanorNot + LDJuncos4$Time +
                  LDJuncos4$Lat)
summary(glm4.LD4)
#AIC = 319.79

glm4.LD5 <- glm(LDJuncos4$LD1~LDJuncos4$UrbanorNot + LDJuncos4$Time)
summary(glm4.LD5)
#AIC = 374.12

glm4.LD6 <- glm(LDJuncos4$LD1~LDJuncos4$UrbanorNot)
summary(glm4.LD6)
#AIC = 426.83

pairs(emmeans(glm4.LD4, "UrbanorNot"))


ResidU2 = as.data.frame(glm4.LD4$residuals)
ResidU2$BirdID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,24,25,26,27,28,29,30,31,
                  32,33,34,35,36,38,39,40,41,42,44,45,46,48,49,50,55,57,58,59,60,61,62,63,64,65,
                  66,68,69,70,71,72,73,74,77,78,79,80,81,82,83,85,86,87,88,89,90,91,92,93,94,95,
                  96,97,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,116,117,118,
                  119,120,131,132,133,153,154)
View(ResidU2)
ResidUJuncos2 <- merge(noOld, ResidU2, by = "BirdID")
View(ResidUJuncos2)

ggplot(data = ResidUJuncos2, aes(x = UrbanorNot, y = ResidUJuncos2$`glm4.LD4$residuals`,
                                colour = UrbanorNot)) + geom_point()

bartlett.test(ResidUJuncos2$`glm4.LD4$residuals`~ ResidUJuncos2$UrbanorNot)
#p-value = 0.6485

#CityorMountainLump
LDAJuncos3 = lda(noOldRep$CityorMountainLump ~ noOldRep$SongLength 
                 + noOldRep$TrillRate + noOldRep$minF
                 + noOldRep$maxF + noOldRep$bandwidthF)

View(LDAJuncos3)
LDAJuncos3

#Coefficients of linear discriminants:
#  LD1        LD2        LD3         LD4
#noOldRep$SongLength  2.4594311  0.5569359 -1.0777492  3.81570431
#noOldRep$TrillRate   0.1991399 -0.1904365  0.1106149 -0.09983883
#noOldRep$minF       -0.3966653  0.6305015  1.2027642  0.87757565
#noOldRep$maxF        0.6593384  1.1897366  0.8700570 -0.17414478
#noOldRep$bandwidthF  0.6635997  0.3330409 -0.2326505 -0.67146255

#Proportion of trace:
#  LD1    LD2    LD3    LD4 
#0.6660 0.1907 0.1018 0.0414 

LDAJuncos3.values = predict(LDAJuncos3, noOldRep)
LDIndi3 = as.data.frame(LDAJuncos3.values$x)
LDIndi3$Observation = 1:nrow(LDIndi3)

LDJuncos3 <- merge(noOldRep, LDIndi3, by = "Observation")
View(LDJuncos3)

ggplot(data = LDJuncos3, aes(x = LD1, y = LD2, colour = CityorMountainLump)) +
  stat_ellipse(aes(x=LD1, y=LD2,color=CityorMountainLump, group=CityorMountainLump),type = "norm") +
  theme() + geom_point() + theme_classic()

glm3.LD <- glm(LDJuncos3$LD1~LDJuncos3$CityorMountainLump + LDJuncos3$Date + LDJuncos3$Time +
                 LDJuncos3$DistRoad + LDJuncos3$Lat + LDJuncos3$Long)
summary(glm3.LD)
#AIC = 318.06

glm3.LD2 <- glm(LDJuncos3$LD1~LDJuncos3$CityorMountainLump + LDJuncos3$Date + LDJuncos3$Time +
                 LDJuncos3$DistRoad + LDJuncos3$Lat)
summary(glm3.LD2)
#AIC = 316.14

glm3.LD3 <- glm(LDJuncos3$LD1~LDJuncos3$CityorMountainLump + LDJuncos3$Date +
                  LDJuncos3$DistRoad + LDJuncos3$Lat)
summary(glm3.LD3)
#AIC = 314.31

glm3.LD4 <- glm(LDJuncos3$LD1~LDJuncos3$CityorMountainLump +
                  LDJuncos3$DistRoad + LDJuncos3$Lat)
summary(glm3.LD4)
#AIC = 313.46

glm3.LD5 <- glm(LDJuncos3$LD1~LDJuncos3$CityorMountainLump +
                   LDJuncos3$Lat)
summary(glm3.LD5)
#AIC = 312.17

glm3.LD6 <- glm(LDJuncos3$LD1~LDJuncos3$CityorMountainLump)
summary(glm3.LD6)
#AIC = 375.83

pairs(emmeans(glm3.LD5, "CityorMountainLump"))

ResidCITY = as.data.frame(glm3.LD5$residuals)
ResidCITY$BirdID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,24,25,26,27,28,29,30,31,
                     32,33,34,35,36,38,39,40,41,42,44,45,46,48,49,50,55,57,58,59,60,61,62,63,64,65,
                     66,68,69,70,71,72,73,74,77,78,79,80,81,82,83,85,86,87,88,89,90,91,92,93,94,95,
                     96,97,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,116,117,118,
                     119,120,131,132,133,153,154)
View(ResidCITY)

ResidCITYJuncos <- merge(noOldRep, ResidCITY, by = "BirdID")
View(ResidCITYJuncos)

ggplot(data = ResidCITYJuncos, aes(x = CityorMountainLump, y = ResidCITYJuncos$`glm3.LD5$residuals`,
                                   colour = CityorMountainLump)) + geom_point()

bartlett.test(ResidCITYJuncos$`glm3.LD5$residuals`~ResidCITYJuncos$CityorMountainLump)
#p-value = 0.7944

