#GLM SongLength by Location - all populations
allPops = noOldRep
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
allPops2 = noOldRep

TR.glm.Location = glm(allPops2$TrillRate~allPops2$Location+ allPops2$Date + 
                        allPops2$Time + allPops2$DistRoad + allPops2$Lat + allPops2$Long)

summary(TR.glm.Location)
#AIC = 598.51

TR.glm.Location2 = glm(allPops2$TrillRate~allPops2$Location+ allPops2$Date + 
                         allPops2$DistRoad + allPops2$Lat + allPops2$Long)

summary(TR.glm.Location2)
#AIC = 596.95

TR.glm.Location3 = glm(allPops2$TrillRate~allPops2$Location+ allPops2$Date + 
                         allPops2$DistRoad + allPops2$Long)

summary(TR.glm.Location3)
#AIC = 595.52

TR.glm.Location4 = glm(allPops2$TrillRate~allPops2$Location+ allPops2$Date + 
                         allPops2$DistRoad)

summary(TR.glm.Location4)
#AIC = 594.16

TR.glm.Location5 = glm(allPops2$TrillRate~allPops2$Location+ allPops2$Date)
summary(TR.glm.Location5)
#AIC = 696.24

TR.glm.Location6 = glm(allPops2$TrillRate~allPops2$Location)
summary(TR.glm.Location6)
#AIC = 697.43

pairs(emmeans(TR.glm.Location4, "Location"))

ResidTRLocation = as.data.frame(TR.glm.Location4$residuals)
View(ResidTRLocation)
ResidTRLocation$BirdID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,24,25,26,27,28,29,30,31,
                           32,33,34,35,36,38,39,40,41,42,44,45,46,48,49,50,55,57,58,59,60,61,62,63,64,65,
                           66,68,69,70,71,72,73,74,77,78,79,80,81,82,83,85,86,87,88,89,90,91,92,93,94,95,
                           96,97,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,116,117,118,
                           119,120,131,132,133,153,154)


ResidTRLocationJuncos <- merge(allPops2, ResidTRLocation, by = "BirdID")

ggplot(data = ResidTRLocationJuncos, aes(x = Location, y = ResidTRLocationJuncos$`TR.glm.Location4$residuals`,
                                         colour = Location)) + geom_point()

bartlett.test(ResidTRLocationJuncos$`TR.glm.Location4$residuals`~ResidTRLocationJuncos$Location)
#p-value =0.3545

#GLM minF by Location - all populations
allPops3 = noOldRep
minF.glm.Location = glm(allPops3$minF~allPops3$Location+ allPops3$Date + 
                          allPops3$Time + allPops3$DistRoad + allPops3$Lat + allPops3$Long)

summary(minF.glm.Location)
#AIC = 153.31

minF.glm.Location2 = glm(allPops3$minF~allPops3$Location+ allPops3$Date + 
                           allPops3$Time + allPops3$DistRoad + allPops3$Lat)

summary(minF.glm.Location2)
#AIC = 151.38

minF.glm.Location3 = glm(allPops3$minF~allPops3$Location+ allPops3$Date +
                           allPops3$DistRoad + allPops3$Lat)

summary(minF.glm.Location3)
#AIC = 150.02

minF.glm.Location4 = glm(allPops3$minF~allPops3$Location+ allPops3$Date +
                           allPops3$Lat)

summary(minF.glm.Location4)
#AIC = 148.86

minF.glm.Location5 = glm(allPops3$minF~allPops3$Location+ allPops3$Date)
summary(minF.glm.Location5)
#AIC = 179.35

minF.glm.Location6 = glm(allPops3$minF~allPops3$Location)
summary(minF.glm.Location6)
#AIC = 179.67

pairs(emmeans(minF.glm.Location4 , "Location"))

minF.ResidLocation = as.data.frame(minF.glm.Location4$residuals)
View(minF.ResidLocation)
minF.ResidLocation$BirdID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,24,25,26,27,28,29,30,31,
                              32,33,34,35,36,38,39,40,41,42,44,45,46,48,49,50,55,57,58,59,60,61,62,63,64,65,
                              66,68,69,70,71,72,73,74,77,78,79,80,81,82,83,85,86,87,88,89,90,91,92,93,94,95,
                              96,97,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,116,117,118,
                              119,120,131,132,133,153,154)


minF.ResidLocationJuncos <- merge(allPops3, minF.ResidLocation, by = "BirdID")

ggplot(data = minF.ResidLocationJuncos, aes(x = Location, y = minF.ResidLocationJuncos$`minF.glm.Location4$residuals`,
                                            colour = Location)) + geom_point()

bartlett.test(minF.ResidLocationJuncos$`minF.glm.Location4$residuals`~minF.ResidLocationJuncos$Location)
#p-value = 0.05393

#GLM maxF by Location - all populations
allPops4 = noOldRep
maxF.glm.Location = glm(allPops4$maxF~allPops4$Location+ allPops4$Date + 
                          allPops4$Time + allPops4$DistRoad + allPops4$Lat + allPops4$Long)

summary(maxF.glm.Location)
#AIC = 154.38

maxF.glm.Location2 = glm(allPops4$maxF~allPops4$Location+ allPops4$Date + 
                           allPops4$DistRoad + allPops4$Lat + allPops4$Long)

summary(maxF.glm.Location2)
#AIC = 152.38

maxF.glm.Location3 = glm(allPops4$maxF~allPops4$Location+ allPops4$Date+ allPops4$Lat + allPops4$Long)

summary(maxF.glm.Location3)
#AIC = 150.39

maxF.glm.Location4 = glm(allPops4$maxF~allPops4$Location + allPops4$Lat + allPops4$Long)
summary(maxF.glm.Location4)
#AIC = 149.46

maxF.glm.Location5 = glm(allPops4$maxF~allPops4$Location + allPops4$Long)
summary(maxF.glm.Location5)
#AIC = 149.58

maxF.glm.Location6 = glm(allPops4$maxF~allPops4$Location)
summary(maxF.glm.Location6)
#AIC = 171.05

pairs(emmeans(maxF.glm.Location4, "Location"))

maxF.ResidLocation = as.data.frame(maxF.glm.Location4$residuals)
View(maxF.ResidLocation)
maxF.ResidLocation$BirdID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,24,25,26,27,28,29,30,31,
                              32,33,34,35,36,38,39,40,41,42,44,45,46,48,49,50,55,57,58,59,60,61,62,63,64,65,
                              66,68,69,70,71,72,73,74,77,78,79,80,81,82,83,85,86,87,88,89,90,91,92,93,94,95,
                              96,97,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,116,117,118,
                              119,120,131,132,133,153,154)


maxF.ResidLocationJuncos <- merge(allPops4, maxF.ResidLocation, by = "BirdID")

ggplot(data = maxF.ResidLocationJuncos, aes(x = Location, y = maxF.ResidLocationJuncos$`maxF.glm.Location4$residuals`,
                                            colour = Location)) + geom_point()

bartlett.test(maxF.ResidLocationJuncos$`maxF.glm.Location4$residuals`~maxF.ResidLocationJuncos$Location)
#p-value = 0.1899

#GLM bandF by Location - all populations
allPops5 = noOldRep
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
#AIC = 203.37

bandF.glm.Location4 = glm(allPops5$bandwidthF~allPops5$Location+ allPops5$Date + 
                            allPops5$Long)

summary(bandF.glm.Location4)
#AIC = 201.8

bandF.glm.Location5 = glm(allPops5$bandwidthF~allPops5$Location+ allPops5$Long)
summary(bandF.glm.Location5)
#AIC = 200.42

bandF.glm.Location6 = glm(allPops5$bandwidthF~allPops5$Location)
summary(bandF.glm.Location6)
#AIC = 234.26

pairs(emmeans(bandF.glm.Location5, "Location"))

bandF.ResidLocation = as.data.frame(bandF.glm.Location5$residuals)
View(bandF.ResidLocation)
bandF.ResidLocation$BirdID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,24,25,26,27,28,29,30,31,
                               32,33,34,35,36,38,39,40,41,42,44,45,46,48,49,50,55,57,58,59,60,61,62,63,64,65,
                               66,68,69,70,71,72,73,74,77,78,79,80,81,82,83,85,86,87,88,89,90,91,92,93,94,95,
                               96,97,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,116,117,118,
                               119,120,131,132,133,153,154)


bandF.ResidLocationJuncos <- merge(allPops5, bandF.ResidLocation, by = "BirdID")

ggplot(data = bandF.ResidLocationJuncos, aes(x = Location, y = bandF.ResidLocationJuncos$`bandF.glm.Location5$residuals`,
                                             colour = Location)) + geom_point()

bartlett.test(bandF.ResidLocationJuncos$`bandF.glm.Location5$residuals`~bandF.ResidLocationJuncos$Location)
#p-value = 0.181

#GLM SongLength by UrbanorNot - all populations
all2Pops = noOldRep


SL.glm.UorN = glm(all2Pops$SongLength~all2Pops$UrbanorNot+ all2Pops$Date + 
                    all2Pops$Time + all2Pops$DistRoad + all2Pops$Lat + all2Pops$Long)

summary(SL.glm.UorN)
#AIC = -15.127

SL.glm.UorN2 = glm(all2Pops$SongLength~all2Pops$UrbanorNot+ all2Pops$Date + 
                     all2Pops$Time + all2Pops$DistRoad + all2Pops$Long)

summary(SL.glm.UorN2)
#AIC = -17.126

SL.glm.UorN3 = glm(all2Pops$SongLength~all2Pops$UrbanorNot+ all2Pops$Date + 
                     all2Pops$DistRoad + all2Pops$Long)

summary(SL.glm.UorN3)
#AIC = -19.122

SL.glm.UorN4 = glm(all2Pops$SongLength~all2Pops$UrbanorNot+ all2Pops$Date + 
                     all2Pops$DistRoad)

summary(SL.glm.UorN4)
#AIC = -20.96

SL.glm.UorN5 = glm(all2Pops$SongLength~all2Pops$UrbanorNot+ 
                     all2Pops$Date)

summary(SL.glm.UorN5)
#AIC = -21.514

SL.glm.UorN6 = glm(all2Pops$SongLength~all2Pops$UrbanorNot)
summary(SL.glm.UorN6)
#AIC = -22.523

pairs(emmeans(SL.glm.UorN6, "UrbanorNot"))


SL.resid.UorN = as.data.frame(SL.glm.UorN6$residuals)
View(SL.resid.UorN)
SL.resid.UorN$Observation = 1:nrow(noOldRep)
all2Pops$Observation = 1:nrow(noOldRep)
View(all2Pops)
SL.resid.UorNJuncos <- merge(all2Pops, SL.resid.UorN, by = "Observation")
View(SL.resid.UorNJuncos)

ggplot(data = SL.resid.UorNJuncos, aes(x = UrbanorNot, y = SL.resid.UorNJuncos$`SL.glm.UorN6$residuals`,
                                       colour = UrbanorNot)) + geom_point()

bartlett.test(SL.resid.UorNJuncos$`SL.glm.UorN6$residuals`~SL.resid.UorNJuncos$UrbanorNot)
#p-value = 0.1655

#w/ repository

SL.glm.UorN7 = rlm(noOld$SongLength~noOld$UrbanorNot)
summary(SL.glm.UorN7)
pairs(emmeans(SL.glm.UorN7, "UrbanorNot"))

SL.resid.UorN2 = as.data.frame(SL.glm.UorN7$residuals)
View(SL.resid.UorN2)
SL.resid.UorN2$Observation = 1:nrow(noOld)
noOld$Observation = 1:nrow(noOld)
SL.resid.UorNJuncos2 <- merge(noOld, SL.resid.UorN2, by = "Observation")
View(SL.resid.UorNJuncos2)

ggplot(data = SL.resid.UorNJuncos2, aes(x = UrbanorNot, y = SL.resid.UorNJuncos2$`SL.glm.UorN7$residuals`,
                                       colour = UrbanorNot)) + geom_point()

bartlett.test(SL.resid.UorNJuncos2$`SL.glm.UorN7$residuals`~SL.resid.UorNJuncos2$UrbanorNot)
#p-value = 0.001066

#GLM TrillRate by UrbanorNot - all populations
all2Pops2 = noOldRep

TR.glm.UorN = glm(all2Pops2$TrillRate~all2Pops2$UrbanorNot+ all2Pops2$Date + 
                    all2Pops2$Time + all2Pops2$DistRoad + all2Pops2$Lat + all2Pops2$Long)

summary(TR.glm.UorN)
#AIC = 591.06

TR.glm.UorN2 = glm(all2Pops2$TrillRate~all2Pops2$UrbanorNot+ all2Pops2$Date + 
                     all2Pops2$Time + all2Pops2$DistRoad + all2Pops2$Lat)

summary(TR.glm.UorN2)
#AIC = 589.13

TR.glm.UorN3 = glm(all2Pops2$TrillRate~all2Pops2$UrbanorNot+ all2Pops2$Date + 
                     all2Pops2$DistRoad + all2Pops2$Lat)

summary(TR.glm.UorN3)
#AIC = 587.39

TR.glm.UorN4 = glm(all2Pops2$TrillRate~all2Pops2$UrbanorNot+ all2Pops2$Date + 
                     + all2Pops2$Lat)

summary(TR.glm.UorN4)
#AIC = 589.9

TR.glm.UorN5 = glm(all2Pops2$TrillRate~all2Pops2$UrbanorNot+ all2Pops2$Lat)
summary(TR.glm.UorN5)
#AIC = 590.37

TR.glm.UorN6 = glm(all2Pops2$TrillRate~all2Pops2$UrbanorNot)
summary(TR.glm.UorN6)
#AIC = 695.78

pairs(emmeans(TR.glm.UorN3, "UrbanorNot"))

TR.resid.UorN = as.data.frame(TR.glm.UorN3$residuals)
View(TR.resid.UorN)
TR.resid.UorN$BirdID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,24,25,26,27,28,29,30,31,
                         32,33,34,35,36,38,39,40,41,42,44,45,46,48,49,50,55,57,58,59,60,61,62,63,64,65,
                         66,68,69,70,71,72,73,74,77,78,79,80,81,82,83,85,86,87,88,89,90,91,92,93,94,95,
                         96,97,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,116,117,118,
                         119,120,131,132,133,153,154)


TR.resid.UorNJuncos <- merge(all2Pops2, TR.resid.UorN, by = "BirdID")

ggplot(data = TR.resid.UorNJuncos, aes(x = UrbanorNot, y = TR.resid.UorNJuncos$`TR.glm.UorN3$residuals`,
                                       colour = UrbanorNot)) + geom_point()

bartlett.test(TR.resid.UorNJuncos$`TR.glm.UorN3$residuals`~TR.resid.UorNJuncos$UrbanorNot)
#p-value = 0.7072

#w/ repository

TR.glm.UorN7 = glm(noOld$TrillRate~noOld$UrbanorNot)
summary(TR.glm.UorN7)
pairs(emmeans(TR.glm.UorN7, "UrbanorNot"))

TR.resid.UorN2 = as.data.frame(TR.glm.UorN7$residuals)
View(TR.resid.UorN2)
TR.resid.UorN2$Observation = 1:nrow(noOld)
noOld$Observation = 1:nrow(noOld)
TR.resid.UorNJuncos2 <- merge(noOld, TR.resid.UorN2, by = "Observation")
View(TR.resid.UorNJuncos2)

ggplot(data = TR.resid.UorNJuncos2, aes(x = UrbanorNot, y = TR.resid.UorNJuncos2$`TR.glm.UorN7$residuals`,
                                        colour = UrbanorNot)) + geom_point()

bartlett.test(TR.resid.UorNJuncos2$`TR.glm.UorN7$residuals`~TR.resid.UorNJuncos2$UrbanorNot)
#p-value = 0.1986

#GLM minF by UrbanorNot - all populations
all2Pops3 = noOldRep
minF.glm.UorN = glm(all2Pops3$minF~all2Pops3$UrbanorNot+ all2Pops3$Date + 
                      all2Pops3$Time + all2Pops3$DistRoad + all2Pops3$Lat + all2Pops3$Long)

summary(minF.glm.UorN)
#AIC = 146.07

minF.glm.UorN2 = glm(all2Pops3$minF~all2Pops3$UrbanorNot+ all2Pops3$Date + 
                       all2Pops3$Time + all2Pops3$Lat + all2Pops3$Long)

summary(minF.glm.UorN2)
#AIC = 144.56

minF.glm.UorN3 = glm(all2Pops3$minF~all2Pops3$UrbanorNot+ all2Pops3$Date + 
                       all2Pops3$Time + all2Pops3$Long)

summary(minF.glm.UorN3)
#AIC = 143.06

minF.glm.UorN4 = glm(all2Pops3$minF~all2Pops3$UrbanorNot+ all2Pops3$Date + 
                       all2Pops3$Long)

summary(minF.glm.UorN4)
#AIC = 141.44

minF.glm.UorN5 = glm(all2Pops3$minF~all2Pops3$UrbanorNot+ all2Pops3$Date)
summary(minF.glm.UorN5)
#AIC = 172.72

minF.glm.UorN6 = glm(all2Pops3$minF~all2Pops3$UrbanorNot)
summary(minF.glm.UorN6)
#AIC = 172.86

pairs(emmeans(minF.glm.UorN4, "UrbanorNot"))


minF.resid.UorN = as.data.frame(minF.glm.UorN4$residuals)
View(minF.resid.UorN)
minF.resid.UorN$BirdID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,24,25,26,27,28,29,30,31,
                           32,33,34,35,36,38,39,40,41,42,44,45,46,48,49,50,55,57,58,59,60,61,62,63,64,65,
                           66,68,69,70,71,72,73,74,77,78,79,80,81,82,83,85,86,87,88,89,90,91,92,93,94,95,
                           96,97,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,116,117,118,
                           119,120,131,132,133,153,154)


minF.resid.UorNJuncos <- merge(all2Pops3, minF.resid.UorN, by = "BirdID")

ggplot(data = minF.resid.UorNJuncos, aes(x = UrbanorNot, y = minF.resid.UorNJuncos$`minF.glm.UorN4$residuals`,
                                         colour = UrbanorNot)) + geom_point()

bartlett.test(minF.resid.UorNJuncos$`minF.glm.UorN4$residuals`~minF.resid.UorNJuncos$UrbanorNot)
#p-value = 0.1951

#w/ repository

minF.glm.UorN7 = glm(noOld$minF~noOld$UrbanorNot)
summary(minF.glm.UorN7)
pairs(emmeans(minF.glm.UorN7, "UrbanorNot"))

minF.resid.UorN2 = as.data.frame(minF.glm.UorN7$residuals)
View(minF.resid.UorN2)
minF.resid.UorN2$Observation = 1:nrow(noOld)
noOld$Observation = 1:nrow(noOld)
minF.resid.UorNJuncos2 <- merge(noOld, minF.resid.UorN2, by = "Observation")
View(minF.resid.UorNJuncos2)

ggplot(data = minF.resid.UorNJuncos2, aes(x = UrbanorNot, y = minF.resid.UorNJuncos2$`minF.glm.UorN7$residuals`,
                                        colour = UrbanorNot)) + geom_point()

bartlett.test(minF.resid.UorNJuncos2$`minF.glm.UorN7$residuals`~minF.resid.UorNJuncos2$UrbanorNot)
#p-value = 0.5313

#GLM maxF by Location - all populations
all2Pops4 = noOldRep
maxF.glm.UorN = glm(all2Pops4$maxF~all2Pops4$UrbanorNot+ all2Pops4$Date + 
                      all2Pops4$Time + all2Pops4$DistRoad + all2Pops4$Lat + all2Pops4$Long)

summary(maxF.glm.UorN)
#AIC = 153.41

maxF.glm.UorN2 = glm(all2Pops4$maxF~all2Pops4$UrbanorNot+ all2Pops4$Date + 
                       all2Pops4$DistRoad + all2Pops4$Lat + all2Pops4$Long)

summary(maxF.glm.UorN2)
#AIC = 151.5

maxF.glm.UorN3 = glm(all2Pops4$maxF~all2Pops4$UrbanorNot + 
                       all2Pops4$DistRoad + all2Pops4$Lat + all2Pops4$Long)

summary(maxF.glm.UorN3)
#AIC = 150.26

maxF.glm.UorN4 = glm(all2Pops4$maxF~all2Pops4$UrbanorNot+ all2Pops4$Date + 
                       all2Pops4$DistRoad)

summary(maxF.glm.UorN4)
#AIC = 149.66

maxF.glm.UorN5 = glm(all2Pops4$maxF~all2Pops4$UrbanorNot+
                       all2Pops4$DistRoad)

summary(maxF.glm.UorN5)
#AIC = 147.97

maxF.glm.UorN6 = glm(all2Pops4$maxF~all2Pops4$UrbanorNot)
summary(maxF.glm.UorN6)
#AIC = 167.46

pairs(emmeans(maxF.glm.UorN5, "UrbanorNot"))

maxF.resid.UorN = as.data.frame(maxF.glm.UorN5$residuals)
View(maxF.resid.UorN)
maxF.resid.UorN$BirdID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,24,25,26,27,28,29,30,31,
                           32,33,34,35,36,38,39,40,41,42,44,45,46,48,49,50,55,57,58,59,60,61,62,63,64,65,
                           66,68,69,70,71,72,73,74,77,78,79,80,81,82,83,85,86,87,88,89,90,91,92,93,94,95,
                           96,97,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,116,117,118,
                           119,120,131,132,133,153,154)


maxF.resid.UorNJuncos <- merge(all2Pops4, maxF.resid.UorN, by = "BirdID")

ggplot(data = maxF.resid.UorNJuncos, aes(x = UrbanorNot, y = maxF.resid.UorNJuncos$`maxF.glm.UorN5$residuals`,
                                         colour = UrbanorNot)) + geom_point()

bartlett.test(maxF.resid.UorNJuncos$`maxF.glm.UorN5$residuals`~maxF.resid.UorNJuncos$UrbanorNot)
#p-value = 0.4102

#w/ repository

maxF.glm.UorN7 = glm(noOld$maxF~noOld$UrbanorNot)
summary(maxF.glm.UorN7)
pairs(emmeans(maxF.glm.UorN7, "UrbanorNot"))

maxF.resid.UorN2 = as.data.frame(maxF.glm.UorN7$residuals)
View(maxF.resid.UorN2)
maxF.resid.UorN2$Observation = 1:nrow(noOld)
noOld$Observation = 1:nrow(noOld)
maxF.resid.UorNJuncos2 <- merge(noOld, maxF.resid.UorN2, by = "Observation")
View(maxF.resid.UorNJuncos2)

ggplot(data = maxF.resid.UorNJuncos2, aes(x = UrbanorNot, y = maxF.resid.UorNJuncos2$`maxF.glm.UorN7$residuals`,
                                          colour = UrbanorNot)) + geom_point()

bartlett.test(maxF.resid.UorNJuncos2$`maxF.glm.UorN7$residuals`~maxF.resid.UorNJuncos2$UrbanorNot)
#p-value = 0.235

#GLM bandF by Location - all populations
all2Pops5 = noOldRep
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
#AIC = 202.16

bandF.glm.UorN4 = glm(all2Pops5$bandwidthF~all2Pops5$UrbanorNot+ all2Pops5$Date
                      + all2Pops5$Lat)

summary(bandF.glm.UorN4)
#AIC = 200.74

bandF.glm.UorN5 = glm(all2Pops5$bandwidthF~all2Pops5$UrbanorNot+ all2Pops5$Date)
summary(bandF.glm.UorN5)
#AIC = 232.5

bandF.glm.UorN6 = glm(all2Pops5$bandwidthF~all2Pops5$UrbanorNot)
summary(bandF.glm.UorN6)
#AIC = 231.9

pairs(emmeans(bandF.glm.UorN4, "UrbanorNot"))


bandF.resid.UorN = as.data.frame(bandF.glm.UorN4$residuals)
View(bandF.resid.UorN)
bandF.resid.UorN$BirdID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,24,25,26,27,28,29,30,31,
                            32,33,34,35,36,38,39,40,41,42,44,45,46,48,49,50,55,57,58,59,60,61,62,63,64,65,
                            66,68,69,70,71,72,73,74,77,78,79,80,81,82,83,85,86,87,88,89,90,91,92,93,94,95,
                            96,97,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,116,117,118,
                            119,120,131,132,133,153,154)


bandF.resid.UorNJuncos <- merge(all2Pops5, bandF.resid.UorN, by = "BirdID")

ggplot(data = bandF.resid.UorNJuncos, aes(x = UrbanorNot, y = bandF.resid.UorNJuncos$`bandF.glm.UorN4$residuals`,
                                          colour = UrbanorNot)) + geom_point()

bartlett.test(bandF.resid.UorNJuncos$`bandF.glm.UorN4$residuals`~bandF.resid.UorNJuncos$UrbanorNot)
#p-value = 0.4176

#w/ repository

bandF.glm.UorN7 = glm(noOld$bandwidthF~noOld$UrbanorNot)
summary(bandF.glm.UorN7)
pairs(emmeans(bandF.glm.UorN7, "UrbanorNot"))

bandF.resid.UorN2 = as.data.frame(bandF.glm.UorN7$residuals)
View(bandF.resid.UorN2)
bandF.resid.UorN2$Observation = 1:nrow(noOld)
noOld$Observation = 1:nrow(noOld)
bandF.resid.UorNJuncos2 <- merge(noOld, bandF.resid.UorN2, by = "Observation")
View(bandF.resid.UorNJuncos2)

ggplot(data = bandF.resid.UorNJuncos2, aes(x = UrbanorNot, y = bandF.resid.UorNJuncos2$`bandF.glm.UorN7$residuals`,
                                          colour = UrbanorNot)) + geom_point()

bartlett.test(bandF.resid.UorNJuncos2$`bandF.glm.UorN7$residuals`~bandF.resid.UorNJuncos2$UrbanorNot)
#p-value = 0.7811


#GLM SongLength by CityorMountainLump - all populations
all3Pops = noOldRep
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
#AIC = -20.59

SL.glm.CITY5 = glm(all3Pops$SongLength~all3Pops$CityorMountainLump + 
                     all3Pops$Lat)

summary(SL.glm.CITY5)
#AIC = -22.266

SL.glm.CITY6 = glm(all3Pops$SongLength~all3Pops$CityorMountainLump)
summary(SL.glm.CITY6)
#AIC = -21.983

pairs(emmeans(SL.glm.CITY5, "CityorMountainLump"))

SL.resid.CITY = as.data.frame(SL.glm.CITY5$residuals)
View(SL.resid.CITY)
SL.resid.CITY$BirdID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,24,25,26,27,28,29,30,31,
                         32,33,34,35,36,38,39,40,41,42,44,45,46,48,49,50,55,57,58,59,60,61,62,63,64,65,
                         66,68,69,70,71,72,73,74,77,78,79,80,81,82,83,85,86,87,88,89,90,91,92,93,94,95,
                         96,97,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,116,117,118,
                         119,120,131,132,133,153,154)


SL.resid.CITYJuncos <- merge(all3Pops, SL.resid.CITY, by = "BirdID")
View(SL.resid.CITYJuncos)

ggplot(data = SL.resid.CITYJuncos, aes(x = CityorMountainLump, y = SL.resid.CITYJuncos$`SL.glm.CITY5$residuals`,
                                       colour = CityorMountainLump)) + geom_point()

bartlett.test(SL.resid.CITYJuncos$`SL.glm.CITY5$residuals`~SL.resid.CITYJuncos$CityorMountainLump)
#p-value = 0.03255

#GLM TrillRate by CityorMountainLump - all populations
all3Pops2 = noOldRep
TR.glm.CITY = glm(all3Pops2$TrillRate~all3Pops2$CityorMountainLump+ all3Pops2$Date + 
                    all3Pops2$Time + all3Pops2$DistRoad + all3Pops2$Lat + all3Pops2$Long)

summary(TR.glm.CITY)
#AIC = 595.46

TR.glm.CITY2 = glm(all3Pops2$TrillRate~all3Pops2$CityorMountainLump+ all3Pops2$Date + 
                     all3Pops2$DistRoad + all3Pops2$Lat + all3Pops2$Long)

summary(TR.glm.CITY2)
#AIC = 593.89

TR.glm.CITY3 = glm(all3Pops2$TrillRate~all3Pops2$CityorMountainLump+ all3Pops2$Date + 
                     all3Pops2$DistRoad + all3Pops2$Long)

summary(TR.glm.CITY3)
#AIC = 592.68

TR.glm.CITY4 = glm(all3Pops2$TrillRate~all3Pops2$CityorMountainLump+ all3Pops2$Date + 
                     all3Pops2$DistRoad)

summary(TR.glm.CITY4)
#AIC = 591.1

TR.glm.CITY5 = glm(all3Pops2$TrillRate~all3Pops2$CityorMountainLump+ all3Pops2$Date)
summary(TR.glm.CITY5)
#AIC = 694.25

TR.glm.CITY6 = glm(all3Pops2$TrillRate~all3Pops2$CityorMountainLump)
summary(TR.glm.CITY6)
#AIC = 696.41

pairs(emmeans(TR.glm.CITY4, "CityorMountainLump"))

TR.resid.CITY = as.data.frame(TR.glm.CITY4$residuals)
TR.resid.CITY$BirdID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,24,25,26,27,28,29,30,31,
                         32,33,34,35,36,38,39,40,41,42,44,45,46,48,49,50,55,57,58,59,60,61,62,63,64,65,
                         66,68,69,70,71,72,73,74,77,78,79,80,81,82,83,85,86,87,88,89,90,91,92,93,94,95,
                         96,97,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,116,117,118,
                         119,120,131,132,133,153,154)


TR.resid.CITYJuncos <- merge(all3Pops2, TR.resid.CITY, by = "BirdID")

ggplot(data = TR.resid.CITYJuncos, aes(x = CityorMountainLump, y = TR.resid.CITYJuncos$`TR.glm.CITY4$residuals`,
                                       colour = CityorMountainLump)) + geom_point()

bartlett.test(TR.resid.CITYJuncos$`TR.glm.CITY4$residuals`~TR.resid.CITYJuncos$CityorMountainLump)
#p-value = 0.1504

#GLM minF by CityorMountainLump - all populations
all3Pops3 = noOldRep
minF.glm.CITY = glm(all3Pops3$minF~all3Pops3$CityorMountainLump+ all3Pops3$Date + 
                      all3Pops3$Time + all3Pops3$DistRoad + all3Pops3$Lat + all3Pops3$Long)

summary(minF.glm.CITY)
#AIC = 150.37

minF.glm.CITY2 = glm(all3Pops3$minF~all3Pops3$CityorMountainLump+ all3Pops3$Date + 
                       all3Pops3$DistRoad + all3Pops3$Lat + all3Pops3$Long)

summary(minF.glm.CITY2)
#AIC = 148.94

minF.glm.CITY3 = glm(all3Pops3$minF~all3Pops3$CityorMountainLump+ all3Pops3$Date + 
                       all3Pops3$DistRoad + all3Pops3$Lat)

summary(minF.glm.CITY3)
#AIC = 147.66

minF.glm.CITY4 = glm(all3Pops3$minF~all3Pops3$CityorMountainLump+ all3Pops3$Date + 
                       all3Pops3$DistRoad)

summary(minF.glm.CITY4)
#AIC = 146.09

minF.glm.CITY5 = glm(all3Pops3$minF~all3Pops3$CityorMountainLump+ all3Pops3$Date)
summary(minF.glm.CITY5)
#AIC = 175.89

minF.glm.CITY6= glm(all3Pops3$minF~all3Pops3$CityorMountainLump)
summary(minF.glm.CITY6)
#AIC = 176.4


pairs(emmeans(minF.glm.CITY4, "CityorMountainLump"))

minF.resid.CITY = as.data.frame(minF.glm.CITY4$residuals)
View(minF.resid.CITY)
minF.resid.CITY$BirdID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,24,25,26,27,28,29,30,31,
                           32,33,34,35,36,38,39,40,41,42,44,45,46,48,49,50,55,57,58,59,60,61,62,63,64,65,
                           66,68,69,70,71,72,73,74,77,78,79,80,81,82,83,85,86,87,88,89,90,91,92,93,94,95,
                           96,97,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,116,117,118,
                           119,120,131,132,133,153,154)


minF.resid.CITYJuncos <- merge(all3Pops3, minF.resid.CITY, by = "BirdID")

ggplot(data = minF.resid.CITYJuncos, aes(x = CityorMountainLump, y = minF.resid.CITYJuncos$`minF.glm.CITY4$residuals`,
                                         colour = CityorMountainLump)) + geom_point()

bartlett.test(minF.resid.CITYJuncos$`minF.glm.CITY4$residuals`~minF.resid.CITYJuncos$CityorMountainLump)
#p-value = 0.1796

#GLM maxF by CityorMountainLump - all populations
all3Pops4 = noOldRep
maxF.glm.CITY = glm(all3Pops4$maxF~all3Pops4$CityorMountainLump+ all3Pops4$Date + 
                      all3Pops4$Time + all3Pops4$DistRoad + all3Pops4$Lat + all3Pops4$Long)

summary(maxF.glm.CITY)
#AIC = 152.66

maxF.glm.CITY2 = glm(all3Pops4$maxF~all3Pops4$CityorMountainLump+ all3Pops4$Date + 
                       all3Pops4$DistRoad + all3Pops4$Lat + all3Pops4$Long)

summary(maxF.glm.CITY2)
#AIC = 150.66

maxF.glm.CITY3 = glm(all3Pops4$maxF~all3Pops4$CityorMountainLump+ all3Pops4$Date + 
                       all3Pops4$Lat + all3Pops4$Long)

summary(maxF.glm.CITY3)
#AIC = 148.68

maxF.glm.CITY4 = glm(all3Pops4$maxF~all3Pops4$CityorMountainLump+ all3Pops4$Date + 
                       all3Pops4$Long)

summary(maxF.glm.CITY4)
#AIC = 146.77

maxF.glm.CITY5 = glm(all3Pops4$maxF~all3Pops4$CityorMountainLump+
                       all3Pops4$Long)

summary(maxF.glm.CITY5)
#AIC = 146.17

maxF.glm.CITY6 = glm(all3Pops4$maxF~all3Pops4$CityorMountainLump)
summary(maxF.glm.CITY6)
#AIC = 171.89

pairs(emmeans(maxF.glm.CITY5, "CityorMountainLump"))

maxF.resid.CITY = as.data.frame(maxF.glm.CITY5$residuals)
View(maxF.resid.CITY)
maxF.resid.CITY$BirdID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,24,25,26,27,28,29,30,31,
                           32,33,34,35,36,38,39,40,41,42,44,45,46,48,49,50,55,57,58,59,60,61,62,63,64,65,
                           66,68,69,70,71,72,73,74,77,78,79,80,81,82,83,85,86,87,88,89,90,91,92,93,94,95,
                           96,97,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,116,117,118,
                           119,120,131,132,133,153,154)


maxF.resid.CITYJuncos <- merge(all3Pops4, maxF.resid.CITY, by = "BirdID")

ggplot(data = maxF.resid.CITYJuncos, aes(x = CityorMountainLump, y = maxF.resid.CITYJuncos$`maxF.glm.CITY5$residuals`,
                                         colour = CityorMountainLump)) + geom_point()

bartlett.test(maxF.resid.CITYJuncos$`maxF.glm.CITY5$residuals`~maxF.resid.CITYJuncos$CityorMountainLump)
#p-value = 0.5321

#GLM bandF by CityorMountainLump - all populations
all3Pops5 = noOldRep
bandF.glm.CITY = glm(all3Pops5$bandwidthF~all3Pops5$CityorMountainLump+ all3Pops5$Date + 
                       all3Pops5$Time + all3Pops5$DistRoad + all3Pops5$Lat + all3Pops5$Long)

summary(bandF.glm.CITY)
#AIC = 204.69

bandF.glm.CITY2 = glm(all3Pops5$bandwidthF~all3Pops5$CityorMountainLump+ all3Pops5$Date + 
                        all3Pops5$Time + all3Pops5$DistRoad + all3Pops5$Lat)

summary(bandF.glm.CITY2)
#AIC = 202.98

bandF.glm.CITY3 = glm(all3Pops5$bandwidthF~all3Pops5$CityorMountainLump+ all3Pops5$Date + 
                        all3Pops5$Time + all3Pops5$Lat)

summary(bandF.glm.CITY3)
#AIC = 201.25

bandF.glm.CITY4 = glm(all3Pops5$bandwidthF~all3Pops5$CityorMountainLump+ all3Pops5$Date + 
                        all3Pops5$Lat)

summary(bandF.glm.CITY4)
#AIC = 199.65

bandF.glm.CITY5 = glm(all3Pops5$bandwidthF~all3Pops5$CityorMountainLump+ all3Pops5$Lat)
summary(bandF.glm.CITY5)
#AIC = 198.06

bandF.glm.CITY6 = glm(all3Pops5$bandwidthF~all3Pops5$CityorMountainLump)
summary(bandF.glm.CITY6)
#AIC = 234

pairs(emmeans(bandF.glm.CITY5, "CityorMountainLump"))

bandF.resid.CITY= as.data.frame(bandF.glm.CITY5$residuals)
View(bandF.resid.CITY)
bandF.resid.CITY$BirdID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,24,25,26,27,28,29,30,31,
                            32,33,34,35,36,38,39,40,41,42,44,45,46,48,49,50,55,57,58,59,60,61,62,63,64,65,
                            66,68,69,70,71,72,73,74,77,78,79,80,81,82,83,85,86,87,88,89,90,91,92,93,94,95,
                            96,97,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,116,117,118,
                            119,120,131,132,133,153,154)


bandF.resid.CITYJuncos <- merge(all3Pops5, bandF.resid.CITY, by = "BirdID")

ggplot(data = bandF.resid.CITYJuncos, aes(x = CityorMountainLump, y = bandF.resid.CITYJuncos$`bandF.glm.CITY5$residuals`,
                                          colour = CityorMountainLump)) + geom_point()

bartlett.test(bandF.resid.CITYJuncos$`bandF.glm.CITY5$residuals`~bandF.resid.CITYJuncos$CityorMountainLump)
#p-value = 0.3475