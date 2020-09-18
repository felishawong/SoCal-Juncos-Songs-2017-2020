#GLM SongLength by Location - no Small Populations
noSmallPops = noSmallRep
View(noSmallRep)
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
#AIC = 525.54

TR2.glm.Location2 = glm(noSmallPops2$TrillRate~noSmallPops2$Location+ noSmallPops2$Date + 
                          noSmallPops2$DistRoad + noSmallPops2$Lat + noSmallPops2$Long)

summary(TR2.glm.Location2)
#AIC = 524.19

TR2.glm.Location3 = glm(noSmallPops2$TrillRate~noSmallPops2$Location+ noSmallPops2$Date + 
                          noSmallPops2$DistRoad + noSmallPops2$Long)

summary(TR2.glm.Location3)
#AIC = 523.02

TR2.glm.Location4 = glm(noSmallPops2$TrillRate~noSmallPops2$Location+ noSmallPops2$Date + 
                          noSmallPops2$DistRoad)

summary(TR2.glm.Location4)
#AIC = 521.69

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

install.packages("sfsmisc")
install.packages("clickR")
library(sfsmisc)
library(clickR)

#minF noSmallPopulations
noSmallPops3=noSmallRep
noSmallPops3 = noSmallPops3[complete.cases(noSmallPops3),]
minF2.glm.Location = rlm(noSmallPops3$minF~noSmallPops3$Location+ noSmallPops3$Date + 
                           noSmallPops3$Time + noSmallPops3$DistRoad + noSmallPops3$Lat + noSmallPops3$Long)

summary(minF2.glm.Location)
rob.pvals(minF2.glm.Location)
minF2.glm.Location
AIC(minF2.glm.Location)
#AIC = 130.5692
#[Intercept] 0.61966940 
#OXY-Angeles 0.51700810 
#UCLA-Angeles 0.50549666
#UCSD-Angeles 0.23313851 
#Date 0.04524599 
#Time 0.50171432 
#DistRoad 0.56820625 
#Lat 0.36159535
#Long 0.52772499

minF2.glm.Location2 = rlm(noSmallPops3$minF~noSmallPops3$Location+ noSmallPops3$Date + 
                           noSmallPops3$Time + noSmallPops3$Lat + noSmallPops3$Long)

summary(minF2.glm.Location2)
rob.pvals(minF2.glm.Location2)
minF2.glm.Location2
AIC(minF2.glm.Location2)
#AIC = 129.2449
#[Intercept] 0.59591205 
#OXY-Angeles 0.49608139 
#UCLA-Angeles 0.48188619
#UCSD-Angeles 0.22021420 
#Date 0.04650729 
#Time 0.49850218 
#Lat 0.34549762
#Long 0.50602652

minF2.glm.Location3 = rlm(noSmallPops3$minF~noSmallPops3$Location+ noSmallPops3$Date + 
                            noSmallPops3$Time + noSmallPops3$Lat)

summary(minF2.glm.Location3)
rob.pvals(minF2.glm.Location3)
minF2.glm.Location3
AIC(minF2.glm.Location3)
#AIC = 127.6394
#[Intercept] 0.1675105 
#OXY-Angeles 0.8734548 
#UCLA-Angeles 0.5471203
#UCSD-Angeles 0.1796645 
#Date 0.0396957 
#Time 0.4961022 
#Lat 0.1823961

minF2.glm.Location4 = rlm(noSmallPops3$minF~noSmallPops3$Location+ noSmallPops3$Date + 
                            noSmallPops3$Lat)

summary(minF2.glm.Location4)
rob.pvals(minF2.glm.Location4)
minF2.glm.Location4
AIC(minF2.glm.Location4)
#AIC = 126.2938
#[Intercept]  0.15838140
#OXY-Angeles 0.89182216 
#UCLA-Angeles 0.58471937
#UCSD-Angeles 0.16971233 
#Date 0.04055242 
#Lat 0.17239750

minF2.glm.Location5 = rlm(noSmallPops3$minF~noSmallPops3$Location+ noSmallPops3$Date)

summary(minF2.glm.Location5)
rob.pvals(minF2.glm.Location5)
minF2.glm.Location5
AIC(minF2.glm.Location5)
#AIC = 126.3095
#[Intercept]  7.173754e-21
#OXY-Angeles 9.242512e-01 
#UCLA-Angeles 8.341790e-01
#UCSD-Angeles 8.771099e-01 
#Date 3.873624e-02 

minF2.glm.Location6 = rlm(noSmallPops3$minF~noSmallPops3$Location)

summary(minF2.glm.Location6)
rob.pvals(minF2.glm.Location6)
minF2.glm.Location6
AIC(minF2.glm.Location6)
#AIC = 127.8216

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

SL2.resid.UorN = as.data.frame(SL2.glm.UorN6$residuals)
View(SL2.resid.UorN)
SL2.resid.UorN$Observation = 1:nrow(SL2.resid.UorN)
noSmall$Observation = 1:nrow(noSmall)

View(noSmall2Pops)
SL2.resid.UorNJuncos <- merge(noSmall, SL2.resid.UorN, by = "Observation")

View(SL2.resid.UorNJuncos)

ggplot(data = SL2.resid.UorNJuncos, aes(x = UrbanorNot, y = SL2.resid.UorNJuncos$`SL2.glm.UorN6$residuals`,
                                        colour = UrbanorNot)) + geom_point()

bartlett.test(SL2.resid.UorNJuncos$`SL2.glm.UorN6$residuals`~SL2.resid.UorNJuncos$UrbanorNot)
#p-value = 0.4289

#w repository
SL2.glm.UorN7 = rlm(noSmall$SongLength~noSmall$UrbanorNot)
summary(SL2.glm.UorN7)
pairs(emmeans(SL2.glm.UorN7, "UrbanorNot"))

SL2.resid.UorN = as.data.frame(SL2.glm.UorN7$residuals)
View(SL2.resid.UorN)
SL2.resid.UorN$Observation = 1:nrow(SL2.resid.UorN)
noSmall$Observation = 1:nrow(noSmall)

View(noSmall2Pops)
SL2.resid.UorNJuncos <- merge(noSmall, SL2.resid.UorN, by = "Observation")

View(SL2.resid.UorNJuncos)

ggplot(data = SL2.resid.UorNJuncos, aes(x = UrbanorNot, y = SL2.resid.UorNJuncos$`SL2.glm.UorN7$residuals`,
                                        colour = UrbanorNot)) + geom_point()

bartlett.test(SL2.resid.UorNJuncos$`SL2.glm.UorN7$residuals`~SL2.resid.UorNJuncos$UrbanorNot)
#p-value = 0.0004075

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

TR2.resid.UorN = as.data.frame(TR2.glm.UorN3$residuals)
View(TR2.resid.UorN)
TR2.resid.UorN$BirdID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
                          21,22,24,25,26,27,28,29,30,31,32,33,34,35,36,38,39,40,41,42,
                          44,45,46,48,49,50,55,57,58,59,60,61,68,69,70,71,
                          72,73,74,77,80,81,82,83,84,85,86,87,88,89,90,91,
                          92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,
                          110,111,112,131,132,133,154)

TR2.resid.UorNJuncos <- merge(noSmall, TR2.resid.UorN, by = "BirdID")
View(TR2.resid.UorNJuncos)
ggplot(data = TR2.resid.UorNJuncos, aes(x = UrbanorNot, y = TR2.resid.UorNJuncos$`TR2.glm.UorN3$residuals`,
                                        colour = UrbanorNot)) + geom_point()

bartlett.test(TR2.resid.UorNJuncos$`TR2.glm.UorN3$residuals`~TR2.resid.UorNJuncos$UrbanorNot)
#p-value = 0.5636


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
noSmall3Pops = noSmallRep

SL3.glm.CITY = glm(noSmall3Pops$SongLength~noSmall3Pops$CityorMountainLump+ noSmall3Pops$Date + 
                     noSmall3Pops$Time + noSmall3Pops$DistRoad + noSmall3Pops$Lat + noSmall3Pops$Long)

summary(SL3.glm.CITY)
#AIC = -29.134

SL3.glm.CITY2 = glm(noSmall3Pops$SongLength~noSmall3Pops$CityorMountainLump+ noSmall3Pops$Date + 
                      noSmall3Pops$DistRoad + noSmall3Pops$Lat + noSmall3Pops$Long)

summary(SL3.glm.CITY2)
#AIC = -31.087

SL3.glm.CITY3 = glm(noSmall3Pops$SongLength~noSmall3Pops$CityorMountainLump+ noSmall3Pops$Date + 
                      noSmall3Pops$Lat + noSmall3Pops$Long)

summary(SL3.glm.CITY3)
#AIC = -33.063

SL3.glm.CITY4 = glm(noSmall3Pops$SongLength~noSmall3Pops$CityorMountainLump + 
                      noSmall3Pops$Date + noSmall3Pops$Long)

summary(SL3.glm.CITY4)
#AIC = -34.958

SL3.glm.CITY5 = glm(noSmall3Pops$SongLength~noSmall3Pops$CityorMountainLump + 
                      noSmall3Pops$Long)

summary(SL3.glm.CITY5)
#AIC = -36.447

SL3.glm.CITY6 = glm(noSmall3Pops$SongLength~noSmall3Pops$CityorMountainLump)
summary(SL3.glm.CITY6)
#AIC = -37.969


pairs(emmeans(SL3.glm.CITY6, "CityorMountainLump"))

SL3.resid.CITY = as.data.frame(SL3.glm.CITY6$residuals)
View(SL3.resid.CITY)
SL3.resid.CITY$BirdID = 1:nrow(noSmallRep)


SL3.resid.CITYJuncos <- merge(noSmall3Pops, SL3.resid.CITY, by = "BirdID")
View(SL3.resid.CITYJuncos)

ggplot(data = SL3.resid.CITYJuncos, aes(x = CityorMountainLump, y = SL3.resid.CITYJuncos$`SL3.glm.CITY6$residuals`,
                                        colour = CityorMountainLump)) + geom_point()

bartlett.test(SL3.resid.CITYJuncos$`SL3.glm.CITY6$residuals`~SL3.resid.CITYJuncos$CityorMountainLump)
#p-value = 0.6058

#Trill Rate City or Mountain Lump - small pop
noSmall3Pops2 = noSmallRep

TR3.glm.CITY = glm(noSmall3Pops2$TrillRate~noSmall3Pops2$CityorMountainLump+ noSmall3Pops2$Date + 
                     noSmall3Pops2$Time + noSmall3Pops2$DistRoad + noSmall3Pops2$Lat + noSmall3Pops2$Long)

summary(TR3.glm.CITY)
#AIC = 525.54

TR3.glm.CITY2 = glm(noSmall3Pops2$TrillRate~noSmall3Pops2$CityorMountainLump+ noSmall3Pops2$Date + 
                      noSmall3Pops2$DistRoad + noSmall3Pops2$Lat + noSmall3Pops2$Long)

summary(TR3.glm.CITY2)
#AIC = 524.19

TR3.glm.CITY3 = glm(noSmall3Pops2$TrillRate~noSmall3Pops2$CityorMountainLump+ noSmall3Pops2$Date + 
                      noSmall3Pops2$DistRoad + noSmall3Pops2$Long)

summary(TR3.glm.CITY3)
#AIC = 523.02

TR3.glm.CITY4 = glm(noSmall3Pops2$TrillRate~noSmall3Pops2$CityorMountainLump+ noSmall3Pops2$Date + 
                      noSmall3Pops2$DistRoad)

summary(TR3.glm.CITY4)
#AIC = 521.69

TR3.glm.CITY5 = glm(noSmall3Pops2$TrillRate~noSmall3Pops2$CityorMountainLump + 
                      noSmall3Pops2$Date)

summary(TR3.glm.CITY5)
#AIC = 613.17

TR3.glm.CITY6 = glm(noSmall3Pops2$TrillRate~noSmall3Pops2$CityorMountainLump)
summary(TR3.glm.CITY6)
#AIC = 614.37

pairs(emmeans(TR3.glm.CITY4, "CityorMountainLump"))

TR3.resid.CITY = as.data.frame(TR3.glm.CITY4$residuals)
View(TR3.resid.CITY)
TR3.resid.CITY$BirdID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
                          21,22,24,25,26,27,28,29,30,31,32,33,34,35,36,38,39,40,41,42,
                          44,45,46,48,49,50,55,57,58,59,60,61,68,69,70,71,
                          72,73,74,77,80,81,82,83,84,85,86,87,88,89,90,91,
                          92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,
                          110,111,112,131,132,133,154)


TR3.resid.CITYJuncos <- merge(noSmall3Pops2, TR3.resid.CITY, by = "BirdID")
View(TR3.resid.CITYJuncos)

ggplot(data = TR3.resid.CITYJuncos, aes(x = CityorMountainLump, y = TR3.resid.CITYJuncos$`TR3.glm.CITY4$residuals`,
                                        colour = CityorMountainLump)) + geom_point()

bartlett.test(TR3.resid.CITYJuncos$`TR3.glm.CITY4$residuals`~TR3.resid.CITYJuncos$CityorMountainLump)
#p-value = 0.1224

#minF City or Mountain Lump - small pop
noSmall3Pops3 = noSmallRep

minF3.glm.CITY = glm(noSmall3Pops3$minF~noSmall3Pops3$CityorMountainLump+ noSmall3Pops3$Date + 
                       noSmall3Pops3$Time + noSmall3Pops3$DistRoad + noSmall3Pops3$Lat + noSmall3Pops3$Long)

summary(minF3.glm.CITY)
#AIC = 130.19

minF3.glm.CITY2= glm(noSmall3Pops3$minF~noSmall3Pops3$CityorMountainLump+ noSmall3Pops3$Date + 
                       noSmall3Pops3$Time + noSmall3Pops3$DistRoad + noSmall3Pops3$Lat)

summary(minF3.glm.CITY2)
#AIC = 128.55

minF3.glm.CITY3= glm(noSmall3Pops3$minF~noSmall3Pops3$CityorMountainLump+ noSmall3Pops3$Date + 
                       noSmall3Pops3$DistRoad + noSmall3Pops3$Lat)

summary(minF3.glm.CITY3)
#AIC = 127.23

minF3.glm.CITY4 = glm(noSmall3Pops3$minF~noSmall3Pops3$CityorMountainLump+ noSmall3Pops3$Date + 
                        noSmall3Pops3$Lat)

summary(minF3.glm.CITY4)
#AIC = 126.02

minF3.glm.CITY5 = glm(noSmall3Pops3$minF~noSmall3Pops3$CityorMountainLump+ noSmall3Pops3$Date)
summary(minF3.glm.CITY5)
#AIC = 154.92

minF3.glm.CITY6 = glm(noSmall3Pops3$minF~noSmall3Pops3$CityorMountainLump)
summary(minF3.glm.CITY6)
#AIC = 154.9

pairs(emmeans(minF3.glm.CITY4, "CityorMountainLump"))

min3.resid.CITY = as.data.frame(minF3.glm.CITY4$residuals)
View(min3.resid.CITY)
min3.resid.CITY$BirdID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
                           21,22,24,25,26,27,28,29,30,31,32,33,34,35,36,38,39,40,41,42,
                           44,45,46,48,49,50,55,57,58,59,60,61,68,69,70,71,
                           72,73,74,77,80,81,82,83,84,85,86,87,88,89,90,91,
                           92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,
                           110,111,112,131,132,133,154)


min3.resid.CITYJuncos <- merge(noSmall3Pops3, min3.resid.CITY, by = "BirdID")
View(min3.resid.CITYJuncos)

ggplot(data = min3.resid.CITYJuncos, aes(x = CityorMountainLump, y = min3.resid.CITYJuncos$`minF3.glm.CITY4$residuals`,
                                         colour = CityorMountainLump)) + geom_point()

bartlett.test(min3.resid.CITYJuncos$`minF3.glm.CITY4$residuals`~min3.resid.CITYJuncos$CityorMountainLump)
#p-value = 0.02896

#maxF City or Mountain Lump - small pop
noSmall3Pops4 = noSmallRep

maxF3.glm.CITY = glm(noSmall3Pops4$maxF~noSmall3Pops4$CityorMountainLump+ noSmall3Pops4$Date + 
                       noSmall3Pops4$Time + noSmall3Pops4$DistRoad + noSmall3Pops4$Lat + noSmall3Pops4$Long)

summary(maxF3.glm.CITY)
#AIC = 134.13

maxF3.glm.CITY2 = glm(noSmall3Pops4$maxF~noSmall3Pops4$CityorMountainLump+ noSmall3Pops4$Date + 
                        noSmall3Pops4$DistRoad + noSmall3Pops4$Lat + noSmall3Pops4$Long)

summary(maxF3.glm.CITY2)
#AIC = 132.13

maxF3.glm.CITY3 = glm(noSmall3Pops4$maxF~noSmall3Pops4$CityorMountainLump+ noSmall3Pops4$Date + 
                        noSmall3Pops4$Long + noSmall3Pops4$Lat)
summary(maxF3.glm.CITY3)
#AIC = 130.33

maxF3.glm.CITY4 = glm(noSmall3Pops4$maxF~noSmall3Pops4$CityorMountainLump+ noSmall3Pops4$Lat + 
                        noSmall3Pops4$Long)
summary(maxF3.glm.CITY4)
#AIC = 130.16

maxF3.glm.CITY5 = glm(noSmall3Pops4$maxF~noSmall3Pops4$CityorMountainLump+ noSmall3Pops4$Long)
summary(maxF3.glm.CITY5)
#AIC = 130.26

maxF3.glm.CITY6 = glm(noSmall3Pops4$maxF~noSmall3Pops4$CityorMountainLump)
summary(maxF3.glm.CITY6)
#AIC = 148.94

pairs(emmeans(maxF3.glm.CITY4, "CityorMountainLump"))

max3.resid.CITY = as.data.frame(maxF3.glm.CITY4$residuals)
View(max3.resid.CITY)
max3.resid.CITY$BirdID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
                           21,22,24,25,26,27,28,29,30,31,32,33,34,35,36,38,39,40,41,42,
                           44,45,46,48,49,50,55,57,58,59,60,61,68,69,70,71,
                           72,73,74,77,80,81,82,83,84,85,86,87,88,89,90,91,
                           92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,
                           110,111,112,131,132,133,154)


max3.resid.CITYJuncos <- merge(noSmall3Pops4, max3.resid.CITY, by = "BirdID")
View(max3.resid.CITYJuncos)

ggplot(data = max3.resid.CITYJuncos, aes(x = CityorMountainLump, y = max3.resid.CITYJuncos$`maxF3.glm.CITY4$residuals`,
                                         colour = CityorMountainLump)) + geom_point()

bartlett.test(max3.resid.CITYJuncos$`maxF3.glm.CITY4$residuals`~max3.resid.CITYJuncos$CityorMountainLump)
#p-value = 0.05467

#bandF City or Mountain Lump - small pop
noSmall3Pops5 = noSmallRep

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
#AIC = 168.44

bandF3.glm.CITY6 = glm(noSmall3Pops5$bandwidthF~noSmall3Pops5$CityorMountainLump)
summary(bandF3.glm.CITY6)
#AIC = 199.08

pairs(emmeans(bandF3.glm.CITY5, "CityorMountainLump"))

bandF3.resid.CITY = as.data.frame(bandF3.glm.CITY5$residuals)
View(bandF3.resid.CITY)
bandF3.resid.CITY$BirdID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
                             21,22,24,25,26,27,28,29,30,31,32,33,34,35,36,38,39,40,41,42,
                             44,45,46,48,49,50,55,57,58,59,60,61,68,69,70,71,
                             72,73,74,77,80,81,82,83,84,85,86,87,88,89,90,91,
                             92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,
                             110,111,112,131,132,133,154)


bandF3.resid.CITYJuncos <- merge(noSmall3Pops5, bandF3.resid.CITY, by = "BirdID")
View(bandF3.resid.CITYJuncos)

ggplot(data = bandF3.resid.CITYJuncos, aes(x = CityorMountainLump, y = bandF3.resid.CITYJuncos$`bandF3.glm.CITY5$residuals`,
                                           colour = CityorMountainLump)) + geom_point()

bartlett.test(bandF3.resid.CITYJuncos$`bandF3.glm.CITY5$residuals`~bandF3.resid.CITYJuncos$CityorMountainLump)
#p-value = 0.05135