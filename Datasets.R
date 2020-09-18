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

noOld = subset(IndividualJuncos, Location != "UCSD 2006/2007")
noOld$Location = as.character(noOld$Location)
noOld$Location = as.factor(noOld$Location)
levels(noOld$Location)

noRep = subset(IndividualJuncos, Location != "REPOSITORY")
noRep$Location = as.character(noRep$Location)
noRep$Location = as.factor(noRep$Location)
levels(noRep$Location)

noOldRep = subset(IndividualJuncos, Location != "UCSD 2006/2007")
noOldRep = subset(noOldRep, Location != "REPOSITORY")
noOldRep$Location = as.character(noOldRep$Location)
noOldRep$Location = as.factor(noOldRep$Location)
levels(noOldRep$Location)

NoSmallPop = subset(IndividualJuncos, Location != "UCSB")
NoSmallPop = subset(NoSmallPop, Location != "STUNT")
NoSmallPop = subset(NoSmallPop, Location != "JAMES")
NoSmallPop$Location <- as.character(NoSmallPop$Location)
NoSmallPop$Location <- as.factor(NoSmallPop$Location)
levels(NoSmallPop$Location)


noSmall = NoSmallPop
noSmall = subset(noSmall, Location != "UCSD 2006/2007")
noSmall$Location <- as.character(noSmall$Location)
noSmall$Location <- as.factor(noSmall$Location)
levels(noSmall$Location)

noSmall$TR.transform = log(noSmall$TrillRate)
noSmall$maxF.transform = log(noSmall$maxF)
noSmall$bandF.transform = log(noSmall$bandwidthF)

noSmallRep = noSmall
noSmallRep = subset(noSmallRep, Location !="REPOSITORY")
noSmallRep$Location = as.character(noSmallRep$Location)
noSmallRep$Location = as.factor(noSmallRep$Location)
levels(noSmallRep$Location)

TrillRateZ$TR.transform = log(TrillRateZ$TrillRate)

noSmallRep$TR.transform = log(noSmallRep$TrillRate)
noSmallRep$maxF.transform = log(noSmallRep$maxF)
noSmallRep$bandF.transform = log(noSmallRep$bandwidthF)

View(noSmallRep)

justSD = subset(IndividualJuncos, Location == "UCSD" | Location == "UCSD 2006/2007")
justSD$Location = as.character(justSD$Location)
justSD$Location = as.factor(justSD$Location)
levels(justSD$Location)
View(justSD)
