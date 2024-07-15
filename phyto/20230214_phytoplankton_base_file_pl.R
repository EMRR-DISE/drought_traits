
# using > 1% freq occurrence for all stations combined
raw100 <- read.table(file.choose(),header=T)
#2022100619752021bvolallb
names(raw100)
#
library(dplyr)
# select size fraction split at 58 um3 for Nubvol for analysis based on old work of 58 Nubvol to account for magnification differences
# biovolumes are diatom versus non diatom corrected biovolumes
head(raw100)
szfract1<- filter(raw100, Nubvol > 57.9)

# select years with quality data - omit low count data - for analysis of trait data

yrszfile <- filter(szfract1, year < 1986 | year > 2007)
names(yrszfile)


# compute table of number of occurrences of each genus for all 6 stations

table2 <- table(yrszfile$genus)
table2
write.csv(table2, file="2023T3.csv",row.names=FALSE)

# using table 2 select genera with >1% frequency (grand total number divided by total for each genus)


# selected genera GT 1% or more of freq from yrszfile

yrszgenfile <- filter (yrszfile, genus=="Achnanthes"| 	genus==	"Aulacoseira"| 	genus==	"Bacillaria"| genus==	"Chlamydomonas"| genus==	"Chroococcus"|  genus==	"Cocconeis"| genus==	"Cryptomonas"| genus==		"Cyclotella" | genus=="Cymbella"|genus==	"Diatoma"|genus==	"Entomoneis" |		genus==	"Fragilaria" |	genus==	"Gomphonema"| genus==	"Gyrosigma"|genus==	"Melosira"|genus=="Navicula" |genus=="Nitzschia" | genus== "Plagioselmis" | genus=="Rhoicosphenia" | genus=="Scenedesmus"| genus=="Skeletonema"|  genus=="Synedra"|genus=="Thalassiosira"|genus=="Trachelomonas"|genus=="Unknown")
names(yrszgenfile)

# mean over month by station for Numl and omit Nubvol now which is not needed for this analysis
yrszgenfile2 <- with(yrszgenfile,aggregate(Numl,by=list(year=year, month=month, station=station, genus=genus),mean))
# rename new variable
names(yrszgenfile2)[5] <- "mNuml"
## 

# expand file to square matrix for both time periods
# 
T2 <- expand.grid(genus=c("Achnanthes", 	"Aulacoseira", "Bacillaria",		"Chlamydomonas",  "Chroococcus" ,		"Cocconeis" ,		"Cryptomonas" ,	"Cyclotella" ,	"Cymbella" , "Diatoma",  "Entomoneis" ,	 "Fragilaria", 		"Gomphonema" , "Gyrosigma",		"Melosira" , "Navicula",	"Nitzschia" ,		"Plagioselmis" ,			"Rhoicosphenia", 	"Scenedesmus" ,		"Skeletonema", 	"Synedra",	"Thalassiosira", "Trachelomonas", "Unknown"	), station=c("C3", "D4", "D26", "P8", "D7", "D8"), year=seq(2008,2021,1),month=seq(1,12,1))

T1 <- expand.grid(genus=c("Achnanthes", 	"Aulacoseira", "Bacillaria",		"Chlamydomonas",  "Chroococcus" ,		"Cocconeis" ,		"Cryptomonas" ,	"Cyclotella" ,	"Cymbella" , "Diatoma",  "Entomoneis" ,	 "Fragilaria", 		"Gomphonema" , "Gyrosigma",		"Melosira" , "Navicula",	"Nitzschia" ,		"Plagioselmis" ,			"Rhoicosphenia", 	"Scenedesmus" ,		"Skeletonema", 	"Synedra",	"Thalassiosira", "Trachelomonas", "Unknown"	), station=c("C3", "D4", "D26", "P8", "D7", "D8"), year=seq(1975,1985,1),month=seq(1,12,1))

# merge expanded files
T3 <- merge(T1,T2,all=T)
T3

#   merge exapnded file with base file
expfile1 <- merge(T3, yrszgenfile2, all=T)
1
# change missing values zero values
expfile1[is.na(expfile1)] <- 0

write.csv(expfile1,file="20230115genusGT1pct.csv",row.names=FALSE)


# convert to wide format
# 

library(tidyr)
library(dplyr)

one <- subset(expfile1,select=c(station, month,year, genus, mNuml))
head(one)
# make horizontal file from vertical file using spread
sone <- group_by(one, station, month, year)
head(sone)
#genera of GT 1% freq  values are mean numl by month station year
stwo <- spread(sone,key='genus',value='mNuml')
head(stwo)
write.csv(stwo,file="20230115widegenusGT1a1.csv",row.names=FALSE)


