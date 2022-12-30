#Special Studies Drought Synthesis
#Explore Dray et al 2014 example analysis

#required packages
library(ade4) #package by paper authors

#look at data sets-------------------
#these are all included in the ade4 package

data("aravo")

#species abundance matrix
species <- aravo$spe

#species traits
traits <- aravo$traits

#environmental predictors
env <- aravo$env
