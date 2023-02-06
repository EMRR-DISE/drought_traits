#Special Studies Drought Synthesis
#Environmental Monitoring Program data
#Benthic Invertebrate Survey data
#Check whether species are native using USGS list

# Load required packages -----------------
library(tidyverse) #suite of data science tools
library(janitor) #misc functions for cleaning data

# read in data ---------------

#USGS list of nonnatives downloaded from sciencebase
#https://www.sciencebase.gov/catalog/item/62d59ae5d34e87fffb2dda99
nonnatives <- read_csv("./usgs_nonnatives/USRIISv2csvFormat/USRIISv2_MasterList.csv")







