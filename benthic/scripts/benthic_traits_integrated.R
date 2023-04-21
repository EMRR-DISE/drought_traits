#Special Studies Drought Synthesis
#Environmental Monitoring Program data
#Benthic Invertebrate Survey data
#Combine trait data from the various sources
#for now, just focus on body size

#included sources
#USGS
#NEMESIS
#WoRMS
#CDFW zoop lab (eventually)

# Load required packages -----------------
library(tidyverse) #suite of data science tools
library(janitor) #misc functions for cleaning data

# read in data ---------------
#read in all files from traits subfolder

usgs <- read_csv("./benthic/data_output/traits/benthic_traits_usgs_size.csv") %>% 
  glimpse()

worms <- read_csv("./benthic/data_output/traits/benthic_traits_worms_size.csv") %>% 
  glimpse()

nemesis <- read_csv("./benthic/data_output/traits/benthic_traits_nemesis_size&physiology.csv") %>% 
  glimpse()

#Combining the data sets----
#I'm sure the columns won't all match perfectly

traits_comb <- bind_rows(usgs,worms,nemesis)
#31 columns so clearly not a perfect match of columns


