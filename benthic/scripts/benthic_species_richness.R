#Special Studies Drought Synthesis
#Environmental Monitoring Program data
#Benthic Invertebrate Survey data
#Number of non-native species present in upper estuary through time

#Nick Rasmussen, nicholas.rasmussen@water.ca.gov

# Metadata notes ---------------------------
#1975 - 2021
#Catch per unit effort (CPUE)
#Organisms/m2 = (sum per visit /# of grabs per visit)/(0.052 grabs/meter squared)
#GRTS amphipod papers says ponar sample was to a depth that varied from 4-10 cm with sediment type

# Load required packages -----------------
#all packages available on CRAN except deltamapr
#see GitHub repository for deltamapr at https://github.com/InteragencyEcologicalProgram/deltamapr

library(tidyverse) #suite of data science tools
library(janitor) #used to quickly clean up column names
library(lubridate) #format date
library(EDIutils) #download EDI data


# Read in the data-----------------------

#use EDIutils package to read in all file names and download the ones you want to use
#https://docs.ropensci.org/EDIutils/index.html

#most of what I need is published on EDI
#https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=1036&revision=2

#list all data files from EMP benthic inverts EDI package
benthic_pkg <- read_data_entity_names(packageId = "edi.1036.4")

#benthic invert CPUE, 1975-2021
#data have been converted to CPUE (organisms/m2)
#replicate grabs have been averaged for each site visit
#all non-occurrence (zero) data for a site visit has been removed
#Nick: samples with no organisms at all are probably included as "No catch"
benthic_invert_cpue <- read_csv(read_data_entity(packageId = "edi.1036.4", entityId= benthic_pkg$entityId[2])) %>% 
  clean_names() %>% 
  glimpse()

#file with native/non-native status of taxa


#look at the number of unique taxa collected from the upper estuary---------------------

spp_rich <- benthic_invert_cpue %>% 
  distinct(organism_code)
#420 taxa




