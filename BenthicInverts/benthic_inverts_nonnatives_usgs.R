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

#read in our list of target taxa
target_tax <- read_csv("./BenthicInverts/benthic_taxonomy_common5_2023-02-02.csv")

# Matching target taxa with USGS list----------
#the USGS dataset includes ITIS TSN and GBIF key, 
#which I could get for my taxa and use to search
#quick summary of results: Identified 9 of my target taxa as nonnnative
#obvously, just because a target taxon doesn't match the USGS database
#doesn't necessarily mean it is native

#quick look at column names of two dfs
glimpse(target_tax)
glimpse(nonnatives)

#try simply matching by species name
match_sp <- inner_join(target_tax,nonnatives,by=c("taxon"="scientificName")) %>% 
  #only keep hit for continental US (not AK or HI)
  filter(locality=="L48")
#8 matches
#haven't checked for matches with synonyms

#create vector of matching species
match_sp_list <- match_sp %>% 
  pull(taxon)

#try simply matching by genus name
#note that this doesn't definitive say if a taxon is nonnative because there can be native congeners
#also there will be overlap with the species list above
nonnatives_gf <- nonnatives %>% 
  #create a genus column
  separate(scientificName,into = c("genus","species"),remove=F) %>% 
  #only keep hit for continental US (not AK or HI)
  filter(locality=="L48")


match_gn <-  inner_join(target_tax,nonnatives_gf,by=c("genus"="genus"))  %>% 
  filter(!(taxon %in% match_sp_list)) %>% 
  relocate(scientificName,.after=taxon)
#probably mostly not useful results
#BUT did pick up a different spelling for Melanoides tuberculata (ie,Melanoides tuberculatus)









