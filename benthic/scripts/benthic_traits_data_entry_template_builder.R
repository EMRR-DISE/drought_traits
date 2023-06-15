#Special Studies Drought Synthesis
#Environmental Monitoring Program data
#Benthic Invertebrate Survey data
#Build part of manual trait data entry template

#all combinations of the 14 most abundant taxa and target traits
#also assignment of species to Nick or Leela

# Load required packages
library(tidyverse) #suite of data science tools
library(janitor) #misc functions for cleaning data

# read in data 

#read in relative abundance data file
abundance <- read_csv("./benthic/data_output/benthic_relative_abundances.csv") 

#filter the abundance data frame to only keep the 14 most abundant
abund14 <- abundance %>% 
  filter(rank_no<15) %>% 
  select(organism_code,species_name,rank_no)

#make the other needed data frames

#trait categories
trait <- data.frame(
  trait_group = c("trophic_habit","habit","armoring","voltinism","dispersal","salinity_tolerance")
)

#assigned person
assign <- data.frame(
  assigned = c(rep("Leela",42),rep("Nick",42))
)

#combine data frames

combo <- crossing(abund14,trait) %>% 
  arrange(rank_no,organism_code)

combo2 <- data.frame(cbind(combo,assign))

#write file
#write_csv(combo2,"./benthic/data_output/benthic_traits_data_entry_rows.csv")






