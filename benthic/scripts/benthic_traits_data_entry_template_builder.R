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

origin <-read_csv("./benthic/data_output/traits/benthic_traits_nemesis_origin_taxa64.csv")

#filter the abundance data frame to only keep the 14 most abundant and just needed columns
abund14 <- abundance %>% 
  filter(rank_no<15) %>% 
  select(organism_code,species_name,rank_no)

#reduce size of origin dataframe to just needed columns
origin_sub <- origin %>% 
  select(organism_code,native)

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

#all combinations of taxa and traits
combo <- crossing(abund14,trait) %>% 
  arrange(rank_no,organism_code)

#add the assignments
combo2 <- data.frame(cbind(combo,assign))

#add the origin for each
combo3 <- left_join(combo2,origin_sub)

#write file
#write_csv(combo3,"./benthic/data_output/benthic_traits_data_entry_rows.csv")






