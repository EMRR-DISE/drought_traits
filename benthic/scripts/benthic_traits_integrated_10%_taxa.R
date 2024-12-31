#Special Studies Drought Synthesis
#Environmental Monitoring Program data
#Benthic Invertebrate Survey data
#Taxa present in at least 10% of samples for at least one station
#Combine trait data from the various sources

#packages
library(tidyverse)

#read in data--------------

#list of taxa that are in at least 10% of samples for at least one station
#includes organism codes and frequency of occurrence for each stations and across all three stations
freq <- read_csv("./benthic/data_output/benthic_common10_prop_stations.csv") 

#updated taxonomic info for this group of taxa
taxonomy <- read_csv("./benthic/data_output/benthic_common10_by_stn_taxonomy_2024-12-30.csv") 

#native/non-native status for the 64 non-rare taxa
origin <- read_csv("./benthic/data_output/edi/benthic_nonrare_taxonomy_origin_edi.csv") 

#body lengths for the 64 non-rare taxa
size <- read_csv("./benthic/data_output/edi/benthic_nonrare_size_edi_2024-07-12.csv") 

#dominant taxa (n = 14) trait data
dom <- read_csv("./benthic/data_output/edi/benthic_dominant_traits_edi_2024-07-12.csv") 


#combine the frequency, taxonomy, and origin info------------

#reduce taxonomy dataset to just the columns needed for data entry
#keep organism_code, taxon, rank
#also drop the three taxa that aren't IDed to at least genus
taxon <- taxonomy %>% 
  select(organism_code
         ,taxon
         ,target_taxon_level = rank) %>% 
  filter(target_taxon_level=="Species" | target_taxon_level=="Genus") %>%  
  #change target_taxon_level to lowercase
  mutate(target_taxon_level = str_to_lower(target_taxon_level)) %>% 
  glimpse()
#the updated taxonomy has the "sp. A" part missing for genus level IDed taxa

#grab the full names for genus level taxa from origin df
genm <- origin %>% 
  select(organism_code,target_taxon_name)

#update the genus level taxa to include "sp. A"
taxona <- taxon %>% 
  left_join(genm) %>% 
  mutate(taxon_name = case_when(target_taxon_level=="genus"~target_taxon_name
                                ,TRUE~taxon)) %>% 
  arrange(target_taxon_level) %>% 
  select(organism_code
         ,target_taxon_name = taxon_name
         ,target_taxon_level)


#reduce origin df to just needed columns
or <- origin %>% 
  select(organism_code,native)

  
#combine frequency and taxonomy info
#match by organism_code
#join freq to taxon to drop the three high level taxa we won't use from freq
tfo <- taxona %>% 
  left_join(freq) %>% 
  left_join(or)
#just missing origin for the three newly added taxa

#create trait template before combining with existing traits---------------
#body_size_max, trophic_habit, disperal

#make the other needed data frames

#trait categories
trait <- data.frame(
  trait = c("body_size_max","trophic_habit","dispersal")
)

#all combinations of taxa and traits
combo <- crossing(tfo,trait) %>% 
  arrange(-prop,organism_code)

#fill in existing size data-----

# scombo <- combo %>% 
#   left_join(size)
#size data didn't match properly for 4410; it's because of mismatch in target_taxon_level

#drop target_taxon_level from size df to avoid this problem
sz <- size %>% 
  select(-target_taxon_level)

#add size data again
scombo <- combo %>%
  left_join(sz) %>% 
  #need to make trait_value of type character to combine properly with rest of traits below
  mutate(trait_value = as.character(trait_value)) %>% 
  glimpse()

#make sure it joined properly
scomboc <- scombo %>% 
  filter(trait == "body_size_max" & is.na(trait_value))
#now only missing data for the three new taxa as expected

#fill in data from dominant taxa df------------
#most interested in trophic_habit, dispersal

dm <- dom %>% 
  filter(trait == "trophic_habit" | trait == "dispersal") %>% 
  glimpse()

sdcombo <- scombo %>% 
  left_join(dm)

#make sure data combined properly
sdcomboc <- sdcombo %>% 
  filter((trait == "trophic_habit" | trait=="dispersal") & !is.na(trait_value)) %>% 
  arrange(organism_code)
#this didn't work; look back at structure of the two df's to figure out why


