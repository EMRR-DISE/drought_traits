#A Database of Lotic Invertebrate Traits for North America
#https://pubs.usgs.gov/ds/ds187/

#to do list
#read through metadata to decide which columns of database are useful to me (most probably aren't)
#then cut down the database before looking for traits for my taxa

#load packages
library(tidyverse)
library(janitor)

#read in USGS data
traits <- read_tsv("https://pubs.usgs.gov/ds/ds187/htodcs/InvertTraitsTable_v1.txt") %>% 
  clean_names()

citations <- read_tsv("https://pubs.usgs.gov/ds/ds187/htodcs/InvertTraitsCitations_v1.txt")
metadata <- read_tsv("https://pubs.usgs.gov/ds/ds187/htodcs/InvertTraitsFields_v1.txt")

#read in taxonomy info for my target taxa
target <- read_csv("./BenthicInverts/benthic_inverts_taxa_common_5_updated_2023-01-26.csv")

#combine my target taxa with the traits database to see how many match
#probably not many because USGS database is mostly insects
matched <- left_join(target,traits) %>% 
  filter(!is.na(trait_record_id))
#only 9 matches for species; one snail and two annelids

#does it get better if I match just by genus rather than species?
gmatched <- target %>% 
  select(organism_code,kingdom:genus) %>% 
  filter(!is.na(genus)) %>% 
  left_join(traits) %>% 
  filter(!is.na(trait_record_id))
#65 genus level matches so quite a bit better than species level matches

#now how many genera have at least one record?
gmatchedtax <- gmatched %>% 
  group_by (genus) %>% 
  summarize(records = n())
#10 taxa; most hits for chironomids, amphipods, annelids
