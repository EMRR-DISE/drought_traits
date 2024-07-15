#Phyto
#convert taxa table from wide to long format

#packages
library(tidyverse)
library(here)

#read in data
phyto <- read_csv("phyto/20240528_PHYTOtaxatable.csv")

traits <- read_csv("phyto/20240606_PHYTOtraitstable4v4.csv")

#convert wide to long
phyto_long <- phyto %>% 
  pivot_longer(cols=starts_with("species"), names_to = "species_number", values_to = "species") %>% 
  select(taxon_name = Taxon_name
         ,empire:family
         ,species
         ) %>% 
  #drop rows with NA for species
  filter(!is.na(species)) %>% 
  #sort by genus, then species
  arrange(taxon_name,species) %>% 
  #remove new lines (ie, "\n")
  mutate(species = str_replace_all(string = species, pattern = "[\r\n]", replacement="")) %>% 
  glimpse()

#write long form file
#write_csv(phyto_long, "phyto/phyto_taxatable.csv")

#does the trait data and taxonomy data merge together properly?

#first, make version of taxonomy without species
phyto_tax <- phyto_long %>% 
  select(-species) %>% 
  distinct()

comb <- left_join(traits,phyto_tax)
#looks good

