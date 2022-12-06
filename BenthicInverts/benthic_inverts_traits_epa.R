#Special Studies Drought Synthesis
#Benthic Invertebrates
#EPA Freshwater Biological Traits

#Nick Rasmussen, nicholas.rasmussen@water.ca.gov

#Freshwater Biological Traits Database 
#https://www.epa.gov/risk/freshwater-biological-traits-database-traits#:~:text=The%20Freshwater%20Biological%20Traits%20Database,on%20river%20and%20stream%20ecosystems

# Load required packages -----------------
library(tidyverse) #suite of data science tools
library(janitor) #use for cleaning up column names
library(readxl) #read excel files

#read in data-----------------

#EPA traits database
epa_traits <- read_excel("./BenthicInverts/FreshwaterBioTraits_20100927.xlsx") %>% 
  #clean up column header formatting
  clean_names()

#Benthic invert taxa present in at least 5% of samples
#NOTE: consider adding TSN to this dataset to match with EPA dataset
emp_spp <- read_csv("./BenthicInverts/benthic_inverts_taxa_common_5_updated_2022-10-25.csv")

#try matching taxa with traits by taxon column--------------

#simplify dataset first
#note that 3 of 65 taxa are higher level than genus so won't match using this simple approach
emp <- emp_spp %>% 
  select(organism_code,taxon,genus)

epa <- epa_traits %>% 
  select(taxon,tsn,genus) %>% 
  #for now, simplify to one row per taxon
  distinct()

#match by taxon and genus
sp_traits <- left_join(emp,epa)

sp_traits_match <- sp_traits %>% 
  filter(!is.na(tsn))
#only 8 of 65 taxa matched

#try matching taxa with traits by genus column--------------

emp2 <- emp_spp %>% 
  select(organism_code,taxon_emp=taxon,genus)

epa2 <- epa_traits %>% 
  select(taxon_epa=taxon,tsn,genus) %>% 
  #for now, simplify to one row per taxon
  distinct()

#match by taxon and genus
sp_traits2 <- left_join(emp2,epa2)

sp_traits_match2 <- sp_traits2 %>% 
  filter(!is.na(taxon_epa)) %>% 
  distinct(organism_code,taxon_emp)
#34 of 65 (52%) taxa matched to a reasonable extent
#exact match, congener match, genus level match

#note there may be additional matches if names of taxa differ between data sets (ie, synonyms)




