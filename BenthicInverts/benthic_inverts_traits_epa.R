#Special Studies Drought Synthesis
#Benthic Invertebrates
#EPA Freshwater Biological Traits

#Nick Rasmussen, nicholas.rasmussen@water.ca.gov

#To do list-------------------
#look at database metadata and figure out which available traits match our traits
#for matching taxa, pick traits most relevent based on things like geography

# Load required packages -----------------
library(tidyverse) #suite of data science tools
library(janitor) #use for cleaning up column names
library(readxl) #read excel files

#read in data-----------------

#Freshwater Biological Traits Database 
#https://www.epa.gov/risk/freshwater-biological-traits-database-traits#:~:text=The%20Freshwater%20Biological%20Traits%20Database,on%20river%20and%20stream%20ecosystems
epa_traits <- read_excel("./BenthicInverts/epa_trait_database/FreshwaterBioTraits_20100927.xlsx") %>% 
  #clean up column header formatting
  clean_names()

#Benthic invert taxa present in at least 5% of samples
#NOTE: consider adding TSN to this dataset to match with EPA dataset
emp_spp <- read_csv("./BenthicInverts/benthic_inverts_taxa_common_5_updated_2023-01-26.csv")

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
#made sure the column headers I want to match are same between the two data frames
#adds epa taxa to list of our taxa
sp_traits <- inner_join(emp,epa)
#only 8 of 65 (12%) taxa matched
#only 2 of these matches are species

#try matching taxa with traits by genus column--------------

#format EMP data
emp2 <- emp_spp %>% 
  select(organism_code
         #rename taxon column to prevent taxon matching between data frames
         ,taxon_emp=taxon
         ,genus) %>% 
  glimpse()

#format EPA data
epa2 <- epa_traits %>% 
  select(taxon_epa=taxon,tsn,genus) %>% 
  #for now, simplify to one row per taxon
  distinct() %>% 
  glimpse()

#match by genus only
sp_traits2 <- inner_join(emp2,epa2, multiple="all")

sp_traits_match2 <- sp_traits2 %>% 
  filter(!is.na(taxon_epa)) %>% 
  distinct(organism_code,taxon_emp)
#34 of 65 (52%) taxa matched to a reasonable extent
#exact match, congener match, genus level match

#note there may be additional matches if names of taxa differ between data sets (ie, synonyms)

#look at traits for a few example taxa--------------------------

#Physa traits
physa <- epa_traits %>% 
  filter(taxon=="Physa")






