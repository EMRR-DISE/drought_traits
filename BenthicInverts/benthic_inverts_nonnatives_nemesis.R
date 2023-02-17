#Special Studies Drought Synthesis
#Environmental Monitoring Program data
#Benthic Invertebrate Survey data
#non-native status of taxa
#provided by Sharon Shiba based on NEMESIS/CalNEMO database

#Nick Rasmussen, nicholas.rasmussen@water.ca.gov

#To do list-----------------

#Changes to make upstream of this script in taxonomy script: 
#Sinelobus sp. A to Sinelobus stanfordi
#Synidotea laevidorsalis to S. laticauda

#should add estimated year of introduction for non-natives
#useful because some of the important taxa arrived during the time series we're analyzing
#also need to pair this trait data with literature references (largely Cohen and Carlton 1995)

# Load required packages -----------------

library(janitor) #used to quickly clean up column names
library(tidyverse) #suite of data science tools
library(readxl) #reading xlsx files

# Read in the data-----------------------

#read in non-native status file
benthic_aliens <- read_excel("./BenthicInverts/IEP_BenthicOrganismList2021_w_Intro_status_From Sharon Shiba.xlsx") %>% 
  clean_names() %>% 
  glimpse()

#read in my list of focal taxa
focal_taxa <- read_csv("./BenthicInverts/benthic_taxonomy_common5_2023-02-02.csv") %>% 
  glimpse()

# Format data in prep for combining ---------------------

#focal taxa data
focal_taxa_format <- focal_taxa %>% 
  select(organism_code
         , taxon_worms = taxon) %>% 
  glimpse()

#NEMESIS data
aliens_format <- benthic_aliens %>% 
  select(organism_code
         , genus
         , species
         , origin = status_in_cal_nemo
         , comments) %>% 
  glimpse()

#take a closer look at those with a non-native status
aliens <- aliens_format %>% 
  filter(!is.na(origin))
#72 taxa

#look at categories for origin
aliens_cat <- aliens %>% 
  group_by(origin) %>% 
  count() %>% 
  glimpse()
#nearly all simply say introduced; all other categories are singletons

# Filter data set to just my focal taxa ------------------

aliens_focal <- left_join(focal_taxa_format,aliens_format) %>% 
  glimpse()
#one cryptogenic species; maybe just include as introduced or native since only one

#format for taxonomic comparison
focal_taxa_comp <- aliens_focal %>% 
  separate(col = taxon_worms, into = c("genus_worms","species_worms"), sep = " ", remove = F) %>% 
  rename(genus_emp = genus, species_emp = species) %>% 
  mutate(genus_match = case_when(genus_emp == genus_worms ~ 1, TRUE ~ 0)
         ,species_match = case_when((species_emp == species_worms |species_emp == "sp. A") ~ 1, TRUE ~ 0)
         ,taxon_match = case_when((genus_match==1 & species_match==1) ~ 1, TRUE ~ 0)
         ) %>% 
  arrange(taxon_match,comments)

#create subset of taxa with taxonomic discrepancies or other issues to examine
focal_taxa_issue <- focal_taxa_comp %>% 
  filter(taxon_match==0 | !is.na(comments))
#most discrepancies are simply from me updating names using worms so they're fine as is
#Sharon indicates that Laonome calida should be Laonome cf. calida, so use literature data with caution
#she also labels as Sinelobus sp. A as Sinelobus cf. stanfordi, which is better than just genus level but again use species data with caution
#she labeled Manayunkia speciosa as cryptogenic; maybe just label as invasive for simplicity (following Cohen and Carlton 1995)
#she labeled Synidotea laevidorsalis as S. laticauda. both are valid names
#unclear if ours is S. laevidorsalis or S. laticauda but NEMESIS suggests probably S. laticauda so let's use that

#changes to make here: 
#Manayunkia speciosa change from cryptogenic to non-native
#assume Sinelobus sp. A is Sinelobus stanfordi and change from Introduced? to Introduced

#changes to make upstream in taxonomy script: 
#Sinelobus sp. A to Sinelobus stanfordi
#Synidotea laevidorsalis to S. laticauda

# Format the data set filtered to just my target taxa ---------------
#NOTE: assuming any taxon not specificly IDed as non-native is native
#clearly some of these taxa left as native could be non-native, particularly the ones not IDed to species

aliens_focal_format <- aliens_focal %>% 
  select(organism_code,origin) %>% 
  mutate(
    #change one case of Introduced? and Cryptogenic to Introduced (following Cohen and Carlton 1995 which might be outdated)
    #fill NAs with Native
    native = case_when((origin == "Introduced?" | origin == "Cryptogenic" | origin == "Introduced") ~ "0"
                        , is.na(origin) ~ "1")
                        #,TRUE~origin)
    ) %>% 
  select(organism_code,native) %>% 
  arrange(organism_code) %>% 
  glimpse()

#export the complete file
#write_csv(aliens_focal_format,"./BenthicInverts/bentic_inverts_trait_origin.csv")
