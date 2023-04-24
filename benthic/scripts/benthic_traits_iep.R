#Special Studies Drought Synthesis
#Environmental Monitoring Program data
#Benthic Invertebrate Survey data
#Format data from the CDFW zooplankton lab
#got this from Christina Burdi

#Notes-------
#I just grabbed the max body length but could use the equations to estimate max body mass too

# Load required packages -----------------
library(readxl) #read in excel files
library(tidyverse) #suite of data science tools
library(janitor) #misc functions for cleaning data

# read in data ---------------

#IEP data
iep <- read_excel("./benthic/data_input/traits_iep/Biomass conversions_CEB Updated 2021.xlsx",sheet = "Macro-zooplankton") %>% 
  clean_names() %>% 
  glimpse()

#taxonomy file with organism codes
target_tax <- read_csv("./benthic/data_output/benthic_common5_taxonomy_2023-03-27.csv")

#combine IEP data with taxonomy file----------------
iep_tax <- left_join(iep, target_tax,by=join_by(taxname==taxon)) %>% 
  arrange(taxname)
#did a quick visual comparison between full taxonomy and this output and it looks like the matches worked fine
#there may be size data in the IEP data set that are higher level than our target taxa but that would be useful to use

#drop the non-matching taxa rows
iep_tax_trunc <- iep_tax %>% 
  filter(!is.na(organism_code)) %>% 
  glimpse()
#there are multiple rows for some taxa (ethanol vs formalin)

#let's just keep the largest measurement for each taxon
iep_tax_max <- iep_tax_trunc %>% 
  group_by(organism_code,taxname,level,reference,rank) %>% 
  summarize(trait_value = max(max_length),.groups = "drop") %>% 
  glimpse()


#Format the data--------

iep_format <- iep_tax_max %>%
  mutate(
    target_taxon_name =taxname
    ,lit_taxon_type = "target"
    ,lit_taxon_type_ord = 1
    ,database = "IEP"
    ,trait_group = "body_size_max" 
    ,trait_unit = "mm"
  ) %>% 
  select(
    organism_code
    ,target_taxon_name
    ,target_taxon_level = rank
    ,lit_taxon_name = taxname
    ,lit_taxon_level = level
    ,lit_taxon_type
    ,lit_taxon_type_ord
         ,database
         ,citation = reference
         ,trait_group
         ,trait_value
         ,trait_unit
  ) %>% 
  glimpse()
