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

#Format the data--------

#look at distribution of sample sizes
hist(iep$n)
range(iep$n) #19-700

iep_format <- iep %>%
  mutate(
    database = "IEP"
    ,trait_group = "body_size_max" 
    ,trait_unit = "mm"
  ) %>% 
  select(lit_taxon_name = taxname
         ,lit_taxon_level = level
         ,preservative
         ,weight_type
         ,n
         ,database
         ,citation = reference
         ,trait_group
         ,trait_value = max_length
         ,trait_unit
  ) %>% 
  glimpse()
