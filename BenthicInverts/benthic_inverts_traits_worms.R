#Special Studies Drought Synthesis
#Environmental Monitoring Program data
#Benthic Invertebrate Survey data
#Pull trait data from WoRMS online database

#steps
#create named vector of all AphiaIDs
#feed this into the wm_attr_data() function
#filter dataset to just desired trait
#do a bunch of unnesting of this poorly structured dataset

# Load required packages -----------------
library(tidyverse) #suite of data science tools
library(worrms) #package for pulling data from WoRMS
library(janitor) #misc functions for cleaning data

# read in data ---------------

#just need the taxonomy info for our target taxa
target_tax <- read_csv("./BenthicInverts/benthic_taxonomy_common5_2023-02-02.csv")

# Example ---------
#first two taxa in our data set

#vector of aphia ids
b_aphia <- c(992855,1040874)

#run aphia ids through the function that grabs the trait data from worms
w_traits <- wm_attr_data_(id = b_aphia) 

#explore the type of trait data in resulting output dataframe
w_traits_dist <- w_traits %>% 
  #count number of records (ie, rows) for all distinct combos of the measurement types and associated ID numbers
  group_by(measurementTypeID,measurementType) %>% 
  summarize(records = n())
#four types of traits, most records for body size

#body size: unnesting
w_bsize <- w_traits %>% 
  #need to subset to just the body size records first
  #otherwise the content of columns for trait data is a mix of unrelated info
  filter(measurementTypeID==15) %>% 
  #first unnesting; need to do name repair because of duplication of column names
  #this unnested the size measurement units (mm)
  unnest(cols = children, names_repair = "universal") %>%  
  #unnest again to get the type of body size (min vs max)
  unnest(cols = children) %>% 
  #unnest again to get body width vs body length category
  unnest(cols = children, names_repair = "universal") %>% 
  #unnest again to get columns indicating these are traits for adults
  unnest(children) %>% 
  #clean up format of column names
  clean_names() %>% 
  glimpse()

#body size: format records of max adult body length
w_bsize_ft <- w_bsize %>% 
  #only keep needed columns and rename them
  #lots of repetition of columns once unnested
  select(
    aphia_id = aphia_id_2
    ,trait_value=measurement_value_5
    ,units = measurement_value_14
    ,life_stage = measurement_value
    ,body_dimension = measurement_value_32
    ,body_measure_type = measurement_value_23
    ,quality_status=qualitystatus_8
    ,citation=reference_7
      ) %>% 
  #just keep adult max body length
  filter(life_stage=="adult" & body_dimension=="length" & body_measure_type=="maximum") %>% 
  glimpse()
  
  
  
