#Special Studies Drought Synthesis
#Environmental Monitoring Program data
#Benthic Invertebrate Survey data
#Pull trait data from WoRMS online database

#NOTE---------------
#for now, just ignore the taxa that aren't IDed at least to genus; we might just drop these from analysis anyway
#looks like WoRMS does move traits from old synonyms to current names so we probably don't need to search for traits
#in old synonym webpages

#steps
#create named vector of all AphiaIDs
#feed this into the wm_attr_data() function
#filter dataset to just desired trait
#do a bunch of unnesting of this poorly structured dataset

# Load required packages -----------------
library(tidyverse) #suite of data science tools
library(worrms) #package for pulling data from WoRMS
library(janitor) #misc functions for cleaning data
library(dplyr) #changing col names

# read in data ---------------

#just need the taxonomy info for our target taxa
target_tax <- read_csv("./benthic/data_output/benthic_common5_taxonomy_2023-03-27.csv")

#Grab data from WorMS using aphiaIDs---------

#this will create a vector of all the IDs in the original file
b_aphia <- target_tax %>% 
  pull(aphia_id)

#No data for: 1040507,608090,181574,156905,158104,148669,118375,247016,415154,127595,397131,717164,181551,582922,1591089,391379,794
#some only have qualitative body size****

#run aphia ids through the function that grabs the trait data from worms
#NOTE: wm_attr_category() allows for grabbing a particular type of trait data rather than grabbing all of it
w_traits <- wm_attr_data_(id = b_aphia) 

#explore the type of trait data in resulting output dataframe
w_traits_dist <- w_traits %>% 
  #count number of records (ie, rows) for all distinct combos of the measurement types and associated ID numbers
  group_by(measurementTypeID,measurementType) %>% 
  summarize(records = n())
#doesn't look like other trait types will be very useful for us so just focus on body size

#body size: unnesting
w_bsize <- w_traits %>% 
  #need to subset to just the body size records first
  #otherwise the content of columns for trait data is a mix of unrelated info
  filter(measurementTypeID==15) %>% 
  #first unnesting; need to do name repair because of duplication of column names
  #this unnested the size measurement units (mm)
  unnest(cols = children, names_repair = "universal") %>%  
  #unnest again to get the type of body size (min, max, etc)
  unnest(children) %>% 
  #clean up format of column names
  clean_names() %>% 
  glimpse()
#records have two different levels of nestedness, so in next step split the two groups of records

#format data with greater amount of nestedness
w_bsize_str1 <- w_bsize %>% 
  #for now, filter out the three records that are "Dimension" instead of "Type"
  #and just keep size records that are maximum
  filter(measurement_type=="Type" & measurement_value =="maximum") %>%  
  #unnest again to get body width vs body length category
  #one of records shows Locality instead of Dimension; probably because its a diameter, not a length/width
  unnest(cols = children, names_repair = "universal") %>%   
  #filter out width measurements (retains lengths and other types that need examination)
  filter(measurementValue != "width") %>% 
  #unnest again to get columns indicating these are traits for adults
  #one of these shows at this level locality and many show NA (probably because not specified as adults)
  unnest_wider(children, names_repair = "universal") %>% 
  #clean up names again
  clean_names() %>% 
  glimpse()

#then format data with lesser amount of nestedness (only three records)
#format data with greater amount of nestedness
w_bsize_str2 <- w_bsize %>% 
  #just keep the three that aren't Type and then drop the one that is width
  filter(measurement_type=="Dimension" & measurement_value!= "width") %>%  
  #unnest to get columns indicating these are traits for adults
  unnest_wider(children, names_repair = "universal")  %>% 
  #clean up names again
  clean_names() %>% 
  glimpse()

#should probably look at webpage entries for taxa with body size measurements that aren't max adult length
#might not have length because of body shape (eg, barnacles have diameter)

#structure 1: format records of max adult body length
w_bsize_ft_str1 <- w_bsize_str1 %>% 
  #only keep needed columns and rename them
  #lots of repetition of columns once unnested
  select(
    aphia_id = aphia_id_2
    ,trait_value=measurement_value_5
    ,units = measurement_value_14
    ,life_stage = measurement_value_41
    ,body_dimension = measurement_value_32
    ,body_measure_type = measurement_value
    ,quality_status=qualitystatus_8
    ,citation=reference_7
  )  %>% 
  glimpse()
#a couple columns still need work: 
#life_stage: a number of NAs; also one that is "1920"
#body_dimension: one that is "1920"

#structure 2: format records of max adult body length
w_bsize_ft_str2 <- w_bsize_str2 %>% 
  #only keep needed columns and rename them
  #lots of repetition of columns once unnested
  select(
    aphia_id = aphia_id_2
    ,trait_value=measurement_value_5
    ,units = measurement_value_14
    ,life_stage = measurement_value_2
    ,body_dimension = measurement_value
    #,body_measure_type = measurement_value #this column is missing
    ,quality_status=qualitystatus_8
    ,citation=reference_7
  )  %>% 
  #add missing column to match with str1 data
  add_column("body_measure_type" = as.character(NA)) %>% 
  glimpse()

#combine the two data sets
w_bsize_ft <-bind_rows(w_bsize_ft_str1,w_bsize_ft_str2) %>% 
  #format aphid_id and trait_value as numeric
  mutate(aphia_id = as.numeric(aphia_id)
         ,trait_value = as.numeric(trait_value)
         ) %>% 
  arrange(aphia_id) %>% 
  glimpse()

#look at how many distinct taxa have size measurements
w_bsize_taxa <- w_bsize_ft %>% 
  distinct(aphia_id)
#23 out of 64 taxa, so about one third

#if multiple size values for a taxon, choose the largest. If ties for largest, choose most recent citation
#if adult or maximum are NA (or "1920"), look closer at record to decide if reasonable to use measurement
#for transparency, don't delete unused trait records, just add a column indicating which used and a column explaning why a given
#record was used or not used

#add organism code and taxon name to size data

#first, trim the target taxa data set to just the code, taxon, rank
target_tax_truc <- target_tax %>% 
  select(organism_code,aphia_id,taxon,rank) %>% 
  glimpse()

#combine data frames
w_bsize_ft_code <- left_join(w_bsize_ft,target_tax_truc) %>% 
  #move code, taxon, and rake columns toward front
  relocate(organism_code:rank,.after = aphia_id) %>% 
  #add column indicating these data were pulled from WoRMS
  add_column("trait_database" = "worms",.before = "quality_status")

#export as .csv
#write_csv(w_bsize_ft_code, "./benthic/data_output/traits/benthic_traits_worms_size.csv")  

#look at taxa for which size data were not found--------------

w_bsize_ft_miss <- anti_join(target_tax_truc,w_bsize_ft)  %>% 
  arrange(rank,taxon)
#41 taxa as expected  

#for genus level taxa, search all species level taxa within genus for traits
#try worrms function wm_children() for genera
#example
w_tax_gen1 <- wm_children_(name = c('Candona'))
w_tax_gen1_id <- w_tax_gen1 %>%
  pull(AphiaID)
w_traits_gen1 <- wm_attr_data_(id = w_tax_gen1_id) 
#no content :( 
w_tax_gen2 <- wm_children_(name = c('Hyalella'))
w_tax_gen2_id <- w_tax_gen2 %>%
  pull(AphiaID)
w_traits_gen2 <- wm_attr_data_(id = w_tax_gen2_id) 
#no content :(
w_tax_gen3 <- wm_children_(name = c('Physa'))
w_tax_gen3_id <- w_tax_gen3 %>%
  pull(AphiaID)
#no content :(
w_tax_gen4 <- wm_children_(name = c('Sinelobus'))
w_tax_gen4_id <- w_tax_gen4 %>%
  pull(AphiaID)
w_traits_gen4 <- wm_attr_w_traits_gen3 <- wm_attr_data_(id = w_tax_gen3_id)
data_(id = w_tax_gen4_id)
#no content :( 
w_tax_gen5 <- wm_children_(name = c('Uromunna'))
w_tax_gen5_id <- w_tax_gen5 %>%
  pull(AphiaID)
w_traits_gen5 <- wm_attr_data_(id = w_tax_gen5_id)
#no content :( 
#No data for Cryptochironomus, Isocypris, Fluminicola, Procladius
#partial data for Cyprideis, Dorylaimus

#for species level taxa with no match in search, we could look for data for congeners to see what is available
#but might be better to just search literature and other databases before resorting to this 

