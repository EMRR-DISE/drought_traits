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
#target_tax <- read_csv("benthic_taxonomy_common5_2023-02-02.csv")

#this file is nested one level into the project so adding the file path info
target_tax <- read_csv("./BenthicInverts/benthic_taxonomy_common5_2023-02-02.csv")


# Example ---------
#first two taxa in our data set

#vector of aphia ids
#b_aphia <- c(992855,1040874,1037336,1037349,157597,137577,1041002,131114,181523,177538,1040661,234850,131191,129884,482186,689413,421139,
#             238740,431367,182361,107414,260458,1264222,397175,885716,177538,	157593,	137556, 992890, 1040676, 1040959, 182695, 1040573, 
#             1040647,1040644,1040874,
#             333585,155021,131167,751827,225694,181580,1299015,431330,431331,158020,158091)

#an easier way to do this which I should have told you about
#this will create a vector of all the IDs in the original file
b_aphia <- target_tax %>% 
  pull(aphia_id)

#No data for: 1040507,608090,181574,156905,158104,148669,118375,247016,415154,127595,397131,717164,181551,582922,1591089,391379,794
#some only have qualitative body size****
#qualitative size might still be useful for us

#run aphia ids through the function that grabs the trait data from worms
w_traits <- wm_attr_data_(id = b_aphia) 

#explore the type of trait data in resulting output dataframe
w_traits_dist <- w_traits %>% 
  #count number of records (ie, rows) for all distinct combos of the measurement types and associated ID numbers
  group_by(measurementTypeID,measurementType) %>% 
  summarize(records = n())
#in addition to max body size, some of the other types of traits in database might be useful

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
  filter(measurement_type=="Type") %>% 
  #unnest again to get body width vs body length category
  #a few records show at this level gender or locality
  unnest(cols = children, names_repair = "universal") %>% 
  #remove rows with incorrect nesting level, some nested too far
  #filter(measurementType...31=="Dimension")%>%
  #unnest again to get columns indicating these are traits for adults
  #some of these show at this level locality and many show NA (probably because not specified as adults)
  unnest_wider(children, names_repair = "universal") %>% 
  #clean up names again
  clean_names() %>% 
  glimpse()

#then format data with lesser amount of nestedness (only three records)
#then combine the two groups of records

#A lot aren't nested to the same level, so removed many along the way

#body size: format records of max adult body length
w_bsize_ft <- w_bsize_str1 %>% 
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
  #just keep max body length
  #removed life_stage=="adult" as filter because removes records I probably want to keep
  filter(body_dimension!="width" & body_measure_type=="maximum") %>% 
  glimpse()
#columns that still need work: life_stage, body_dimension

#export as .csv
#write.csv(w_bsize_ft, "benthic.bodysize.worms.csv", row.names=FALSE)

