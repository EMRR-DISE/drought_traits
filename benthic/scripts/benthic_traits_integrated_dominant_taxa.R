#Special Studies Drought Synthesis
#Environmental Monitoring Program data
#Benthic Invertebrate Survey data
#Dominant 14 benthic taxa (rather than 64 non-rare taxa)
#Combine trait data from the various sources

#to do list
#probably need to choose which trait value to use for a small subset of traits where there are multiple for a taxon
#check to see how prevalent that is while formatting data set
#are all units standardized within trait group?

#traits included:
#body length
#native/non-native
#armoring
#habit
#dispersal
#salinity tolerance
#thermal tolerance (incomplete)
#trophic habit
#voltinism
#reproduction

# Load required packages -----------------
library(tidyverse) #suite of data science tools
library(readxl) #read excel workbooks

# read in data ---------------

#read in file with body lengths and origin for all 64 non-rare taxa
size_origin_all <- read_csv("./benthic/data_output/traits/benthic_table_q.csv") 

#read in most recent draft of literature trait data
#main file located on the Drought Synthesis SharePoint site
#saved the most recent version of this on the drought_traits GitHub repo
#just grab the first tab where data is; this is the default so don't need to specify
traits_lit <- read_excel("./benthic/data_input/traits_lit/Traits_DataEntry_Benthic_2023-12-04.xlsx") %>% 
  glimpse()

#format literature derived trait data------------------------------

#non-dominant taxa added to end of spreadsheet for size data
#these size data already incorporated into size data frame
#filter them from the lit trait data frame using this list
nondom <- c(1090, 1230, 1270, 1290, 3330, 4030, 4050, 6570)
#4090 is one of missing mass but need to keep that one because one of the dominant ones

traits_lit_dom <- traits_lit %>% 
  #filter out non-dominant taxa
  filter(!(organism_code %in% nondom))


#look at list of unique trait groups
unique(traits_lit_dom$trait_group) #13 trait groups

#which taxa have multiple values for a trait group?
traits_lit_group_sum <- traits_lit_dom %>% 
  group_by(organism_code,trait_group) %>% 
  summarise(n = sum(!is.na(trait_value)),.groups ="drop") %>% 
  arrange(-n)
#four instances of multiple taxon x trait combos
#use trait_value_rank == 1 to reduce to just the best trait value

#still not doing quite what I want
traits_lit_group_complete <-as.data.frame(table(traits_lit_dom$organism_code,traits_lit_dom$trait_group)) %>% 
  group_by(Var2) %>% 
  count()

traits_lit_format <- traits_lit_dom %>% 
  #filter out the trait values that aren't the best one in cases of multiple trait values
  filter(trait_value_rank==1) %>% 
  #just keep the needed columns
  select(
    organism_code
    ,trait_group
    ,trait_value
    ,trait_unit
  ) %>% 
  #drop thermal_tolerance for now because incomplete
  filter(trait_group!="thermal_tolerance")

#format body size and origin data set----------------------
#filter to just the 14 dominant taxa
#probably just use a left_join() to the formatted lit traits data set 




