#Special Studies Drought Synthesis
#Environmental Monitoring Program data
#Benthic Invertebrate Survey data
#Dominant 14 benthic taxa (rather than 64 non-rare taxa)
#Combine trait data from the various sources

#to do list
#salinity is a mix of PSU and PPT; I think these are pretty similar but maybe consider standardizing these if possible
#maybe need to convert categorical traits to integers instead of letters for analysis; not sure
#for final analysis, make sure traits like salinity and thermal tolerance are as comparable as possible

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

#create df with dominant organism codes and names
traits_lit_dom_taxa <- traits_lit_dom %>% 
  distinct(organism_code,target_taxon_name)

#look at list of unique trait groups
unique(traits_lit_dom$trait_group) #13 trait groups

#which taxa have multiple values for a trait group?
traits_lit_group_sum <- traits_lit_dom %>% 
  group_by(organism_code,trait_group) %>% 
  summarise(n = as.numeric(sum(!is.na(trait_value)),.groups ="drop")) %>% 
  arrange(-n) %>% 
  glimpse()
#four instances of multiple taxon x trait combos
#use trait_value_rank == 1 to reduce to just the best trait value

traits_lit_format <- traits_lit_dom %>% 
  #filter out the trait values that aren't the best one in cases of multiple trait values
  #this also drops the missing trait data rows because those are NA instead of 1
  filter(trait_value_rank==1) %>% 
  #just keep the needed columns
  select(
    organism_code
    ,trait_group
    ,trait_value
    ,trait_unit
  ) %>% 
  glimpse()

#which traits have values for all taxa?
traits_lit_group_miss <- traits_lit_format %>% 
  group_by(trait_group) %>% 
  summarise(n = sum(!is.na(trait_value)),.groups = 'drop') %>% 
  arrange(-n)
#complete (n = 14): armoring, dispersal, habit, trophic_habit, voltinism, reproduction; salinity_tolerance_cont
#close: thermal_tolerance (11); drop this one for now
#drop the other trait groups

#create list of trait groups to keep
trait_group_complete <- traits_lit_group_miss %>% 
  filter(n==14) %>% 
  pull(trait_group)

#filter out trait groups with missing data and convert long to wide
traits_lit_group_wide <- traits_lit_format %>% 
  #just keep complete trait groups
  filter(trait_group %in% trait_group_complete) %>% 
  arrange(trait_group,trait_value) %>% 
  #drop unneeded units column
  select(-trait_unit)  %>% 
  pivot_wider(names_from = trait_group, values_from = trait_value)
  

#format body size and origin data set----------------------
#filter to just the 14 dominant taxa
#probably just use a left_join() to the formatted lit traits data set 

traits_dom_all <- left_join(traits_lit_group_wide,size_origin_all) %>%   
  #reorder columns
  select(organism_code
         ,name = target_taxon_name
         ,armoring:voltinism
         ,body_size
         ,native
         ) %>% 
  arrange(organism_code) %>% 
  glimpse()

#write file with trait matrix
#write_csv(traits_dom_all,"./benthic/data_output/traits/benthic_dom_table_q.csv")


