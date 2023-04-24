#Special Studies Drought Synthesis
#Environmental Monitoring Program data
#Benthic Invertebrate Survey data
#Combine trait data from the various sources
#for now, just focus on body size

#included sources
#USGS
#NEMESIS
#WoRMS
#CDFW zoop lab (eventually)

# Load required packages -----------------
library(tidyverse) #suite of data science tools
library(janitor) #misc functions for cleaning data

# read in data ---------------

#read in all files from traits subfolder
usgs <- read_csv("./benthic/data_output/traits/benthic_traits_usgs_size.csv") 
worms <- read_csv("./benthic/data_output/traits/benthic_traits_worms_size.csv") 
nemesis <- read_csv("./benthic/data_output/traits/benthic_traits_nemesis_size&physiology.csv") 
iep <- read_csv("./benthic/data_output/traits/benthic_traits_iep_size.csv") 


#also read in full list of taxa
emp <- read_csv("./benthic/data_output/benthic_common5_taxonomy_2023-03-27.csv")

#Combining the data sets----
#some columns will be unique to particular data sets but bind_rows() can handle that

#might want to reorder these columns a bit
traits_comb <- bind_rows(usgs,worms,nemesis,iep) %>% 
  glimpse()

#figure out which taxa have no body size data from any database--------

#first filter to just the size traits (most but not all of the trait data)
#this will keep all kinds of matches (target taxon, synonyms, congeners, congener synonyms, confamilials)
traits_size <- traits_comb %>% 
  filter(trait_group =="body_size_max")

#are all sizes in mm? 
unique(traits_size$trait_unit)
#yes

#look at distribution of size match types
hist(traits_size$lit_taxon_type_ord)
#most are either target taxon matches (1) or confamilial matches (5)

#use anti_join to see what's missing
size_miss <- anti_join(emp,traits_size,by="organism_code")
#9 out of 64 taxa with no size data

#summarize taxonomic level of missing taxa
size_miss_sum <- size_miss %>% 
  group_by(rank) %>% 
  count()
#of 9 missing taxa, only 1 is species
#5 are genera, others are higher level

#figure out which taxa have no target taxon specific body size data from any database--------

#first filter to just the species specific size traits 
traits_size_spec <- traits_comb %>% 
  #only keep matches for size that are for the target taxon
  filter(trait_group =="body_size_max" & lit_taxon_type_ord==1) %>% 
  arrange(target_taxon_name)
#77 values for size that are specifically for the target taxon 
#there are obviously multiple matches for some taxa
#because we are pulling info from multiple databases some of those multiple records per taxon
#could even be repeats of the same data from the same original source

#use anti_join again to see how many missing taxa now (should be more taxa)
size_miss_spec <- anti_join(emp,traits_size_spec,by="organism_code")
#19 taxa with no size matches

#again summarize taxonomic level of missing taxa
size_miss_spec_sum <- size_miss_spec %>% 
  group_by(rank) %>% 
  count()
#of 19 missing taxa, 8 are species
#8 are genera, others are higher level

#do this again but this time include target taxa and synonyms of target taxa---------

traits_size_semispec <- traits_comb %>% 
  #only keep matches for size that are for the target taxon and target taxa synonyms
  filter(trait_group =="body_size_max" & (lit_taxon_type_ord==1 | lit_taxon_type_ord==2)) %>% 
  arrange(target_taxon_name)
#78 values for size that are specifically for the target taxon 

#use anti_join again to see how many missing taxa now 
size_miss_semispec <- anti_join(emp,traits_size_semispec,by="organism_code")
#18 taxa with no size matches

#again summarize taxonomic level of missing taxa
size_miss_semispec_sum <- size_miss_semispec %>% 
  group_by(rank) %>% 
  count()
#missing are 7 species, 8 genera, and 3 higher level

#format the size data--------
#mostly just need to pick the record for each taxon that is best
#start by keeping only the most specifically matching size data
#eg, an exact taxonomic match is better than, say, a congener match

#filter to keep on the size data that best matches the target taxon
#ie, best outcome is size is for the target taxon (lit_taxon_type_ord = 1)
size_spec <- traits_size %>% 
  group_by(organism_code) %>% 
  slice_min(lit_taxon_type_ord) %>% 
  arrange(organism_code)
#common for a taxon to have multiple records still
#eg, WoRMS sometimes has values from multiple sources

#filter size data set further by only keeping largest size measurement for each taxon
#NOTE: might want to use a more nuanced approach for final version of data set 
size_spec_max <- size_spec %>% 
  group_by(organism_code) %>% 
  slice_max(trait_value) %>% 
   arrange(organism_code)
#there are still some cases of multiple records per species because multiple sources provide the same max size value

#filter size data even further to only keep the first row for each taxon
#NOTE: might want to use a more nuanced approach for final version of data set 
size_spec_max_distinct <- size_spec_max %>% 
  group_by(organism_code) %>% 
  slice(1) %>% 
  arrange(organism_code)

#quick look at body size distribution

#plot histogram of sizes
range(size_spec_max_distinct$trait_value) #1.5 270.0
hist(size_spec_max_distinct$trait_value)
#most 50 mm or smaller

#add the missing taxa back to df

#first format the emp df
emp_trunc <- emp %>% 
  select(organism_code
         ,target_taxon_name = taxon)

#now combine the two dfs
size_all <- left_join(emp_trunc,size_spec_max_distinct) %>% 
  #sort so taxa with missing data are at bottom
  arrange(trait_value)

#write integrated and filtered size data set------
#write_csv(size_all,"./benthic/data_output/traits/benthic_traits_integrated_size.csv")






