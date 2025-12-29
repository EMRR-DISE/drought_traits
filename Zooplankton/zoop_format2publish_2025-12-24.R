#Drought traits project
#zooplankton

#Packages
library(tidyverse)

#Read in data---------

#taxonomy for all taxa present in at least 5% of samples from focal stations
taxonomy <- read_csv("./Zooplankton/zoop_taxonomy_2025-12-24.csv") %>% 
  glimpse()

#traits
trait <- read_csv("./Zooplankton/zoop trait table final.csv") %>% 
  glimpse()

#make sure dfs combine properly
combo <- left_join(trait,taxonomy)
#match perfectly