#Drought traits project
#Benthic invertebrates
#distribution of traits through time


#packages----
#install.packages("tidyverse")
library(tidyverse)

#read in data----

#abundances for the common 10 stn taxa by station and month (n = 66)
abund10stn <- read_csv("./benthic/data_output/benthic_common10_abundances_by_station.csv") %>% 
  #drop the three taxa that are not IDed below family level
  filter(!(organism_code %in% high_taxa) & !(organism_code %in% new_taxa))

#trait data for the common 10 stn taxa
#mostly just origin and size data but also more trait data for the 14 dominant taxa
traits <- read_csv("./benthic/data_output/benthic_common10_by_stn_trait_data_partially_filled.csv") %>% 
  glimpse()

#Stacked bar plot - planktonic larvae----
#stacked bar plot - trophic habit----

#by station----

#community composition by station----
#community compostion by station, pre and post clam invasion----