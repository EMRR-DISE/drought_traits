#Drought traits project
#Benthic Inverts
#Format taxonomy table for EDI

#To do
#drop some high level taxa that we didn't try to collect traits for
#drop some unneeded columns
#add native/non-native as a column 

#Packages
library(tidyverse)

#Read in data---------

#taxonomy
taxonomy <- read_csv("./benthic/data_output/benthic_common5_taxonomy_2023-03-27.csv")

#origin
origin <- read_csv("./benthic/data_output/traits/benthic_traits_nemesis_origin_taxa64.csv")

#reduce origin df to just the needed columns
originf <- origin %>% 
  select(organism_code,native)

#combine data frames----------

taxor <- left_join(taxonomy,originf) %>% 
  glimpse()

#format dataframe for publishing-------------

final <- taxor %>% 
  #drop the taxa that aren't genus or species
  filter(rank == "Species"| rank=="Genus") %>% 
  select(organism_code
         ,taxon
         ,aphia_id
         ,source
         ,rank:native
    )

#write_csv(final,"./benthic/data_output/benthic_nonrare_taxonomy_origin_EDI.csv")







