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

#dominant taxa traits
traitd <- read_csv("./benthic/data_output/traits/benthic_traits_nemesis_origin_taxa64.csv")

#non-rare traits
traitnr <- read_csv("./benthic/data_output/traits/benthic_nonrare_size_edi_2024-05-17.csv") %>% 
  glimpse()


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
         #,target_taxon_name = taxon
         #,target_taxon_level = rank
         ,source
         ,aphia_id
         ,kingdom:native
    ) %>% 
  glimpse()

# Make sure the trait and taxonomy tables will join--------

#combine taxonomy and body size
tt1 <- left_join(traitnr,final) %>% 
  glimpse()
#these two tables matched perfectly by organism_code
#will use the size trait table version of target_taxon_name and target_taxon_level

final_formatted <- tt1 %>% 
  select(organism_code:target_taxon_level
         ,source:native
         ) %>% 
  glimpse()


#write_csv(final_formatted,"./benthic/data_output/edi/benthic_nonrare_taxonomy_origin_edi.csv")







