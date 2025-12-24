#Drought traits project
#Benthic Inverts
#Format all tables for EDI
#make sure they are complete and work together

#Packages
library(tidyverse)

#Read in data---------

#taxonomy for all taxa present in at least 10% of samples from focal stations
taxonomy <- read_csv("./benthic/data_output/benthic_common10_by_stn_taxonomy_2024-12-30.csv")

#origin
origin <- read_csv("./benthic/data_output/traits/benthic_traits_nemesis_origin_taxa64.csv")

#common 66 taxa traits
trait <- read_csv("./benthic/data_output/edi/benthic_common66_traits_edi_2025-12-24.csv") %>% 
  glimpse()

#non-native intros details
intro <- read_csv("./benthic/data_output/edi/benthic_nonnatives.csv")

#format data sets------------

#reduce origin df to just the needed columns
originft <- origin %>% 
  select(organism_code,native) %>% 
  glimpse()

#add three missing taxa to origin
origin_miss <- as.data.frame(
  cbind(
  organism_code = c(5130, 4740, 6565)
  ,native = c(1,0,1)
  )
) %>% 
  glimpse()

originf <- bind_rows(originft,origin_miss)


#join taxonomy and origin
taxor <- left_join(taxonomy,originf) %>% 
  glimpse()

taxor_final <- taxor %>% 
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
#63 taxa instead of 66 because three taxa are above genus level

# nonnatives <- taxor_final %>% 
#   filter(native == 0)
#25 taxa 

#make sure non-natives match intro df
#intro_taxor <- left_join(intro,taxor_final)
#looks good

# Make sure the trait and taxonomy tables will join--------

#look at structure of dfs to combine
#glimpse(trait)
#glimpse(final)

#combine taxonomy and trait dfs
tt1 <- left_join(trait,taxor_final) %>% 
  glimpse()
#these two tables matched perfectly by organism_code
#will use the trait table version of target_taxon_name and target_taxon_level

taxon_final_formatted <- tt1 %>% 
  select(organism_code:target_taxon_level
         ,source:native
  ) %>% 
  glimpse()

#write_csv(final_formatted,"./benthic/data_output/edi/benthic_common66_taxonomy_origin_edi.csv")

