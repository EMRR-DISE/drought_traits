# Pete Nelson, PhD
# Department of Water Resources
# project: drought traits (special studies)
# purpose: select fish indices (abundance data) from Fall Midwater Trawl data file

# created: 2024-02-02
# last modified: 2024-02-02

# getting started -----
library(tidyverse)

# fish occurrence data with years prior to 1975 + 1979 removed bc environmental data incomplete for these
fish <- read_csv("fish traits/fish_data/fmwt.csv") %>% # derived from fmwt.R, collapsed striper data
  filter(year >= "1975" & year != "1979" & year != "2022") %>% # no (env) data these years
  column_to_rownames(var = "year") %>% 
  select(american_shad, pacific_herring, threadfin_shad, northern_anchovy, delta_smelt,
         striped_bass, chinook_salmon, splittail, longfin_smelt, yellowfin_goby,
         white_sturgeon, white_catfish, topsmelt, jacksmelt, shiner_perch, white_croaker,
         channel_catfish, starry_flounder, plainfin_midshipman) %>% 
  print()

saveRDS(fish, file = "fish traits/fish_data/fish.rds")
write_csv(fish, "fish traits/fish_data/fish.csv")
