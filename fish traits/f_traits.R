# Pete Nelson, PhD
# Department of Water Resources
# project: drought traits (special studies)
# purpose: format fish trait data

# created: 2023-05-23
# last modified: 2023-10-17
# Step 4 in fish work flow

library(readxl)
library(tidyverse)
library(janitor)

dat <- read_excel("fish traits/fish_data/Traits_DataEntryFishes_2023-02-28.xlsx", sheet = "data")
glimpse(dat)
names(dat)

ftrait <-
  dat %>% 
  clean_names() %>% 
  select(c(name_abr, origin:fish_vul)) %>% 
  remove_rownames %>% 
  column_to_rownames(var = "name_abr") %>% # convert spp names to rownames
  mutate_at(c("origin", "life_hist", "residency", "habitat",
              "diet", "reproduction"), as.factor) %>% 
  mutate_at(c("fecundity", "life_span", "l_mat", "l_max", "therm_tol"), as.numeric)

rm(dat)

saveRDS(ftrait, file = "fish traits/fish_data/ftrait.rds")
write_csv(ftrait, "fish traits/fish_data/ftrait.csv")

# SCRATCH ###############################
dat <- read_excel("fish traits/fish_data/Traits_DataEntryFishes_2023-02-28.xlsx", sheet = "og data")
glimpse(dat)
names(dat)

ftrait <-
  dat %>% 
  pivot_wider(id_cols = target_taxon_name, 
              names_from = trait_group, 
              values_from = trait_value) %>% 
  clean_names() %>% 
  select(c(target_taxon_name, fecundity:thermal_tolerance)) %>% 
  mutate(species = target_taxon_name,
         fecundity = as.numeric(fecundity),
         habitat = as_factor(habitat),
         life_span = as.integer(life_span),
         lmax = as.numeric(lmax),
         life_hist = as_factor(migratory_pattern),
         origin = as_factor(origin),
         therm_tol = as.numeric(thermal_tolerance),
         .keep = "none") %>% 
  relocate(species, .before = fecundity)
