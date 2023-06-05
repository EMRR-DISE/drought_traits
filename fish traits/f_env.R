# Pete Nelson, PhD
# Department of Water Resources
# project: drought traits (special studies)
# purpose: format drought - environmental data

# created: 2023-05-23
# last modified: 2023-06-05

library(readxl)
library(tidyverse)
library(janitor)

fenv <- read_csv("fish traits/fish_data/drought_variables.csv") %>% 
  mutate(drought_period_type = as_factor(drought_period_type),
         drought_period_number = as_factor(drought_period_number),
         water_year_sac = as_factor(water_year_sac),
         .keep = "unused")

fenv

saveRDS(fenv, file = "fish traits/fish_data/fenv.rds")
write_csv(fenv, "fish traits/fish_data/fenv.csv")
