# Pete Nelson, PhD
# Department of Water Resources
# project: drought traits (special studies)
# purpose: format drought - environmental data

# created: 2023-05-23
# last modified: 2023-06-05
# Step 3 in fish work flow

library(readxl)
library(tidyverse)
library(janitor)

fenv <- read_csv("fish traits/fish_data/drought_variables.csv") %>% 
  mutate(drought_period_type = as_factor(drought_period_type),
         drought_period_number = as_factor(drought_period_number),
         water_year_sac = as_factor(water_year_sac),
         .keep = "unused")

# replace Phos 2021 datum (NA) with mean of all previous years
fenv[55,10] <- mean(fenv$Phos, na.rm = T) # replace NA w mean Phos for 2021

print(fenv, n=Inf)

saveRDS(fenv, file = "fish traits/fish_data/fenv.rds")
write_csv(fenv, "fish traits/fish_data/fenv.csv")
