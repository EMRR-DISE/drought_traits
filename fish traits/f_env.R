# Pete Nelson, PhD
# Department of Water Resources
# project: drought traits (special studies)
# purpose: format drought - environmental data

# created: 2023-05-23
# last modified: 2023-06-05
# Step 3 in fish work flow

library(readxl)
library(tidyverse)
library(forcats)
library(janitor)

temp <- read_csv("fish traits/fish_data/drought_variables.csv") %>% 
  mutate(drought_period_type = as_factor(drought_period_type),
         drought_period_number = as_factor(drought_period_number),
         drought_year = as.integer(drought_year),
         drought_year_trunc = factor(drought_year_trunc, 
                                     levels = c("0", "1", "2", "3+"), 
                                     ordered = T),
         water_year_sac = factor(water_year_sac, 
                                 levels = c("Wet", "Above Normal", "Below Normal", "Dry", "Critical"), 
                                 ordered = T),
         .keep = "unused") %>% 
  filter(year >= "1975" & year != "1979" & year != "2022") %>% 
  select(!c(drought_period_type, drought_period_number)) %>% 
  column_to_rownames(var = "year") 

temp[46,7] <- mean(temp$Phos, na.rm = T) # replace NA w mean Phos for 2021
  
fenv <- temp %>% print()

rm(temp)

saveRDS(fenv, file = "fish traits/fish_data/fenv.rds")
write_csv(fenv, "fish traits/fish_data/fenv.csv")
