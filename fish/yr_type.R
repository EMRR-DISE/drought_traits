# generate list of year types by water year

library(waterYearType)

yr_type <- water_year_indices %>% 
  filter(WY >= 1967 & location == "Sacramento Valley") %>% # exclude San Joaquin Vly
  add_row(WY = 2018:2021, 
          Yr_type = c("Below Normal", "Wet", "Dry", "Critical")) %>% 
  # add missing data
  mutate(yr_type = as.factor(Yr_type)) %>% 
  select(WY, yr_type)
