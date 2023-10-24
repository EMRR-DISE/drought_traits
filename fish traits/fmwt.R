# Pete Nelson, PhD
# Department of Water Resources
# project: drought traits (special studies)
# purpose: load _tidied_ Fall Midwater Trawl data

# last modified: 2023-06-05
# Step 2 in fish work flow

# data source and program information:
# https://filelib.wildlife.ca.gov/Public/TownetFallMidwaterTrawl/FMWT%20Data/
# https://iep.ca.gov/Science-Synthesis-Service/Monitoring-Programs/Fall-Midwater-Trawl
# https://www.dfg.ca.gov/delta/projects.asp?ProjectID=FMWT

# data: https://filelib.wildlife.ca.gov/Public/TownetFallMidwaterTrawl/FMWT%20Data/FMWT%201967-2021%20Catch%20Matrix_updated.zip

# downloaded the zipped file, then transfered both files to "~/.../fish traits/fish_data"
# data available wide and long; starting here w wide

# process/flow: load wide format data > filter for stations?; limit to Sep-Dec (other
# months also sampled but reduced time series or sporadically) >
# pivot to long format...

library(tidyverse)
library(lubridate)
library(janitor)
library(readr)

# load fmwt indices ----
FMWT_raw <- readRDS("fish traits/fish_data/FMWT_raw.rds")

# collapse striper data -----
# sums age-0, -1, -2, -3 striped bass indices into a total for "striped bass" 

# temporary df of annual weighted catch ('index') for all year classes of striped bass
temp1 <- 
  FMWT_raw %>% 
  filter(index == "1") %>% # limit to index sites
  group_by(year) %>% 
  filter(species == "Striped Bass age-0" | 
           species == "Striped Bass age-1" | 
           species == "Striped Bass age-2" |
           species == "Striped Bass age-3" |
           species == "Striped Bass age-3+") %>% 
  summarise(species = "Striped Bass", 
            index = sum(catch, na.rm = TRUE)) # note that 'index' becomes summed annual weighted catch!

# temporary df of annual weighted catch ('index') for all fishes except striped bass
temp2 <- 
  FMWT_raw %>% 
  group_by(year, species) %>% 
  summarise(index = sum(catch, na.rm = TRUE)) %>% 
  filter(species != "Striped Bass age-0" & 
           species != "Striped Bass age-1" & 
           species != "Striped Bass age-2" &
           species != "Striped Bass age-3" &
           species != "Striped Bass age-3+")

# annual totals, index sites only, no area subtotals -----
# combines these for striped bass + all other fishes
fmwt1 <-
  bind_rows(temp1, temp2) %>% 
  arrange(year, species)

# spp x year (pivot) ----

fmwt <- 
  fmwt1 %>% 
  pivot_wider(names_from = species, 
              values_from = index, 
              values_fill = 0) %>% 
  clean_names() %>% 
  arrange(year)

write_csv(fmwt, "fish traits/fish_data/fmwt.csv")
saveRDS(fmwt, file = "fish traits/fish_data/fmwt.rds")

rm(fmwt1, temp1, temp2, FMWT_raw)

# STOP HERE ----

# SCRATCH ######################################

### area subtotals ----
temp <- 
  fmwt_by_area_long %>% 
  group_by(year, area) %>% 
  filter(species == "Striped Bass age-0" | species == "Striped Bass age-1" | species == "Striped Bass age-2") %>% 
  summarise(species = "Striped Bass", index_area = sum(index_area)) %>% 
  bind_rows(fmwt_by_area_long %>% 
              filter(species != "Striped Bass age-0" & 
                       species != "Striped Bass age-1" & 
                       species != "Striped Bass age-2")) %>% 
  arrange(year, area, species) %>% 
  ungroup() %>% 
  add_row(year = 1974, area = c(12:17)) %>% 
  add_row(year = 1979, area = c(12:17)) %>% 
  arrange(year)

fmwt2 <- # differs from FMWT_index_wide.csv in having collapsed the striper data...
  temp %>% 
  pivot_wider(names_from = species, values_from = index_area) %>% 
  clean_names() %>% 
  select(-na)

write_csv(fmwt2, "fish_data/fmwt2.csv")

rm(temp, # temp object
   fmwt_annual_long, fmwt_by_area_long, # temp call on saved data
   FMWT_trait_long, FMWT_trait_wide) # raw data
  
# load raw fmwt data ----
# these data are wide
# still includes sites in San Pablo Bay!
data <- read_csv("fish traits/fish_data/FMWT 1967-2021 Catch Matrix_updated.csv") %>% 
  rename(date = SampleDate, station = StationCode, latitude = StationLat,
         longitude = StationLong, time_start = SampleTimeStart, 
         depth = DepthBottom, temp = WaterTemperature, 
         temp_bottom = BottomTemperature) %>% 
  clean_names() %>% 
  filter(index == "1") # retain only index site samples
str(data)

write_csv(data, "fish_data/fmwt_index_sta_complete.csv")

# load index station GIS data
sta <- read_csv("fish traits/fish_data/index_stations.csv")

# presence-absence version
dat_pa <- data %>% 
  mutate(across(.cols = c(29:142), ~ ifelse(.>0,1,0)))

write_csv(dat_pa, "fish_data/dat_pa.csv")

# list index stations, join GIS data
temp <- dat_pa %>% distinct(station = as.character(station))
temp2 <- sta %>% mutate(station = as.character(station))
index_stations <- left_join(temp, temp2, by = "station") %>% relocate(area) %>% arrange(area)
rm(temp, temp2)
write_csv(index_stations, "fish_data/index_stations.csv")

# spp frequency ----

# load fmwt long
# useful for calculating frequency of occurrence 
data_long <- read_csv("fish traits/fish_data/FMWT 1967-2021 Catch Matrix_updated_tidy.csv") %>% 
  rename(date = SampleDate, station = StationCode, latitude = StationLat,
         longitude = StationLong, time_start = SampleTimeStart, 
         depth = DepthBottom, temp = WaterTemperature, 
         temp_bottom = BottomTemperature) %>% 
  clean_names() %>% 
  filter(index == "1") # retain only index site samples
str(data_long)

## % occurrence ----
spp_occ <- data_long %>% 
  filter(index == "1") %>% # limit to index sites
  add_count(wt = catch, name = "grand_N") %>% 
  add_count(trawls_N = n_distinct(paste(date, survey_number, station))) %>% 
  group_by(species, grand_N, trawls_N) %>% 
  summarise(trawls_present = sum(catch > 0), # countif catch>0
            sp_N = sum(catch),
            .groups = "drop") %>% 
  mutate(pct_grand_N = sp_N / grand_N,
         pct_freq = trawls_present / trawls_N)

write_csv(spp_occ %>% arrange(desc(pct_freq)), "fish_data/spp_occ.csv")

spp_occ %>% filter(pct_freq >= 0.05) %>% 
  arrange(desc(pct_freq)) %>% 
  select(!grand_N)

five_plus <- spp_occ %>% filter(pct_freq >= 0.04) %>% 
  arrange(desc(pct_freq)) %>% 
  select(!grand_N) %>% 
  top_n(10)

write_csv(five_plus, "fish_data/five_plus.csv")

# clean-up -----
rm(list = setdiff(ls(),
                c("spp_occ", # occurrence data on complete FMWT spp list
                  "five_plus", # most common FMWT fish spp
                  "fmwt1", "fmwt2", # FMWT indices for spp of interest, striped bass year classes collapsed to one
                  "index_stations", # lat lon data for FMWT index station
                   "dat_pa", # presence/absence data
                  "sta" # station data
                ))) # removes all objects except those listed



fmwt_index_sta_complete <- read_csv("fish traits/fish_data/fmwt_index_sta_complete.csv")
View(fmwt_index_sta_complete)
fmwt_index_sta_complete %>% 
  count(year, station)

# SCRATCH PAPER ######################################
## cpue
# essentially what the indices are...available at the station level?
# do we need that level of detail?

## sta-yr matrix
# group by station-year for selected spp
# what about controlling for effort? should prob calc cpue

# code to calc percent presence
# fake data
set.seed(1)
df <- tibble(sampleID = c(rep("trawl1",4),rep("trawl2",4),
                          rep("trawl3",4), rep("trawl4",4)),
                 Species = rep(c("amsh","desm","stb0","chsa"),4),
                 Catch = sample(0:4, size = 16, replace = TRUE))
df %>% 
  add_count(wt = Catch, name = "grand_N") %>% 
  add_count(trawls_N = n_distinct(sampleID)) %>% 
  group_by(Species, grand_N, trawls_N) %>% 
  summarise(trawls_present = sum(Catch > 0), # countif catch>0
            sp_N = sum(Catch),
            .groups = "drop") %>% 
  mutate(pct_grand_N = sp_N / grand_N,
         pct_freq = trawls_present / trawls_N)

# real data (subset)
temp <- data_long[1:6000,] %>% filter(index == "1") %>% 
  add_count(wt = Catch, name = "grand_N") %>% 
  add_count(trawls_N = n_distinct(paste(SampleDate, SurveyNumber, StationCode))) %>% 
  group_by(Species, grand_N, trawls_N) %>% 
  summarise(trawls_present = sum(Catch > 0), # countif catch>0
            sp_N = sum(Catch),
            .groups = "drop") %>% 
  mutate(pct_grand_N = sp_N / grand_N,
         pct_freq = trawls_present / trawls_N)

temp2 <- data_long[1:6000,] %>% filter(index == "1") %>% 
  group_by(Species, trawls = paste(SampleDate, SurveyNumber, StationCode)) %>% 
  summarise(present = sum(Catch > 0)) %>% 
  filter(Species == "American Shad")
sum(temp2$present)/46 # yep, check out
