# Pete Nelson, PhD
# Department of Water Resources
# purpose: acquire trawl data from the Delta Juvenile Fish Monitoring Program

# drought/trait study ----
# load midwater trawl fish data

# Interagency Ecological Program (IEP), R. McKenzie, J. Speegle, A. Nanninga,
# J.R. Cook, J. Hagen, and B. Mahardja. 2022. Interagency Ecological Program: 
# Over four decades of juvenile fish monitoring data from the San Francisco 
# Estuary, collected by the Delta Juvenile Fish Monitoring Program, 1976-2021 
# ver 9. Environmental Data Initiative. 
# https://doi.org/10.6073/pasta/30a3232084be9c936c976fbb6b31c5a2 
# (Accessed 2022-06-07).

# To access the DJFMP data referenced above, use 'edi.244.9.r'
# If only the fall midwater trawl (trawl) data are of interest (ie not using 
# seine data), run lines 18-304 and lines 307-593 from 'edi.244.9.5'.
# Lines 885-936 gets you the data on the names of the species captured (dt4), and 
# lines 967-1008 generates an object (dt5) with the MethodCode, Location, 
# StationCode, latitude and longitude.

# Objects dt1 & dt2, then, will be the trawl data, object dt4 provides the 
# species' names, and dt5 lists methods and GPS locations where applied. dt3 is
# the beach seine data.

setwd("C:/Users/pnelson/OneDrive - California Department of Water Resources/3-Projects/05-drought monitoring/drought traits project/drought traits analyses/fish traits")

# prep ----
ls()

# Clear everything except the trawl data, plus the names & location info.
rm(list= ls()[!(ls() %in% c("dt1", "dt2", "dt3", "dt4", "dt5", "trawls"))])

library(tidyverse)
library(readxl)
library(janitor) # clean variable names
library(tsibble) # yearweek()
library(dataRetrieval) # addWaterYear()
library(lubridate) # yday()

# fun() day of water year ####
# used to determine the day of the water year (numeric)
wy_day_new = function(x, start.month = 10L){
  start.yr = year(x) - (month(x) < start.month)
  start.date = make_date(start.yr, start.month, 1L)
  as.integer(x - start.date + 1L)
}

# If you've already run the trawl (and meta data) lines from edi.244.9, the raw
# data files should already be in the working directory and can be more quickly
# loaded here:

# read data ----
dt1 <- read_csv("fish_data/dt1.csv") # trawl data, 1976-2001; ignore parsing issues
dt2 <- read_csv("fish_data/dt2.csv") # trawl data, 2002-2021; ignore parsing issues
dt3 <- read_csv("fish_data/dt3.csv") # beach seine data, 1976-2021; ignore parsing issues
dt4 <- read_csv("fish_data/dt4.csv") # organism names
dt5 <- read_csv("fish_data/dt5.csv") # location & station latitude & longitude

# combine files ----
## combine trawl data files (1976-2021) and select variables #####
trawls <- bind_rows(dt1, dt2) %>%
  select(Location, RegionCode, StationCode, SampleDate, 
         MethodCode, GearConditionCode,
         WaterTemp, Turbidity, Secchi, SpecificConductance,
         TowNumber, TowDuration, FlowDebris,
         FlowmeterStart, FlowmeterEnd, FlowmeterDifference, Volume, 
         IEPFishCode, CommonName, MarkCode, StageCode,
         ForkLength, RaceByLength, TagCode, RaceByTag, Count) %>% 
  clean_names()

write_csv(trawls, "fish_data/trawls.csv")

## length of time series ----_
history_mwtr_sta <- trawls %>% 
  filter(method_code == "MWTR") %>% 
  group_by(region_code, location, station_code) %>% 
  summarise(year_start = min(year(sample_date)), 
            year_end = max(year(sample_date)),
            n = n()) # crude measure of effort; not really "sample size"
print(history_mwtr_sta, n = Inf)

## limit location x station ---- 
# to those w n>10000 and add lat/lon for ea station
select_locations <- history_mwtr_sta %>% 
  filter(n > 10000) %>% 
  left_join(dt5) %>% 
  select(location, station_code, latitude, longitude, 
         year_start, year_end, method_code, n)

write_csv(select_locations, "fish_data/select_locations.csv")

rm(dt1, dt2) # BIG objects no longer needed

## limit trawl data ----
# to those from the selected locations & gear = MWTR
trawls_select <- trawls %>% 
  filter(method_code == "MWTR",         # eliminates Kodiak trawl data
         station_code == "SR043M" |     # Clarksburg
           station_code == "SR055M" |   # Sherwood Harbor
           station_code == "SB018M" |   # Chipps Island
           station_code == "SB018N" |    # Chipps Island
           station_code == "SB018S" |    # Chipps Island
           station_code == "SB018X")    # Chipps Island

write_csv(trawls_select, "fish_data/trawls_select.csv")

#####################
### SCRATCH ###
#####################

# calc cpue ----
# for selected species: Chinook (lumping runs), Delta smelt, white sturgeon (too
# little data on green), tule perch, striped bass
# consider adding Mississippi silverside and splittail
# consider method too: KDTR and MWTR
# and location(s)!

# Oncorhynchus tshawytscha = CHISAL
# Hypomesus transpacificus = DELSME
# Acipenser transmontanus = WHISTU
# Hysterocarpus traskii = TULPER
# Morone saxatilis = STRBAS
# Menidia audens = MISSIL; note that unidentified silversides are coded UNIATH
# Pogonichthys macrolepidotus = SPLITT

## Chinook ----
# trawls includes 1,387,547 records (ie rows)
# trawlsSPLITT N = 486,972
trawlsSPLITT <- trawls %>% 
  filter(method_code == "MWTR" & 
           location == "Chipps Island" & 
           volume != "NA" & # 914560 w NAs removed
           volume < "50000") %>% # 905976 w volume >= 50000 removed
  # remove records of CHN >= 250 mm FL
  filter(!(iep_fish_code == "SPLITT" & fork_length >= 250),
         !(gear_condition_code == "9")) %>% 
  # limit tow_duration to between 15 & 22 minutes
  filter(between(tow_duration, 15, 22)) %>% 
  pivot_wider(id_cols = c(location, station_code, sample_date, tow_number, volume,
                          gear_condition_code, water_temp, turbidity),
              names_from = iep_fish_code,
              values_from = count,
              values_fn = sum,
              values_fill = 0) %>% 
  mutate(year_wk = yearweek(sample_date),
         CPUE = SPLITT/volume) %>% 
  select(-c(RAITRO:YELBUL)) %>% 
  arrange(sample_date, tow_number)
# notes on SPLITT: 
# consider combining captures w both MWTR and KDTR? choose one? on what basis?
# what locations to include/exclude?
summary(trawls %>% filter(method_code == "KDTR" & iep_fish_code == "SPLITT"))
# KDTR: max count = 1932, mean = 2.28, N = 88,568; locations: Mossdale & 
          # Sherwood Harbor; regions 2:38,413 and 5:50,155; 1994-2021;
          # if used, drop gear_condition_code=9; some extreme volumes
summary(trawls %>% filter(method_code == "MWTR" & iep_fish_code == "SPLITT"))
# MWTR: max count = 2777, mean = 1.894, N = 396,831; locations: various but most 
          # from Chipps Island, some at Sherwood, none at Mossdale; regions
          # 2:107,252 and 3:289,509; 1976-2021; some extreme volumes

# MWTR annual summary data
write_csv(trawlsSPLITT %>% 
  group_by(year = year(sample_date)) %>% 
  summarise(total_CPUE = sum(CPUE), 
            max_CPUE = max(CPUE), 
            mean_CPUE = mean(CPUE),
            sd_CPUE = sd(CPUE)),
  "fish_data/MWTR_SPLITT.csv")


## striped bass ----
# Morone saxatilis = STRBAS
# trawlsSTRBAS N = 249,531
trawlsSTRBAS <- trawls %>% 
  filter(method_code == "MWTR" & 
           location == "Chipps Island" & 
           volume != "NA" & 
           volume < "50000") %>% 
  # may only be important using KDTR
  filter(!(gear_condition_code == "9")) %>% 
  # limit tow_duration to between 15 & 22 minutes
  filter(between(tow_duration, 15, 22)) %>% 
  pivot_wider(id_cols = c(location, station_code, sample_date, tow_number, volume,
                          gear_condition_code, water_temp, turbidity),
              names_from = iep_fish_code,
              values_from = count,
              values_fn = sum,
              values_fill = 0) %>% 
  mutate(year_wk = yearweek(sample_date),
         CPUE = STRBAS/volume) %>% 
  select(-c(SPLITT:SPLITT, THRSHA:YELBUL)) %>% 
  arrange(sample_date, tow_number)
# notes on STRBAS: 
# consider combining captures w both MWTR and KDTR? choose one? on what basis?
# MWTR catches larger fish...
# what locations to include/exclude?
summary(trawls %>% filter(method_code == "KDTR" & iep_fish_code == "STRBAS"))
# KDTR: max count = 260, mean = 1.893, N = 7988; locations: Mossdale (few at 
# Sherwood Harbor); regions 2:19 and 5:7,969; 1994-2021;
# if used, drop gear_condition_code=9; some extreme volumes (n=185); 
# FL Q1=30, m=55.7, Q3=53, max=736
summary(trawls %>% filter(method_code == "MWTR" & iep_fish_code == "STRBAS"))
# MWTR: max count = 4000 (rounded), mean = 3.193, N = 81,454; locations: various 
# but most from Chipps Island, some at Clarksburg; regions
# 2:1,738 and 3:79,445 and a handful at 6:271; 1976-2021; 
# FL Q1=80, m=151.3, Q3=187.0, max=1290

# MWTR annual summary data
write_csv(trawlsSTRBAS %>% 
            group_by(year = year(sample_date)) %>% 
            summarise(total_CPUE = sum(CPUE), 
                      max_CPUE = max(CPUE), 
                      mean_CPUE = mean(CPUE),
                      sd_CPUE = sd(CPUE)),
          "fish_data/MWTR_STRBAS.csv")

## splittail ----
# Pogonichthys macrolepidotus = SPLITT
# N = 37,324
trawlsSPLITT <- trawls %>% 
  filter(method_code == "MWTR" & 
           location == "Chipps Island" & 
           volume != "NA" & # 914560 w NAs removed
           volume < "50000") %>% 
  # limit tow_duration to between 15 & 22 minutes
  filter(between(tow_duration, 15, 22)) %>% 
  pivot_wider(id_cols = c(location, station_code, sample_date, tow_number, volume,
                          gear_condition_code, water_temp, turbidity),
              names_from = iep_fish_code,
              values_from = count,
              values_fn = sum,
              values_fill = 0) %>% 
  mutate(year_wk = yearweek(sample_date),
         CPUE = SPLITT/volume) %>% 
  select(-c(SPLITT:AMESHA, STRBAS:YELBUL)) %>% 
  arrange(sample_date, tow_number)
# notes on SPLITT: 
# is this right gear to use for this species? beach seine?
# consider combining captures w both MWTR and KDTR? choose one? on what basis?
# what locations to include/exclude?
summary(trawls %>% filter(method_code == "KDTR" & iep_fish_code == "SPLITT"))
# KDTR: max count = 260, mean = 1.893, N = 33,491; 
# locations: Mossdale; regions 5:33,445;
# some volumes to check; 
# FL Q1=35, m=42.99, Q3=50, max=375
summary(trawls %>% filter(method_code == "MWTR" & iep_fish_code == "SPLITT"))
# MWTR: max count = 554, mean = 1.51, N = 25,599; locations: various 
# but most from Chipps Island; 
# regions 3: 24,760, 2:788; 1976-2021; 
# FL Q1=56, m=138.4, Q3=213.0, max=480

# MWTR annual summary data
write_csv(trawlsSPLITT %>% 
            group_by(year = year(sample_date)) %>% 
            summarise(total_CPUE = sum(CPUE), 
                      max_CPUE = max(CPUE), 
                      mean_CPUE = mean(CPUE),
                      sd_CPUE = sd(CPUE)),
          "fish_data/MWTR_SPLITT.csv")

## tule perch ----
# Hysterocarpus traskii = TULPER
trawlsTULPER <- trawls %>% 
  filter(method_code == "MWTR" & 
           location == "Chipps Island" & 
           volume != "NA" & # 914560 w NAs removed
           volume < "50000") %>% 
  # limit tow_duration to between 15 & 22 minutes
  filter(between(tow_duration, 15, 22)) %>% 
  pivot_wider(id_cols = c(location, station_code, sample_date, tow_number, volume,
                          gear_condition_code, water_temp, turbidity),
              names_from = iep_fish_code,
              values_from = count,
              values_fn = sum,
              values_fill = 0) %>% 
  mutate(year_wk = yearweek(sample_date),
         CPUE = TULPER/volume) %>% 
  select(-c(SPLITT:BLACRA, STAFLO:YELBUL)) %>% 
  arrange(sample_date, tow_number)
# notes on TULPER: 
# is this right gear to use for this species? beach seine?
sum(trawlsTULPER$TULPER)
# N = 785; focus on beach seine data instead!

## silversides ----
# DON'T BOTHER...use beach seine data
# what locations to include/exclude?
summary(trawls %>% filter(method_code == "KDTR" & iep_fish_code == "MISSIL"))
# KDTR: max count = 260, mean = 1.893, N = 7988; locations: Mossdale & 
# Sherwood Harbor; regions 2:5,122 and 5:50,710; 1994-2021;
# if used, drop gear_condition_code=9; some extreme volumes; 
# FL Q1=41, m=53.6, Q3=67, max=186
summary(trawls %>% filter(method_code == "MWTR" & iep_fish_code == "MISSIL"))
# MWTR: max count = 6, mean = 1.12, N = 292; locations: various 
# FL Q1=1, m=1.1, Q3=1, max=6
