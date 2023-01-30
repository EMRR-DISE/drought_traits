# Pete Nelson, PhD
# Department of Water Resources
# purpose: acquire beach seine data from the DJFMP

# drought/trait study ----
# load beach seine fish data

# Interagency Ecological Program (IEP), R. McKenzie, J. Speegle, A. Nanninga,
# J.R. Cook, J. Hagen, and B. Mahardja. 2022. Interagency Ecological Program: 
# Over four decades of juvenile fish monitoring data from the San Francisco 
# Estuary, collected by the Delta Juvenile Fish Monitoring Program, 1976-2021 
# ver 9. Environmental Data Initiative. 
# https://doi.org/10.6073/pasta/30a3232084be9c936c976fbb6b31c5a2 
# (Accessed 2022-06-07).

# To access the DJFMP data referenced above, use 'edi.244.9.r'
# If only the beach seine data are of interest (ie not using trawl data), run
# lines 596-800 (lines beyond this provide summary details) from 'edi.244.9.5'.
# Lines 885-936 gets you the data on the names of the species captured, and 
# lines 967-1008 generates an object (dt5) with the MethodCode, Location, 
# StationCode, latitude and longitude.

# Object dt3, then, will be the beach seine data, object dt4 provides the 
# species' names, dt5 lists methods and GPS locations where applied.

ls()

# Clear everything except the beach seine data, plus the names & location info.
rm(list= ls()[!(ls() %in% c("dt3", "dt4", "dt5"))])

# Code below assumes you've run edi.244.9.5 and saved the raw data files to
# the fish_data folder.

# Save time by loading those raw data files ("dt3", "dt4", "dt5"):
dt3 <- read.csv("fish_data/dt3.csv") # beach seine data only (other methods excluded)
dt4 <- read.csv("fish_data/dt4.csv") # fish names
dt5 <- read.csv("fish_data/dt5.csv") # locations (beach seine + other methods)

dt3_test <- read.csv("fish_data/dt3_test.csv") # if not there, see code below

# Not always necessary....######

# AFTER running those lines from edi.244.9.5, replace or update the stored
# data files with:
library(janitor)
dt3 %>% clean_names() %>% write_csv("fish_data/dt3.csv")
dt4 %>% clean_names() %>% write_csv("fish_data/dt4.csv")
dt5 %>% clean_names() %>% write_csv("fish_data/dt5.csv")

library(tidyverse)
library(readxl)
library(lubridate)
library(hms)

dt3_test <- tibble(read_excel("fish_data/dt3_meta.xlsx", sheet = "test data")) %>% 
  mutate(across(c(1:3, 7:8, 28:29), factor)) %>% 
  mutate(fl = as.integer(fork_length),
         count = as.integer(count),
         .keep = "unused") %>% 
  mutate(date = as_date(sample_date),
         time = as_hms(sample_time), # convert to time of day (pkg hms)
         .keep = "unused",
         .after = station_code)

write_csv(dt3_test, "fish_data/dt3_test.csv")
