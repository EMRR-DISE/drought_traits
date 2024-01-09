# check FMWT index station locations
library(tidyverse)
FMWT_raw <- readRDS("fish traits/fish_data/FMWT_raw.rds")

# samples from each station for each year
sta <-
  FMWT_raw %>% 
  filter(index == 1) %>% 
  select(year, station, station_lat, station_long) %>% 
  group_by(station, station_lat, station_long) %>% 
  mutate(start = min(year), end = max(year)) %>% 
  count(station, station_lat, station_long, start, end) %>% 
  select(-n) %>% 
  ungroup()

write_csv(sta, "fish traits/fish_data/sta.csv")

# SCRATCH -----
tdat <- read_csv("fish traits/temp hold/tdat.csv")

tdat %>% 
  count(station, lat, long)

tdat %>% 
  group_by(station) %>% 
  mutate(n = n()) %>% 
  ungroup()

read_csv("fish traits/fish_data/ftrait.csv")
test <- read_csv("fish traits/fish_data/ftrait.csv")

write_csv(test, "fish traits/fish_data/test.csv")

FMWT_raw %>% 
  filter(index == 1) %>% 
  select(year, station, station_lat, station_long) %>% 
  group_by(station, station_lat, station_long) %>% 
  mutate(start = min(year), end = max(year)) %>% 
  count(station, station_lat, station_long, start, end) %>% 
  ungroup()
