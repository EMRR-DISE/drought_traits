#script to calculate Fall Midwater Trawl indices for species of interest
#Originally created March 2020 by Rosemary Hartman at CA DWR
#Modified by James White March 27, 2020 for external use
#updated June 2022
#_________________________________________________________________________________

# Further modified by Pete Nelson 20 June 2023 
# 1976 indices for added species need attention; these are
# chinook, northern anchovy, pac herring, striped bass age-1 & 2

# prep ----
#load necessary libraries
# Package names
packages <- c("tidyverse", "here", "data.table", "janitor")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
# Packages loading
lapply(packages, library, character.only = TRUE)

# special studies area ----
# index calculation, using only the stations w/in our study area (ie excluding San Pablo Bay)

#station lookup table to help calculate the index
stas <- data.frame("Station" = as.character(c(337:340, 401, 403:418, 501:505, 507:513, 515:519, 601:606, 608, 701, 703:711, 802, 804, 806:815, 902:906, 908:915)),
                   "Area" = c(rep(1,3), 10, rep(11,7), rep(12,10), rep(13,17), rep(14,7), rep(15,10), rep(16,12), rep(17,13)))

#water volumes used to calculate index
wwm <- data.frame("Area" = as.integer(c(1, 10:17)), 
                  "Weight" = c(8.1, 4.8, 16, 14, 18, 5, 12, 14, 20)) #acre-ft x 10^4

#update current year
cy <- as.character(as.numeric(format(Sys.Date(), '%Y'))-1)

#use up to date year for naming convention for URL
fnurl <- paste0("https://filelib.wildlife.ca.gov/Public/TownetFallMidwaterTrawl/FMWT%20Data/FMWT%201967-", cy,"%20Catch%20Matrix_updated.zip")

#use up to date year for naming convention for file wide format
fn <- paste0("FMWT 1967-",cy," Catch Matrix_updated_tidy.csv")

## load raw data ----
#use this bit of code to download the web version
#new file uploaded every January, check file name is correct
#Now download the data. It's a .csv file in a zipped folder
#so first we have to download the zip folder and extract the file
temp <- tempfile()
download.file(fnurl, temp)
FMWT_raw <- fread(unzip(temp, files = fn)) %>%
  rename(Date = SampleDate, 
         # Survey or SurveyNumber: number ascribed to each month of the survey 
         # starting in July (1) and ending in June (12); index surveys are from
         # Sept-Dec, surveys 3-6 respectively
         Survey = SurveyNumber, 
         Station = StationCode, 
         Index = index)
unlink(temp)

# clear work space of temp/intermed objects: retains raw data only
rm(list= ls()[!(ls() %in% c("FMWT_raw"))]) 

## spp selection ----
# SCRATCH -----
# % occurrence
spp_occ <- FMWT_raw %>% 
  filter(Index == "1") %>% # limit to index sites
  add_count(wt = Catch, name = "grand_N") %>% # inserts total number of organisms recorded from all trawls (why is it weighted?)
  add_count(trawls_N = n_distinct(paste(Date, Survey, Station))) %>% # number of trawls completed at each unique combination of date * survey * station
  group_by(Species, grand_N, trawls_N) %>% 
  summarise(trawls_present = sum(Catch > 0), # count if catch>0
            sp_N = sum(Catch),
            .groups = "drop") %>% 
  mutate(pct_grand_N = sp_N / grand_N,
         pct_freq = trawls_present / trawls_N)
# SCRATCH -----
#
dataset <-
  tibble(
    species = c("acropsis", "gonulux", "equipsis", "horribulus"),
    abundance = c(12, 2, 49, 1)
  )
temp <-
  as_tibble(
    FMWT_raw[1:500,c(1:5,28:30)] %>%
      filter(Index == "1") %>% # limit to index sites
      add_count(wt = Catch, name = "grand_N")
  )


FMWT_sel <- FMWT_raw %>%
  #subset just fish (>5% freq by # + Chinook) and variables we care about
  select(Year, Date, Survey, Station, Index, Species, Catch) %>%
  filter(Species %in% c("American Shad", "Striped Bass age-0", "Longfin Smelt", 
                        "Northern Anchovy", "Threadfin Shad", "Delta Smelt", 
                        "Striped Bass age-1", "Pacific Herring", "Striped Bass age-2", 
                        "Chinook Salmon","Splittail"))

#make variables correct format for joining
FMWT_sel$Station <- as.character(FMWT_sel$Station)

#add areas and weights
FMWTl <- left_join(FMWT_sel, stas, by = "Station") %>%
  left_join(., wwm, by = "Area")

###areas used in index calculation
ia <- as.integer(c(1, 10:17))

##index calculation ----
# includes only index surveys (subset of the stations)
FMWTli <- filter(FMWTl, Area %in% ia, Survey %in% c(3,4,5,6), Index == 1) %>%
  group_by(Year, Survey, Area, Weight, Species) %>%
  summarize(mcatch = mean(Catch))%>%
  mutate(wcatch = mcatch*Weight) %>%
  ungroup()

### trait study ----

# index calc with station (Pete Nelson)
# includes only index surveys (subset of the stations)
# I'm not convinced this is the best way to look at spatial variation for
# the drought/trait special studies project...see alternative approach below
FMWTlis <- filter(FMWTl, Area %in% ia, Survey %in% c(3,4,5,6), Index == 1) %>%
  group_by(Year, Survey, Area, Station, Weight, Species) %>%
  summarize(mcatch = mean(Catch))%>%
  # weighting factor determined by area of each location; get feedback on this
  mutate(wcatch = mcatch*Weight) %>% 
  ungroup()

# index calc for trait study; same as index calculation above for normal FMWT
# indices (FMWTli) but limited to Area: 12-14 = Suisun Bay, 15 = Lower 
# Sacramento River, 16 = Lower San Joaquin River, 17 = Eastern Delta; in other
# words, excluding those areas NOT included in our study

### fmwt for trait study ----
# annual indices by area
FMWT_trait_long <- FMWTli %>% 
  filter(Area >= 12) %>% 
  group_by(Year, Area, Species) %>% 
  summarise(index_area = round(sum(wcatch))) %>% 
  clean_names()

# save data as .Rdata file
save(FMWT_trait_long, file = "fish traits/fish_data/FMWT_trait_long.Rdata")

# pivot data to 'wide'
FMWT_trait_wide <- FMWT_trait_long %>% 
  pivot_wider(names_from = species,
              values_from = index_area) %>% 
  clean_names()

# save data as .Rdata file
save(FMWT_trait_wide, file = "fish traits/fish_data/FMWT_trait_wide.Rdata")

# add them all up for the monthly index (ie by annual survey)
# may/may not be useful
FMWTim <- group_by(FMWTli, Year, Survey, Species) %>%
  summarize(Index = round(sum(wcatch)))

# TEMP ------
# clear work space of temp/intermed objects
rm(list= ls()[!(ls() %in% c("FMWT_trait_wide",
                            "FMWT_trait_long"))]) 
# leaves objects w basic fmwt data, selected spp; no data: 1974 & 1979

## caveats to index calculations####

#per the FMWT protocol "Only 15 stations used in the index calculation were 
#sampled during Nov. 1969. Therefore, the Nov 1969 index is the average of the 
#Oct and Dec 1969 index."

## CDFW index calculations ----
FMWTim$Index[which(FMWTim$Year==1969 & FMWTim$Survey==5)] <- round(((FMWTim$Index[which(FMWTim$Year==1969 & FMWTim$Survey==4)]+FMWTim$Index[which(FMWTim$Year==1969 & FMWTim$Survey==6)]))/2)

#add monthly indices to get the annual index
FMWT <- group_by(FMWTim, Year, Species) %>%
  summarize(Index = sum(Index))

#1976 uses a quotient for the Sept index and an average of Nov & Jan for the Dec index
#see Appendix 10 (pages 55-60) in protocol here: https://filelib.wildlife.ca.gov/Public/TownetFallMidwaterTrawl/FMWT%20Data/FMWT%20Protocol.pdf

FMWT <- FMWT %>%
  mutate(
    Index = case_when(Year == 1976 & Species == "Striped Bass age-0" ~ 1420,
                      Year == 1976 & Species == "Delta Smelt" ~ 359,
                      Year == 1976 & Species == "Longfin Smelt" ~ 658,
                      Year == 1976 & Species == "American Shad" ~ 346,
                      Year == 1976 & Species == "Splittail" ~ 2,
                      Year == 1976 & Species == "Threadfin Shad" ~ 2957,
                      TRUE ~ Index))

#save additional output as csv

# NOTE: "FMWT 1967-2021 Catch Matrix_updated_tidy.csv" is saved in the working directory and is the full data set, all species
# I've moved a copy "FMWT 1967-2021 Catch Matrix_updated_tidy.csv" to drought traits analyses/fish traits/fish_data

fwrite(FMWT, "C:/Users/pnelson/Documents/Personal/stats & data science/fmwt_index_long.csv") # includes only spp found in 5+% of the samples, plus Chinook (just <5%)

FMWT_wide <- FMWT %>% 
  pivot_wider(names_from = Species, values_from = Index) %>% 
  clean_names()

fwrite(FMWT_wide, "C:/Users/pnelson/Documents/Personal/stats & data science/fmwt_index_wide.csv")

## clear variables ---- 
# from local environment
rm(list = ls())

# all index stations----
#station lookup table to help calculate the index
stas <- data.frame("Station" = as.character(c(336:339, 321:326, 327:329, 330:335, 312:316, 303:311, 340, 401:408, 409:418, 501:519, 601:608, 701:711, 802, 804, 806:815, 901:915)),
                   "Area" = c(rep(1,4), rep(3,6), rep(4,3), rep(5,6), rep(7,5), rep(8,9), 10, rep(11,8), rep(12,10), rep(13,19), rep(14,8), rep(15,11), rep(16,12), rep(17,15)))

#water volumes used to calculate index
wwm <- data.frame("Area" = as.integer(c(1, 3:5, 7, 8, 10:17)), 
                  "Weight" = c(8.1, 11.3, 6.5, 12.2, 10.2, 18.5, 4.8, 16, 14, 18, 5, 12, 14, 20)) #acre-ft x 10^4

#update current year
cy <- as.character(as.numeric(format(Sys.Date(), '%Y'))-1)

#use up to date year for naming convention for URL
fnurl <- paste0("https://filelib.wildlife.ca.gov/Public/TownetFallMidwaterTrawl/FMWT%20Data/FMWT%201967-", cy,"%20Catch%20Matrix_updated.zip")

#use up to date year for naming convention for file wide format
fn <- paste0("FMWT 1967-",cy," Catch Matrix_updated_tidy.csv")

## load data ----
#use this bit of code to download the web version
#new file uploaded every January, check file name is correct
#Now download the data. It's an csv file in a zipped folder
#so first we have to download the zip folder and extract the file
temp <- tempfile()
download.file(fnurl, temp)
FMWT_raw <- fread(unzip(temp, files = fn))
unlink(temp)

## data manipulation ----
#rename a few variables
FMWT_raw <- FMWT_raw %>%
  rename(Date = SampleDate, 
         # Survey or SurveyNumber: number ascribed to each month of the survey 
         # starting in July (1) and ending in June (12); index surveys are from
         # Sept-Dec, surveys 3-6 respectively
         Survey = SurveyNumber, 
         Station = StationCode, 
         Index = index) %>%

#subset just fish (>5% freq by # + Chinook) and variables we care about
  select(Year, Date, Survey, Station, Index, Species, Catch) %>%
  filter(Species %in% c("American Shad", "Striped Bass age-0", "Longfin Smelt", 
                        "Northern Anchovy", "Threadfin Shad", "Delta Smelt", 
                        "Striped Bass age-1", "Pacific Herring", "Striped Bass age-2", 
                        "Chinook Salmon","Splittail"))

#make variables correct format for joining
FMWT_raw$Station <- as.character(FMWT_raw$Station)

#add areas and weights
FMWTl <- left_join(FMWT_raw, stas, by = "Station") %>%
  left_join(., wwm, by = "Area")

#areas used in index calculation
ia <- as.integer(c(1, 3, 4, 5, 7, 8, 10, 11, 12, 13, 14, 15, 16, 17))

##index calculation ----
# includes only index surveys (subset of the stations)
FMWTli <- filter(FMWTl, Area %in% ia, Survey %in% c(3,4,5,6), Index == 1) %>%
  group_by(Year, Survey, Area, Weight, Species) %>%
  summarize(mcatch = mean(Catch))%>%
mutate(wcatch = mcatch*Weight) %>%
  ungroup()

## trait study ----

# index calc with station (Pete Nelson)
# includes only index surveys (subset of the stations)
# I'm not convinced this is the best way to look at spatial variation for
# the drought/trait special studies project...see alternative approach below
FMWTlis <- filter(FMWTl, Area %in% ia, Survey %in% c(3,4,5,6), Index == 1) %>%
  group_by(Year, Survey, Area, Station, Weight, Species) %>%
  summarize(mcatch = mean(Catch))%>%
  # weighting factor determined by area of each location; get feedback on this
  mutate(wcatch = mcatch*Weight) %>% 
  ungroup()

# index calc for trait study; same as index calculation above for normal FMWT
# indices (FMWTli) but limited to Area: 12-14 = Suisun Bay, 15 = Lower 
# Sacramento River, 16 = Lower San Joaquin River, 17 = Eastern Delta; in other
# words, excluding those areas NOT included in our study

### fmwt for trait study ----
# annual indices by area
FMWT_trait_long <- FMWTli %>% 
  filter(Area >= 12) %>% 
  group_by(Year, Area, Species) %>% 
  summarise(index_area = round(sum(wcatch))) %>% 
  clean_names()

write_csv(FMWT_trait_long, "C:/Users/pnelson/Documents/Personal/stats & data science/FMWT_trait_long.csv")

FMWT_trait_wide <- FMWT_trait_long %>% 
  pivot_wider(names_from = species,
              values_from = index_area) %>% 
  clean_names()

write_csv(FMWT_trait_wide, "C:/Users/pnelson/Documents/Personal/stats & data science/FMWT_trait_wide.csv")

# add them all up for the monthly index (ie by annual survey)
# may/may not be useful
FMWTim <- group_by(FMWTli, Year, Survey, Species) %>%
  summarize(Index = round(sum(wcatch)))

# clear work space of temp/intermed objects
rm(list= ls()[!(ls() %in% c("FMWT_trait_wide",
                            "FMWT_trait_long"))]) 
# leaves objects w basic fmwt data, selected spp; no data: 1974 & 1979

## caveats to index calculations####

#per the FMWT protocol "Only 15 stations used in the index calculation were 
#sampled during Nov. 1969. Therefore, the Nov 1969 index is the average of the 
#Oct and Dec 1969 index."

# CDFW index calculations ----
FMWTim$Index[which(FMWTim$Year==1969 & FMWTim$Survey==5)] <- round(((FMWTim$Index[which(FMWTim$Year==1969 & FMWTim$Survey==4)]+FMWTim$Index[which(FMWTim$Year==1969 & FMWTim$Survey==6)]))/2)

#add monthly indices to get the annual index
FMWT <- group_by(FMWTim, Year, Species) %>%
  summarize(Index = sum(Index))

#1976 uses a quotient for the Sept index and an average of Nov & Jan for the Dec index
#see Appendix 10 (pages 55-60) in protocol here: https://filelib.wildlife.ca.gov/Public/TownetFallMidwaterTrawl/FMWT%20Data/FMWT%20Protocol.pdf

FMWT <- FMWT %>%
  mutate(
    Index = case_when(Year == 1976 & Species == "Striped Bass age-0" ~ 1420,
                      Year == 1976 & Species == "Delta Smelt" ~ 359,
                      Year == 1976 & Species == "Longfin Smelt" ~ 658,
                      Year == 1976 & Species == "American Shad" ~ 346,
                      Year == 1976 & Species == "Splittail" ~ 2,
                      Year == 1976 & Species == "Threadfin Shad" ~ 2957,
                      TRUE ~ Index))

#save additional output as csv

# NOTE: "FMWT 1967-2021 Catch Matrix_updated_tidy.csv" is saved in the working directory and is the full data set, all species
# I've moved a copy "FMWT 1967-2021 Catch Matrix_updated_tidy.csv" to drought traits analyses/fish traits/fish_data

fwrite(FMWT, "C:/Users/pnelson/Documents/Personal/stats & data science/fmwt_index_long.csv") # includes only spp found in 5+% of the samples, plus Chinook (just <5%)

FMWT_wide <- FMWT %>% 
  pivot_wider(names_from = Species, values_from = Index) %>% 
  clean_names()

fwrite(FMWT_wide, "C:/Users/pnelson/Documents/Personal/stats & data science/fmwt_index_wide.csv")

# clear variables ---- 
# from local environment
rm(list = ls())

# SCRATCH ----


FMWTlis %>% 
  group_by(Year, Survey, Area, Station, Species) %>% 
  bind_rows(FMWTlis %>% 
              filter(Species %in% c("Striped Bass age-0", "Striped Bass age-1", "Striped Bass age-2")) %>% 
              summarise_if(is.numeric, funs(sum))) %>% 
  mutate(Species = ifelse(is.na(Species), "Striped Bass", Species)) %>% 
  filter(Species != "Striped Bass age-0")
print(FMWTlis, n=30)

FMWT_area <- FMWTlis %>% 
  group_by(Year, Area, Species) %>% 
  summarise(catch_wtd_mean = mean(wcatch), catch_wtd_sd = sd(wcatch)) %>% 
  ungroup()

d <- tribble(
  ~year, ~species, ~count,
  "2001", "fish_a", 1,
  "2001", "fish_b", 1,
  "2001", "fish_c", 1,
  "2001", "fish_d", 1,
  "2002", "fish_a", 1,
  "2002", "fish_b", 1,
  "2002", "fish_c", 1,
  "2002", "fish_d", 1
)

d1 <- tribble(
  ~year, ~species, ~count,
  "2001", "fish_a", 1,
  "2001", "fish_bc", 2,
  "2001", "fish_d", 1,
  "2002", "fish_a", 1,
  "2002", "fish_bc", 2,
  "2002", "fish_d", 1
)

d %>% group_by(year, species) %>% 
  bind_rows(d %>% 
              filter(species %in% c("fish_b", "fish_c")) %>% 
              summarise_if(is.numeric, funs(sum))) %>% 
  mutate(species = ifelse(is.na(species), "fish_bc", species))
# nope!

d %>% 
  add_row(species = "fish_bc", 
          count = sum(.$species == "fish_b" | "fish_c"))
# nope!

d %>% 
  filter(species == "fish_b" | species == "fish_c") %>% 
  summarise(species = "fish_bc", count = sum(count)) %>% 
  bind_rows(d, .)
# nope! (more elegant version of the first fail)

d %>% 
  group_by(year) %>% 
  filter(species == "fish_b" | species == "fish_c") %>% 
  summarise(species = "fish_bc", count = sum(count)) %>% 
  bind_rows(d, .) %>% 
  arrange(year, species)
# getting closer; need to drop the spp filtered (fish_b & fish_c)

d %>%
  group_by(year) %>%
  filter(species == "fish_b" | species == "fish_c") %>%
  summarise(species = "fish_bc", count = sum(count)) %>%
  bind_rows(d %>% 
              filter(species != "fish_b" & species != "fish_c")) %>%
  arrange(year, species)
# HURRAY!