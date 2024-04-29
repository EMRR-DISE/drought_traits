# Special Studies Drought Synthesis
# Purpose: Process water quality data from EMP to be used as environmental
  # predictors. See https://emrr-dise.github.io/drought_traits/explore_emp_wq_data.html 
  # for maps, plots, and code that helped with making decisions on how to process
  # this data.
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov


# 1. Global code and functions --------------------------------------------

# Load packages
library(tidyverse)
library(sf)
# Make sure we are using `deltamapr` version 1.0.0, commit 5b11044a14aada45562d92899e2d8db42c0e8daf
# install.packages("devtools") 
# devtools::install_github("InteragencyEcologicalProgram/deltamapr", ref = "5b11044a14aada45562d92899e2d8db42c0e8daf")
library(deltamapr)
library(contentid)
library(withr)
library(here)
library(conflicted)

# Declare package conflict preferences 
conflicts_prefer(dplyr::filter())

# Check if we are in the correct working directory
i_am("drought_variables/process_emp_wq_data.R")

# Function to replace values below the reporting limit with simulated values
  # between `min_val` and the RL
replace_blw_rl <- function(df, min_val = 0, seed = 1) {
  # Pull out values that are below the RL
  df_blw_rl <- df %>% filter(Sign == "<")
  
  # Replace below RL values with simulated ones
  with_seed(
    # Set seed for reproducibility
    seed = seed,
    df_blw_rl_sim <- df_blw_rl %>% 
      mutate(Result = round(runif(nrow(df_blw_rl), min = min_val, max = Result), 4))
  )
  
  # Add simulated values back to main data frame
  df %>% filter(Sign != "<") %>% bind_rows(df_blw_rl_sim)
}


# 2. Import Data -------------------------------------------------------------

# EDI data package 458.10: 
# Define contentid for EMP station coordinates
id_emp_coord <- "hash://sha256/120d7790b505f22a103ee392d13ca1c02b37fb9842d549942580b04c94eb2c67"

# Define contentid for EMP WQ data
id_emp_wq <- "hash://sha256/4c4b56d7d2a888b35efe2a6f2818f2b8b2fab26d0adb8f1c73ef522996b26ea8"

# Resolve the contentid's storing local copies for faster import
file_emp_coord <- resolve(id_emp_coord, store = TRUE)
file_emp_wq <- resolve(id_emp_wq, store = TRUE)

# Import EMP WQ data and station coordinates from the local copies using their
  # contentid's
df_emp_coord <- read_csv(file_emp_coord)

df_emp_wq <- read_csv(
  file = file_emp_wq,
  col_types = cols_only(
    Station = "c",
    Date = "c",
    Secchi = "d",
    SpCndSurface = "d",
    WTSurface = "d",
    DissAmmonia_Sign = "c",
    DissAmmonia = "d",
    DissNitrateNitrite_Sign = "c",
    DissNitrateNitrite = "d",
    DissOrthophos_Sign = "c",
    DissOrthophos = "d"
  )
)

# Import station coordinates for biological data
df_bio_coord <- read_csv(here("Station_coordinates.csv"))

# Load Delta subregion polygons from the Drought Synthesis
sf_delta <- R_EDSM_Subregions_Mahardja_FLOAT %>% 
  select(SubRegion) %>% 
  # Remove a few subregions outside our area of interest
  filter(!SubRegion %in% c("San Francisco Bay", "South Bay", "Upper Napa River")) 


# 3. Prepare Data for Averaging ----------------------------------------------

# Prepare EMP WQ data for filtering
df_emp_wq_c <- df_emp_wq %>% 
  mutate(
    Date = ymd(Date),
    # The nutrient values have a few non-detect values without reporting limits.
      # We'll fill in 0.01 for the reporting limits for these values for now as
      # suggested by Perry.
    DissAmmonia = if_else(DissAmmonia_Sign == "<" & is.na(DissAmmonia), 0.01, DissAmmonia),
    DissNitrateNitrite = if_else(DissNitrateNitrite_Sign == "<" & is.na(DissNitrateNitrite), 0.01, DissNitrateNitrite),
    DissOrthophos = if_else(DissOrthophos_Sign == "<" & is.na(DissOrthophos), 0.01, DissOrthophos)
  ) %>% 
  # Remove records where values for all parameters are NA
  filter(!if_all(where(is.numeric), is.na)) %>% 
  # Only include 2021 and earlier - use adjusted calendar year:
    # December-November, with December of the previous calendar year included with
    # the following year
  mutate(
    Month = month(Date),
    YearAdj = if_else(Month == 12, year(Date) + 1, year(Date)),
    .after = Date
  ) %>% 
  filter(YearAdj <= 2021) %>% 
  # Rename a few parameters
  rename(
    SpCond = SpCndSurface,
    WaterTemp = WTSurface
  )

# Prepare station coordinates of biological data for filtering
sf_bio_coord <- df_bio_coord %>%
  rename_with(str_to_title) %>% 
  drop_na(Lat_wgs84, Long_wgs84) %>%
  # Remove phytoplankton stations because we're using a different set of
    # environmental predictors for them
  filter(Taxon != "phytoplankton") %>% 
  st_as_sf(coords = c("Long_wgs84", "Lat_wgs84"), crs = 4326) %>% 
  # Convert CRS so that its the same as the region and subregion polygons
  st_transform(crs = st_crs(sf_delta))

# Prepare EMP station coordinates for filtering
sf_emp_coord <- df_emp_coord %>% 
  drop_na(Latitude, Longitude) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
  # Convert CRS so that its the same as the subregion polygons
  st_transform(crs = st_crs(sf_delta))

# 3.1 Filter Data Spatially ---------------------------------------------------

# Filter polygons from sf_delta to where biological data has been collected
sf_delta_bio <- sf_delta %>% 
  st_join(sf_bio_coord, join = st_intersects) %>% 
  drop_na(Station) %>% 
  distinct(SubRegion, geometry) %>% 
  # Add Franks Tract subregion to the polygons used for filtering
  bind_rows(sf_delta %>% filter(SubRegion == "Franks Tract"))

# Spatially join subregions to the EMP WQ coordinates and remove stations
  # outside of subregions
sf_emp_coord_sfilt <- sf_emp_coord %>% 
  st_join(sf_delta_bio, join = st_intersects) %>% 
  drop_na(SubRegion)

# Remove data for stations outside of subregions from EMP WQ dataset
df_emp_wq_sfilt <- df_emp_wq_c %>% filter(Station %in% unique(sf_emp_coord_sfilt$Station))

# 3.2 Filter Data Temporally --------------------------------------------------

# Combine overlapping stations - we'll combine the following stations based on
  # their proximity and sampling effort:
# D41 and D42
# MD10 and MD10A
# S42 and NZS42
df_emp_wq_sfilt_c <- df_emp_wq_sfilt %>% 
  mutate(
    Station = case_when(
      Station %in% c("D41", "D42") ~ "D41-D42",
      Station %in% c("MD10", "MD10A") ~ "MD10-MD10A",
      Station %in% c("S42", "NZS42") ~ "S42-NZS42",
      .default = Station
    )
  )

# Now, we’ll filter stations temporally based on their time series completeness.
  # We’ll keep stations that have less than 10% missing data assuming a monthly
  # sampling frequency. This will be for each station’s overall sampling record -
  # not for each individual parameter.
# Calculate total number of monthly sampling events expected between 1975-2021
yrs_range <- range(df_emp_wq_sfilt_c$YearAdj)
num_yrs_mo <- length(yrs_range[1]:yrs_range[2]) * 12

# Calculate threshold for number of sampling events (10% missing)
num_yrs_thresh <- round(num_yrs_mo - num_yrs_mo * 0.1)

# Determine which stations meet the time series completeness criteria
lt_sta_keep <- df_emp_wq_sfilt_c %>% 
  distinct(Station, Month, YearAdj) %>% 
  count(Station) %>% 
  filter(n >= num_yrs_thresh) %>% 
  pull(Station)

# Filter stations temporally in EMP WQ dataset
df_emp_wq_sfilt_tf <- df_emp_wq_sfilt_c %>% filter(Station %in% lt_sta_keep)

# 3.3 Check for Outliers ------------------------------------------------------

# Restructure data to long format to allow for easier implementation of
  # functions for outlier inspection
# Separate nutrient parameters from WQ measurements because only the nutrients
  # have "Sign" variables
# Nutrient parameters
df_emp_wq_f_nutr <- df_emp_wq_sfilt_tf %>% 
  select(Station, Date, Month, YearAdj, starts_with("Diss")) %>% 
  rename_with(~ paste0(.x, "_Result"), starts_with("Diss") & !ends_with("_Sign")) %>% 
  pivot_longer(
    cols = starts_with("Diss"), 
    names_to = c("Parameter", ".value"), 
    names_sep = "_"
  ) %>% 
  drop_na(Result)

# WQ measurement parameters
wq_params <- c("Secchi", "SpCond", "WaterTemp")

df_emp_wq_f_wqmeas <- df_emp_wq_sfilt_tf %>% 
  select(Station, Date, Month, YearAdj, all_of(wq_params)) %>% 
  pivot_longer(
    cols = all_of(wq_params), 
    names_to = "Parameter", 
    values_to = "Result"
  ) %>% 
  drop_na(Result)

# 3.3.1 WQ measurement parameters -----------------------------------------

# Look for outliers by using a Z-score test to flag data points to potentially
  # remove from the data set. We’ll use a standard Z-score test to flag data more
  # than 10 SDs away from the mean of each station.
df_emp_wq_f_wqmeas_c1 <- df_emp_wq_f_wqmeas %>% 
  group_by(Parameter, Station) %>% 
  mutate(
    tmp_mean = mean(Result),
    tmp_sd = sd(Result),
    Zscore = if_else(tmp_sd == 0, NA_real_, abs((Result - tmp_mean) / tmp_sd)),
    Zscore_flag = case_when(Zscore > 10 ~ TRUE, .default = FALSE)
  ) %>% 
  ungroup() %>% 
  # Three specific conductance values were flagged. After reviewing these values
    # in the explore_emp_wq_data.Rmd document, we decided to remove suspect values
    # collected at D22 and D12 from the data set, but keep the flagged value
    # collected at D16.
  mutate(
    Zscore_flag = if_else(
      Parameter == "SpCond" & Station == "D16" & Date == "1994-09-27",
      FALSE,
      Zscore_flag
    )
  ) %>% 
  filter(!Zscore_flag) %>% 
  select(!starts_with(c("tmp", "Zscore")))

# 3.3.2 Nutrient parameters -----------------------------------------------

# There are a few nutrient values that are less than the reporting limit with
  # reporting limits that are very high compared to the range of the values for
  # the parameter. We'll remove all values less than the reporting limit with RL's
  # greater than the 75th percentile of all data for the parameter.
df_emp_wq_f_nutr_c1 <- df_emp_wq_f_nutr %>% 
  group_by(Parameter) %>% 
  mutate(Quant_75 = quantile(Result, probs = 0.75)) %>% 
  ungroup() %>% 
  filter(!(Sign == "<" & Result > Quant_75)) %>% 
  select(-Quant_75)

# Now, look for outliers by using a modified Z-score test grouped by station and
  # flag data with scores greater than 10. We used the modified test for nutrients
  # because its more resistant to censored data.
df_emp_wq_f_nutr_c2 <- df_emp_wq_f_nutr_c1 %>% 
  group_by(Parameter, Station) %>% 
  mutate(
    tmp_median = median(Result),
    tmp_mad = mad(Result),
    ModZscore = if_else(tmp_mad == 0, NA_real_, abs(0.6745 * (Result - tmp_median) / tmp_mad)),
    ModZscore_flag = case_when(ModZscore > 15 ~ TRUE, .default = FALSE)
  ) %>% 
  ungroup() %>% 
  # Two dissolved ammonia and three dissolved nitrate + nitrite values were
    # flagged. After reviewing these values in the explore_emp_wq_data.Rmd
    # document, we decided to remove the suspect nitrate + nitrite values from the
    # data set, but keep the flagged ammonia values.
  mutate(ModZscore_flag = if_else(Parameter == "DissAmmonia", FALSE, ModZscore_flag)) %>% 
  filter(!ModZscore_flag) %>% 
  select(!starts_with(c("tmp", "ModZscore")))


# 4. Calculate Averages ------------------------------------------------------

# Prepare nutrient data for averaging. Remove data collected at D10, D12, D16,
  # and D22 because of inadequate sampling, and substitute random numbers from a
  # uniform distribution for the <RL values.
df_emp_wq_f_nutr_c3 <- df_emp_wq_f_nutr_c2 %>% 
  filter(!Station %in% c("D10", "D12", "D16", "D22")) %>% 
  nest(.by = Parameter, .key = "df_data") %>% 
  mutate(df_data = map(df_data, replace_blw_rl)) %>% 
  unnest(df_data) %>% 
  select(-Sign)

# Recombine WQ measurement and nutrient data now that all data is ready for
  # averaging, and calculate monthly averages for each station
df_emp_wq_avg_mon <- 
  bind_rows(df_emp_wq_f_wqmeas_c1, df_emp_wq_f_nutr_c3) %>% 
  summarize(Result = mean(Result), .by = c(Station, YearAdj, Month, Parameter)) %>% 
  # Create column for season
  mutate(
    Season = case_when(
      Month %in% 3:5 ~ "Spring",
      Month %in% 6:8 ~ "Summer",
      Month %in% 9:11 ~ "Fall",
      Month %in% c(12, 1, 2) ~ "Winter"
    ),
    .after = Month
  ) 

# Calculate seasonal and annual averages across all sites, creating one set for
  # the fish analysis and another for the benthic and zooplankton analyses because
  # the included sites vary for these based on sampling coverage for each
  # taxonomic group
ls_emp_wq <- list(
  fish = df_emp_wq_avg_mon,
  benth_zoop = df_emp_wq_avg_mon %>% filter(!Station %in% c("D41-D42", "D6"))
)

# Seasonal averages:
ls_emp_wq_avg_seas <- ls_emp_wq %>% 
  map(~ summarize(.x, Result = mean(Result), .by = c(Parameter, YearAdj, Season)))

# Annual averages using seasonal averages:
ls_emp_wq_avg_yr <- ls_emp_wq_avg_seas %>% 
  map(~ summarize(.x, Result = mean(Result), .by = c(Parameter, YearAdj)))

# Combine average data sets into one list and restructure to wide format by
  # parameter
ls_emp_wq_avg_c <- ls_emp_wq_avg_seas %>% 
  set_names(~ paste0("seasonal_avg_", .)) %>% 
  append(set_names(ls_emp_wq_avg_yr, ~ paste0("annual_avg_", .))) %>% 
  map(~ pivot_wider(.x, names_from = Parameter, values_from = Result))


# 5. Export Average Data -----------------------------------------------------

# Define file path to export average data sets
fp_avg_data <- here("drought_variables/data_processed")

# Export average data sets as csv files
ls_emp_wq_avg_c %>% 
  iwalk(\(x, idx) write_csv(x, file = file.path(fp_avg_data, paste0("emp_wq_", idx, ".csv"))))

# Export average data sets as rds files
ls_emp_wq_avg_c %>% 
  iwalk(\(x, idx) saveRDS(x, file = file.path(fp_avg_data, paste0("emp_wq_", idx, ".rds"))))

