# Special Studies Drought Synthesis
# Purpose: Process water quality data from EMP to be used as environmental
  # predictors. See https://emrr-dise.github.io/drought_traits/explore_emp_wq_data.html 
  # for maps and plots that helped with making decisions on how to process this data.
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

# Load packages
library(tidyverse)
library(sf)
# Make sure we are using `deltamapr` version 1.0.0, commit 5b11044a14aada45562d92899e2d8db42c0e8daf
# install.packages("devtools") 
# devtools::install_github("InteragencyEcologicalProgram/deltamapr", ref = "5b11044a14aada45562d92899e2d8db42c0e8daf")
library(deltamapr)
library(contentid)
library(here)
library(conflicted)

# Declare package conflict preferences 
conflicts_prefer(dplyr::filter())

# Check if we are in the correct working directory
i_am("drought_variables/process_wq_data.R")


# 1. Import Data -------------------------------------------------------------

# EDI data package 458.10: 
# Define contentid for EMP station coordinates
id_emp_coord <- "hash://sha256/120d7790b505f22a103ee392d13ca1c02b37fb9842d549942580b04c94eb2c67"

# Define contentid for EMP WQ data
id_emp_wq <- "hash://sha256/4c4b56d7d2a888b35efe2a6f2818f2b8b2fab26d0adb8f1c73ef522996b26ea8"

# Resolve the contentid's storing local copies for faster import
file_emp_coord <- resolve(id_emp_coord, store = TRUE)
file_emp_wq <- resolve(id_emp_wq, store = TRUE)

# Import EMP WQ data and station coordinates from the local copies using their contentid's
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


# 2. Prepare Data for Averaging ----------------------------------------------

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
  filter(YearAdj <= 2021)

# Prepare spatial data for filtering


# 2.1 Filter Data Spatially ---------------------------------------------------



# 2.2 Filter Data Temporally --------------------------------------------------

# Combine overlapping stations



# 2.3 Check for Outliers ------------------------------------------------------


# 3. Calculate Averages ------------------------------------------------------

# df_emp_wq_mavg <- df_emp_wq_sfilt_c %>% 
#   mutate(Month = month(Date), .after = Date) %>%
#   summarize(
#     across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
#     .by = c(Station, Month, Year)
#   ) %>% 
#   mutate(
#     across(
#       where(is.numeric) & !c(Month, Year),
#       ~ if_else(is.nan(.x), NA_real_, .x)
#     )
#   )

# For fish analysis

# For benthic and zooplankton analyses


# 4. Export Average Data -----------------------------------------------------




