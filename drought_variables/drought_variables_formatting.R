#Special Studies Drought Synthesis
#Drought environmental variables
#Round up variables and put into one data table

#Nick Rasmussen, nicholas.rasmussen@water.ca.gov

#Drought variables
#categorical: water year type, Mahardja et al 2021 drought periods 
#continuous: mean annual temperature, annual total delta inflow

#To do list-------------
#consider condensing water year types into 2 or 3 categories instead of 5
#look closer at how water year type categories are defined to figure this out
#maybe make a column that is water year type as ordinal category instead of factors

#Load required packages------------
library(tidyverse)
library(lubridate)
library(janitor)

#read in data-------------------

#hydrology data including delta inflow  
# Create vector of URL paths for the Dayflow data
# Dayflow data for 2022 isn't available yet
url_dayflow <- c(
  "https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/3109f3ef-b77b-4288-9ece-3483899d10da/download/dayflow-results-1956-1969.csv",
  "https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/a0a46a1d-bec5-4db9-b331-655e306860ba/download/dayflow-results-1970-1983.csv",
  "https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/cb04e626-9729-4105-af81-f6e5a37f116a/download/dayflow-results-1984-1996.csv",
  "https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/21c377fe-53b8-4bd6-9e1f-2025221be095/download/dayflow-results-1997-2020.csv",
  "https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/83122ce7-e7f5-4ad1-b5e9-6a7032cb1117/download/dayflowcalculations2021.csv"
)

# Create vector of file names for the Dayflow data
filename_dayflow <- str_extract(url_dayflow, "(?<=download/).+\\.csv$")

# Create function to download Dayflow data to a temporary directory
download_dayflow <- function(url, filename) {
  download.file(url = url, destfile = file.path(tempdir(), filename), mode = "wb", method = "libcurl")
}

# Download Dayflow data from CNRA portal: https://data.cnra.ca.gov/dataset/dayflow
walk2(url_dayflow, filename_dayflow, download_dayflow)

# Import Dayflow data - only keep Total Inflow data
inflow <- map_dfr(
  map(filename_dayflow, ~ file.path(tempdir(), .x)), 
  ~ read_csv(.x, col_types = cols_only(Date = "c", TOT = "d"))
)

#drought variables from IEP drought synthesis
#I think I just want the sacramento valley water year types 
#data originates from http://cdec.water.ca.gov/reportapp/javareports?name=WSIHIST
#years 1906-2022
iep_drought <- read_csv("https://raw.githubusercontent.com/InteragencyEcologicalProgram/DroughtSynthesis/main/data/yearassignments.csv") %>% 
  clean_names()

#drought periods from Mahardja et al 2021
#years 1967 to 2021
dperiod <- 
  read_csv("./drought_variables/drought_periods_mahardja2021.csv") %>% 
  filter(year %in% 1967:2021)

# Annual averages of environmental variables from Rosie
# These are most likely based on an adjusted calendar water year (Dec - Nov)
# Years 1975-2021
# At some point it would be good to get the processing code for this data
load("drought_variables/DroughtNick.Rdata")

#format delta inflow data----------
#calculate annual totals
#units are cubic feet per second
#based on adjusted water year (Dec - Nov) - years 1967 to 2021
annual_inflow <- inflow %>% 
  mutate(
    # Dates are in two different formats
    Date = parse_date_time(Date, c("mdY", "Ymd")),
    Month = month(Date),
    Year = year(Date),
    WY_adj = if_else(Month > 11, Year + 1, Year)
  ) %>% 
  filter(WY_adj > 1966) %>% 
  group_by(WY_adj) %>% 
  summarise(inflow_annual_cfs = sum(TOT)) %>% 
  rename(year = WY_adj)

#format water year type data---------
#this should be based on standard water year

water_year <- iep_drought %>% 
  select(
    year,
    wy_index_sac = index,
    wy_type_sac = yr_type,
    drought_category = drought
  ) %>%
  filter(year %in% 1967:2021) %>% 
  # Correct SV index values for 2020 and 2021 based on most recent report from:
  # https://cdec.water.ca.gov/reportapp/javareports?name=WSIHIST
  mutate(
    wy_index_sac = case_when(
      year == 2020 ~ 6.12,
      year == 2021 ~ 3.8,
      TRUE ~ wy_index_sac
    )
  ) %>% 
  arrange(year)


# format environmental variables ------------------------------------------

df_env <- Drought4Nick %>% 
  # remove log-transformed values
  filter(!str_detect(Metric, "^log")) %>% 
  # pivot data wide based on Metric
  select(year = Year, Metric, Value) %>% 
  pivot_wider(names_from = Metric, values_from = Value)

#join the four data sets by year------------
#again, note that the year range still differs between data sets
#and year is defined differently for inflow and the environmental variables than
  #the other variables (both standard water year)

df_list <- list(dperiod, water_year, annual_inflow, df_env)
drought_vars <- df_list %>% reduce(full_join, by = join_by(year))
#maybe make a column that is water year type as ordinal category instead of factors

#write file
write_csv(drought_vars,"./drought_variables/drought_variables.csv")

