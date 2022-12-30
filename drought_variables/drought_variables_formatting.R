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
#need to round up missing years for inflow and also calculate inflow for standard water year

#Load required packages------------
library(tidyverse)
library(janitor)
library(DroughtData) #Dave's drought data package, which includes delta inflow

#read in data-------------------

#hydrology data including delta inflow 
#note that data are organized so years are Dec - Nov
#which isn't standard water year or calendar year
#years 1975-2021
#need to add 1967-1974 and 2022
hydro <- raw_hydro_1975_2021

#drought variables from IEP drought synthesis
#I think I just want the sacramento valley water year types 
#data originates from http://cdec.water.ca.gov/reportapp/javareports?name=WSIHIST
#years 1906-2022
iep_drought <- read_csv("https://raw.githubusercontent.com/InteragencyEcologicalProgram/DroughtSynthesis/main/data/yearassignments.csv") %>% 
  clean_names()

#drought periods from Mahardja et al 2021
#years 1967 to 2021
dperiod <- read_csv("./drought_variables/drought_periods_mahardja2021.csv")

#format delta inflow data----------
#calculate annual totals
#units are cubic feet per second
#for now, just use the Dec-Nov year definition used in Dave's data package
#need to decide whether to do calendar year or water year
#also the fish data set starts in 1967 (instead of 1975), so need to add data for these extra years

inflow <- hydro %>% 
  select(year = YearAdj,InflowTotal) %>% 
  group_by(year) %>% 
  summarise(inflow_annual_cfs = sum(InflowTotal)) %>% 
  glimpse()

#format water year type data---------
#this should be based on standard water year

water_year <- iep_drought %>% 
  select(year,water_year_sac = yr_type) %>%
  filter(year > 1966) %>% 
  arrange(year) %>% 
  glimpse()

#join the three data sets by year------------
#again, note that the year range still differs between data sets
#and year is defined differently for inflow than the other variables (both standard water year)

df_list <- list(dperiod,water_year,inflow)
drought_vars <- df_list %>% 
  reduce(full_join)
#maybe make a column that is water year type as ordinal category instead of factors

#write file
#write_csv(drought_vars,"./drought_variables/drought_variables.csv")

