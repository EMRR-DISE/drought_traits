#Drought Synthesis
#Special Studies Project
#Environmental Monitoring Program data
#Benthic Invertebrate Survey data
#1975 - October 2020
#Catch per unit effort (CPUE)
#Organisms/m2 = (sum per visit /# of grabs per visit)/(0.052 grabs/meter squared)

#to do list
#add water year type to abundance data (note that invert data categorized by calendar year)
#create map showing all stations, active and historic

#required packages
library(tidyverse)
library(janitor)
library(hms)
library(readxl) #importing data from excel files
library(waterYearType) #lists all water year types 1901 - 2017

#read in data from EDI-----------------------
#https://portal.edirepository.org/nis/metadataviewer?packageid=edi.1036.1

#station data
benthic_stn <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.1036.1&entityid=4e6948186ad756dc2b6de4de41b601f3") %>% 
  clean_names() %>% 
  glimpse()

#benthic invert CPUE
#data have been converted to CPUE (organisms/m2)
#replicate grabs have been averaged for each site visit
#all non-occurrence (zero) data for a site visit has been removed
#Nick: samples with no organisms at all are probably included as "No catch"
benthic_cpue <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.1036.1&entityid=1dfeb0e5a58f2167faca4087b717aae4") %>% 
  clean_names() %>% 
  glimpse()

#organism key list
#probably don't need this because this info is also in the cpue data frame
#benthic_spp <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.1036.1&entityid=d0f0dd3c1835fe5b669342f8c8e77024")

#total annual site visits, 1975-2020
benthic_visits <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.1036.1&entityid=ae8df5b1a7b1406e01ee7934a2f38822") %>% 
  clean_names() %>% 
  glimpse()

#total annual grab samples, 1975-2020
benthic_grabs <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.1036.1&entityid=c377b4b8f9c1c7168214be6604a4a5e5") %>% 
  clean_names() %>% 
  glimpse()

#EMP water quality data
benthic_wq <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.458.4&entityid=98b400de8472d2a3d2e403141533a2cc") %>% 
  clean_names() %>% 
  glimpse()

#Water year type from waterYearType package
#the only function in package brings in water year type data frame
water_year <- water_year_indices
#glimpse(water_year) #looks like column types are correct

#explore sampling effort-----------

visits <- benthic_visits %>%
  #drop years with no visits
  filter(number_of_site_visits!=0) %>%
  #look at number of years with visits by station
  group_by(station_code) %>% 
  summarise(n=n())

#plot histogram to see distribution of number of years with visits for sites
ggplot(visits, aes(x=n))+
  geom_histogram()
#very few stations with 40+ years of data, as expected

#filter out any stations with less than 40 years of data
visits_most <- visits %>% 
  filter(n>40)
unique(visits_many$station_code)
#

#now take a closer look at effort for those mostly complete stations
visits_most_eff <- benthic_visits %>% 
  #just keep the three needed stations
  #redo this so it isn't hard coded
  filter(station_code == "D28A-L" | station_code == "D4-L"   | station_code == "D7-C")

#plot effort by station
ggplot(visits_most_eff,aes(x = year, y = number_of_site_visits))+
  geom_point()+
  geom_line()+
  facet_grid(station_code~.)
#after about 1980, sampling is pretty consistently high except for 2004-2005

#format EDI data-------------------------

#filter CPUE data set to just the three longterm stations
cpue_oldest <- benthic_cpue %>% 
  #only three longest surveyed stations
  filter(station_code == "D28A-L" | station_code == "D4-L"   | station_code == "D7-C") %>% 
  #how many rows if we just look at distinct date, station, organism code, cpue
  #necessary to do this because I think there is a duplication error throughout this data set
  distinct(sample_date,station_code,genus, species,organism_code,mean_cpue) %>% 
  glimpse()

#try various approaches for excluding rare taxa
#how many taxa remain if only keeping those that are present in 
#at least 5% of samples? 10% of samples?

total <- cpue_oldest %>% 
  #see if date and station combo give same number
  distinct(sample_date, station_code) %>%  
  count()
samp_denom <-as.numeric(total[1,1])

cpue_sample_prop <- cpue_oldest %>% 
  group_by(genus,species,organism_code) %>% 
  #rows with an organism absent are already removed in original data frame
  #so row counts by taxon should be number of samples with the taxon
  summarise(n_samp=n()) %>% 
  #order rows by number of samples, most to least
  arrange(-n_samp) %>% 
  mutate(prop = n_samp/samp_denom) %>% 
  glimpse()

#plot histogram of proportion of samples containing species
ggplot(cpue_sample_prop, aes(x=prop))+
  geom_histogram()

#filter to just the species in more than 5% of samples
cpue_5plus <- cpue_sample_prop %>% 
  filter(prop>0.05)
#dropped from 243 to 65 taxa

#filter to just the species in more than 10% of samples
cpue_10plus <- cpue_sample_prop %>% 
  filter(prop>0.10)
#dropped from 243 to 42 taxa; 17.3% of taxa retained




#generate annual mean CPUE values for each taxon



#read in and format data from sharepoint--------------------

#Define path on SharePoint site for data
#sharepoint_path <- normalizePath(
#  file.path(
#    Sys.getenv("USERPROFILE"),
#    "California Department of Water Resources/Drought Synthesis - Component data sets"
#  )
#)  

#read in data from excel file
#separate the taxonomy in the headers from the abundance data
#eventually combine these into one data frame

#first the abundances
#abundance <- read_excel(path = paste0(sharepoint_path,"/BenthicInverts_EMP_CPUE_1975-Oct2020_20210511.xlsx")
#                        #specify sheet and cell range
#                        , range = "75-20 CPUE m2!A8:PD4534"
#                        , col_names = T
#                        )
#glimpse(abundance) #looks like columns were categorized correctly when imported

#then the taxonomy
#taxonomy <- read_excel(path = paste0(sharepoint_path,"/BenthicInverts_EMP_CPUE_1975-Oct2020_20210511.xlsx")
#                        #specify sheet and cell range
#                       , range = "75-20 CPUE m2!E2:PD8"
#                       , col_names = F
#                       )
#glimpse(taxonomy)

#then station data
#stations <- read_excel(path = paste0(sharepoint_path,"/BenthicInverts_EMP_CPUE_1975-Oct2020_20210511.xlsx")
#                       #specify sheet 
#                       , sheet = "75-19 station locations"
#                       , col_names = T
#)
#glimpse(stations) #column types look good
           
#convert abundance data frame from wide to long
#names(abundance)
#abund <- abundance %>% 
#  pivot_longer(cols = "2970":"1093"
#               ,names_to = "sp_code"
#               , values_to = "cpue") %>% 
#  clean_names() #from janitor package
#glimpse(abund) #column types look correct

#convert taxonomy data frame from wide to long 
#simple approach is probably just a transpose
#taxon <- data.frame(t(taxonomy)) %>% 
#  rename(phylum = X1
#         ,class = X2
#         ,order = X3
#         ,family = X4
#         ,genus = X5
#         ,species = X6
#         ,sp_code = X7
#  )
#glimpse(taxon) #column types look correct

#join abundance and taxonomy data
#sp_abund <-left_join(abund,taxon)

#explore station data
#almost no stations that cover full time period starting in 1975 to present

#stns <- stations %>% 
  #simplify names
#  clean_names() %>% #from janitor package
  #some more column name changes
#  rename(station_code = site_code #to match column in abundance df
#         ,start = "period_of_record_from"
#         ,end = "period_of_record_to")

#replace "Present" with current year
#stns$end2<-str_replace_all(stns$end, "Present", "2020")
#need to decide how to categorize stations in a useful way
#mostly should focus on active stations 
#but there are some historical stations with many years of data

#explore abundance data

#range of dates
#range(sp_abund$date) #"1975-05-19 UTC" "2020-10-20 UTC"

#look at stations
#unique(sp_abund$station_code) #53 stations

#look at histograms of frequencies by species

#look at histogram of total abundances by species

#look at which species are most abundant in wet years vs Critical years
#also look at Rosie's proposed definitions for multi-year periods of drought vs. wet
#also look at Betsy's manuscript

#create definition for rare taxa and exclude them from data set




















