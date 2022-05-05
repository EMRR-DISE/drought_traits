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

#Create Table L (taxon x sample)-------------------------

#filter CPUE data set to just the three longterm stations
cpue_oldest <- benthic_cpue %>% 
  #only three longest surveyed stations
  filter(station_code == "D28A-L" | station_code == "D4-L"   | station_code == "D7-C") %>% 
  #how many rows if we just look at distinct date, station, organism code, cpue
  #necessary to do this because I think there is a duplication error throughout this data set
  distinct(year,sample_date,station_code,genus, species,organism_code,mean_cpue) %>% 
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
#dropped from 243 to 65 taxa; 26.7% taxa retained

#filter to just the species in more than 10% of samples
cpue_common <- cpue_sample_prop %>% 
  filter(prop>0.10)
#dropped from 243 to 42 taxa; 17.3% of taxa retained

#combine proportion info with main data frame and then filter to those with more than 10%
#or use a join function to just keep the taxa taht are in more than 10% of samples
cpue_oldest_prop <- left_join(cpue_oldest,cpue_sample_prop) %>% 
  #only keep the taxa that appear in at least 10% of taxa
  filter(prop>0.10) %>%
  select(-c(n_samp,prop))

#this is basically Table L; it just needs to be reshaped so it is a taxon x sample matrix
#generate annual mean CPUE values for each taxon
#consider changing this to water year instead of calendar year
cpue_mean_annual <- cpue_oldest_prop %>% 
  group_by(year,station_code,organism_code) %>% 
  summarize(cpue_annual=mean(mean_cpue))

#make a big faceted plot showing time series of each taxon in each station
ggplot(cpue_mean_annual,aes(x=year, y=cpue_annual, group=station_code,color=station_code))+
  geom_point()+
  geom_line()+
  facet_wrap(vars(organism_code),scales="free",nrow=6)

#look at water quality data and pick the best parameters for our purposes based largely on how long they've been 
#consistently collected

#match wq with all sample data

#filter to just the common taxa

#plot distributions by taxa and wq parameter

#calculat 95th percentile (temp, sal) or 5th percentile (DO, turb)

























