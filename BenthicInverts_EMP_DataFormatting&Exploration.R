#Drought Synthesis
#Special Studies Project
#Environmental Monitoring Program data
#Benthic Invertebrate Survey data
#1975 - October 2020
#Catch per unit effort (CPUE)
#Organisms/m2 = (sum per visit /# of grabs per visit)/(0.052 grabs/meter squared)
#GRTS amphipod papers says ponar sample was to a depth that varied from 4-10 cm with sediment type

#to do list
#add water year type to abundance data (note that invert data categorized by calendar year)
#create map showing all stations, active and historic

#required packages
library(tidyverse)
library(janitor)
library(lubridate) #format date
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
benthic_spp <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.1036.1&entityid=d0f0dd3c1835fe5b669342f8c8e77024")

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

#EMP water quality stations
wq_stn <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/458/4/827aa171ecae79731cc50ae0e590e5af")

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
unique(visits_most$station_code)

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

#first add back in the rows with zeros
#need to include zeros in the calculations of annual mean CPUE
#probably easiest to convert to wide form and then back to long form
benthic_cpue_with_zeros <- benthic_cpue %>% 
  #drop unneeded columns
  select(sample_date,station_code, organism_code,mean_cpue) %>% 
  #sort by organism code so when I replace NA with zero the starting column is always the same
  arrange(organism_code) %>%     
  #only keep unique rows; current EDI data set has an error so lots of rows are duplicated
  distinct(sample_date,station_code, organism_code,mean_cpue) %>% 
  #make data frame wide which will add NA every time an organism was not detected in a visit 
  pivot_wider(id_cols = c(sample_date,station_code),names_from = organism_code,values_from=mean_cpue) %>% 
  #replace NAs with zeros; maybe find a way to do this that is less hard coded
  mutate(across('0':'8000', ~replace_na(.,0))) %>% 
  #make long version again
  pivot_longer(cols= c('0':'8000'), names_to = "organism_code", values_to = "mean_cpue")


#filter CPUE data set to just the three longterm stations
cpue_oldest <- benthic_cpue_with_zeros %>% 
  #only three longest surveyed stations
  filter(station_code == "D28A-L" | station_code == "D4-L"   | station_code == "D7-C") %>% 
  glimpse()

#number of visits differs among years. which month(s) are most consistently sampled through time?
vmonth <- cpue_oldest %>% 
  mutate(month=month(sample_date)
         ,year=year(sample_date)
         ) %>% 
  distinct(station_code, year, month) %>%  #1389 instead of 1399, not sure why the difference
  group_by(station_code,month) %>% 
  summarise(n=n()) %>% 
  arrange(station_code,-n)
#each month has 35-43 years of visits; October is most sampled month

#try various approaches for excluding rare taxa
#how many taxa remain if only keeping those that are present in 
#at least 5% of samples? 10% of samples?

#need to calculate total number of samples
total <- cpue_oldest %>% 
  #look at distinct combinations of station and date
  distinct(sample_date, station_code) %>%  
  count()
samp_denom <-as.numeric(total[1,1]) #1399

#calculate the proportion of samples in which each taxon is present
cpue_sample_prop <- cpue_oldest %>%
  #drop rows where cpue is zero for sake of counting samples in which species are present
  filter(mean_cpue!=0) %>%   
  group_by(organism_code) %>% 
  #row counts by taxon now should be number of samples with the taxon
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
  #drop unneeded columns
  select(-c(n_samp,prop)) %>% 
  #add year column back in
  mutate(year=year(sample_date)) %>%
  #sort by date, station, organism code
  arrange(sample_date, station_code, organism_code)

#generate annual mean CPUE values for each taxon
#consider changing this to water year instead of calendar year
cpue_mean_annual <- cpue_oldest_prop %>% 
  group_by(year,station_code,organism_code) %>% 
  summarize(cpue_annual=mean(mean_cpue))
#keep in mind that number of samples per station varies among years
#probably should drop years with, say, only one sample because hard to compare with other years
#could also just use data for the sampling month that is best represented across all years (Oct)
#instead of calculating annual means based on a variable number of samples

#make Table L which is taxon x sample matrix
TableL <- cpue_mean_annual %>% 
  pivot_wider(id_cols = c(station_code,year),names_from = organism_code,values_from=cpue_annual)  
#probably just need to now combine station and year into a row name and format as matrix

#make a big faceted plot showing time series of each taxon in each station
ggplot(cpue_mean_annual,aes(x=year, y=cpue_annual, group=station_code,color=station_code))+
  geom_point()+
  geom_line()+
  facet_wrap(vars(organism_code),scales="free",nrow=6)

#explore water quality data----------------

#reduce data set to just the columns of interest
#glimpse(benthic_wq)

wq_format <- benthic_wq %>% 
  #format date
  mutate(sample_date = mdy(date)) %>% 
  select(station
         , sample_date
         , time
         , secchi
         , turbidity_surface 
         , turbidity_bottom
         ,sp_cnd_surface 
         ,sp_cnd_bottom
         , wt_surface 
         , wt_bottom
         ,do_surface
         ,do_bottom 
  ) %>% 
  glimpse()

#look at earliest date that each of these parameters was collected
wq_format_min <- wq_format %>% 
  #convert wide to long; probably easier that way
  pivot_longer(cols= c(secchi:do_bottom), names_to = "parameter", values_to = "value") %>% 
  #drop rows with NAs for value
  filter(!is.na(value)) %>% 
  group_by(parameter) %>% 
  summarize(year_min = min(sample_date))
#bottom measurements started in 2017 so can't use those
#but might be good to see how correlated bottom and top measurements are
#all other parameters go back to 1975 so they probably work, as least for stations where WQ and benthic overlap

#only keep the WQ parameters that, more or less, cover the full benthic time series
wq_format_focus <- wq_format %>%
  #add month column
  mutate(month = month(sample_date)
        , year = year(sample_date)) %>% 
  select(station
         #, sample_date #drop because different from benthic sample dates
         , month
         , year
         , secchi
         , turbidity_surface 
         ,sp_cnd_surface 
         , wt_surface 
         ,do_surface
  ) %>% 
  glimpse()


#Combine benthic invert and WQ data------------------
#need to do a bit of work to match up station names; they're similar but not the same generally
#start with the full benthic data set (with zeros) instead of just the selected stations
#we want to look at where the target taxa are broadly in the environmental gradients

#add column with benthic station names that does not include R, L, C
#then the WQ and benthic stations will match
benthic_cpue_mod <- benthic_cpue_with_zeros %>% 
  #add column that drops the last two characters of the benthic station names 
  #so they will match the WQ station names
  mutate(station = substr(station_code,1,nchar(station_code)-2)
         #add month which will be used to match sampling months between benthic and wq
         ,month = month(sample_date)
         ,year=year(sample_date)
  ) %>% 
 glimpse()

#combine benthic and WQ stations
#should match by station, year, month
bwq <-left_join(benthic_cpue_mod,wq_format_focus) %>% 
  #change organism code from numeric to character
  #so it matches with cpue_common data frame
  mutate(organism_code=as.character(organism_code)) %>% 
  glimpse()

#next filter cpue data to just that of taxa common (>10% samples) in the three long term stations
#first combine main data frame with the proportion of samples data frame
#should combine by organism_code
bwp <- inner_join(bwq, cpue_common) %>% 
  #create a new column that rounds temperature to nearest degree
  mutate(wt_surface_r = round(wt_surface,0)) 

unique(bwp$wt_surface_r) 
#why are there NAs for the rounded temp?
#probably cases in which WQ not collected in month of benthic data 
#but could be other reason

#look at rows with NA for temp
temp_na <- bwp %>% 
  filter(is.na(wt_surface_r))
#are are a whole group of benthic visits with no WQ
#look closer at those

#which visits are missing WQ?
wq_miss <- bwp %>% 
  #start with focus on temp but should filter to where any WQ parameter is NA
  filter(is.na(wt_surface)) %>% 
  #look at visits with NA
  distinct(station, sample_date) %>%  #664 visits
  #arrange(sample_date, station) %>% 
  group_by(station) %>% 
  count()
#mostly D24, C9, D41A so look into those
#are the WQ data missing or just not matched up to the benthic data well?


#a couple quick checks
range(bwp$prop) 
#0.1000715 0.7698356
#looks right

ctax <- unique(bwp$organism_code)
#got the right number of taxa (n=42)

#plot distributions by taxa and wq parameter-----------------------
#need to spend some time figuring out how to do this with map functions

#calculate mean CPUE by temperature for each taxa
btemp <- bwp %>% 
  group_by(organism_code,wt_surface_r) %>%
  summarize(spp_temp_mean = mean(mean_cpue))
#there are NAs in the temperature column but not sure why; figure that out

#plot temperature
ggplot(btemp,aes(x=wt_surface_r, y=spp_temp_mean))+
  geom_bar(stat="identity")+
  facet_wrap(vars(organism_code),scales="free",nrow=6)+
  ggtitle("water temperature")
#Warning message:Removed 42 rows containing missing values (position_stack)
#probably because of the temp = NA which I need to figure out
#are bimodal distributions indicative of multiple spp lumped together?
#consider also plotting proportion of samples at each temperature
#consider making another panel of plots for rarer taxa, which might show a stronger 
#response than the most common taxa (eg, >5% but <10% of samples)

#calculate 95th percentile temperature for each taxa
btemp_q95 <- bwp %>% 
  group_by(organism_code) %>% 
  summarize(temp_q95 = quantile(wt_surface, probs=0.95, na.rm=T))

#now plot the distribution of the 95th quantiles across taxa
ggplot(btemp_q95, aes(x=temp_q95))+
  geom_histogram()

#consider looking at physiological limits for all Bay-Delta taxa regardless of how common they are
#to see how the distribution of thermal tolerances 

