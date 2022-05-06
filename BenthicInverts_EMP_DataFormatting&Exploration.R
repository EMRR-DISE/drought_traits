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
unique(visits_many$station_code)

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
  #make data frame wide which will add NA every time an organism was detected in a visit 
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

#try various approaches for excluding rare taxa
#how many taxa remain if only keeping those that are present in 
#at least 5% of samples? 10% of samples?

#need to calculate total number of samples
total <- cpue_oldest %>% 
  #look at distinct combinations of station and date
  distinct(sample_date, station_code) %>%  
  count()
samp_denom <-as.numeric(total[1,1]) #1399

#STOPPED UPDATING SCRIPT WITH DATA SET THAT INCLUDES ZEROS HERE------------------
#need to continue working on that below

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

#explore water quality data----------------

#reduce data set to just the columns of interest
glimpse(benthic_wq)

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

wq_format_focus <- wq_format %>%
  #add month column
  mutate(month = month(sample_date)
        , year = year(sample_date)) %>% 
  select(station
         , sample_date
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
#match benthic and wq stations
#but first need to match up names; they're similar but not the same generally

#add column with benthic station names that does not include R, L, C
#I think then the WQ and benthic stations will match
benthic_cpue_mod <- benthic_cpue %>% 
  #drop month column with name
  select(-month) %>% 
  #add column that drops the last two characters of the benthic station names so they will match the WQ station names
  mutate(station = substr(station_code,1,nchar(station_code)-2)
         ,month = month(sample_date)
         ) %>% 
  #drop the date column so R doesn't try to match the non-matching dates for benthic vs wq data
  select(-sample_date) %>% 
  glimpse()

#combine benthic and WQ stations
#should match by station, year, month
bwq <-left_join(benthic_cpue_mod,wq_format_focus)

#next filter cpue data to just that of taxa common (>10% samples) in the three long term stations
#first combine main data frame with the proportion of samples data frame
#should combine by genus, species, organism_code
bwp <- inner_join(bwq, cpue_common) 

#a couple quick checks
range(bwp$prop) 
#0.1000715 0.7698356
#looks right

ctax <- unique(bwp$organism_code)
#got the right number of taxa (n=42)

#plot distributions by taxa and wq parameter-----------------------
#need to spend some time figuring out how to do this with map functions

#count number of samples containing a given taxa across range of temperatures
btemp <- bwp %>% 
  #create a new column that rounds temperature to nearest degree
  mutate(wt_surface_r = round(wt_surface,0)) %>% 
  group_by(organism_code,wt_surface_r) %>% 
  count()

#plot temperature
ggplot(btemp,aes(x=wt_surface_r, y=n))+
  geom_bar(stat="identity")+
  facet_wrap(vars(organism_code),scales="free",nrow=6)+
  ggtitle("water temperature")
#are these plots telling me that the taxa are not frequently found at high temps
#or that high temp samples are just not commonly observed?
#maybe useful to plot the mean CPUE at each temperature level rather than just frequency
#if CPUEs are lower at higher temps that might be better evidence
#if CPUEs are about as high at high temps as more moderate temps, then lower frequency at higher
#temps just means there are fewer samples at those temps

#calculate 95th percentile temperature for each taxa
btemp_q95 <- bwp %>% 
  group_by(organism_code) %>% 
  summarize(temp_q95 = quantile(wt_surface, probs=0.95, na.rm=T))

#now plot the distribution of the 95th quantiles across taxa
ggplot(btemp_q95, aes(x=temp_q95))+
  geom_histogram()

#consider looking at physiological limits for all Bay-Delta taxa regardless of how common they are
#to see how the distribution of thermal tolerances 

