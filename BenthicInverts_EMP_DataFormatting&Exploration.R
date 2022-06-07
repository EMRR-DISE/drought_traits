#Special Studies Drought Synthesis
#Environmental Monitoring Program data
#Benthic Invertebrate Survey data

#Nick Rasmussen, nicholas.rasmussen@water.ca.gov

# Metadata notes ---------------------------
#1975 - October 2020
#Catch per unit effort (CPUE)
#Organisms/m2 = (sum per visit /# of grabs per visit)/(0.052 grabs/meter squared)
#GRTS amphipod papers says ponar sample was to a depth that varied from 4-10 cm with sediment type

# Load required packages -----------------
#all packages available on CRAN except deltamapr
#see GitHub repository for deltamapr at https://github.com/InteragencyEcologicalProgram/deltamapr

library(tidyverse) #suite of data science tools
library(contentid) #reduce time of reloading large data sets
library(janitor) #used to quickly clean up column names
library(lubridate) #format date
library(sf) #work with spatial data like shapefiles
library(deltamapr) #Bay-Delta spatial data
#library(readxl) #importing data from excel files
library(waterYearType) #lists all water year types 1901 - 2017
library(here)

# To do list -----------------------------

#add water year type to abundance data (note that invert data categorized by calendar year)

#figure out why so many NAs for matches between invert cpue and wq means

#Note: need to confirm with Betsy that is is reasonable to assume that all taxa were searched for in all 
#samples through time

#use map functions to plot multiple panels of wq vs abundances

#research ways to exclude rare taxa
#consider plotting the proportion of taxa retained vs proportion of samples occupied
#eg, going from at least 5% of samples to at least 10% of samples
#dropped from 243 to 42 taxa; dropped from 26.7% taxa retained to 17.3% of taxa retained
#rare could be based on proportion of presence in samples
#or number of individuals out of all individuals
#or x% of species with lowest abundances
#proportion of maximum: x% of max abundance of most common species
#will drop out some management relevant species because they are rare


# Read in the data-----------------------

#most of what I need is published on EDI
#https://portal.edirepository.org/nis/metadataviewer?packageid=edi.1036.1
#Betsy said next version will likely be available in June 2022
#it will correct some issues with existing data and add the 2021 data

#station data
benthic_stn <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.1036.1&entityid=4e6948186ad756dc2b6de4de41b601f3") %>% 
  clean_names() %>% 
  glimpse()

#benthic invert CPUE
#data have been converted to CPUE (organisms/m2)
#replicate grabs have been averaged for each site visit
#all non-occurrence (zero) data for a site visit has been removed
#Nick: samples with no organisms at all are probably included as "No catch"
#Nick: there's an issue with the data where all rows are duplicated a number of times equal to the number of grabs per visit
benthic_cpue <- read_csv(resolve(store("https://portal.edirepository.org/nis/dataviewer?packageid=edi.1036.1&entityid=1dfeb0e5a58f2167faca4087b717aae4"))) %>% 
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
benthic_wq <- 
  read_csv(
    resolve(store("https://portal.edirepository.org/nis/dataviewer?packageid=edi.458.4&entityid=98b400de8472d2a3d2e403141533a2cc")),
    col_types = cols_only(
      Station = "c",
      Date = col_date("%m/%d/%Y"),
      Time = "t",
      Secchi = "c",
      TurbiditySurface = "c",
      TurbidityBottom = "c",
      SpCndSurface = "c",
      SpCndBottom = "c",
      WTSurface = "c",
      WTBottom = "c",
      DOSurface = "c",
      DOBottom = "c"
    )
  ) %>% 
  clean_names() %>% 
  glimpse()
# turbidity_surface has two ND values at MD10A - not sure if this matters; if it
  # does, we'll need to decide if we're okay with substituting 0 or some other
  # number for these

#EMP water quality stations
wq_stn <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.458.4&entityid=827aa171ecae79731cc50ae0e590e5af") %>% 
  clean_names() %>% 
  glimpse()

#Water year type from waterYearType package
#the only function in package brings in water year type data frame
water_year <- water_year_indices
#glimpse(water_year) #looks like column types are correct

# Read in region shapefile
region_shape <- read_sf(here("spatial_files/region.shp"))
# Is this region shapefile in deltamapr? If so, we can call it directly in the
  # code by name instead of importing a local copy.


# Filter the stations based on time series completeness ------------
# Note: Decided to filter data temporally before spatially that way, we can map
  # stations that have long time series to see their distribution before spatial
  # filter

#ie, stations that include mostly continuous sampling 1975-present

#count the number of years with visits by station
#this is a bit oversimplified because visits per year also vary
#but it's a good start
visit_years <- benthic_visits %>%
  #drop years with no visits
  filter(number_of_site_visits != 0) %>%
  #look at number of years with visits by station
  count(station_code, name = "years")

#plot histogram to see distribution of number of years with visits for sites
ggplot(visit_years, aes(x=years))+
  geom_histogram()
#very few stations with 40+ years of data, as expected

#filter out any stations with less than 40 years of data
sta_visits_many <- visit_years %>% 
  filter(years >= 40) %>% 
  pull(station_code)

sta_visits_many
#only three stations left: "D28A-L" "D4-L"   "D7-C" 

#now take a closer look at effort for those mostly complete stations - 
#just keep the three needed stations
visits_many_eff <- benthic_visits %>% filter(station_code %in% sta_visits_many)

#plot effort by station
ggplot(visits_many_eff,aes(x = year, y = number_of_site_visits))+
  geom_point()+
  geom_line()+
  facet_grid(station_code~.)
#after about 1980, sampling is pretty consistently high except for 2004-2005

#now filter the full abundance data set using number of years of visits as criterion
#only keep samples from stations with at least 40 years of visits
benthic_cpue_f <- benthic_cpue %>% filter(station_code %in% sta_visits_many)

#make sure the three stations, and only the three stations, are retained
unique(benthic_cpue_f$station_code)
#yes, "D4-L"   "D7-C"   "D28A-L"


# Filter stations spatially -----------
#Note should add some more checks of latitude/longitude columns before filtering
#eg, see if any coordinates fall outside Bay-Delta, indicating error in coordinates
#already checked for NAs in lat/long

#Only keep the stations that fall within our spatial region
#as defined by the region shapefile

# Check to see if station coordinates in metadata and cpue tables match
benthic_stn_coord <- benthic_stn %>% 
  select(
    station_code = site_code, 
    latitude, 
    longitude
  )

benthic_cpue_coord <- benthic_cpue %>% distinct(station_code, latitude, longitude)

setequal(benthic_cpue_coord, benthic_stn_coord)
# FALSE - look for where records don't match
sta_coord_match <- 
  full_join(
    benthic_stn_coord, 
    benthic_cpue_coord,
    by = "station_code",
    suffix = c("_meta", "_cpue")
  ) %>% 
  mutate(
    lat_equal = latitude_meta == latitude_cpue,
    long_equal = longitude_meta == longitude_cpue
  )

sta_coord_match %>% filter(lat_equal == FALSE | long_equal == FALSE | is.na(lat_equal) | is.na(long_equal))
# D7-C is missing coordinates in the cpue table
# 4 other stations have coordinates that don't match between the tables,
# however, the values are practically identical, so it doesn't matter.
# We'll use the benthic_stn table to define station coordinates

#start by looking at data frame with just the station metadata
#add geometry column 
benthic_stn_g <- benthic_stn %>% 
  #specify the crs which the metadata online says is wgs84
  st_as_sf(coords = c(x='longitude',y='latitude'), #identify the lat/long columns
           crs = 4326 #EPSG code for WGS84
           ,remove=F #retains original lat/long columns
  ) %>%  
  #make a combined station status - sampling effort column; needed for color coding in map below
  mutate(
    sample_effort = if_else(site_code %in% sta_visits_many, "LongPOR", "ShortPOR"),
    status_effort = paste(status, sample_effort, sep = "_")
  ) %>% 
  glimpse()

# Dissolve region shapefile to just the outside perimeter, removing subregions
region_shape_diss <- region_shape %>% 
  # Convert to WGS 84, UTM zone 11N
  st_transform(crs = 32611) %>% 
  # Add a 0.5 meter buffer to remove slivers within perimeter
  st_buffer(0.5) %>%
  # Dissolve subregions
  st_union()

#Convert coordinate reference system (CRS) of basemap and benthic_stn_g to 32611
WW_Delta_32611 <- st_transform(WW_Delta, crs = 32611)
benthic_stn_g_32611 <- st_transform(benthic_stn_g, crs = 32611)

#map all stations
(map_benthic <-ggplot()+
    #CDFW Delta waterways
    geom_sf(data= WW_Delta_32611, fill= "skyblue3", color= "black", alpha = 0.6)+
    #region perimeter
    geom_sf(data =region_shape_diss, alpha = 0.4, size = 1, color = "red", fill = "grey") +
    #all stations
    geom_sf(data =benthic_stn_g_32611, aes(fill = status_effort), shape = 22, size = 2.5) +
    #add title
    ggtitle("All Benthic Invert Stations")+
    theme_bw()
)

#filter stations to just those within the shapefile bounds
stn_within_reg <- benthic_stn_g_32611 %>% 
  st_filter(region_shape_diss) %>% 
  pull(site_code)
#drops two rows as expected (ie, the two San Pablo Bay stations)

#next filter the survey data set to stations within the shapefile bounds
benthic_cpue_stf <- benthic_cpue_f %>% filter(station_code %in% stn_within_reg)


# Add zeros for absences back into abundance data set-------------------------

#these zeros were excluded from EDI data set, presumably to reduce total number of rows
#need to include zeros in the calculations of annual mean CPUE below
#probably easiest to convert to wide form and then back to long form
#Note: need to confirm with Betsy that is is reasonable to assume that all taxa were searched for in all 
#samples through time

#start with data set already filtered spatially and temporally
benthic_cpue_stfz <- benthic_cpue_stf %>% 
  #drop unneeded columns
  select(sample_date,station_code, organism_code,mean_cpue) %>% 
  #sort by organism code so when I replace NA with zero the starting column is always the same
  arrange(organism_code) %>%     
  #only keep unique rows; current EDI data set has an error so lots of rows are duplicated
  distinct(sample_date,station_code, organism_code,mean_cpue) %>% 
  #make data frame wide which will add NA every time an organism was not detected in a visit 
  pivot_wider(id_cols = c(sample_date,station_code),names_from = organism_code,values_from=mean_cpue) %>%   
  #replace NAs with zeros
  mutate(across(where(is.numeric), ~replace_na(.,0))) %>% 
  #make long version again
  pivot_longer(where(is.numeric), names_to = "organism_code", values_to = "mean_cpue") %>% 
  glimpse()

  
#number of visits differs among years
#which month(s) are most consistently sampled through time?
vmonth <- benthic_cpue_stfz %>% 
  mutate(month=month(sample_date)
         ,year=year(sample_date)
         ) %>% 
  distinct(station_code, year, month) %>%  #1389 instead of 1399, not sure why the difference
  group_by(station_code,month) %>% 
  summarise(n=n()) %>% 
  arrange(station_code,-n)
#each month has 35-43 years of visits; October is most sampled month

# Remove rare taxa -------------------

#NOTE: should determine what proportion of bay-delta wide taxa are represented
#in the three stations I have kept

#try various approaches for excluding rare taxa
#how many taxa remain if only keeping those that are present in: 
# just drop a fixed proportion of species (eg, least abundant 25% of spp)
        #this approach keeps too many taxa: 183 remain of 243 spp (75% of spp)
# comprise at least 5% of total abundances
        #keeps too few taxa: 7 taxa remain out of 243 (2.9% of taxa remaining)
# remove those below 5% of abundance of the most abundant spp
        #keeps too few taxa: 14 spp remain out of 243 (5.8% of spp)
# found in at least 5% of samples
        #keeps quite a few: 65 of 243 taxa remaining; 26.7% taxa retained
# found in at least 10% of samples
        #keeps a manageable number: 42 of 243 taxa remaining; 17.3% of taxa retained

#calculate total number of individuals
total_individuals <- sum(benthic_cpue_stfz$mean_cpue) 

#calculate proportion of total individuals each spp comprises
cpue_indiv_prop <- benthic_cpue_stfz %>%
  group_by(organism_code) %>% 
  #row counts by taxon now should be number of samples with the taxon
  summarise(n_indiv=sum(mean_cpue)) %>% 
  #order rows by number of samples, most to least
  arrange(-n_indiv) %>% 
  mutate(prop = n_indiv/total_individuals
         ,rank = as.numeric(n():1)
         ,rank_prop = rank/n()
         #determine abundance of most abundant taxa
         ,prop_dom = n_indiv/max(n_indiv)
         ) %>% 
  glimpse()

#plot histogram of proportion of individuals each ssp comprises
ggplot(cpue_indiv_prop, aes(x=prop))+
  geom_histogram()

#plot histogram of proportion of individuals for each spp relative to
#the most abundant spp
ggplot(cpue_indiv_prop, aes(x=prop_dom))+
  geom_histogram()
#almost all species are rare compared to most abundant spp (>5% dominant spp abundance)

#drop the 25% of species with lowest total individuals
cpue_rarest25 <- cpue_indiv_prop %>% 
  filter(rank_prop > 0.25)
#keeps 183 of 243 species (75.3% of taxa remaining)

#drop all ssp that comprise less than 5% of all individuals
cpue_indiv_5plus <- cpue_indiv_prop %>% 
  filter(prop > 0.05)
#only 7 taxa remain out of 243 (2.9% of taxa remaining)

#drop all ssp with abundances less that 5% of that of most abundant spp
cpue_prop_dom_5plus <- cpue_indiv_prop %>% 
  filter(prop_dom > 0.05)
#only 14 spp remain out of 243 (5.8% of spp)

#need to calculate total number of samples
total_samples <- benthic_cpue_stfz %>% 
  #look at distinct combinations of station and date
  distinct(sample_date, station_code) %>%  
  count()
samp_denom <-as.numeric(total_samples[1,1]) #1399

#calculate the proportion of samples in which each taxon is present
cpue_sample_prop <- benthic_cpue_stfz %>%
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
#or use a join function to just keep the taxa that are in more than 10% of samples
benthic_cpue_stfz_prop <- left_join(benthic_cpue_stfz,cpue_sample_prop) %>% 
  #only keep the taxa that appear in at least 10% of taxa
  filter(prop>0.10) %>%
  #drop unneeded columns
  select(-c(n_samp,prop)) %>% 
  #add year column back in
  mutate(year=year(sample_date)) %>%
  #sort by date, station, organism code
  arrange(sample_date, station_code, organism_code)

# Calculate annual mean CPUE------------------

#could use a different summary statistic (eg, median)
#consider changing this to water year instead of calendar year

#generate annual mean CPUE values for each taxon
cpue_mean_annual <- benthic_cpue_stfz_prop %>% 
  group_by(year,station_code,organism_code) %>% 
  summarize(cpue_annual=mean(mean_cpue))
#keep in mind that number of samples per station varies among years
#probably should drop years with, say, only one sample because hard to compare with other years
#could also just use data for the sampling month that is best represented across all years (Oct)
#instead of calculating annual means based on a variable number of samples

# Format Table L (station-year x taxon) -----------

#make Table L which is taxon x sample matrix
TableL <- cpue_mean_annual %>% 
  pivot_wider(id_cols = c(station_code,year),names_from = organism_code,values_from=cpue_annual)  
#probably just need to now combine station and year into a row name and format as matrix

#make a big faceted plot showing time series of each taxon in each station
ggplot(cpue_mean_annual,aes(x=year, y=cpue_annual, group=station_code,color=station_code))+
  geom_point()+
  geom_line()+
  facet_wrap(vars(organism_code),scales="free",nrow=6)

# Explore water quality data----------------

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


# Combine benthic invert and WQ data------------------
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
  mutate(wt_surface_r = round(wt_surface,0)
         ,do_surface_r = round(do_surface,0)
         ) 

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

# Plot distributions by taxa and wq parameter-----------------------
#need to spend some time figuring out how to do this with map functions

#calculate mean CPUE by temperature for each taxa
btemp <- bwp %>% 
  group_by(organism_code,wt_surface_r) %>%
  summarize(spp_temp_mean = mean(mean_cpue))
#there are NAs in the temperature column but not sure why; figure that out
bdo <- bwp %>% 
  group_by(organism_code,do_surface_r) %>%
  summarize(spp_do_mean = mean(mean_cpue))

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

ys1 <- c("mpg","cyl","disp")
ys1 %>% map(function(y) 
  ggplot(mtcars, aes(hp)) + geom_point(aes_string(y=y)))


#use map function 
ys <- c(
  #"secchi","turbidity_surface", "sp_cnd_surface",
  "wt_surface_r","do_surface_r" )

ys %>% map(function(y) 
  ggplot(bwp, aes(mean_cpue)) + geom_point(aes_string(y=y)))

#calculate 95th percentile temperature for each taxa
btemp_q95 <- bwp %>% 
  group_by(organism_code) %>% 
  summarize(temp_q95 = quantile(wt_surface, probs=0.95, na.rm=T))

#now plot the distribution of the 95th quantiles across taxa
ggplot(btemp_q95, aes(x=temp_q95))+
  geom_histogram()

#consider looking at physiological limits for all Bay-Delta taxa regardless of how common they are
#to see how the distribution of thermal tolerances 

