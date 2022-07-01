#Special Studies Drought Synthesis
#Environmental Monitoring Program data
#Benthic Invertebrate Survey data

#Nick Rasmussen, nicholas.rasmussen@water.ca.gov

# Metadata notes ---------------------------
#1975 - 2021
#Catch per unit effort (CPUE)
#Organisms/m2 = (sum per visit /# of grabs per visit)/(0.052 grabs/meter squared)
#GRTS amphipod papers says ponar sample was to a depth that varied from 4-10 cm with sediment type

# Load required packages -----------------
#all packages available on CRAN except deltamapr
#see GitHub repository for deltamapr at https://github.com/InteragencyEcologicalProgram/deltamapr

library(data.table) #matches data by nearest date 
#NOTE: data.table masks some useful tidyverse functions so load first
library(tidyverse) #suite of data science tools
library(contentid) #reduce time of reloading large data sets
library(janitor) #used to quickly clean up column names
library(lubridate) #format date
library(sf) #work with spatial data like shapefiles
library(geosphere) #matching stations by distance
library(leaflet) #make interactive maps
library(ggrepel) #no-noverlapping point labels on maps
library(deltamapr) #Bay-Delta spatial data
#library(waterYearType) #lists all water year types 1901 - 2017
library(here)

# To do list -----------------------------

#make sure there isn't overlap in taxa between zoop and benthic inverts
#eg, I saw ostracods in benthic inverts data which are probably also in zoop

#check functions in smonitor package for code that will automate process of checking for
#data set updates on EDI

#add water year type to abundance data (note that invert data categorized by calendar year)

#figure out why so many NAs for matches between invert cpue and wq means

#Note: need to confirm with Betsy that is is reasonable to assume that all taxa were searched for in all 
#samples through time

#use map functions to plot multiple panels of wq vs abundances

# Read in the data-----------------------

#most of what I need is published on EDI
#https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=1036&revision=2

#station data
benthic_stn <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.1036.2&entityid=4e6948186ad756dc2b6de4de41b601f3") %>% 
  clean_names() %>% 
  glimpse()

#benthic invert CPUE, 1975-2021
#data have been converted to CPUE (organisms/m2)
#replicate grabs have been averaged for each site visit
#all non-occurrence (zero) data for a site visit has been removed
#Nick: samples with no organisms at all are probably included as "No catch"
benthic_invert_cpue <- read_csv(resolve(store("https://portal.edirepository.org/nis/dataviewer?packageid=edi.1036.2&entityid=df1caeb717202f06171601f793ca46bf"))) %>% 
  clean_names() %>% 
  glimpse()

#organism key list
benthic_spp <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.1036.2&entityid=d0f0dd3c1835fe5b669342f8c8e77024")

#total annual site visits, 1975-2021
benthic_visits <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.1036.2&entityid=304bc4562046e0a6c35fbad3e2c85645") %>% 
  clean_names() %>% 
  glimpse()

#total annual grab samples, 1975-2021
benthic_grabs <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.1036.2&entityid=c6c7b2ed7165cfa93cc1eda43fbb29f5") %>% 
  clean_names() %>% 
  glimpse()

#EMP water quality data
benthic_wq <- 
  read_csv(
    resolve(store("https://portal.edirepository.org/nis/dataviewer?packageid=edi.458.6&entityid=dfeaee030be901ae00b8c0449ea39e9c")),
    col_types = cols_only(
      Station = "c",
      Date = col_date("%Y-%m-%d"),
      Time = "t",
      Secchi = "d",
      TurbiditySurface_FNU = "d", #some non-detects present
      TurbiditySurface_NTU = "d", #some non-detects present
      SpCndSurface = "d",
      WTSurface = "d",
      DOSurface = "d",
    )
  ) %>% 
  clean_names() %>% 
  glimpse()

glimpse(benthic_wq)
glimpse(benthic_cpue)
# turbidity_surface has two ND values at MD10A - not sure if this matters; if it
  # does, we'll need to decide if we're okay with substituting 0 or some other
  # number for these

#EMP water quality stations
wq_stn <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.458.4&entityid=827aa171ecae79731cc50ae0e590e5af") %>% 
  clean_names() %>% 
  glimpse()

#Water year type from waterYearType package
#the only function in package brings in water year type data frame
#water_year <- water_year_indices
#glimpse(water_year) #looks like column types are correct

# Read in region shapefile
region_shape <- read_sf(here("spatial_files/region.shp"))


#Look at no catch samples ------------------------

#look at all no catch samples in whole data set
benthic_cpue_no_catch_all <- benthic_invert_cpue %>% 
  filter(organism_code == "0") %>% 
  distinct(station_code,sample_date,organism_code,mean_cpue)
#5 samples and all have non-zero CPUE for no catch
#Betsy said the cpues are just meaningless placeholder numbers
#should change the cpues to zeros,
#at least for those that are true no catch samples

#look closer at five samples with no catch
#need to specify that the filter date is a date because in dataset it is a date-time
#probably not the most efficient way to code this
#asked Betsy about these samples with no catch entries
no_catch_visits <- benthic_invert_cpue %>% 
  filter(
    #the first three samples include erroneous no catch entries
    #just drop the no catch rows for these
    (station_code == "D7-C" & sample_date == as.Date("2012-05-24")) |
      (station_code == "P8-R" & sample_date == as.Date("2012-07-18" )) |
      (station_code == "C9-L" & sample_date == as.Date("2013-04-11" )) |
      #one of four grabs was no catch just need to drop the no catch row
      (station_code == "D16-L" & sample_date == as.Date("2015-09-15" )) |
      #a true no catch sample; only one row as expected (ie, no catch)
      #keep this in data set
      (station_code == "D24-L" & sample_date == as.Date("2017-02-22" ))
  )

#write the csv file with the confusing no catch samples to send to Betsy
#write_csv(no_catch_visits,"./BenthicInverts/benthic_inverts_no_catch_samples.csv")

#filtered out the four unneeded no catch rows
#NOTE: one of the five no catch samples was a true no catch D24-L on 2017-02-22 so not filtered out
benthic_cpue <- benthic_invert_cpue %>% 
  filter(
    !(
      (station_code == "D7-C" & sample_date == as.Date("2012-05-24") & organism_code == "0") |
        (station_code == "P8-R" & sample_date == as.Date("2012-07-18") & organism_code == "0") |
        (station_code == "C9-L" & sample_date == as.Date("2013-04-11") & organism_code == "0") |
        (station_code == "D16-L" & sample_date == as.Date("2015-09-15") & organism_code == "0") 
    )
  )
#count(benthic_invert_cpue)-count(benthic_cpue) 
#looks like it removed the four rows as expected

# Filter the stations based on time series completeness ------------
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
#Only keep the stations that fall within our spatial region
#as defined by the region shapefile
#NOTE: for benthic inverts, no additional station filtering will occur
#because all three within our focal region

# Check to see if station coordinates in metadata and cpue tables match
benthic_stn_coord <- benthic_stn %>% 
  select(
    station_code, 
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
# 4 stations have coordinates that don't match between the tables,
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
    sample_effort = if_else(station_code %in% sta_visits_many, "LongPOR", "ShortPOR"),
    status_effort = paste(status, sample_effort, sep = "_")
  ) %>% 
  glimpse()

#Convert coordinate reference system (CRS) of basemap and benthic_stn_g to EPSG = 26910
WW_Delta_26910 <- st_transform(WW_Delta, crs = 26910)
benthic_stn_g_26910 <- st_transform(benthic_stn_g, crs = 26910)

#map all stations
(map_benthic <-ggplot()+
    #CDFW Delta waterways
    geom_sf(data= WW_Delta_26910, fill= "skyblue3", color= "black", alpha = 0.6)+
    #region perimeter
    geom_sf(data =region_shape, alpha = 0.4, size = 1, color = "red", fill = "grey") +
    #all stations
    geom_sf(data =benthic_stn_g_26910, aes(fill = status_effort), shape = 22, size = 2.5, alpha = 0.8) +
    #add title
    ggtitle("All Benthic Invert Stations")+
    theme_bw()
)

#filter stations to just those within the shapefile bounds
stn_within_reg <- benthic_stn_g_26910 %>% 
  st_filter(region_shape) %>% 
  pull(station_code)
#drops two rows as expected (ie, the two San Pablo Bay stations)

#next filter the survey data set to stations within the shapefile bounds
benthic_cpue_stf <- benthic_cpue_f %>% filter(station_code %in% stn_within_reg)

#check which stations are included
unique(benthic_cpue_stf$station_code)
#three stations as expected: "D4-L"   "D7-C"   "D28A-L"


# Add zeros for absences back into abundance data set-------------------------

#these zeros were excluded from EDI data set, presumably to reduce total number of rows
#need to include zeros in the calculations of annual mean CPUE below
#Note: need to confirm with Betsy that is is reasonable to assume that all taxa were searched for in all 
#samples through time

#start with data set already filtered spatially and temporally
benthic_cpue_stfu <- benthic_cpue_stf %>% 
  #simplify df to just the needed columns
  select(station_code, sample_date, organism_code, mean_cpue) %>% 
  # Convert organism_code to character since it represents discrete organisms
  mutate(organism_code = as.character(organism_code)) %>% 
  glimpse()

# Pull out one record with organism_code = 0 (no catch) and add in all
  # organism_codes along with mean_cpue = 0 to indicate no catch
# NOTE: The mean_cpue for this one record without catch is 4.81 - this doesn't seem
  # correct, may need to look into this
#checked with Betsy - she gave no catch rows a CPUE value just so they didn't get
#filtered out; for these rows, CPUE value is meaningless and should be zero
benthic_cpue_no_catch <- benthic_cpue_stfu %>% 
  filter(organism_code == "0") %>% 
  complete(
    sample_date,
    station_code,
    organism_code = unique(benthic_cpue_stfu$organism_code),
    fill = list(mean_cpue = 0)
  ) %>% 
  # Don't include record for "no catch" in the end
  filter(organism_code != "0")

# Fill in zeros for absent organisms within each sample
benthic_cpue_stfz <- benthic_cpue_stfu %>% 
  # Pull out record with "no catch"
  filter(organism_code != "0") %>% 
  complete(nesting(sample_date, station_code), organism_code, fill = list(mean_cpue = 0))  %>% 
  # Add back record with "no catch"
  #for now, not adding this back in
  #there's an issue with this no catch sample in the original data set
  #even though it is no catch there are still CPUEs for other taxa
  #as a result, adding this back in create duplicates
  #once I hear back from Betsy on how to handle this, I'll fix it
  #bind_rows(benthic_cpue_no_catch)
  glimpse()

#look at no catch sample
no_catch_d7 <- benthic_cpue_stfz %>% 
  filter(station_code == "D7-C" & sample_date =="2012-05-24" )
#extra set of rows for D7-C from 5/24/12

#look at number of distinct combinations of date, station, organism, cpue
#should be same number of rows as benthic_cpue_stfz
benthic_cpue_stfz2 <- benthic_cpue_stfz %>% 
  distinct(sample_date, station_code, organism_code, mean_cpue)
count(benthic_cpue_stfz)-count(benthic_cpue_stfz2)
#zero, now that I commented out code above that adds back in rows of zero CPUE
#for the supposed no catch sample (D7-C from 5/24/12) 

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

#what proportion of bay-delta wide taxa are represented
#in the three stations I have kept
spp_all <- unique(benthic_cpue$organism_code)
#408 taxa in data set; 243 in those three stations
#so 59.6% of Bay-Delta taxa captured by our stations

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
#Note that sampling not equal across years, which potentially causes problems
#with using the presence in x% of samples metrics
#eg, lower sampling early in series could bias against taxa that were only common
#in early years

#should look at which taxa are retained across methods. are the most common ones the same?

#calculate abundance metrics by taxon
cpue_indiv_prop <- benthic_cpue_stfz %>%
  group_by(organism_code) %>% 
  #sum cpue across all samples by taxon
  summarise(indiv_n=sum(mean_cpue)) %>% 
  #order rows by total cpue, most to least
  arrange(-indiv_n)  %>% 
  mutate(
    #calculate the proportion of all individuals that a taxon comprises
    indiv_prop = indiv_n/sum(indiv_n)
    #rank all taxa by total cpue and determine their quantile
    ,rank_prop = as.numeric(n():1)/n()
    #determine the taxon with highest total cpue and 
    #then calculate the proportion of this max cpue each taxon comprises 
    ,dom_prop = indiv_n/max(indiv_n)
         ) %>% 
  glimpse()

#plot histogram of proportion of individuals each ssp comprises
ggplot(cpue_indiv_prop, aes(x=indiv_prop))+
  geom_histogram()
#vast majority of taxa comprise less than 1.25% of total individuals

#plot histogram of proportion of individuals for each spp relative to
#the most abundant spp
ggplot(cpue_indiv_prop, aes(x=dom_prop))+
  geom_histogram()
#almost all species are rare compared to most abundant spp (<5% dominant spp abundance)

#drop the 25% of species with lowest total individuals
cpue_rarest25 <- cpue_indiv_prop %>% 
  filter(rank_prop > 0.25)
#keeps 183 of 243 species (75.3% of taxa remaining)

#drop all ssp that comprise less than 5% of all individuals
cpue_indiv_5plus <- cpue_indiv_prop %>% 
  filter(indiv_prop > 0.05)
#only 7 taxa remain out of 243 (2.9% of taxa remaining)

#drop all ssp with abundances less that 5% of that of most abundant spp
cpue_prop_dom_5plus <- cpue_indiv_prop %>% 
  filter(dom_prop > 0.05)
#only 14 spp remain out of 243 (5.8% of spp)

#need to calculate total number of samples
total_samples <- benthic_cpue_stfz %>% 
  #look at distinct combinations of station and date
  #necessary because there's a row for each taxon within each sample
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

#create vector of organism codes to then filter main df
organisms_common <- cpue_common %>% 
  pull(organism_code)

#combine proportion info with main data frame and then filter to those with more than 10%
#or use a join function to just keep the taxa that are in more than 10% of samples
benthic_cpue_stfz_com <- benthic_cpue_stfz %>% 
  filter(organism_code %in% organisms_common) %>% 
  #add year column back in
  mutate(year=year(sample_date)) %>%
  #sort by date, station, organism code
  arrange(sample_date, station_code, organism_code) %>% 
  glimpse()

# Calculate annual mean CPUE------------------

#could use a different summary statistic (eg, median)
#consider changing this to water year instead of calendar year,
#though sampling per year is probably based on calendar year

#generate annual mean CPUE values for each taxon
cpue_mean_annual <- benthic_cpue_stfz_com %>% 
  group_by(year,station_code,organism_code) %>% 
  summarize(cpue_annual=mean(mean_cpue))
#keep in mind that number of samples per station varies among years
#probably should drop years with, say, only one sample because hard to compare with other years

# Format Table L (station-year x taxon) -----------
# once this is done, it would be good to do some NMDS to see how communities compare
# among stations and among water year types; maybe work with Leela on that

#make Table L which is taxon x sample matrix
TableL <- cpue_mean_annual %>% 
  pivot_wider(id_cols = c(station_code,year),names_from = organism_code,values_from=cpue_annual)  
#probably just need to now combine station and year into a row name and format as matrix

#separate label columns from cpue columns for NMDS analysis
b_pred <- TableL[1:2]
b_dat <- TableL [3:44]

#write_csv(b_pred,"BenthicInverts/benthic_nmds_predictors.csv")
#write_csv(b_dat,"BenthicInverts/benthic_nmds_abundance_matrix.csv")

#make a big faceted plot showing time series of each taxon in each station
ggplot(cpue_mean_annual,aes(x=year, y=cpue_annual, group=station_code,color=station_code))+
  geom_point()+
  geom_line()+
  facet_wrap(vars(organism_code),scales="free",nrow=6)


# Explore water quality data----------------
#should carefully read over the metadata for this data set at some point
#should do more QAQC checks of WQ data before combining with benthic invert data
#including plots of WQ through time
#should look for outliers too
#WQ data aren't collected at same time as benthic samples
#so calculate how far apart in time benthic and WQ samples are for each pair

#map all the WQ stations and categorize them by status
#presumably lat/long in wq station metadata is WGS84 but EDI description doesn't specify
wq_stn_g <- wq_stn %>% 
  #drop the floating EZ stations; otherwise get an error because of NA for lat/long
  filter(!is.na(latitude) & !is.na(longitude)) %>% 
  #specify the crs (wgs84)
  st_as_sf(coords = c(x='longitude',y='latitude'), #identify the lat/long columns
           crs = 4326 #EPSG code for WGS84
           ,remove=F #retains original lat/long columns
  ) %>%  
  glimpse()

#Convert coordinate reference system (CRS) of basemap and benthic_stn_g to EPSG = 26910
wq_stn_g_26910 <- st_transform(wq_stn_g, crs = 26910)

#create object from bounding box for the stations
#add a buffer around points to improve map aesthetic
bbox_p <- st_bbox(st_buffer(wq_stn_g_26910,2000))

#map all stations
(map_wq <-ggplot()+
    #CDFW Delta waterways
    geom_sf(data= WW_Delta_26910, fill= "skyblue3", color= "black", alpha = 0.6)+
    #all stations
    geom_sf(data =wq_stn_g_26910, aes(fill = status), shape = 22, size = 2.5, alpha = 0.8) +
    #add station names as labels and make sure they don't overlap each other or the points
    geom_sf_label(data = wq_stn_g_26910, aes(x=longitude,y=latitude, label=station) #label the points
                     ,nudge_x = -0.008, nudge_y = 0.008 #can specify the magnitude of nudges if necessary
                     , size = 2 #adjust size and position relative to points
                     ,inherit.aes = F #tells it to look at points not base layer
    ) + 
    #zoom in on region where stations are located using bounding box
    coord_sf( 
      xlim =c(bbox_p$xmin,bbox_p$xmax)
      ,ylim = c(bbox_p$ymin,bbox_p$ymax)
    )+
    #add title
    ggtitle("All WQ Stations")+
    theme_bw()
)

#Examine spatial matches between WQ and benthic stations----------------
#probably just the stations between the two surveys with the most similar names

#create simplified df's of the station locations for both surveys
benthic_stn_ltm <- benthic_stn %>% 
  select(station_code,longitude,latitude) %>% 
  glimpse()

wq_stn_ltm <- wq_stn %>% 
  #drop the floating EZ stations which have NA for lat/long
  filter(!is.na(latitude) & !is.na(longitude)) %>% 
  select(station,longitude,latitude) %>% 
  glimpse()

  
# create distance matrix
#https://stackoverflow.com/questions/31668163/geographic-geospatial-distance-between-2-lists-of-lat-lon-points-coordinates/31668415#31668415
mat <- distm(benthic_stn_ltm[,c('longitude','latitude')], wq_stn_ltm[,c('longitude','latitude')], fun=distVincentyEllipsoid)

# assign the name to the point in list1 based on shortest distance in the matrix
benthic_stn_ltm$wq_station <- wq_stn_ltm$station[max.col(-mat)]
#as expected, the wq station with the name most similar to the benthic station name
#is pretty much always the closest in space
#exception: benthic D12-C is closest to WQ D14A
#though D14A is closer, the habitat of WQ D12 is still more similar to D12-C, which is still fairly close

#add distance between pairs of benthic and wq stations (in meters)
benthic_stn_ltm$near_dist_m <- mat[matrix(c(1:53, max.col(-mat)), ncol = 2)]

#sort stations by location for easy visual examination
benthic_stn_ltm <- benthic_stn_ltm %>% 
  arrange(longitude,latitude)

#range of distances between pairs
range(benthic_stn_ltm$near_dist_m) #0.7769833 3596.4906952 (meters)

#histogram of distances between pairs
ggplot(benthic_stn_ltm, aes(x=near_dist_m)) + 
  geom_histogram()
#as expected vast majority of station pairs are very close together
#look closer at the two that are over 2000 m

#look at two pairs with large distances
benthic_stn_ltm_far <- benthic_stn_ltm %>% 
  filter(near_dist_m>2000)

#look at sampling effort per month by station
#there are periods when sampling occurred twice per month
wq_effort <- benthic_wq %>% 
  mutate(year = year(date)
         ,month = month(date)
         ,ym = ym(str_c(year,"-", month))) %>% 
  group_by(station, ym) %>% 
  count() %>% 
  glimpse()

#make leaflet map of all wq and benthic stations

# Prepare station coordinates for leaflet map

#benthic
#lat/long are already in WGS84 (crs = 4326)
benthic_stn_g_ltm <- benthic_stn_g %>%
  add_column(type = "benthic") %>% 
  select(type
         , station = station_code
         , geometry) 
#st_crs(benthic_stn_g_ltm)

#WQ
#lat/long are already in WGS84 (crs = 4326)
wq_stn_g_ltm <- wq_stn_g %>%
  add_column(type = "wq") %>% 
  select(type
         , station
         , geometry) 
#st_crs(wq_stn_g_ltm)

#combine benthic and wq stations
stations_all <- bind_rows(benthic_stn_g_ltm, wq_stn_g_ltm)

# Define color palette for Surveys 
color_pal <- colorFactor(palette = "viridis", domain = stations_all$type)

# Create map using leaflet
leaflet() %>% 
  addTiles() %>%
  addCircleMarkers(
    data = stations_all,
    radius = 5,
    fillColor = ~color_pal(type),
    fillOpacity = 0.8,
    weight = 0.5,
    color = "black",
    opacity = 1,
    label = paste0("station: ", stations_all$station)
  ) %>% 
  addLegend(
    position = "topright",
    pal = color_pal,
    values = stations_all$type,
    title = "Type"
  )


#look at range of sampling effort     
range(wq_effort$n) # up to 8 samples per month

#histogram of sampling effort per month
ggplot(wq_effort, aes(x=n))+
  geom_histogram()

#plot number of samples per month by station
ggplot(wq_effort, aes(x=ym, y = n))+
  geom_point()+
  geom_line()+
  facet_wrap("station")
#too many stations to visualize well
#just look at subset that are relevant to benthic inverts
#probably should make a leaflet map to look at wq and benthic stations

#look at earliest date that each of the WQ parameters was collected
wq_start <- benthic_wq %>% 
  #convert wide to long; probably easier that way
  pivot_longer(cols= c(secchi:turbidity_surface_ntu), names_to = "parameter", values_to = "value") %>%    
  #drop rows with NAs for value
  filter(!is.na(value)) %>% 
  group_by(parameter) %>% 
  summarize(year_min = min(date)) %>% 
  arrange(parameter)
#all parameters go back to 1975 so they probably work, as least for stations where WQ and benthic overlap
#turbidity units changed in 2018, so need to decide how to handle that
#I thought NTU and FNU were basically the same

#only keep the WQ parameters that, more or less, cover the full benthic time series
wq_focus1 <- benthic_wq %>%
  #add month and year column
  #will be used to match WQ to benthic samples
  mutate(month = month(date)
        , year = year(date)
        ) %>% 
  #columns to keep
  select(station
         , month
         , year
         , date
         , secchi
         , turbidity_surface_fnu
         , turbidity_surface_ntu
         , sp_cnd_surface 
         , wt_surface 
         , do_surface
  ) %>% 
  glimpse()

#decide how to deal with different turbidity units
#first see whether they are ever both collected
#if not, for now just combine them into a single column
#talk to Morgan to decide what is best to do
wq_focus_turb <- wq_focus1 %>% 
  filter(!is.na(turbidity_surface_fnu) & !is.na(turbidity_surface_ntu))
#no cases in which both were recorded which makes combining the two simpler

#modify WQ data set so turbidity is just one column
wq_focus <- wq_focus1 %>% 
  mutate(turbidity_surface = coalesce(turbidity_surface_fnu,turbidity_surface_ntu)
         #make date into dttm so I can round date to nearest month
         , date_time = as_datetime(date)
                   ) %>% 
  #drop the unneeded turbidity columns
  #station, and date_time must be first two columns
  #for samples to match by closest date
  select(station
         ,date_time
         ,secchi
         ,sp_cnd_surface
         ,wt_surface
         ,do_surface
         ,turbidity_surface
  )  %>% 
  glimpse()
  

#create long form for plotting 
wq_focus_long <- wq_focus %>% 
  pivot_longer(c(secchi:turbidity_surface),names_to = "parameter", values_to = "value")  %>% 
  glimpse()

#turb_mean <-mean(wq_focus_turb$value,na.rm=T) #17.92123
#turb_sd <- sd(wq_focus_turb$value,na.rm=T) #16.86459

#look at effects of standardizing and log transforming turbidity
#someone on ResearchGate recommended power normalization 
#for dealing with skewed distributions and making different parameters comparable
#https://www.researchgate.net/post/log_transformation_and_standardization_which_should_come_first
wq_focus_turb <- wq_focus_long %>% 
  filter(parameter == "turbidity_surface") %>% 
  mutate(
    nlog = log(value)
    #for power normalization, typical values range 0.1-0.5
    #0.1 resulted in most normal looking distribution
    ,pwr_norm_0.1 = sign(value)*abs(value)^0.1 
    #calcuate z-score to standardize data (mean = 0, SD = 1)
    #NOTE: should have a normal-ish distribution before standardization
    ,zscore = (value-mean(value,na.rm=T))/sd(value,na.rm = T)
    #normalize data (all values between 0 and 1)
    ,normalize = (value-min(value,na.rm=T))/(max(value,na.rm=T)-min(value,na.rm=T))
    ,znlog = (nlog-mean(nlog,na.rm=T)/sd(nlog,na.rm=T))
    ,zpwr_norm_0.1 = (pwr_norm_0.1-mean(pwr_norm_0.1,na.rm=T))/sd(pwr_norm_0.1,na.rm=T)
    ) %>% 
  pivot_longer(c(value:zpwr_norm_0.1),names_to = "type", values_to = "value2")  %>% 
  glimpse()

turb_means <- wq_focus_turb %>% 
  group_by(type) %>% 
  summarise(turb_mean = mean(value2,na.rm=T)
            ,turb_sd = sd(value2,na.rm = T)
            )
  

#plot histograms for different forms of turbidity values
ggplot(wq_focus_turb,aes(x=value2))+
  geom_histogram()+
  facet_wrap(vars(type),scales="free",nrow=4)+
  ggtitle("Turbidity")
  
  
#plot histograms for each WQ parameter
ggplot(wq_focus_long,aes(x=value))+
  geom_histogram()+
  facet_wrap(vars(parameter),scales="free",nrow=3)+
  ggtitle("WQ distributions")
#fairly different looking distributions
#Do and Temp normal-ish; others right skewed, especially SC

#make same plot with data log transformed
ggplot(wq_focus_long,aes(x=log(value)))+
  geom_histogram()+
  facet_wrap(vars(parameter),scales="free",nrow=3)+
  ggtitle("WQ distributions")

#NOTE: also should look at correlations among all the WQ parameters
#eg, water temperature and DO will likely be highly correlated

#need to decide whether to use turbidity or secchi 

# Combine benthic invert and WQ data------------------
#need to do a bit of work to match up station names; they're similar but not the same generally
#start with the full benthic data set (with zeros) instead of just the selected stations
#we want to look at where the target taxa are broadly in the environmental gradients

#add column with benthic station names that does not include R, L, C
#then the WQ and benthic stations will match; generally station C is closest to WQ station
#NOTE: In many cases, one set of monthly WQ values will match to multiple benthic stations (up to three)
#NOTE: there are sometimes multiple WQ samples per station so some benthic samples could have multiple WQ matches
benthic_cpue_mod <- benthic_cpue %>% 
  #reduce df to just the needed columns
  select(station_code, sample_date, organism_code, mean_cpue) %>% 
  #for now, just drop the five samples with no organisms at all
  #probably should include these so we know where all the organisms are not
  filter(organism_code!=0) %>% 
  complete(nesting(sample_date, station_code), organism_code, fill = list(mean_cpue = 0)) %>% 
  #add column that drops the last two characters of the benthic station names 
  #so they will match the WQ station names
  mutate(station = substr(station_code,1,nchar(station_code)-2)
         ) %>% 
  #reduce data frame to just the needed columns
  select(
    #station, and date_time must be first two columns
    #for samples to match by closest date
          station
         #change name of date column so it's the same as wq data set
         , date_time = sample_date
         , station_code
         , organism_code
         , mean_cpue
    ) %>%
 glimpse()

#combine benthic and WQ stations
#should match by station and closest date_time (dttm)
#NOTE: wq and benthic stations differ in when they're sampled
#so the best matching wq for a benthic sample might be in a different year
#we will have to decide how much difference in date is acceptable; probably 15 days max
#this approach also means there won't be any cases in which there are NAs for all wq parameters for a benthic sample
#as long as at least one set of wq data was collected at the station at some time
#it's just that the wq data that will match the benthic sample will be from a potentially very distant time
#NOTE: it might be a problem that up to three benthic samples share same wq data in terms of building regressions
#might need to pick of the three stations, generally prioritizing the station that retains the most data
#https://stackoverflow.com/questions/28072542/merge-nearest-date-and-related-variables-from-a-another-dataframe-by-group?noredirect=1&lq=1
glimpse(benthic_cpue_mod)
glimpse(wq_focus)

#convert data frames to data tables
setDT(benthic_cpue_mod)
setDT(wq_focus)

setkey(wq_focus, station, date_time)[, dateMatch:=date_time]
comb_wq_b <- wq_focus[benthic_cpue_mod, roll='nearest']
count(matched)-count(benthic_cpue_mod) 
#same number of rows between benthic_cpue_mod and new df which is good sign

#performs some quick visual checks to make sure the closest date matching worked properly
#looks like the match worked fine

wq_focus <- wq_focus %>% 
  arrange(date_time,station)

benthic_focus <- benthic_cpue_mod %>% 
  distinct(station_code,date_time) %>% 
  arrange(date_time,station_code)

comb_wq_b_d <- comb_wq_b %>% 
  distinct(station,station_code, date_time, dateMatch) %>% 
  arrange(date_time,station_code)
  
#clean up combined data frame a bit
bwq <- comb_wq_b %>% 
  mutate(
    #add column that calculates difference in dates between wq and benthic samples
    date_diff = abs(as.numeric(as_date(date_time) - as_date(dateMatch)))
    #change organism code from numeric to character
    #so it matches with cpue_common data frame
    ,organism_code=as.character(organism_code)
  ) %>% 
  #filter out any cases in which closest wq and benthic sample time match 
  #exceeds 15 days 
  #a bit more time difference than I might use for more vagile organisms
  #but a lot of benthic inverts don't move that much
  filter(date_diff<15) %>% 
  select(
    b_stn = station_code
    ,wq_stn = station
    ,b_date = date_time
    ,wq_date = dateMatch
    ,date_diff
    ,organism = organism_code
    ,mean_cpue
    ,temp = wt_surface
    ,do = do_surface
    ,scond = sp_cnd_surface
    ,secchi
    ,turb = turbidity_surface
    ) %>% 
  glimpse()

#histogram of difference in dates between benthic and WQ samples
bwq_dates <- bwq %>% 
  distinct(b_stn,b_date,wq_date,date_diff)

range(bwq_dates$date_diff) 
# before filtering out big date differences, range was 0 to 4626

ggplot(bwq_dates,aes(x=date_diff))+
  geom_histogram()
#some date differences are insanely large
#because stations weren't sampled in same month or year in some cases

dates_far <- bwq_dates %>% 
  filter(date_diff> 15) %>% 
  arrange(-date_diff)
#771 cases

#what proportion of benthic invert samples had no usable discrete wq data?
count(benthic_focus) # starting number of benthic samples: 4625
count(benthic_focus)-count(bwq_dates) #number without wq: 845
(count(benthic_focus)-count(bwq_dates))/count(benthic_focus) 
#proportion without wq data: 18.3% 

#look for cases where WQ data are missing
bwq_issues <- bwq %>% 
  #just need to look at station matching so simplify to remove organism codes
  distinct(wq_stn,b_date,secchi, turb, scond, temp,do) %>% 
  filter(is.na(secchi) | is.na(turb) | is.na(scond) | is.na(temp) | is.na(do) )

#just look at cases in which there is no wq data
bwq_issues2 <- bwq %>% 
  #just need to look at station matching so simplify to remove organism codes
  distinct(b_stn,b_date,secchi, turb, scond, temp, do) %>% 
  filter(is.na(secchi) & is.na(turb) & is.na(scond) & is.na(temp) & is.na(do) )
#zero - because we matched benthic samples with closest wq data
#as long as at least one set of wq data was collected at a station, there will always be a match,
#though a very poor one in some cases, and those have already been filtered out (ie, any date difference over 15 d)

#next filter cpue data to just that of taxa common (>10% samples) in the three long term stations
#NOTE: check to make sure it removed and retained all the right taxa
bw_of <- bwq %>% 
  filter(organism %in% organisms_common) %>% 
  glimpse()

ctax <- unique(bw_of$organism)
#got the right number of taxa (n=42)

bwp <- bw_of %>% 
  #create a new column that rounds WQ parameters to nearest degree
  #some don't need rounding because they are already whole number values
  #NOTE: need to try a better approach
  mutate(temp_r = round(temp,0)
         ,do_r = round(do,0)
         ,turb_r = round(turb,0)
         ,sc_r = round(scond,-2)
         ) %>% 
  #drop unneeded columns
  select(-c(scond,temp,do,turb)) %>%  
    glimpse()



#maybe it's easier if we convert from wide to long
bwp_long <- bwp %>% 
  pivot_longer(c(secchi:sc_r),names_to = "parameter", values_to = "value") %>%  
  glimpse()

unique(bwp$temp_r) 
#why are there NAs for the rounded temp?
#probably cases in which WQ not collected in sample that best matched benthic sample date

#look at rows with NA for temp
temp_na <- bwp %>% 
  filter(is.na(temp_r))
#126)

#which visits are missing WQ?
wq_miss <- bwp %>% 
  #start with focus on temp but should filter to where any WQ parameter is NA
  filter(is.na(temp_r)) %>% 
  #look at visits with NA
  distinct(b_stn,b_date) %>%
  group_by(b_stn) %>% 
  count()
#just three samples missing temp, which is pretty good

# Plot distributions by taxa and wq parameter-----------------------
#probably should plot sample sizes for each level of each parameter too
#how often are extreme mean values based on a single year (or sample within year)?

#instead of averaging CPUE by WQ parameter bins, just plot raw data
#should produce unimodal curve for most taxa
#for taxa for which CPUE distribution is not fully within Bay-Delta gradient,
#we could try to extrapolate out to estimate where most extreme non-zero CPUE
#would be; not ideal to extrapolate beyond data but could check against literature

#calculate mean cpue for each level of each WQ parameter
bwp_means <- bwp_long %>% 
  group_by(organism,parameter,value) %>% 
  summarise(cpue = mean(mean_cpue,na.rm=T))

#start with just temperature panel
#filter data to just temp
bwp_means_temp <- bwp_means %>% 
  filter(parameter == "temp_r") %>% 
  glimpse()

#faceted plot showing distribution for all taxa across temperature gradient
ggplot(bwp_means_temp,aes(x=value, y=cpue))+
  geom_bar(stat="identity")+
  facet_wrap(vars(organism),scales="free",nrow=6)+
  ggtitle("water temperature")
#Warning message:Removed 42 rows containing missing values (position_stack)
#probably because of the temp = NA which I need to figure out
#are bimodal distributions indicative of multiple spp lumped together?
#consider also plotting proportion of samples at each temperature
#consider making another panel of plots for rarer taxa, which might show a stronger 
#response than the most common taxa (eg, >5% but <10% of samples)

#make plotting function for panels of plots showing distribution of abundances across env gradients
distr_plot <- function(df, param){
  ggplot(df,aes(x=value, y=cpue))+
    geom_bar(stat="identity")+
    facet_wrap(vars(organism),scales="free",nrow=6)+
    ggtitle(param)
}

#test the function
test<-distr_plot(bwp_means_temp,"water temperature")
#ggsave(test,filename="test.png",dpi=300, width = 8, height = 8, units = "in")
#worked fine

#nest data by parameter in preparation for making panels of plots
#make a plot of distribution across temp gradient for every taxon
parameter_nest <- bwp_means %>% 
  nest(abund_distr = -parameter) %>%  
  mutate(plots = map2(abund_distr,parameter, distr_plot))


#now save plot panel for every parameter
#walk2(parameter_nest$plots,parameter_nest$parameter
#      ,~ggsave(filename=paste0("BenthicInverts_WQ_Gradient/TaxonAbundace_",.y,".png")
#               ,plot = .x
#               ,dpi = 300
#               ,width = 12
#               ,height = 8
#               ,units = "in"
#      ))



#try generating distributions a different way---------------------------
#plotting raw data isn't working
#try normalizing or standardizing wq data 
#then calculating mean cpue for each unit or half unit
#then try producing curve from that

#start with plotting relationship between temperature and CPUE for one taxon

#first filter dataset to one taxon
bw_temp_ex <- bw_of %>% 
  filter(organism==4510) %>% 
  glimpse()

bw_temp_exz <- bw_of %>% 
  filter(organism==4510 & mean_cpue>0) %>% 
  glimpse()

#distributions of taxon abundances across temperature gradient
#aren't working well
#it could be that temperature tolerance varies across other
#wq parameters, like salinity
#it could also be that phenology is causing issues
#for a given taxon consider plotting temperature vs cpue
#for each month of the year
#maybe also plot temp vs cpue for a given value of another
#wq parameter like salinity

#plot showing distribution for this one taxon across the temperature gradient
#with zeros
ggplot(bw_temp_ex,aes(x=temp, y=mean_cpue))+
  geom_point()+
  stat_smooth(method='lm', formula = y~poly(x,2))+
  ylim(0,5)+
  ggtitle("water temperature")

#plot showing distribution for this one taxon across the temperature gradient
#without zeros
ggplot(bw_temp_exz,aes(x=temp, y=mean_cpue))+
  geom_point()+
  stat_smooth(method='lm', formula = y~poly(x,2))+
  ylim(0,50)+
  ggtitle("water temperature")

#faceted plot showing distribution for all taxa across temperature gradient
ggplot(bwq,aes(x=wt_surface, y=mean_cpue))+
  geom_point()+
  geom_smooth(method='lm', na.rm=T)+
  facet_wrap(vars(organism_code),scales="free",nrow=6)+
  ggtitle("water temperature")


#make plotting function for panels of plots showing distribution of abundances across env gradients
distr_plot <- function(df, param){
  ggplot(df,aes(x=value, y=cpue))+
    geom_bar(stat="identity")+
    facet_wrap(vars(organism_code),scales="free",nrow=6)+
    ggtitle(param)
}

#test the function
test<-distr_plot(bwp_means_temp,"water temperature")

#nest data by parameter in preparation for making panels of plots
#make a plot of distribution across temp gradient for every taxon
parameter_nest <- bwp_means %>% 
  nest(abund_distr = -parameter) %>%  
  mutate(plots = map2(abund_distr,parameter, distr_plot))


#now save plot panel for every parameter
#walk2(parameter_nest$plots,parameter_nest$parameter
#      ,~ggsave(filename=paste0("BenthicInverts_WQ_Gradient/TaxonAbundace_",.y,".png")
#               ,plot = .x
#               ,dpi = 300
#               ,width = 12
#               ,height = 8
#               ,units = "in"
#      ))











#calculate 95th percentile temperature for each taxa
#btemp_q95 <- bwp %>% 
#  group_by(organism) %>% 
#  summarize(temp_q95 = quantile(temp, probs=0.95, na.rm=T))

#now plot the distribution of the 95th quantiles across taxa
#ggplot(btemp_q95, aes(x=temp_q95))+
 # geom_histogram()

#consider looking at physiological limits for all Bay-Delta taxa regardless of how common they are
#to see how the distribution of thermal tolerances 

