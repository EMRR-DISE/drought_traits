#match Benthic Invert data with WQ data
#difficult because not collected at exact same time and place

# Load required packages -----------------

library(data.table) #matches data by nearest date 
#NOTE: data.table masks some useful tidyverse functions so load first
library(ggpmisc) #add correlation results to plots; load before ggplot2
library(tidyverse) #suite of data science tools
library(janitor) #used to quickly clean up column names
library(lubridate) #format date
library(sf) #work with spatial data like shapefiles
library(geosphere) #matching stations by distance
library(leaflet) #make interactive maps
library(ggrepel) #no-noverlapping point labels on maps
library(deltamapr) #Bay-Delta spatial data
library(here)

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
benthic_invert_cpue <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.1036.2&entityid=df1caeb717202f06171601f793ca46bf") %>% 
  clean_names() %>% 
  glimpse()

#organism key list
benthic_spp <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.1036.2&entityid=d0f0dd3c1835fe5b669342f8c8e77024") %>% 
  clean_names()

#organism key with column for latin name for taxa
#see BenthicInverts_EMP_TaxonomyUpdating for how this was done
benthic_spp_names <- read_csv("./benthic/data_output/nmds/benthic_taxonomy_name_labels.csv") %>% 
  arrange(organism_code)

#total annual site visits, 1975-2021
benthic_visits <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.1036.2&entityid=304bc4562046e0a6c35fbad3e2c85645") %>% 
  clean_names() %>% 
  glimpse()

#total annual grab samples, 1975-2021
benthic_grabs <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.1036.2&entityid=c6c7b2ed7165cfa93cc1eda43fbb29f5") %>% 
  clean_names() %>% 
  glimpse()

#EMP water quality data
#specify the column types upon reading in data
benthic_wq <-
 read_csv(
   "https://portal.edirepository.org/nis/dataviewer?packageid=edi.458.9&entityid=cf231071093ac2861893793517db26f3",
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

# turbidity_surface has two ND values at MD10A - not sure if this matters; if it
# does, we'll need to decide if we're okay with substituting 0 or some other
# number for these

#EMP water quality stations
wq_stn <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.458.4&entityid=827aa171ecae79731cc50ae0e590e5af") %>%
 clean_names() %>%
 glimpse()


# Read in region shapefile
region_shape <- read_sf(here("spatial_files/region.shp"))


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

#Convert coordinate reference system (CRS) of basemap and benthic_stn_g to EPSG = 26910
WW_Delta_26910 <- st_transform(WW_Delta, crs = 26910)

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
    # geom_sf_label(data = wq_stn_g_26910, aes(x=longitude,y=latitude, label=station) #label the points
    #               ,nudge_x = -0.008, nudge_y = 0.008 #can specify the magnitude of nudges if necessary
    #               , size = 2 #adjust size and position relative to points
    #               ,inherit.aes = F #tells it to look at points not base layer
    # ) + 
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
    #so it matches with cpue_10plus data frame
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
  filter(organism %in% organisms_common5) %>% 
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

