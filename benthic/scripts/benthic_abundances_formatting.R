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
library(ggpmisc) #add correlation results to plots; load before ggplot2
library(tidyverse) #suite of data science tools
library(contentid) #reduce time of reloading large data sets
library(janitor) #used to quickly clean up column names
library(lubridate) #format date
library(sf) #work with spatial data like shapefiles
library(geosphere) #matching stations by distance
library(leaflet) #make interactive maps
library(ggrepel) #no-noverlapping point labels on maps
library(deltamapr) #Bay-Delta spatial data
library(here)

# To do list -----------------------------

#could add new data; current EDI package is 1975-2022; so far, I'm using 1975-2021

#make sure there isn't overlap in taxa between zoop and benthic inverts
#eg, I saw ostracods in benthic inverts data which are probably also in zoop

#figure out why so many NAs for matches between invert cpue and wq means

#look at number of grabs per visit after spatially and temporally filtering station data


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

# Read in region shapefile
region_shape <- read_sf(here("spatial_files/region.shp"))

#Quick look at number of taxa in full data set------------

#taxon_number_all <- benthic_invert_cpue %>% 
 # distinct(organism_code) %>%
  #count()
#410 taxa (including "no catch")

# Look at no catch samples ------------------------
#summary: four of five no catch rows are erroneous; true no catch at station D24-L on 2017-02-22
#NOTE: checked with Betsy - she gave no catch rows a CPUE value just so they didn't get
#filtered out; CPUE value is meaningless and should be zero

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

# Add zeros for absences back into abundance data set-------------------------
#these zeros were excluded from EDI data set, presumably to reduce total number of rows
#need to include zeros in the calculations of annual mean CPUE below

#start with CPUE dataset from EDI
benthic_cpue <- benthic_invert_cpue %>% 
  #simplify df to just the needed columns
  select(station_code, sample_date, organism_code, mean_cpue) %>% 
  # Convert organism_code to character since it represents discrete organisms
  mutate(organism_code = as.character(organism_code)) %>% 
  #adds missing combos of date, station, and organism and fills in CPUE with zero
  complete(nesting(sample_date, station_code), organism_code, fill = list(mean_cpue = 0))  %>% 
  #remove the now unneeded no catch rows
  #four out of five were erroneous; and the one true one has now been replaced with cpue=0 for all taxa like we want
  filter(organism_code!=0) %>% 
  glimpse()

#look at number of distinct combinations of date, station, organism, cpue
#should be same number of rows as benthic_cpue
benthic_cpue2 <- benthic_cpue %>% 
  distinct(sample_date, station_code, organism_code, mean_cpue)
count(benthic_cpue)-count(benthic_cpue2)
#zero, as expected 


# Filter the stations based on time series completeness ------------
#ie, stations that include mostly continuous sampling 1975-2021
#summary: after exploring data set, decided to use 1981-2021 (excluding 2004, 2005)

#date range of data set
range(benthic_cpue$sample_date) #"1975-05-19 UTC" "2021-12-16 UTC"

#create data set with just unique combos of station, month, and year (ie, visits)
#also add a column with adjusted year (Dec-Nov)
benthic_cpue_visits <-benthic_cpue %>% 
  #adjust year definition from Jan 1 - Dec 31 to Dec 1 - Nov 30
  mutate(
    month = month(sample_date),
    year = year(sample_date),
    year_adjusted = if_else(month > 11, year + 1, year)
  ) %>% 
  relocate(year_adjusted,.after = sample_date) %>% 
  #filter to one row per station visit
  distinct(year,year_adjusted,sample_date,month,station_code) %>% 
  select(year,year_adjusted,sample_date,month,station_code)

#count visits per station and year
benthic_cpue_visits_cy <- benthic_cpue_visits %>% 
  group_by(year,station_code) %>% 
  summarize(visits = n(), .groups = 'drop') %>% 
  glimpse()

#unique(benthic_cpue_visits_cyear$station_code) #53 stations

#create heat map of survey effort by station and calendar year
(p_heat_cy <- ggplot(benthic_cpue_visits_cy, aes(year, station_code)) +   
  geom_tile(aes(fill = visits))
)
#after 1980, survey effort is pretty consistently high
#exception is 2004-2005; might need to drop those years from analysis

#expected number of samples per station for period 1981-2021
#41 years x 12 samples 
samp_all <- 41*12 #492 samples per station
samp_enough <- samp_all-(samp_all*.1) #443

#count visits per station across years 1981-2021
#use adjusted year instead of calendar year
benthic_cpue_visits_tot_ay <- benthic_cpue_visits %>% 
  #drop early years of survey with low survey effort (pre-1981)
  filter(year_adjusted>1980) %>% 
  group_by(station_code) %>% 
  summarize(visits = n(), .groups = 'drop') %>% 
  glimpse()

#plot histogram to see distribution of number of visits per station
ggplot(benthic_cpue_visits_tot_ay, aes(x=visits))+
  geom_histogram()
#only three stations with more than 350 visits

#filter out stations without enough survey effort
stn_visits_many <- benthic_cpue_visits_tot_ay %>% 
  filter(visits > samp_enough) %>%
  pull(station_code)
#three stations: D28A-L D4-L   D7-C

#now filter the full abundance data set 
#only keep the stations and years that are well sampled
benthic_cpue_tm <- benthic_cpue %>% 
  #only keep the three long term stations
  filter(station_code %in% stn_visits_many) %>% 
  #adjust year definition from Jan 1 - Dec 31 to Dec 1 - Nov 30
  mutate(
    month = month(sample_date)
    ,year = year(sample_date)
    ,year_adjusted = if_else(month > 11, year + 1, year)
  ) %>% 
  relocate(year_adjusted,.after = sample_date) %>%
  #drop early years of survey with low survey effort (pre-1981)
  filter(year_adjusted>1980 & year_adjusted < 2022) %>% 
  glimpse()

#make sure the three stations, and only the three stations, are retained
#unique(benthic_cpue_tm$station_code)
#yes, "D4-L"   "D7-C"   "D28A-L"

#also check the final date range
#range(benthic_cpue_tm$sample_date)
#1980-12-22 UTC" "2021-11-22 UTC"
#looks good

#redo heat map with just our three remaining stations

#create data set with just unique combos of station and adjusted year (ie, visits)
benthic_cpue_visits2 <-benthic_cpue_tm %>% 
  #filter to one row per station visit
  distinct(year_adjusted,month,station_code) %>% 
  #count visits per station and year
  group_by(year_adjusted,station_code) %>% 
  summarize(visits = n(), .groups = 'drop') %>% 
  glimpse()

#updated heat map
(p_heat_comp_ay <- ggplot(benthic_cpue_visits2, aes(year_adjusted, station_code)) +   
    geom_tile(aes(fill = visits))
)
#sampling effort quite similar among stations but not exactly the same

#are there any years in which one or more stations has fewer than 8 months of samples?
#keep all years in which at least 8 months of sampling for all three stations
years_remove <- benthic_cpue_visits2 %>% 
  #convert from long to wide
  pivot_wider(names_from = station_code,values_from = visits) %>% 
  #show which years have at least one station with fewer than 8 months of sampling
  filter(if_any(contains("-"),~ . < 8)) %>% 
  pull(year_adjusted)
#for these three stations, only 2004 and 2005 are short more than 4 samples

#drop the two undersampled years (2004, 2005)
benthic_cpue_t <- benthic_cpue_tm %>% 
  filter(year_adjusted!= 2004 & year_adjusted!=2005) %>% 
  #reorder columns
  relocate(month:year,.after = sample_date) %>% 
  glimpse()

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

benthic_cpue_coord <- benthic_invert_cpue %>% distinct(station_code, latitude, longitude)

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
    sample_effort = if_else(station_code %in% stn_visits_many, "LongPOR", "ShortPOR"),
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
benthic_cpue_stf <- benthic_cpue_t %>% filter(station_code %in% stn_within_reg) %>% 
  glimpse()

#check which stations are included
benthic_stn_remain <-unique(benthic_cpue_stf$station_code)
#three stations as expected: "D4-L"   "D7-C"   "D28A-L"

#how many samples after filtering?
sample_num <- benthic_cpue_stf %>% 
  distinct(sample_date,station_code) %>% 
  count()
#1360 samples

#df with remaining stations
benthic_stn_g_26910_remain <- benthic_stn_g_26910 %>% 
  filter(station_code %in% benthic_stn_remain)

#map remaining stations
(map_benthic_left <-ggplot()+
    #CDFW Delta waterways
    geom_sf(data= WW_Delta_26910, fill= "skyblue3", color= "black", alpha = 0.6)+
    #region perimeter
    geom_sf(data =region_shape, alpha = 0.4, size = 1, color = "black", fill = "grey") +
    #all stations
    geom_sf(data =benthic_stn_g_26910_remain, fill = "red", shape = 22, size = 4, alpha = 0.8) +
    #add title
    ggtitle("All Benthic Invert Stations")+
    theme_bw()
)



# Remove taxa that are never present in remaining three stations-----------

#get vector of taxa remaining in the three stations
taxa_retain <- benthic_cpue_stf %>%
  group_by(organism_code) %>% 
  summarize(presence = sum(mean_cpue), .groups = 'drop') %>% 
  arrange(presence) %>% 
  filter(presence > 0) %>% 
  pull(organism_code)
#drops from 409 to 249 as expected
  
#filter the full data set to just keep these taxa
#ie, remove rows for taxa that are completely absent from the stations
benthic_cpue_stfr <- benthic_cpue_stf %>% 
  filter(organism_code %in% taxa_retain)

# Rare taxa: Try different approaches to removing them -------------------

#what proportion of bay-delta wide taxa are represented
#in the three stations I have kept

#how many taxa after filtering?
taxon_number_remain <- benthic_cpue_stfr %>% 
  distinct(organism_code) %>%
  count()
#249 of original 409 taxa
#so 60.9% of Bay-Delta taxa captured by our stations

#try various approaches for excluding rare taxa
#how many taxa remain if only keeping those that are present in: 
# just drop a fixed proportion of species (eg, least abundant 25% of spp)
        #this approach keeps too many taxa: 187 remain of 249 spp (75.1% of spp)
# comprise at least 1% of total abundances
        #keeps too few taxa: 14 taxa remain out of 249 (5.6% of taxa remaining)
# comprise at least 5% of total abundances
        #keeps too few taxa: 7 taxa remain out of 249 (2.8% of taxa remaining)
# remove those below 5% of abundance of the most abundant spp
        #keeps too few taxa: 15 spp remain out of 249 (6% of spp)
# found in at least 5% of samples
        #keeps quite a few: 64 of 249 taxa remaining; 25.7% taxa retained
        #identical list of taxa as earlier version except dropped one taxon
        #1210 Teratocephalus sp A (nematode)
# found in at least 10% of samples
        #keeps a manageable number: 43 of 249 taxa remaining; 17.2% of taxa retained
#Note that sampling not perfectly equal across years, which potentially causes problems
#with using the presence in x% of samples metrics. I did drop all years with fewer than 8 samples for at least one station

#should look at which taxa are retained across methods. are the most common ones the same?

#calculate abundance metrics by taxon
cpue_indiv_prop <- benthic_cpue_stfr %>%
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
#ggplot(cpue_indiv_prop, aes(x=indiv_prop))+
#  geom_histogram()
#vast majority of taxa comprise less than 1.25% of total individuals

#plot histogram of proportion of individuals for each spp relative to
#the most abundant spp
#ggplot(cpue_indiv_prop, aes(x=dom_prop))+
#  geom_histogram()
#almost all species are rare compared to most abundant spp (<5% dominant spp abundance)

#drop the 25% of species with lowest total individuals
cpue_rarest25 <- cpue_indiv_prop %>% 
  filter(rank_prop > 0.25)
#keeps 187 of 249 species (75.1% of taxa remaining)

#drop all ssp that comprise less than 5% of all individuals
cpue_indiv_5plus <- cpue_indiv_prop %>% 
  filter(indiv_prop > 0.05)
#only 7 taxa remain out of 249 (2.8% of taxa remaining)

#drop all ssp that comprise less than 1% of all individuals
cpue_indiv_1plus <- cpue_indiv_prop %>% 
  filter(indiv_prop > 0.01)
#only 14 taxa remain out of 249 (5.6% of taxa remaining)

#drop all ssp with abundances less that 5% of that of most abundant spp
cpue_prop_dom_5plus <- cpue_indiv_prop %>% 
  filter(dom_prop > 0.05)
#only 15 spp remain out of 249 (6% of spp)

#need to calculate total number of samples
total_samples <- benthic_cpue_stfr %>% 
  #look at distinct combinations of station and date
  #necessary because there's a row for each taxon within each sample
  distinct(sample_date, station_code) %>%  
  count() 
samp_denom <-as.numeric(total_samples[1,1]) #1360

#calculate the proportion of samples in which each taxon is present
cpue_sample_prop <- benthic_cpue_stfr %>%
  #drop rows where cpue is zero for sake of counting samples in which species are present
  filter(mean_cpue!=0) %>%   
  group_by(organism_code) %>% 
  #row counts by taxon now should be number of samples with the taxon
  summarise(n_samp=n()) %>% 
  #order rows by number of samples, most to least
  arrange(-n_samp) %>% 
  mutate(prop = n_samp/samp_denom) %>% 
  glimpse()


# Rare taxa: different threshold for proportional presence in of samples----------------
#try 1, 2, 5, 10% of samples to see how this affects proportion and type of species
#as well as NMDS plotting results

#plot histogram of proportion of samples containing species
ggplot(cpue_sample_prop, aes(x=prop))+
  geom_histogram()

#filter to just the species in more than 1% of samples
cpue_1plus <- cpue_sample_prop %>% 
  filter(prop>0.01)

#filter to just the species in more than 2% of samples
cpue_2plus <- cpue_sample_prop %>% 
  filter(prop>0.02)

#filter to just the species in more than 5% of samples
cpue_5plus <- cpue_sample_prop %>% 
  filter(prop>0.05)
#dropped from 249 to 64 taxa; 25.7% taxa retained

#filter to just the species in more than 10% of samples
cpue_10plus <- cpue_sample_prop %>% 
  filter(prop>0.10)
#dropped from 249 to 43 taxa; 17.2% of taxa retained

#make df of number of taxa retained by the four different filtering levels

#first count number of taxa in samples (249)
taxon_num <- as.numeric(taxon_number_remain)

species_retained <- data.frame(
  "filter" = c(1,2,5,10)
  , "num_taxa" = c(as.numeric(count(cpue_1plus)),as.numeric(count(cpue_2plus)),as.numeric(count(cpue_5plus)),as.numeric(count(cpue_10plus)))
  , "prop_taxa" = c(as.numeric(count(cpue_1plus))/taxon_num,as.numeric(count(cpue_2plus))/taxon_num,as.numeric(count(cpue_5plus))/taxon_num,as.numeric(count(cpue_10plus))/taxon_num)
 ) %>% 
  glimpse()

#plot increase in number of taxa retained when using different thresholds (1%, 2%, 5%, 10% of samples)
(plot_taxa_retained <- ggplot(species_retained,aes(filter,num_taxa)) + 
    geom_point()+
    #label points
    geom_text_repel(aes(label = num_taxa))+
    labs(x = "% samples with taxon present", y = "# of taxa retained")
)

#plot increase in proportion of taxa retained when using different thresholds (1%, 2%, 5%, 10% of samples)
(plot_taxa_retained <- ggplot(species_retained,aes(filter,prop_taxa)) + 
    geom_point()+
    #label points
    geom_text_repel(aes(label = prop_taxa))+
    labs(x = "% samples with taxon present", y = "proportion of taxa retained")
)

#1%: create vector of organism codes to then filter main df
organisms_common1 <- cpue_1plus %>% 
  pull(organism_code)

#2%: create vector of organism codes to then filter main df
organisms_common2 <- cpue_2plus %>% 
  pull(organism_code)

#5%: create vector of organism codes to then filter main df
organisms_common5 <- cpue_5plus %>% 
  pull(organism_code)

#10%: create vector of organism codes to then filter main df
organisms_common10 <- cpue_10plus %>% 
  pull(organism_code)

#export the list of organism codes for taxa in at least 5% of samples
#will update taxonomy for these retained taxa using another script
common5 <- as.data.frame(organisms_common5) %>% 
  rename(organism_code = organisms_common5)
#write_csv(common5,"./benthic/data_output/benthic_common5_codes_2023-01-25")

#1%: filter abundance data set 
benthic_cpue1 <- benthic_cpue_stfr %>% 
  filter(organism_code %in% organisms_common1) %>% 
  #sort by date, station, organism code
  arrange(sample_date, station_code, organism_code) %>% 
  glimpse()

#2%: filter abundance data set 
benthic_cpue2 <- benthic_cpue_stfr %>% 
  filter(organism_code %in% organisms_common2) %>% 
  #sort by date, station, organism code
  arrange(sample_date, station_code, organism_code) %>% 
  glimpse()

#5%: filter abundance data set 
#or use a join function to just keep the taxa that are in more than 5% of samples
benthic_cpue5 <- benthic_cpue_stfr %>% 
  filter(organism_code %in% organisms_common5) %>% 
  #sort by date, station, organism code
  arrange(sample_date, station_code, organism_code) %>% 
  glimpse()

#write a file with this data set
#write_csv(benthic_cpue5,"./benthic/data_output/benthic_common5_abundances.csv")

#10%: filter abundance data set 
benthic_cpue10 <- benthic_cpue_stfr %>% 
  filter(organism_code %in% organisms_common10) %>% 
  #sort by date, station, organism code
  arrange(sample_date, station_code, organism_code) %>% 
  glimpse()

# Rare taxa: compare the taxa retained by each of the different removal methods---------------------------
#main question is whether the specific species retained by the different methods is generally similar

#start with df with all organism codes
#add the 75% of spp that are the most numerically abundant overall (cpue_rarest25, n = 188)
#add those in more than 5% of samples (cpue_5plus, n=65)
#add those that are at least 1% of total abundance (cpue_indiv_1plus, n=14)
#add those with total abundances that are at least 5% of most abundant species total abundance (cpue_prop_dom_5plus, n=14)

#make data frame with just the full list of organism codes for samples in three focal stations
organism_code <- cpue_indiv_prop %>% 
  pull(organism_code) 

#subset and clean df with 75% of spp that are the most numerically abundant overall 
common75 <- cpue_rarest25 %>% 
  select(organism_code, rank_prop75 = rank_prop) %>% 
  #add column with numbers for each column converted to ranks
  mutate(rank_prop75r = rank(desc(rank_prop75))) %>% 
  glimpse()

#subset and clean df with those in more than 5% of samples (cpue_5plus, n=64)
common_sample_5perc <- cpue_5plus %>% 
  select(organism_code,prop_samp_5 = prop)%>% 
  #add column with numbers for each column converted to ranks
  mutate(prop_samp_5r = rank(desc(prop_samp_5))) %>% 
  glimpse()

#subset and clean df with those that are at least 1% of total abundance (cpue_indiv_1plus, n=14)
common_totabund_1perc <- cpue_indiv_1plus %>% 
  select(organism_code,indiv_prop_1perc = indiv_prop) %>% 
  #add column with numbers for each column converted to ranks
  mutate(indiv_prop_1percr = rank(desc(indiv_prop_1perc)))

#subset and clean df with those with total abundances that are at least 5% of most abundant species total abundance (cpue_prop_dom_5plus, n=14)
common_dom_5perc <- cpue_prop_dom_5plus %>% 
  select(organism_code,dom_prop5 = dom_prop)%>% 
  #add column with numbers for each column converted to ranks
  mutate(dom_prop5r = rank(desc(dom_prop5)))

#make list of organism codes in df
codes_focal_stations <- as.data.frame(organism_code) 

#put all data frames into list
df_list <- list(codes_focal_stations,common75
                ,common_sample_5perc,common_totabund_1perc
                ,common_dom_5perc
                )

#merge all data frames in list by organism_code
rare_compare <-df_list %>% 
  reduce(full_join, by='organism_code')  %>% 
  select(organism_code,rank_prop75r,prop_samp_5r,indiv_prop_1percr,dom_prop5r) %>% 
  arrange(dom_prop5r)

#quick summary
#14 taxa are retained by all filtering methods
#makes sense that three of the methods give the same results
#they're just different ways of expressing most abundant taxa
#ie, dom_prop5 (total sp #/total dom sp #),indiv_prop_1perc (total sp #/total #),rank_prop75 (total sp #)
#prop_samp_5 is only really different approach (looks at proportion of samples containing a sp.)
#even then, number of samples and number of individuals seems similar, but plot this to see

#plot for each of the 249 taxa the number of individuals vs number of samples they are present in
svi <- left_join(cpue_sample_prop,cpue_indiv_prop) %>% 
  select(organism_code,n_samp,indiv_n)

#plot raw data
(svi_plot_rw <- ggplot(svi,aes(x = n_samp, y = indiv_n)) + 
    geom_point()+
    #add vertical line showing 1%, 2%, 5%, 10% of samples
    geom_vline(xintercept=c(samp_denom*0.01,samp_denom*0.02,samp_denom*0.05,samp_denom*0.1))+ 
    labs(x = "Number of samples", y = "Number of individuals")
)

#plot log transformed data
(svi_plot_ln <- ggplot(svi,aes(x = log(n_samp), y = log(indiv_n))) + 
  geom_point()+
  #add vertical line showing 1%, 2%, 5%, 10% of samples
  geom_vline(xintercept=c(log(samp_denom*0.01),log(samp_denom*0.02),log(samp_denom*0.05),log(samp_denom*0.1)))+
  #add correlation line with standard error band
  geom_smooth(method = 'lm', se=T)+
    stat_poly_eq(formula = y ~ x,
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
                 parse = TRUE) +
  labs(x = "LN(Number of samples)", y = "LN(Number of individuals)")
)
#clearly shows a strong relationship between total abundance and presence in samples (R^2 = 0.91)
#so probably doesn't matter too much whether filtering based on total individuals or number of samples
#but not clear where to make the cut off for rare taxa based on this info alone
#should look at relationship between community structure and drought metrics to see if that provides clear answer

#look closer at relative abundances of taxa--------------
#do this to see how much it matters whether we drop some taxa that are higher than genus level
#and/or we can't find trait data
#look at relative abundances for all years and stations (did this above in cpue_indiv_prop)
#add columns that show this for each of the three stations separately as well

#format the df with relative abundances across all stations
#will join this with station specific values below
cpue_indiv_prop_all <- cpue_indiv_prop %>% 
  select(organism_code
         ,indiv_n_all = indiv_n
         ,indiv_prop_all = indiv_prop
         ,rank_prop_all = rank_prop
         ) %>% 
  glimpse()

#calculate relative abundance of taxa by station
cpue_indiv_prop_stn <- benthic_cpue_stfr %>%
  group_by(organism_code,station_code) %>% 
  #sum cpue by taxon and station
  summarise(indiv_n=sum(mean_cpue),.groups = "drop") %>%   
  #calculate the proportion of all individuals that a taxon comprises
  mutate(indiv_prop = indiv_n/sum(indiv_n)) %>% 
  #pivot to wide form so each station has it's own columns
  pivot_wider(names_from = "station_code", values_from = indiv_n:indiv_prop) %>% 
  glimpse()

#combine with relative abundance across all stations
cpue_indiv_prop_comb <- left_join(cpue_indiv_prop_all,cpue_indiv_prop_stn) %>% 
  arrange(-rank_prop_all) %>% 
  #add column that ranks species 1-249
  mutate(rank_no = row_number(),.after=rank_prop_all) %>% 
  glimpse()

#first create taxonomy data set with just the needed columns (ie, organism code, species name)
benthic_spp_names_short <- benthic_spp_names %>%
  select(organism_code,species_name) %>% 
  #reformat column type 
  mutate(organism_code = as.character(organism_code)) %>% 
  glimpse()

#add some of the taxonomy info 
cpue_indiv_prop_comb_nm <- left_join(cpue_indiv_prop_comb,benthic_spp_names_short) %>% 
  relocate(species_name,.after = organism_code)

#what proportion of total abundance do first 14 taxa comprise?
most_abundant <- cpue_indiv_prop_comb_nm[1:14,]
sum(most_abundant$indiv_prop_all) #0.9237181

#look at subset of taxa for which we don't have body size data 
missing_size_list <-as.character(c(1090,1230,1270,1290,3330,4030,4050,4090,6570))

#filter to just the missing ones
missing_size_df <- cpue_indiv_prop_comb_nm %>% 
  filter(organism_code %in% missing_size_list) %>% 
  arrange(-rank_prop_all)

#sum the proportion of organisms without size data
sum(missing_size_df$indiv_prop_all) 
#0.03243851 so 3% of organisms
#makes sense given that 14 taxa comprise majority of biomass

#create df with the 249 taxa present in our three stations
#then indicate whether each taxon was in at least 5% of samples

#start with list of 64 taxa in at least 5% of samples
common5_pa <- common5 %>% 
  mutate(common5 =1)

#add column with common 5% to relative abundance df
cpue_indiv_prop_combo_nm_final <- left_join(cpue_indiv_prop_comb_nm,common5_pa) %>% 
  #add zeros where common5 isn't 1
  mutate(common_5perc = case_when(is.na(common5)~0,TRUE ~ common5),.after = organism_code) %>% 
  select(-common5)

#write the output files
#all relative abundances
#write_csv(cpue_indiv_prop_combo_nm_final,"./benthic/data_output/benthic_relative_abundances.csv")

#relative abundances of taxa without size data
#write_csv(missing_size_df,"./benthic/data_output/benthic_relative_abundances_missing_size.csv")


#look at taxa in at least 5% of samples for each of the three stations--------------

#calculate the proportion of samples in which each taxon is present
cpue_sample_prop_stn <- benthic_cpue_stfr %>%
  #drop rows where cpue is zero for sake of counting samples in which species are present
  filter(mean_cpue!=0) %>%   
  group_by(station_code,organism_code) %>%   
  #row counts by taxon now should be number of samples with the taxon
  summarise(n_samp=n(),.groups = 'drop') %>% 
  #order rows by number of samples, most to least
  arrange(organism_code,-n_samp) %>% 
  pivot_wider(id_cols=organism_code,names_from = station_code, values_from = n_samp) %>% 
  rename(d28 = 'D28A-L'
         ,d4 = 'D4-L'
         ,d7 = 'D7-C') %>% 
  glimpse()

#need to calculate total number of samples
total_samples_stn <- benthic_cpue_stfr %>% 
  #look at distinct combinations of station and date
  #necessary because there's a row for each taxon within each sample
  distinct(sample_date, station_code) %>%  
  count(station_code) 

cpue_sample_prop_stn_calc <- cpue_sample_prop_stn %>% 
  mutate(prop_d28 = d28/454 #see total_samples_stn above for station counts
         ,prop_d4 = d4/453
         ,prop_d7 = d7/453
         ) %>% 
  glimpse()

#filter each station so only taxa present in at least 5% of samples remain
cpue_stn_5 <- cpue_sample_prop_stn_calc %>% 
  select(organism_code,starts_with("prop")) %>% 
  #convert wide to long
  pivot_longer(cols= starts_with("prop"),names_to = "station",values_to = "prop") %>% 
  #keep taxa in at least 5% of samples
  filter(prop > 0.05) %>% 
  #convert back to wide
  pivot_wider(id_cols = organism_code,names_from = station, values_from = prop) %>% 
  #arrange so most frequent taxa are at top
  arrange(prop_d28,prop_d4,prop_d7)
#89 taxa across all three stations

#now combine the station specific abundant taxa with the overall abundant taxa table
glimpse(cpue_stn_5)
glimpse(cpue_5plus)
cpue_5_all <- full_join(cpue_5plus,cpue_stn_5) %>% 
  select(-n_samp)
#so there are 25 more taxa beyond the 64 that are currently in the all stations list when you look
#at the three stations individually


#look at taxa in at least 10% of samples for each of the three stations--------------

#filter each station so only taxa present in at least 10% of samples remain
cpue_stn_10 <- cpue_sample_prop_stn_calc %>% 
  select(organism_code,starts_with("prop")) %>% 
  #convert wide to long
  pivot_longer(cols= starts_with("prop"),names_to = "station",values_to = "prop") %>% 
  #keep taxa in at least 10% of samples
  filter(prop > 0.1) %>% 
  #convert back to wide
  pivot_wider(id_cols = organism_code,names_from = station, values_from = prop) %>% 
  #arrange so most frequent taxa are at top
  arrange(prop_d28,prop_d4,prop_d7)
#66 taxa across all three stations

#now combine the station specific abundant taxa with the overall abundant taxa table
cpue_10_all <- full_join(cpue_10plus,cpue_stn_10) %>% 
  select(-n_samp)
#there are 23 more taxa in set of stations than overall 

#but let's instead compare the list of taxa that are in at least 10% of samples in each of the three stations
#with the taxa in 5% of all samples becuase the latter is the list for which we have already gathered traits
cpue_10_all_trait <- full_join(cpue_5plus,cpue_stn_10) %>% 
  select(-n_samp)
#there are only three taxa in the three station list that aren't in the 5% overall list that we used for 
#gathering traits
#seems to be one taxon in top 5% overall but not in top 10% for any of the three stations (6490, snail)

#create a list of remaining taxa for each station
#this will be used to filter the main abundance data set
#NOTE: drop the one taxon that was in our original 5% list but didn't make the 10% by station list (ie, 6490)
cpue_stn_10_list <- cpue_stn_10 %>% 
  #convert wide to long
  pivot_longer(cols= starts_with("prop"),names_to = "station", values_to = "prop") %>% 
  #remove rows with NA for prop
  filter(!is.na(prop)) %>% 
  #drop prop column now
  select(organism_code,station) %>% 
  glimpse()

#are there still 66 taxa?
cpue_stn_10_list_ct <- unique(cpue_stn_10_list$organism_code) #yes, n = 66

#create organism list for each station to use for filtering abundance data set
taxa_10_d28 <- cpue_stn_10_list %>% 
  filter(station == "prop_d28") %>%
  arrange(organism_code) %>% 
  pull(organism_code)
  
taxa_10_d4 <- cpue_stn_10_list %>% 
  filter(station == "prop_d4")%>% 
  arrange(organism_code) %>% 
  pull(organism_code)

taxa_10_d7 <- cpue_stn_10_list %>% 
  filter(station == "prop_d7")%>% 
  arrange(organism_code) %>% 
  pull(organism_code)

#now filter the main abundance data set to only keep data for the most abundant taxa from each station
benthic_cpue_10 <- benthic_cpue_stfr %>% 
  filter((station_code=="D28A-L" & organism_code %in% taxa_10_d28) |
           (station_code=="D4-L" & organism_code %in% taxa_10_d4)  |
           (station_code=="D7-C" & organism_code %in% taxa_10_d7)  
         )

#let's make sure the filtering worked correctly
benthic_cpue_10_check <- benthic_cpue_10 %>% 
  distinct(station_code,organism_code)
#looks good

#benthic_cpue_10_ct <- unique(benthic_cpue_10$organism_code) #n = 66 as expected

#write the file for analysis in another script
#write_csv(benthic_cpue_10, "./benthic/data_output/benthic_common10_abundances_by_station.csv")

#make a df with the taxa in 10% or more of samples and proportion of samples present overall and by station

benthic_spp_names2 <-   benthic_spp_names %>% 
  mutate(organism_code = as.character(organism_code)) %>% 
  glimpse()

benthic_cpue_10_list_prop <- cpue_10_all %>% 
  select(-prop) %>% 
  left_join(cpue_sample_prop) %>% 
  select(-n_samp) %>% 
  left_join(benthic_spp_names2) %>% 
  select(organism_code,prop_d28:prop) %>% 
  glimpse()
  
#write the file for combining with trait data in another script
#write_csv(benthic_cpue_10_list_prop, "./benthic/data_output/benthic_common10_prop_stations.csv")


#Calculate Bay-Delta wide seasonal Mean CPUE-----------------------
#using data set that only keeps species in at least 5% of sample
#note that EMP published their data by calendar year
#so winter (Dec-Feb) for last year (2021) isn't calculated (also no data for Dec 2020)
#should look at number of samples by year for each season (ideally always three months)

#label months by season
benthic_cpue5_seasons <- benthic_cpue5 %>% 
  mutate(
    season = case_when(month==12 | month==1 | month==2 ~ "w"
                       ,month==3 | month==4 | month==5 ~ "sp"
                       ,month==6 | month==7 | month==8 ~ "su"
                       ,month==9 | month==10 | month==11 ~ "f"
                       )
  ) %>% 
  glimpse()

#look at number of samples by station, year, season
benthic_cpue5_seasons_n <- benthic_cpue5_seasons %>% 
  #just keep the needed columns
  select(year_adjusted,season,month,station_code) %>% 
  #shed rows for all the taxa so just one per station-month combo
  distinct(year_adjusted,season,month,station_code) %>% 
  #count samples by season
  group_by(year_adjusted,season,station_code) %>% 
  summarize(n = n())  %>% 
  #just look at the cases where n isn't 3
  filter(n<3) %>% 
  arrange(n,season)
#37 of 465 samples are missing one or two samples, so not too bad (only 8 missing two)
#note there are some station-season combos that don't show up at all because all missing (w 2021)

#check for NA in seasons
# season_na <- benthic_cpue5_seasons %>% 
#   filter(is.na(season))
#no NAs as expected

#spring: March-May
cpue_mean_spring5 <- benthic_cpue5_seasons %>% 
  #keep data just for spring months
  filter(season == "sp") %>% 
  #calculate mean for each station and year
  group_by(year_adjusted,station_code,organism_code) %>% 
  summarize(cpue_stn=mean(mean_cpue)
            , .groups = 'drop'
            )  %>% 
  #now calculate mean for each year (across all stations)
  group_by(year_adjusted,organism_code) %>% 
  summarize(cpue_annual=mean(cpue_stn), .groups = 'drop') %>% 
  #add species names
  left_join(benthic_spp_names_short) %>% 
  #drop organism codes and reorder column
  select(year_adjusted,species_name,cpue_annual) %>% 
  #make Table L
  pivot_wider(id_cols = c(year_adjusted),names_from = species_name,values_from=cpue_annual) %>% 
  glimpse()
#write_csv(cpue_mean_spring5,"./benthic/data_output/benthic_table_l_spring.csv")

#summer: June-August
cpue_mean_summer5 <- benthic_cpue5_seasons %>% 
  #keep data just for summer months
  filter(season == "su") %>% 
  #calculate mean for each station and year
  group_by(year_adjusted,station_code,organism_code) %>% 
  summarize(cpue_stn=mean(mean_cpue), .groups = 'drop') %>% 
  #now calculate mean for each year (across all stations)
  group_by(year_adjusted,organism_code) %>% 
  summarize(cpue_annual=mean(cpue_stn), .groups = 'drop') %>% 
  #add species names
  left_join(benthic_spp_names_short) %>% 
  #drop organism codes and reorder column
  select(year_adjusted,species_name,cpue_annual) %>% 
  #make Table L
  pivot_wider(id_cols = c(year_adjusted),names_from = species_name,values_from=cpue_annual) %>% 
  glimpse()
#write_csv(cpue_mean_summer5,"./benthic/data_output/benthic_table_l_summer.csv")

#fall: Sept-Nov
cpue_mean_fall5 <- benthic_cpue5_seasons %>% 
  #keep data just for fall months
  filter(season == "f") %>% 
  #calculate mean for each station and year
  group_by(year_adjusted,station_code,organism_code) %>% 
  summarize(cpue_stn=mean(mean_cpue), .groups = 'drop') %>% 
  #now calculate mean for each year (across all stations)
  group_by(year_adjusted,organism_code) %>% 
  summarize(cpue_annual=mean(cpue_stn), .groups = 'drop') %>% 
  #add species names
  left_join(benthic_spp_names_short) %>% 
  #drop organism codes and reorder column
  select(year_adjusted,species_name,cpue_annual) %>% 
  #make Table L
  pivot_wider(id_cols = c(year_adjusted),names_from = species_name,values_from=cpue_annual) %>% 
  glimpse()
#write_csv(cpue_mean_fall5,"./benthic/data_output/benthic_table_l_fall.csv")

#winter: Dec-Feb
#2021 is currently based on just Dec 2020
cpue_mean_winter5 <- benthic_cpue5_seasons %>% 
  #keep data just for winter months
  filter(season == "w") %>% 
  #calculate mean for each station and year
  group_by(year_adjusted,station_code,organism_code) %>% 
  summarize(cpue_stn=mean(mean_cpue), .groups = 'drop') %>% 
  #now calculate mean for each year (across all stations)
  group_by(year_adjusted,organism_code) %>% 
  summarize(cpue_annual=mean(cpue_stn), .groups = 'drop') %>% 
  #add species names
  left_join(benthic_spp_names_short) %>% 
  #drop organism codes and reorder column
  select(year_adjusted,species_name,cpue_annual) %>% 
  #make Table L
  pivot_wider(id_cols = c(year_adjusted),names_from = species_name,values_from=cpue_annual) %>% 
  glimpse()
#write_csv(cpue_mean_winter5,"./benthic/data_output/benthic_table_l_winter.csv")


# Calculate Bay-Delta wide annual mean CPUE------------------
#calculate mean for each station and year
#then calculate mean for each year across all station

#could use a different summary statistic (eg, median)

#1%: generate annual mean CPUE values for each taxon
# cpue_mean_annual1 <- benthic_cpue1 %>% 
#   group_by(year,station_code,organism_code) %>% 
#   summarize(cpue_annual=mean(mean_cpue), .groups = 'drop')

#2%: generate annual mean CPUE values for each taxon
# cpue_mean_annual2 <- benthic_cpue2 %>% 
#   group_by(year,station_code,organism_code) %>% 
#   summarize(cpue_annual=mean(mean_cpue), .groups = 'drop')

#5%: generate annual mean CPUE values for each taxon
cpue_mean_annual5 <- benthic_cpue5 %>% 
  #first calculate mean for each station and year
  group_by(year_adjusted,station_code,organism_code) %>% 
  summarize(cpue_stn_annual=mean(mean_cpue), .groups = 'drop') %>% 
  #now calculate mean for each year (across all stations)
  group_by(year_adjusted,organism_code) %>% 
  summarize(cpue_annual=mean(cpue_stn_annual), .groups = 'drop') 
  

#10%: generate annual mean CPUE values for each taxon
# cpue_mean_annual10 <- benthic_cpue10 %>% 
#   group_by(year,station_code,organism_code) %>% 
#   summarize(cpue_annual=mean(mean_cpue), .groups = 'drop')

#add species names for use as vector labels

#1%: add species names to main data set
# cpue_mean_annual_nm1 <- left_join(cpue_mean_annual1,benthic_spp_names_short) %>% 
#   #drop name codes and reorder column
#   select(year,station_code,species_name,cpue_annual) 
# 
# #2%: add species names to main data set
# cpue_mean_annual_nm2 <- left_join(cpue_mean_annual2,benthic_spp_names_short) %>% 
#   #drop name codes and reorder column
#   select(year,station_code,species_name,cpue_annual) 

#5%: add species names to main data set
cpue_mean_annual_nm5 <- left_join(cpue_mean_annual5,benthic_spp_names_short) %>% 
  #drop name codes and reorder column
  select(year_adjusted,species_name,cpue_annual)

#10%: add species names to main data set
# cpue_mean_annual_nm10 <- left_join(cpue_mean_annual10,benthic_spp_names_short) %>% 
#   #drop name codes and reorder column
#   select(year,station_code,species_name,cpue_annual) 
  
# Format Table L (station-year x taxon) -----------

#1%: make Table L which is taxon x sample matrix
#TableL_1 <- cpue_mean_annual_nm1 %>%
#  pivot_wider(id_cols = c(station_code,year),names_from = species_name,values_from=cpue_annual) %>% 
  #add water year categories
#  left_join(wyear) %>% 
  #move year type and drought category to front with rest of predictors
#  relocate(yr_type:drought,.after = year)

#2%: make Table L which is taxon x sample matrix
#TableL_2 <- cpue_mean_annual_nm2 %>%
#  pivot_wider(id_cols = c(station_code,year),names_from = species_name,values_from=cpue_annual) %>% 
  #add water year categories
#  left_join(wyear) %>% 
  #move year type and drought category to front with rest of predictors
#  relocate(yr_type:drought,.after = year)

#5%: make Table L which is taxon x sample matrix
TableL_5 <- cpue_mean_annual_nm5 %>%
  pivot_wider(id_cols = c(year_adjusted),names_from = species_name,values_from=cpue_annual) 
  #add water year categories
  #left_join(wyear) %>% 
  #move year type and drought category to front with rest of predictors
  #relocate(yr_type:drought,.after = year) 
  

#10%: make Table L which is taxon x sample matrix
#TableL_10 <- cpue_mean_annual_nm10 %>%
#  pivot_wider(id_cols = c(station_code,year),names_from = species_name,values_from=cpue_annual) %>% 
  #add water year categories
#  left_join(wyear) %>% 
  #move year type and drought category to front with rest of predictors
#  relocate(yr_type:drought,.after = year)
#probably just need to now combine station and year into a row name and format as matrix

#separate label columns from cpue columns for NMDS analysis
#pred1 <- TableL_1 %>% 
#  select(station_code:drought)

#pred2 <- TableL_2 %>% 
#  select(station_code:drought)

#pred5 <- TableL_5 %>% 
#  select(station_code:drought)

#pred10 <- TableL_10 %>% 
#  select(station_code:drought)

#dat1 <- TableL_1 %>% 
#  select(!(station_code:drought))

#dat2 <- TableL_2 %>% 
#  select(!(station_code:drought))

#dat5 <- TableL_5 %>% 
#  remove_rownames %>% 
#  column_to_rownames(var="year_adjusted")

#dat10 <- TableL_10 %>% 
#  select(!(station_code:drought))

#export resulting matrices
#note that file paths are outdated
#write_csv(pred1,"BenthicInverts/benthic_nmds_predictors_1.csv")
#write_csv(pred2,"BenthicInverts/benthic_nmds_predictors_2.csv")
#write_csv(pred5,"BenthicInverts/benthic_nmds_predictors_5.csv")
#write_csv(pred10,"BenthicInverts/benthic_nmds_predictors_10.csv")

#write_csv(dat1,"BenthicInverts/benthic_nmds_abundance_1.csv")
#write_csv(dat2,"BenthicInverts/benthic_nmds_abundance_2.csv")
#write_csv(dat10,"BenthicInverts/benthic_nmds_abundance_10.csv")

#this is the abundance matrix we will use for analysis
#year adjusted to Dec 1 - Nov 30
#only kept the three long term stations
#dropped low sampling years
#only kept taxa present in at least 5% of samples
#write_csv(TableL_5,"./benthic/data_output/benthic_table_l.csv")



#make a big faceted plot showing time series of each taxon in each station
#ggplot(cpue_mean_annual_nm5,aes(x=year_adjusted, y=cpue_annual, group=station_code,color=station_code))+
 # geom_point()+
  #geom_line()+
  #facet_wrap(vars(organism_code),scales="free",nrow=6)


