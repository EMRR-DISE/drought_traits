
####Zooplankton data manipulation for Special Studies Drought Synthesis
####Author: Laura Twardochleb

#### Startup commands #######################################################################################
setwd("~/IEP_drought_synthesis/Special Studies/drought_traits")

# install Zooper package
#options(repos = c(
  #sbashevkin = 'https://sbashevkin.r-universe.dev',
  #CRAN = 'https://cloud.r-project.org'))

#install.packages('zooper')
library(zooper)
library(tidyverse)

########## download zoop community data ########################################################################################################

########### mesozoops
#account for changes in taxonomic resolution over time
meso_time_consistent<-Zoopsynther(Data_type="Community", Sources = c("EMP"), Size_class = "Meso", Date_range = c("1975-01-01", "2022-01-01"), Time_consistency = TRUE)
unique(meso_time_consistent$Taxname)

########### all zoops
zoops<-Zoopsynther(Data_type="Community", Sources = c("EMP"), Size_class = c("Micro", "Meso", "Macro"), Date_range = c("1975-01-01", "2022-01-01"), Time_consistency = TRUE)
unique(zoops$Taxname)

#remove undersampled taxa- for analyses utilizing abundance data
zoops2<-filter(zoops, Undersampled==FALSE)
unique(zoops2$Taxname)

######### explore metadata before further data cleaning ######################################################################################################################

#summarize stations by date
zoops_summary<-zoops2%>%group_by(Latitude, Longitude, Year, Station)%>%summarize(N_dates=length(unique(Date)), N_samples=length(unique(SampleID)))


######### cleaning steps ########################################################################################################################

#spatial filtering
#download map shapefile
library(sf)
library(here)
library(deltamapr)

#chose this version of the shapefile because it included all the areas we need
#and splits out Grizzly Bay from Suisun Marsh
R_EDSM_Subregions_1617P1 

#look at coordinate reference system (CRS) of regions and basemap
st_crs(R_EDSM_Subregions_1617P1) #NAD83 / UTM zone 10N which is EPSG = 26910

region_focal <- R_EDSM_Subregions_1617P1 %>% 
  filter(!grepl("Napa", SubRegion)
         & SubRegion!="Suisun Marsh" 
         & SubRegion!="East San Pablo Bay"
  )

# Dissolve region shapefile to just the outside perimeter, removing subregions
region_focal_diss <- region_focal %>% 
  # Add a 0.5 meter buffer to remove slivers within perimeter
  st_buffer(0.5) %>%
  # Dissolve subregions
  st_union()

#subset stations to regions and time period of interest

# Filter stations spatially -----------
#Only keep the stations that fall within our spatial region
#as defined by the region shapefile

#add geometry column 
zoops_spatial <-  zoops2 %>% 
  drop_na(Latitude, Longitude)%>%
  #specify the crs which I assume is wgs84; need to verify with Arthur Barros
  st_as_sf(coords = c(x='Longitude',y='Latitude'), #identify the lat/long columns
           crs = 4326 #EPSG code for WGS84
           ,remove=F #retains original lat/long columns
  ) %>% 
  glimpse()

# Convert CRS of stations to NAD83
zoops_spatial_26910<-st_transform(zoops_spatial, crs=26910)

#filter stations to just those within the shapefile bounds
stn_within_reg <- zoops_spatial_26910 %>% 
  st_filter(region_focal_diss)

#remove microzooplankton (could be analyzed separately); these samples are unreliable for combining with macro and mesosampling due to very low volumes of water sampled
meso_macro<-filter(stn_within_reg, SizeClass != "Micro")

#temporal filtering: remove stations that are sparsely sampled over time
#read in station metadata table
metadata<-read_csv("~/IEP_drought_synthesis/Special Studies/drought_traits/Station_Metadata.csv")
unique(metadata$station)

#remove leading characters 'NZ' and leading zeros from station names in zoop dataset to match station names in metadata and in most EMP documentation
meso_macro$NewStation<- gsub('NZ','',meso_macro$Station)
meso_macro$NewStation <- gsub("^0+", "", meso_macro$NewStation)

#produce a list of stations with good data coverage based on metadata document (good coverage = no years missing or fewer than 10% of years missing, and sampling frequency is monthly or more)
stations_zoop<-metadata%>%filter(taxon=="zooplankton")
stations_complete<-unique(stations_zoop$station[which(stations_zoop$complete =="y"|stations_zoop$years_missing_n<4&stations_zoop$sampling_frequency=="Usually once a month, more in some years")])

#subset dataset to stations with complete sample coverage and remove data from after 2020- since most 2021 data unavailable
meso_macro_complete<-meso_macro%>%filter(NewStation%in%stations_complete)%>%filter(Year<2021)

#examine data coverage for these stations- by year, sampling looks pretty evenly distributed
zoops_summary_complete<-meso_macro_complete%>%group_by(Latitude, Longitude, Year, NewStation)%>%summarize(N_dates=length(unique(Date)), N_samples=length(unique(SampleID)))

#Examine sampling coverage by month- create month and season variables
library(lubridate)
meso_macro_complete2<-meso_macro_complete%>% #add month variable
  mutate(Month=month(Date))%>% #add seasons
  mutate(Season=case_when(Month%in%3:5 ~ "Spring",
                          Month%in%6:8 ~ "Summer",
                          Month%in%9:11 ~ "Fall",
                          Month%in%c(12, 1, 2) ~ "Winter",
                          TRUE ~ NA_character_))


meso_macro_summary<-meso_macro_complete2%>% #summarize sampling coverage
  group_by(Latitude, Longitude, Year, Month, NewStation)%>%summarize(N_dates=length(unique(Date)), N_samples=length(unique(SampleID)))
  
#remove samples during winter due to no sampling prior to 1995; remove organisms not ID'd to at least genus
meso_macro_complete3<-meso_macro_complete2%>%filter(Season!="Winter")%>%filter(!is.na(Genus))

#summarize coverage again and plot number of samples by month
meso_macro_summary2<-meso_macro_complete3%>% #summarize sampling coverage
  group_by(Latitude, Longitude, Year, Season, NewStation)%>%summarize(N_dates=length(unique(Date)), N_samples=length(unique(SampleID)))


###### Remove rare taxa -------------------repeat after spatial and temporal filtering ########

#what proportion of bay-delta wide taxa are represented
spp_all <- unique(meso_macro_complete3$Taxname)
#27 taxa ID'd to at least genus

#calculate abundance metrics by taxon
cpue_indiv_prop <- meso_macro_complete3 %>%
  group_by(Taxname) %>% 
  #sum cpue across all samples by taxon
  summarise(indiv_n=sum(CPUE)) %>% 
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

#plot histogram of proportion of individuals for each spp relative to
#the most abundant spp
ggplot(cpue_indiv_prop, aes(x=dom_prop))+
  geom_histogram()
#almost all species are rare compared to most abundant spp

#drop the 25% of species with lowest total individuals
cpue_rarest25 <- cpue_indiv_prop %>% 
  filter(rank_prop > 0.25)
#keeps 29 of 38 species 

#drop all ssp that comprise less than 5% of all individuals
cpue_indiv_5plus <- cpue_indiv_prop %>% 
  filter(indiv_prop > 0.05)
#only 7 taxa remain out of 38 

#drop all ssp with abundances less that 5% of that of most abundant spp
cpue_prop_dom_5plus <- cpue_indiv_prop %>% 
  filter(dom_prop > 0.05)
#only 12 taxa remain out of 38 

#need to calculate total number of samples: 5662
n_samples<-meso_macro_complete3 %>% 
 summarize(count= length(unique(SampleID)))

#calculate the proportion of samples in which each taxon is present
cpue_sample_prop <- meso_macro_complete3 %>%
  #drop rows where cpue is zero for sake of counting samples in which species are present
  filter(CPUE!=0.00)%>%
  group_by(Taxname) %>% 
  #row counts by taxon now should be number of samples with the taxon
  summarise(n_samp=n()) %>% 
  #order rows by number of samples, most to least
  arrange(-n_samp) %>% 
  mutate(prop = n_samp/n_samples$count) %>% 
  glimpse()

#two species of Americorophium are ID'd to species and are in fewer than 5% of samples, but combined into one genus may be in more than 5% of samples
#see what happens when we combine them
meso_macro_complete3$Taxname[meso_macro_complete3$Taxname=="Americorophium stimpsoni"|meso_macro_complete3$Taxname=="Americorophium spinicorne"]<-"Americorophium"

#calculate the proportion of samples in which each taxon is present
cpue_sample_prop <- meso_macro_complete3 %>%
  #drop rows where cpue is zero for sake of counting samples in which species are present
  filter(CPUE!=0.00)%>%
  group_by(Taxname) %>% 
  #row counts by taxon now should be number of samples with the taxon
  summarise(n_samp=n()) %>% 
  #order rows by number of samples, most to least
  arrange(-n_samp) %>% 
  mutate(prop = n_samp/n_samples$count) %>% 
  glimpse()

#plot histogram of proportion of samples containing species
ggplot(cpue_sample_prop, aes(x=prop))+
  geom_histogram()

#filter to just the species in more than 5% of samples
cpue_5plus <- cpue_sample_prop %>% 
  filter(prop>0.05)
#14 taxa are in more than 5% of samples

#filter to just the species in more than 10% of samples
cpue_common <- cpue_sample_prop %>% 
  filter(prop>0.10)
#12 taxa are in more than 10% of samples

#create vector of organism codes to then filter main df
organisms_common <- cpue_5plus %>% 
  pull(Taxname)

#filter dataset to just those taxa
meso_macro_common<-meso_macro_complete3%>%filter(Taxname%in%organisms_common)

#compare benthic and zoop taxa to ensure no more overlap
benthic<-read_csv("~/IEP_drought_synthesis/Special Studies/drought_traits/BenthicInverts/benthic_common5_taxonomy_2023-03-27.csv")
benthic_taxa<-unique(benthic$taxon)

benthic_zoop<-benthic%>%filter(taxon%in%meso_macro_common$Taxname)

#only Gammarus daiberi is shared between the two datasets- remove
meso_macro_common2<-meso_macro_common%>%filter(Taxname!="Gammarus daiberi")

######### summarize data ########################################################################################################################

#calculate annual mean abundance values for each taxon and station- one master Table L, need to subset by micro, meso, macro for further analyis
TableL<-meso_macro_common %>% st_drop_geometry()%>%
  group_by(Year, Month, Station, Taxname)%>% #first calculate mean cpue by station for each taxon and month
  summarize(monthly_mean=mean(CPUE))%>%
  group_by(Year, Taxname)%>%
  summarize(cpue_annual=mean(monthly_mean))%>%
  pivot_wider(names_from = Taxname, values_from = cpue_annual)#next calculate an annual mean for each taxon

write_csv(TableL, "~/IEP_drought_synthesis/Special Studies/drought_traits/Zooplankton/ZoopTableL.csv")

