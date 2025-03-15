#Drought traits project
#Benthic inverts
#Abundances of taxa and distributions of traits
#Taxa in at least 10% of samples in at least one of three stations

#To do list
#look at how many taxa per size class to make sure none have only a single taxon
#make plots that look at data by water year type because benthic inverts respond a lot to that
#make plots that exclude one or both clam species
#could look at correlations between each clam species and each of the other species to see whether they
#increase or decrease; would pair well with trait data like planktonic larvae

#packages
library(tidyverse)

#read in data------------------

#three taxa that are not IDed below family level
high_taxa <- c(1090, 1270, 1290 )

#abundances for the common 10 stn taxa by station and month (n = 66)
abund10stn <- read_csv("./benthic/data_output/benthic_common10_abundances_by_station.csv") %>% 
  #drop the three taxa that are not IDed below family level
  filter(!(organism_code %in% high_taxa))

#trait data for the common 10 stn taxa
#mostly just origin and size data but also more trait data for the 14 dominant taxa
traits <- read_csv("./benthic/data_output/benthic_common10_by_stn_trait_data_filled.csv") %>% 
  glimpse()

#format the trait data--------------

#start by only keeping the origin and size info
traits_so <- traits %>% 
  #keep just the needed columns
  select(organism_code
         ,target_taxon_name
         ,native
         ,trait
         ,trait_value
  ) %>% 
  #filter traits to just include size
  #only 63 taxa because three were dropped because of NAs)
  filter(trait == "body_size_max") %>% 
  #bin size data
  mutate(
    trait_value = as.numeric(trait_value)
    ,native = as.factor(native)
    ,body_size_cat = as.factor(case_when(trait_value <= 10 ~ 1
                                        ,trait_value <= 20 ~ 2
                                        ,trait_value <= 30 ~ 3
                                        ,trait_value <= 40 ~ 4
                                        ,trait_value <= 50 ~ 5
                                        ,trait_value > 50 ~ 6
    )))%>% 
  #drop column with trait name because all the same trait
  select(-trait) %>% 
  glimpse()

#format abundance data and summarize it by trait data---------

#add season column
abund10stn_seas <- abund10stn %>% 
  mutate(
    #add season column
    season = as.factor(
      case_when(month==12 | month==1 | month==2 ~ "w"
                ,month==3 | month==4 | month==5 ~ "sp"
                ,month==6 | month==7 | month==8 ~ "su"
                ,month==9 | month==10 | month==11 ~ "f"
      )),.after = month) %>% 
  glimpse()

#join trait data to abundance data
abund_trait <- abund10stn_seas %>% 
  left_join(traits_so) %>% 
  glimpse()

#trait and abundance data without the two dominant invasive clams
#Corbicula fluminea (6730)
#Potamocorbula amurensis (6890)

clam <- c("6730","6890")

abund_trait_noclam <- abund_trait %>% 
  filter(!(organism_code %in% clam))
  
#with clams: Calculate mean CPUE by size at different time and space scales for plotting--------

#summarizing to do for plots
#year, season, station: 12 plots, 3 stations x 4 seasons (previous step accomplished this already)
#year, season: four plots, one for each season
#year, station: three plots, one for each station
#year: one plot showing annual time series combining data across all samples at all stations

#summarize by year and station
abund_trait_sz_stn_yr <- abund_trait %>% 
  #sum cpue within samples
  group_by(sample_date, station_code,year_adjusted, body_size_cat) %>% 
  summarise(tot_cpue = sum(mean_cpue),.groups ='drop')  %>% 
  #calculate mean cpue across samples within stations and years
  group_by(station_code,year_adjusted, body_size_cat) %>% 
  summarise(cpue = mean(tot_cpue),.groups ='drop') %>% 
  glimpse()

#summarize by year and season
abund_trait_sz_seas_yr <- abund_trait %>% 
  #sum cpue within samples
  group_by(sample_date,station_code,year_adjusted, season, body_size_cat) %>% 
  summarise(tot_cpue = sum(mean_cpue),.groups ='drop')  %>% 
  #calculate mean cpue across samples within stations and years
  group_by(year_adjusted, season, body_size_cat) %>% 
  summarise(cpue = mean(tot_cpue),.groups ='drop') %>% 
  glimpse()

#summarize by year
abund_trait_sz_yr <- abund_trait %>% 
  #sum cpue within samples
  group_by(sample_date, station_code,year_adjusted, body_size_cat) %>% 
  summarise(tot_cpue = sum(mean_cpue),.groups ='drop')  %>% 
  #calculate mean cpue across samples within stations and years
  group_by(year_adjusted, body_size_cat) %>% 
  summarise(cpue = mean(tot_cpue),.groups ='drop') %>% 
  glimpse()

#with clams: Calculate mean CPUE by origin at different time and space scales for plotting--------

#summarize by year and station
abund_trait_orig_stn_yr <- abund_trait %>% 
  #sum cpue within samples
  group_by(sample_date, station_code,year_adjusted, native) %>% 
  summarise(tot_cpue = sum(mean_cpue),.groups ='drop')  %>% 
  #calculate mean cpue across samples within stations and years
  group_by(station_code,year_adjusted, native) %>% 
  summarise(cpue = mean(tot_cpue),.groups ='drop') %>% 
  glimpse()

#summarize by year and season
abund_trait_orig_seas_yr <- abund_trait %>% 
  #sum cpue within samples
  group_by(sample_date,station_code,year_adjusted, season, native) %>% 
  summarise(tot_cpue = sum(mean_cpue),.groups ='drop')  %>% 
  #calculate mean cpue across samples within stations and years
  group_by(year_adjusted, season, native) %>% 
  summarise(cpue = mean(tot_cpue),.groups ='drop') %>% 
  glimpse()

#summarize by year
abund_trait_orig_yr <- abund_trait %>% 
  #sum cpue within samples
  group_by(sample_date, station_code,year_adjusted, native) %>% 
  summarise(tot_cpue = sum(mean_cpue),.groups ='drop')  %>% 
  #calculate mean cpue across samples within stations and years
  group_by(year_adjusted, native) %>% 
  summarise(cpue = mean(tot_cpue),.groups ='drop') %>% 
  glimpse()

#with clams: Calculate mean CPUE by origin and size at different time and space scales for plotting--------

#summarize by year and station
abund_trait_orig_sz_stn_yr <- abund_trait %>% 
  #sum cpue within samples
  group_by(sample_date, station_code,year_adjusted, native,body_size_cat) %>% 
  summarise(tot_cpue = sum(mean_cpue),.groups ='drop')  %>% 
  #calculate mean cpue across samples within stations and years
  group_by(station_code,year_adjusted, native,body_size_cat) %>% 
  summarise(cpue = mean(tot_cpue),.groups ='drop') %>% 
  glimpse()

#summarize by year and season
abund_trait_orig_sz_seas_yr <- abund_trait %>% 
  #sum cpue within samples
  group_by(sample_date, station_code,year_adjusted, season, native,body_size_cat) %>% 
  summarise(tot_cpue = sum(mean_cpue),.groups ='drop')  %>% 
  #calculate mean cpue across samples within stations and years
  group_by(season,year_adjusted, native,body_size_cat) %>% 
  summarise(cpue = mean(tot_cpue),.groups ='drop') %>% 
  glimpse()

#summarize by year
abund_trait_orig_sz_yr <- abund_trait %>% 
  #sum cpue within samples
  group_by(sample_date, station_code,year_adjusted, native,body_size_cat) %>% 
  summarise(tot_cpue = sum(mean_cpue),.groups ='drop')  %>% 
  #calculate mean cpue across samples within stations and years
  group_by(year_adjusted, native,body_size_cat) %>% 
  summarise(cpue = mean(tot_cpue),.groups ='drop') %>% 
  glimpse()


#body size plots---------------------
#might need to group these by water year too
#Potamocorbula amurensis is sz3
#Corbicula fluminea is sz6

#absolute abundance stacked bar plot by year
(plot_abund_sz_yr_abs <- ggplot(abund_trait_sz_yr, aes(x = year_adjusted, y = cpue, fill = body_size_cat))+
   geom_bar(stat = "identity", color = "black")
)

#relative abundance stacked bar plot by year
(plot_abund_sz_yr_rel <- ggplot(abund_trait_sz_yr, aes(x = year_adjusted, y = cpue, fill = body_size_cat))+
    geom_bar(stat = "identity", position = "fill", color = "black")
)
#sz6 generally declining
#sz4 increasing a little but not abundant to start with
#sz3 and sz4 increasing a lot
#sz1 was abundant then declined then increased again

#absolute abundance stacked bar plot by station and year
(plot_abund_sz_stn_yr_abs <- ggplot(abund_trait_sz_stn_yr, aes(x = year_adjusted, y = cpue, fill = body_size_cat))+
   geom_bar(stat = "identity", color = "black")+
   facet_grid(station_code~.)
)

#relative abundance stacked bar plot by station and year
(plot_abund_sz_stn_yr_rel <- ggplot(abund_trait_sz_stn_yr, aes(x = year_adjusted, y = cpue, fill = body_size_cat))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(station_code~.)
)

#absolute abundance stacked bar plot by season and year
(plot_abund_sz_seas_yr_abs <- ggplot(abund_trait_sz_seas_yr, aes(x = year_adjusted, y = cpue, fill = body_size_cat))+
    geom_bar(stat = "identity", color = "black")+
    facet_grid(season~.)
)

#relative abundance stacked bar plot by season and year
(plot_abund_sz_seas_yr_rel <- ggplot(abund_trait_sz_seas_yr, aes(x = year_adjusted, y = cpue, fill = body_size_cat))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(season~.)
)

#origin based plots---------------------

#absolute abundance stacked bar plot by year
(plot_abund_orig_yr_abs <- ggplot(abund_trait_orig_yr, aes(x = year_adjusted, y = cpue, fill = native))+
   geom_bar(stat = "identity", color = "black")
)

#relative abundance stacked bar plot by year
(plot_abund_orig_yr_rel <- ggplot(abund_trait_orig_yr, aes(x = year_adjusted, y = cpue, fill = native))+
    geom_bar(stat = "identity", position = "fill", color = "black")
)
#abundances have increased through time largely throug increase in non-natives
#also less variable through time in recent years

#absolute abundance stacked bar plot by station and year
(plot_abund_orig_stn_yr_abs <- ggplot(abund_trait_orig_stn_yr, aes(x = year_adjusted, y = cpue, fill = native))+
    geom_bar(stat = "identity", color = "black")+
    facet_grid(station_code~.)
)
#wow D7 is virtually all non-natives starting in 1985
#looks like currently maybe the natives only have a chance during really wet years like 2017 and 2011

#relative abundance stacked bar plot by station and year
(plot_abund_orig_stn_yr_rel <- ggplot(abund_trait_orig_stn_yr, aes(x = year_adjusted, y = cpue, fill = native))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(station_code~.)
)

#absolute abundance stacked bar plot by season and year
(plot_abund_orig_seas_yr_abs <- ggplot(abund_trait_orig_seas_yr, aes(x = year_adjusted, y = cpue, fill = native))+
    geom_bar(stat = "identity", color = "black")+
    facet_grid(season~.)
)

#relative abundance stacked bar plot by season and year
(plot_abund_orig_seas_yr_rel <- ggplot(abund_trait_orig_seas_yr, aes(x = year_adjusted, y = cpue, fill = native))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(season~.)
)

#origin and body size plots---------------------

#absolute abundance stacked bar plot by year
(plot_abund_orig_sz_yr_abs <- ggplot(abund_trait_orig_sz_yr, aes(x = year_adjusted, y = cpue, fill = body_size_cat))+
   geom_bar(stat = "identity", color = "black")+
   facet_grid(native~.)
)

#relative abundance stacked bar plot by year
(plot_abund_orig_sz_yr_rel <- ggplot(abund_trait_orig_sz_yr, aes(x = year_adjusted, y = cpue, fill = body_size_cat))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(native~.)
)

#absolute abundance stacked bar plot by station and year
(plot_abund_orig_sz_stn_yr_abs <- ggplot(abund_trait_orig_sz_stn_yr, aes(x = year_adjusted, y = cpue, fill = body_size_cat))+
    geom_bar(stat = "identity", color = "black")+
    facet_grid(station_code~native)
)

#relative abundance stacked bar plot by station and year
(plot_abund_orig_sz_stn_yr_rel <- ggplot(abund_trait_orig_sz_stn_yr, aes(x = year_adjusted, y = cpue, fill = body_size_cat))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(station_code~native)
)

#absolute abundance stacked bar plot by season and year
(plot_abund_orig_sz_seas_yr_abs <- ggplot(abund_trait_orig_sz_seas_yr, aes(x = year_adjusted, y = cpue, fill = body_size_cat))+
    geom_bar(stat = "identity", color = "black")+
    facet_grid(season~.)
)

#relative abundance stacked bar plot by season and year
(plot_abund_orig_sz_seas_yr_rel <- ggplot(abund_trait_orig_sz_seas_yr, aes(x = year_adjusted, y = cpue, fill = body_size_cat))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(season~.)
)


#no clams: Calculate mean CPUE by size at different time and space scales for plotting--------

#summarize by year and station
abund_trait_noclam_sz_stn_yr <- abund_trait_noclam %>% 
  #sum cpue within samples
  group_by(sample_date, station_code,year_adjusted, body_size_cat) %>% 
  summarise(tot_cpue = sum(mean_cpue),.groups ='drop')  %>% 
  group_by(station_code,year_adjusted, body_size_cat) %>% 
  summarise(cpue = mean(tot_cpue),.groups ='drop') %>% 
  glimpse()

#summarize by year and season
abund_trait_noclam_sz_seas_yr <- abund_trait_noclam %>% 
  #sum cpue within samples
  group_by(sample_date, station_code,year_adjusted, season, body_size_cat) %>% 
  summarise(tot_cpue = sum(mean_cpue),.groups ='drop')  %>% 
  group_by(year_adjusted, season, body_size_cat) %>% 
  summarise(cpue = mean(tot_cpue),.groups ='drop') %>% 
  glimpse()

#summarize by year
abund_trait_noclam_sz_yr <- abund_trait_noclam %>% 
  #sum cpue within samples
  group_by(sample_date, station_code,year_adjusted, body_size_cat) %>% 
  summarise(tot_cpue = sum(mean_cpue),.groups ='drop')  %>% 
  group_by(year_adjusted, body_size_cat) %>% 
  summarise(cpue = mean(tot_cpue),.groups ='drop') %>% 
  glimpse()

#No clam: Calculate mean CPUE by origin at different time and space scales for plotting--------

#summarize by year and station
abund_trait_noclam_orig_stn_yr <- abund_trait_noclam %>% 
  #sum cpue within samples
  group_by(sample_date, station_code,year_adjusted, native) %>% 
  summarise(tot_cpue = sum(mean_cpue),.groups ='drop')  %>% 
  group_by(station_code,year_adjusted, native) %>% 
  summarise(cpue = mean(tot_cpue),.groups ='drop') %>% 
  glimpse()

#summarize by year and season
abund_trait_noclam_orig_seas_yr <- abund_trait_noclam %>% 
  #sum cpue within samples
  group_by(sample_date, station_code,year_adjusted, season, native) %>% 
  summarise(tot_cpue = sum(mean_cpue),.groups ='drop')  %>% 
  group_by(year_adjusted, season, native) %>% 
  summarise(cpue = mean(tot_cpue),.groups ='drop') %>% 
  glimpse()

#summarize by year
abund_trait_noclam_orig_yr <- abund_trait_noclam %>% 
  #sum cpue within samples
  group_by(sample_date, station_code,year_adjusted, native) %>% 
  summarise(tot_cpue = sum(mean_cpue),.groups ='drop')  %>% 
  group_by(year_adjusted, native) %>% 
  summarise(cpue = mean(tot_cpue),.groups ='drop') %>% 
  glimpse()

#No clam: Calculate mean CPUE by origin and size at different time and space scales for plotting--------

#summarize by year and station
abund_trait_noclam_orig_sz_stn_yr <- abund_trait_noclam %>% 
  #sum cpue within samples
  group_by(sample_date, station_code,year_adjusted, native, body_size_cat) %>% 
  summarise(tot_cpue = sum(mean_cpue),.groups ='drop')  %>% 
  group_by(station_code,year_adjusted, native, body_size_cat) %>% 
  summarise(cpue = mean(tot_cpue),.groups ='drop') %>% 
  glimpse()

#summarize by year and season
abund_trait_noclam_orig_sz_seas_yr <- abund_trait_noclam %>% 
  #sum cpue within samples
  group_by(sample_date, station_code,year_adjusted, season, native, body_size_cat) %>% 
  summarise(tot_cpue = sum(mean_cpue),.groups ='drop')  %>% 
  group_by(year_adjusted, season, native, body_size_cat) %>% 
  summarise(cpue = mean(tot_cpue),.groups ='drop') %>% 
  glimpse()

#summarize by year
abund_trait_noclam_orig_sz_yr <- abund_trait_noclam %>% 
  #sum cpue within samples
  group_by(sample_date, station_code,year_adjusted, native, body_size_cat) %>% 
  summarise(tot_cpue = sum(mean_cpue),.groups ='drop')  %>% 
  group_by(year_adjusted, native, body_size_cat) %>% 
  summarise(cpue = mean(tot_cpue),.groups ='drop') %>% 
  glimpse()


#no clam: body size plots---------------------

#absolute abundance stacked bar plot by year
(plot_abund_noclam_sz_yr_abs <- ggplot(abund_trait_noclam_sz_yr, aes(x = year_adjusted, y = cpue, fill = body_size_cat))+
   geom_bar(stat = "identity", color = "black")
)

#relative abundance stacked bar plot by year
(plot_abund_noclam_sz_yr_rel <- ggplot(abund_trait_noclam_sz_yr, aes(x = year_adjusted, y = cpue, fill = body_size_cat))+
    geom_bar(stat = "identity", position = "fill", color = "black")
)

#absolute abundance stacked bar plot by station and year
(plot_abund_noclam_sz_stn_yr_abs <- ggplot(abund_trait_noclam_sz_stn_yr, aes(x = year_adjusted, y = cpue, fill = body_size_cat))+
    geom_bar(stat = "identity", color = "black")+
    facet_grid(station_code~.)
)

#relative abundance stacked bar plot by station and year
(plot_abund_noclam_sz_stn_yr_rel <- ggplot(abund_trait_noclam_sz_stn_yr, aes(x = year_adjusted, y = cpue, fill = body_size_cat))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(station_code~.)
)

#absolute abundance stacked bar plot by season and year
(plot_abund_noclam_sz_seas_yr_abs <- ggplot(abund_trait_noclam_sz_seas_yr, aes(x = year_adjusted, y = cpue, fill = body_size_cat))+
    geom_bar(stat = "identity", color = "black")+
    facet_grid(season~.)
)

#relative abundance stacked bar plot by season and year
(plot_abund_noclam_sz_seas_yr_rel <- ggplot(abund_trait_noclam_sz_seas_yr, aes(x = year_adjusted, y = cpue, fill = body_size_cat))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(season~.)
)

#no clams: origin based plots---------------------

#absolute abundance stacked bar plot by year
(plot_abund_noclam_orig_yr_abs <- ggplot(abund_trait_noclam_orig_yr, aes(x = year_adjusted, y = cpue, fill = native))+
   geom_bar(stat = "identity", color = "black")
)

#relative abundance stacked bar plot by year
(plot_abund_noclam_orig_yr_rel <- ggplot(abund_trait_noclam_orig_yr, aes(x = year_adjusted, y = cpue, fill = native))+
    geom_bar(stat = "identity", position = "fill", color = "black")
)


#absolute abundance stacked bar plot by station and year
(plot_abund_noclam_orig_stn_yr_abs <- ggplot(abund_trait_noclam_orig_stn_yr, aes(x = year_adjusted, y = cpue, fill = native))+
    geom_bar(stat = "identity", color = "black")+
    facet_grid(station_code~.)
)

#relative abundance stacked bar plot by station and year
(plot_abund_noclam_orig_stn_yr_rel <- ggplot(abund_trait_noclam_orig_stn_yr, aes(x = year_adjusted, y = cpue, fill = native))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(station_code~.)
)

#absolute abundance stacked bar plot by season and year
(plot_abund_noclam_orig_seas_yr_abs <- ggplot(abund_trait_noclam_orig_seas_yr, aes(x = year_adjusted, y = cpue, fill = native))+
    geom_bar(stat = "identity", color = "black")+
    facet_grid(season~.)
)

#relative abundance stacked bar plot by season and year
(plot_abund_noclam_orig_seas_yr_rel <- ggplot(abund_trait_noclam_orig_seas_yr, aes(x = year_adjusted, y = cpue, fill = native))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(season~.)
)

#no clams: origin and body size plots---------------------

#absolute abundance stacked bar plot by year
(plot_abund_noclam_orig_sz_yr_abs <- ggplot(abund_trait_noclam_orig_sz_yr, aes(x = year_adjusted, y = cpue, fill = body_size_cat))+
   geom_bar(stat = "identity", color = "black")+
   facet_grid(native~.)
)

#relative abundance stacked bar plot by year
(plot_abund_noclam_orig_sz_yr_rel <- ggplot(abund_trait_noclam_orig_sz_yr, aes(x = year_adjusted, y = cpue, fill = body_size_cat))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(native~.)
)

#absolute abundance stacked bar plot by station and year
(plot_abund_noclam_orig_sz_stn_yr_abs <- ggplot(abund_trait_noclam_orig_sz_stn_yr, aes(x = year_adjusted, y = cpue, fill = body_size_cat))+
    geom_bar(stat = "identity", color = "black")+
    facet_grid(station_code~native)
)

#relative abundance stacked bar plot by station and year
(plot_abund_noclam_orig_sz_stn_yr_rel <- ggplot(abund_trait_noclam_orig_sz_stn_yr, aes(x = year_adjusted, y = cpue, fill = body_size_cat))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(station_code~native)
)

#absolute abundance stacked bar plot by season and year
(plot_abund_noclam_orig_sz_seas_yr_abs <- ggplot(abund_trait_noclam_orig_sz_seas_yr, aes(x = year_adjusted, y = cpue, fill = body_size_cat))+
    geom_bar(stat = "identity", color = "black")+
    facet_grid(season~.)
)

#relative abundance stacked bar plot by season and year
(plot_abund_noclam_orig_sz_seas_yr_rel <- ggplot(abund_trait_noclam_orig_sz_seas_yr, aes(x = year_adjusted, y = cpue, fill = body_size_cat))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(season~.)
)
