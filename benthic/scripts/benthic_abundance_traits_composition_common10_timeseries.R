#Drought traits project
#Benthic invertebrates
#distribution of traits through time
#Leela Dixit

#packages----
#install.packages("tidyverse")
library(tidyverse)


#read in data----

#three taxa that are not IDed below family level
high_taxa <- c(1090, 1270, 1290 )

#three new taxa that don't have size data yet
new_taxa <- c(4740, 5130, 6565)

#abundances for the common 10 stn taxa by station and month (n = 66)
abund10stn <- read_csv("./benthic/data_output/benthic_common10_abundances_by_station.csv") %>% 
  #drop the three taxa that are not IDed below family level
  filter(!(organism_code %in% high_taxa) & !(organism_code %in% new_taxa))

#trait data for the common 10 stn taxa
#mostly just origin and size data but also more trait data for the 14 dominant taxa
traits <- read_csv("./benthic/data_output/benthic_common10_by_stn_trait_data_filled.csv") %>% 
  glimpse()


#format trait data----

#only keep planktonic larvae and trophic habit info
traits_pt <- traits %>%
  select(prop_d28, prop_d4, prop_d7,
         organism_code,
         native,
         target_taxon_name,
         trait,
         trait_value) %>%
  #only keeping dispersal and trophic habit trait data
  filter(trait %in% c("dispersal", "trophic_habit")) %>%
  #pivot trait values so they each have their own column
  pivot_wider(names_from = trait,
              values_from = trait_value)

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
  left_join(traits_pt) %>% 
  glimpse()

#trait/abund data without clams

clam <- c("6730","6890")

abund_trait_noclam <- abund_trait %>% 
  filter(!(organism_code %in% clam))





#with clams: sum CPUE by trait----

#sum station by dispersal
abund_trait_disp <- abund_trait %>% 
  #sum cpue within station by dispersal
  group_by(station_code,year_adjusted, season, dispersal) %>% 
  summarise(cpue = sum(mean_cpue),.groups ='drop') %>% 
  glimpse()

#sum station by trophic habit
abund_trait_tro <- abund_trait %>%
  #sum cpue within station by trophic habit
  group_by(station_code,year_adjusted, season, trophic_habit) %>% 
  summarise(cpue = sum(mean_cpue),.groups ='drop') %>% 
  glimpse()

#sum station by dispersal and trophic habit
abund_trait_disp_tro <- abund_trait %>% 
  #sum cpue within station by dispersal and trophic habit
  group_by(station_code,year_adjusted, season, dispersal, trophic_habit) %>% 
  summarise(cpue = sum(mean_cpue),.groups ='drop') %>% 
  glimpse()

#sum station by dispersal, th, and origin
abund_trait_disp_tro_orig <- abund_trait %>% 
  #sum cpue within station by dispersal, th, and origin
  group_by(station_code,year_adjusted, season, native, dispersal, trophic_habit) %>% 
  summarise(cpue = sum(mean_cpue),.groups ='drop') %>% 
  glimpse()


#mean CPUE at different time scales per trait----

#Dispersal
#sum by year and station for dispersal
abund_trait_disp_stn_yr <- abund_trait_disp %>% 
  group_by(station_code,year_adjusted, dispersal) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()

#sum by year and season for dispersal
abund_trait_disp_seas_yr <- abund_trait_disp %>% 
  group_by(year_adjusted, season, dispersal) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()

#summarize by year for dispersal
abund_trait_disp_yr <- abund_trait_disp %>% 
  group_by(year_adjusted, dispersal) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()

#trophic habit
#sum by year and station for trophic habit
abund_trait_tro_stn_yr <- abund_trait_tro %>% 
  group_by(station_code,year_adjusted, trophic_habit) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()

#sum by year and season for trophic habit
abund_trait_tro_seas_yr <- abund_trait_tro %>% 
  group_by(year_adjusted, season, trophic_habit) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()

#summarize by year for trophic habit
abund_trait_tro_yr <- abund_trait_tro %>% 
  group_by(year_adjusted, trophic_habit) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()

#mean CPUE at different time scales by combo traits----

#dispersal and trophic habit
#summarize by year and station
abund_trait_disp_tro_stn_yr <- abund_trait_disp_tro %>% 
  group_by(station_code,year_adjusted, dispersal, trophic_habit) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()

#summarize by year and season
abund_trait_disp_tro_seas_yr <- abund_trait_disp_tro %>% 
  group_by(year_adjusted, season, dispersal, trophic_habit) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()

#summarize by year
abund_trait_disp_tro_yr <- abund_trait_disp_tro %>% 
  group_by(year_adjusted, dispersal, trophic_habit) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()

#dispersal and origin
#summarize by year and station
abund_trait_disp_orig_stn_yr <- abund_trait_disp_tro_orig %>% 
  group_by(station_code,year_adjusted, native, dispersal) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()

#summarize by year and season
abund_trait_disp_orig_seas_yr <- abund_trait_disp_tro_orig %>% 
  group_by(year_adjusted, season, dispersal, native) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()

#summarize by year
abund_trait_disp_orig_yr <- abund_trait_disp_tro_orig %>% 
  group_by(year_adjusted, dispersal, native) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()

#trophic habit and origin
#summarize by year and station
abund_trait_tro_orig_stn_yr <- abund_trait_disp_tro_orig %>% 
  group_by(station_code,year_adjusted, native, trophic_habit) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()

#summarize by year and season
abund_trait_tro_orig_seas_yr <- abund_trait_disp_tro_orig %>% 
  group_by(year_adjusted, season, trophic_habit, native) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()

#summarize by year
abund_trait_tro_orig_yr <- abund_trait_disp_tro_orig %>% 
  group_by(year_adjusted, trophic_habit, native) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()

#dispersal plots----

#absolute abundance stacked bar plot by year
(plot_abund_disp_yr_abs <- ggplot(abund_trait_disp_yr, aes(x = year_adjusted, y = cpue, fill = dispersal))+
   geom_bar(stat = "identity", color = "black"))

#relative abundance stacked bar plot by year
(plot_abund_disp_yr_rel <- ggplot(abund_trait_disp_yr, aes(x = year_adjusted, y = cpue, fill = dispersal))+
    geom_bar(stat = "identity", position = "fill", color = "black"))

#very sharp increase in planktonic dispersal after introduction of clams, but surprising to see it drop pretty quickly

#absolute abundance stacked bar plot by station and year
(plot_abund_disp_stn_yr_abs <- ggplot(abund_trait_disp_stn_yr, aes(x = year_adjusted, y = cpue, fill = dispersal))+
    geom_bar(stat = "identity", color = "black")+
    facet_grid(station_code~.))
#D7 has most planktonic dispersal, makes sense with clams

#relative abundance stacked bar plot by station and year
(plot_abund_disp_stn_yr_rel <- ggplot(abund_trait_disp_stn_yr, aes(x = year_adjusted, y = cpue, fill = dispersal))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(station_code~.))
#increase in P in all, but drops in all but D7

#absolute abundance stacked bar plot by season and year
(plot_abund_disp_seas_yr_abs <- ggplot(abund_trait_disp_seas_yr, aes(x = year_adjusted, y = cpue, fill = dispersal))+
    geom_bar(stat = "identity", color = "black")+
    facet_grid(season~.))

#relative abundance stacked bar plot by season and year
(plot_abund_disp_seas_yr_rel <- ggplot(abund_trait_disp_seas_yr, aes(x = year_adjusted, y = cpue, fill = dispersal))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(season~.))


#trophic habit plots----

#absolute abundance stacked bar plot by year
(plot_abund_tro_yr_abs <- ggplot(abund_trait_tro_yr, aes(x = year_adjusted, y = cpue, fill = trophic_habit))+
   geom_bar(stat = "identity", color = "black"))

#relative abundance stacked bar plot by year
(plot_abund_tro_yr_rel <- ggplot(abund_trait_tro_yr, aes(x = year_adjusted, y = cpue, fill = trophic_habit))+
    geom_bar(stat = "identity", position = "fill", color = "black"))

#filter feeders boom with clams, but decrease over time. looks like their introduction allowed shredders to come into the system

#absolute abundance stacked bar plot by station and year
(plot_abund_tro_stn_yr_abs <- ggplot(abund_trait_tro_stn_yr, aes(x = year_adjusted, y = cpue, fill = trophic_habit))+
    geom_bar(stat = "identity", color = "black")+
    facet_grid(station_code~.))

#relative abundance stacked bar plot by station and year
(plot_abund_tro_stn_yr_rel <- ggplot(abund_trait_tro_stn_yr, aes(x = year_adjusted, y = cpue, fill = trophic_habit))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(station_code~.))

#shredders are mostly in D4 and D28...

#absolute abundance stacked bar plot by season and year
(plot_abund_tro_seas_yr_abs <- ggplot(abund_trait_tro_seas_yr, aes(x = year_adjusted, y = cpue, fill = trophic_habit))+
    geom_bar(stat = "identity", color = "black")+
    facet_grid(season~.))

#relative abundance stacked bar plot by season and year
(plot_abund_tro_seas_yr_rel <- ggplot(abund_trait_tro_seas_yr, aes(x = year_adjusted, y = cpue, fill = trophic_habit))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(season~.))

#combo plots----

#trophic habit and dispersal
#absolute abundance stacked bar plot by year
(plot_abund_disp_tro_yr_abs <- ggplot(abund_trait_disp_tro_yr, aes(x = year_adjusted, y = cpue, fill = trophic_habit))+
   geom_bar(stat = "identity", color = "black")+
   facet_grid(dispersal~.))

#relative abundance stacked bar plot by year
(plot_abund_disp_tro_yr_rel <- ggplot(abund_trait_disp_tro_yr, aes(x = year_adjusted, y = cpue, fill = trophic_habit))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(dispersal~.))

#absolute abundance stacked bar plot by station and year
(plot_abund_disp_tro_stn_yr_abs <- ggplot(abund_trait_disp_tro_stn_yr, aes(x = year_adjusted, y = cpue, fill = trophic_habit))+
   geom_bar(stat = "identity", color = "black")+
   facet_grid(station_code~dispersal))

#relative abundance stacked bar plot by station and year
(plot_abund_disp_tro_stn_yr_rel <- ggplot(abund_trait_disp_tro_stn_yr, aes(x = year_adjusted, y = cpue, fill = trophic_habit))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(station_code~dispersal))

#absolute abundance stacked bar plot by season and year
(plot_abund_disp_tro_seas_yr_abs <- ggplot(abund_trait_disp_tro_seas_yr, aes(x = year_adjusted, y = cpue, fill = dispersal))+
    geom_bar(stat = "identity", color = "black")+
    facet_grid(season~trophic_habit))

#relative abundance stacked bar plot by season and year
(plot_abund_disp_tro_seas_yr_rel <- ggplot(abund_trait_disp_tro_seas_yr, aes(x = year_adjusted, y = cpue, fill = trophic_habit))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(season~dispersal))

#trophic habit and origin
#absolute abundance stacked bar plot by year
(plot_abund_tro_orig_yr_abs <- ggplot(abund_trait_tro_orig_yr, aes(x = year_adjusted, y = cpue, fill = trophic_habit))+
    geom_bar(stat = "identity", color = "black")+
    facet_grid(native~.))

#relative abundance stacked bar plot by year
(plot_abund_tro_orig_yr_rel <- ggplot(abund_trait_tro_orig_yr, aes(x = year_adjusted, y = cpue, fill = trophic_habit))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(native~.))

#absolute abundance stacked bar plot by station and year
(plot_abund_tro_orig_stn_yr_abs <- ggplot(abund_trait_tro_orig_stn_yr, aes(x = year_adjusted, y = cpue, fill = trophic_habit))+
    geom_bar(stat = "identity", color = "black")+
    facet_grid(station_code~native))

#relative abundance stacked bar plot by station and year
(plot_abund_tro_orig_stn_yr_rel <- ggplot(abund_trait_tro_orig_stn_yr, aes(x = year_adjusted, y = cpue, fill = trophic_habit))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(station_code~native))

#absolute abundance stacked bar plot by season and year
(plot_abund_tro_orig_seas_yr_abs <- ggplot(abund_trait_tro_orig_seas_yr, aes(x = year_adjusted, y = cpue, fill = trophic_habit))+
    geom_bar(stat = "identity", color = "black")+
    facet_grid(season~native))

#relative abundance stacked bar plot by season and year
(plot_abund_tro_orig_seas_yr_rel <- ggplot(abund_trait_tro_orig_seas_yr, aes(x = year_adjusted, y = cpue, fill = trophic_habit))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(season~native))

#dispersal and origin
#absolute abundance stacked bar plot by year
(plot_abund_disp_orig_yr_abs <- ggplot(abund_trait_disp_orig_yr, aes(x = year_adjusted, y = cpue, fill = dispersal))+
    geom_bar(stat = "identity", color = "black")+
    facet_grid(native~.))

#relative abundance stacked bar plot by year
(plot_abund_disp_orig_yr_rel <- ggplot(abund_trait_disp_orig_yr, aes(x = year_adjusted, y = cpue, fill = dispersal))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(native~.))

#absolute abundance stacked bar plot by station and year
(plot_abund_disp_orig_stn_yr_abs <- ggplot(abund_trait_disp_orig_stn_yr, aes(x = year_adjusted, y = cpue, fill = dispersal))+
    geom_bar(stat = "identity", color = "black")+
    facet_grid(station_code~native))

#relative abundance stacked bar plot by station and year
(plot_abund_disp_orig_stn_yr_rel <- ggplot(abund_trait_disp_orig_stn_yr, aes(x = year_adjusted, y = cpue, fill = dispersal))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(station_code~native))

#absolute abundance stacked bar plot by season and year
(plot_abund_disp_orig_seas_yr_abs <- ggplot(abund_trait_disp_orig_seas_yr, aes(x = year_adjusted, y = cpue, fill = dispersal))+
    geom_bar(stat = "identity", color = "black")+
    facet_grid(season~native))

#relative abundance stacked bar plot by season and year
(plot_abund_disp_orig_seas_yr_rel <- ggplot(abund_trait_disp_orig_seas_yr, aes(x = year_adjusted, y = cpue, fill = dispersal))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(season~native))


#no clams: sum CPUE by trait----

#sum station by dispersal
abund_trait_noclam_disp <- abund_trait_noclam %>%
  #sum cpue within station by origin
  group_by(station_code,year_adjusted, season, dispersal) %>% 
  summarise(cpue = sum(mean_cpue),.groups ='drop') %>% 
  glimpse()

#sum station by trophic habit
abund_trait_noclam_tro <- abund_trait_noclam %>%
  #sum cpue within station by origin
  group_by(station_code,year_adjusted, season, trophic_habit) %>% 
  summarise(cpue = sum(mean_cpue),.groups ='drop') %>% 
  glimpse()

#summarize station data by dispersal, trophic habit, origin
abund_trait_noclam_disp_tro_orig <- abund_trait_noclam %>% 
  #sum cpue within station by origin, dispersal, trophic habit
  group_by(station_code,year_adjusted, season, native, dispersal, trophic_habit) %>% 
  summarise(cpue = sum(mean_cpue),.groups ='drop') %>% 
  glimpse()

#no clams: mean CPUE by different time scales per trait----

#dispersal
#summarize by year and station for dispersal
abund_trait_noclam_disp_stn_yr <- abund_trait_noclam_disp %>% 
  group_by(station_code,year_adjusted, dispersal) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()

#summarize by year and season for dispersal
abund_trait_noclam_disp_seas_yr <- abund_trait_noclam_disp %>% 
  group_by(year_adjusted, season, dispersal) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()

#summarize by year for dispersal
abund_trait_noclam_disp_yr <- abund_trait_noclam_disp %>% 
  group_by(year_adjusted, dispersal) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()

#trophic habit
#summarize by year and station for trophic habit
abund_trait_noclam_tro_stn_yr <- abund_trait_noclam_tro %>% 
  group_by(station_code,year_adjusted, trophic_habit) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()

#summarize by year and season for trophic habit
abund_trait_noclam_tro_seas_yr <- abund_trait_noclam_tro %>% 
  group_by(year_adjusted, season, trophic_habit) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()

#summarize by year for trophic habit
abund_trait_noclam_tro_yr <- abund_trait_noclam_tro %>% 
  group_by(year_adjusted, trophic_habit) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()


#no clams: mean CPUE by different time scales by combo trait----

#dispersal and trophic habit
#summarize by year and station
abund_trait_noclam_disp_tro_stn_yr <- abund_trait_noclam_disp_tro_orig %>% 
  group_by(station_code,year_adjusted, dispersal, trophic_habit) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()

#summarize by year and season
abund_trait_noclam_disp_tro_seas_yr <- abund_trait_noclam_disp_tro_orig %>% 
  group_by(year_adjusted, season, dispersal, trophic_habit) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()

#summarize by year
abund_trait_noclam_disp_tro_yr <- abund_trait_noclam_disp_tro_orig %>% 
  group_by(year_adjusted, dispersal, trophic_habit) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()

#dispersal and origin
#summarize by year and station
abund_trait_noclam_disp_orig_stn_yr <- abund_trait_noclam_disp_tro_orig %>% 
  group_by(station_code,year_adjusted, dispersal, native) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()

#summarize by year and season
abund_trait_noclam_disp_orig_seas_yr <- abund_trait_noclam_disp_tro_orig %>% 
  group_by(year_adjusted, season, dispersal, native) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()

#summarize by year
abund_trait_noclam_disp_orig_yr <- abund_trait_noclam_disp_tro_orig %>% 
  group_by(year_adjusted, dispersal, native) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()

#trophic habit and origin
#summarize by year and station
abund_trait_noclam_tro_orig_stn_yr <- abund_trait_noclam_disp_tro_orig %>% 
  group_by(station_code,year_adjusted, native, trophic_habit) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()

#summarize by year and season
abund_trait_noclam_tro_orig_seas_yr <- abund_trait_noclam_disp_tro_orig %>% 
  group_by(year_adjusted, season, native, trophic_habit) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()

#summarize by year
abund_trait_noclam_tro_orig_yr <- abund_trait_noclam_disp_tro_orig %>% 
  group_by(year_adjusted, native, trophic_habit) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()


#no clam: dispersal plot----

#absolute abundance stacked bar plot by year
(plot_abund_noclam_disp_yr_abs <- ggplot(abund_trait_noclam_disp_yr, aes(x = year_adjusted, y = cpue, fill = dispersal))+
   geom_bar(stat = "identity", color = "black"))

#relative abundance stacked bar plot by year
(plot_abund_noclam_disp_yr_rel <- ggplot(abund_trait_noclam_disp_yr, aes(x = year_adjusted, y = cpue, fill = dispersal))+
    geom_bar(stat = "identity", position = "fill", color = "black"))

#absolute abundance stacked bar plot by station and year
(plot_abund_noclam_disp_stn_yr_abs <- ggplot(abund_trait_noclam_disp_stn_yr, aes(x = year_adjusted, y = cpue, fill = dispersal))+
    geom_bar(stat = "identity", color = "black")+
    facet_grid(station_code~.))

#relative abundance stacked bar plot by station and year
(plot_abund_noclam_disp_stn_yr_rel <- ggplot(abund_trait_noclam_disp_stn_yr, aes(x = year_adjusted, y = cpue, fill = dispersal))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(station_code~.))

#absolute abundance stacked bar plot by season and year
(plot_abund_noclam_disp_seas_yr_abs <- ggplot(abund_trait_noclam_disp_seas_yr, aes(x = year_adjusted, y = cpue, fill = dispersal))+
    geom_bar(stat = "identity", color = "black")+
    facet_grid(season~.))

#relative abundance stacked bar plot by season and year
(plot_abund_noclam_disp_seas_yr_rel <- ggplot(abund_trait_noclam_disp_seas_yr, aes(x = year_adjusted, y = cpue, fill = dispersal))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(season~.))


#no clam: trophic habit plot----
#absolute abundance stacked bar plot by year
(plot_abund_noclam_tro_yr_abs <- ggplot(abund_trait_noclam_tro_yr, aes(x = year_adjusted, y = cpue, fill = trophic_habit))+
   geom_bar(stat = "identity", color = "black"))

#relative abundance stacked bar plot by year
(plot_abund_noclam_tro_yr_rel <- ggplot(abund_trait_noclam_tro_yr, aes(x = year_adjusted, y = cpue, fill = trophic_habit))+
    geom_bar(stat = "identity", position = "fill", color = "black"))


#absolute abundance stacked bar plot by station and year
(plot_abund_noclam_tro_stn_yr_abs <- ggplot(abund_trait_noclam_tro_stn_yr, aes(x = year_adjusted, y = cpue, fill = trophic_habit))+
    geom_bar(stat = "identity", color = "black")+
    facet_grid(station_code~.))

#relative abundance stacked bar plot by station and year
(plot_abund_noclam_tro_stn_yr_rel <- ggplot(abund_trait_noclam_tro_stn_yr, aes(x = year_adjusted, y = cpue, fill = trophic_habit))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(station_code~.))

#absolute abundance stacked bar plot by season and year
(plot_abund_noclam_tro_seas_yr_abs <- ggplot(abund_trait_noclam_tro_seas_yr, aes(x = year_adjusted, y = cpue, fill = trophic_habit))+
    geom_bar(stat = "identity", color = "black")+
    facet_grid(season~.))

#relative abundance stacked bar plot by season and year
(plot_abund_noclam_tro_seas_yr_rel <- ggplot(abund_trait_noclam_tro_seas_yr, aes(x = year_adjusted, y = cpue, fill = trophic_habit))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(season~.))

#no clam: combo plots----

#trophic habit and dispersal
#absolute abundance stacked bar plot by year
(plot_abund_noclam_disp_tro_yr_abs <- ggplot(abund_trait_noclam_disp_tro_yr, aes(x = year_adjusted, y = cpue, fill = trophic_habit))+
   geom_bar(stat = "identity", color = "black")+
   facet_grid(dispersal~.))

#relative abundance stacked bar plot by year
(plot_abund_noclam_disp_tro_yr_rel <- ggplot(abund_trait_noclam_disp_tro_yr, aes(x = year_adjusted, y = cpue, fill = trophic_habit))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(dispersal~.))

#absolute abundance stacked bar plot by station and year
(plot_abund_noclam_disp_tro_stn_yr_abs <- ggplot(abund_trait_noclam_disp_tro_stn_yr, aes(x = year_adjusted, y = cpue, fill = trophic_habit))+
    geom_bar(stat = "identity", color = "black")+
    facet_grid(station_code~dispersal))

#relative abundance stacked bar plot by station and year
(plot_abund_noclam_disp_tro_stn_yr_rel <- ggplot(abund_trait_noclam_disp_tro_stn_yr, aes(x = year_adjusted, y = cpue, fill = trophic_habit))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(station_code~dispersal))

#absolute abundance stacked bar plot by season and year
(plot_abund_noclam_disp_tro_seas_yr_abs <- ggplot(abund_trait_noclam_disp_tro_seas_yr, aes(x = year_adjusted, y = cpue, fill = trophic_habit))+
    geom_bar(stat = "identity", color = "black")+
    facet_grid(season~dispersal))

#relative abundance stacked bar plot by season and year
(plot_abund_noclam_disp_tro_seas_yr_rel <- ggplot(abund_trait_noclam_disp_tro_seas_yr, aes(x = year_adjusted, y = cpue, fill = trophic_habit))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(season~dispersal))

#trophic habit and origin
#absolute abundance stacked bar plot by year
(plot_abund_noclam_tro_orig_yr_abs <- ggplot(abund_trait_noclam_tro_orig_yr, aes(x = year_adjusted, y = cpue, fill = trophic_habit))+
    geom_bar(stat = "identity", color = "black")+
    facet_grid(native~.))

#relative abundance stacked bar plot by year
(plot_abund_noclam_tro_orig_yr_rel <- ggplot(abund_trait_noclam_tro_orig_yr, aes(x = year_adjusted, y = cpue, fill = trophic_habit))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(native~.))

#absolute abundance stacked bar plot by station and year
(plot_abund_noclam_tro_orig_stn_yr_abs <- ggplot(abund_trait_noclam_tro_orig_stn_yr, aes(x = year_adjusted, y = cpue, fill = trophic_habit))+
    geom_bar(stat = "identity", color = "black")+
    facet_grid(station_code~native))

#relative abundance stacked bar plot by station and year
(plot_abund_noclam_tro_orig_stn_yr_rel <- ggplot(abund_trait_noclam_tro_orig_stn_yr, aes(x = year_adjusted, y = cpue, fill = trophic_habit))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(station_code~native))

#absolute abundance stacked bar plot by season and year
(plot_abund_noclam_tro_orig_seas_yr_abs <- ggplot(abund_trait_noclam_tro_orig_seas_yr, aes(x = year_adjusted, y = cpue, fill = trophic_habit))+
    geom_bar(stat = "identity", color = "black")+
    facet_grid(season~native))

#relative abundance stacked bar plot by season and year
(plot_abund_noclam_disp_tro_seas_yr_rel <- ggplot(abund_trait_noclam_tro_orig_seas_yr, aes(x = year_adjusted, y = cpue, fill = trophic_habit))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(season~native))

#dispersal and origin
#absolute abundance stacked bar plot by year
(plot_abund_noclam_disp_orig_yr_abs <- ggplot(abund_trait_noclam_disp_orig_yr, aes(x = year_adjusted, y = cpue, fill = dispersal))+
    geom_bar(stat = "identity", color = "black")+
    facet_grid(native~.))

#relative abundance stacked bar plot by year
(plot_abund_noclam_disp_orig_yr_rel <- ggplot(abund_trait_noclam_disp_orig_yr, aes(x = year_adjusted, y = cpue, fill = dispersal))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(native~.))

#absolute abundance stacked bar plot by station and year
(plot_abund_noclam_disp_orig_stn_yr_abs <- ggplot(abund_trait_noclam_disp_orig_stn_yr, aes(x = year_adjusted, y = cpue, fill = dispersal))+
    geom_bar(stat = "identity", color = "black")+
    facet_grid(station_code~native))

#relative abundance stacked bar plot by station and year
(plot_abund_noclam_disp_orig_stn_yr_rel <- ggplot(abund_trait_noclam_disp_orig_stn_yr, aes(x = year_adjusted, y = cpue, fill = dispersal))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(station_code~native))

#absolute abundance stacked bar plot by season and year
(plot_abund_noclam_disp_orig_seas_yr_abs <- ggplot(abund_trait_noclam_disp_orig_seas_yr, aes(x = year_adjusted, y = cpue, fill = dispersal))+
    geom_bar(stat = "identity", color = "black")+
    facet_grid(season~native))

#relative abundance stacked bar plot by season and year
(plot_abund_noclam_disp_orig_seas_yr_rel <- ggplot(abund_trait_noclam_disp_orig_seas_yr, aes(x = year_adjusted, y = cpue, fill = dispersal))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(season~native))

#community composition by station----
#community compostion by station, pre and post clam invasion----