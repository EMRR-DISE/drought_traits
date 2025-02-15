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


#Plots of proportion of organisms with planktonic larvae vs total organisms---------------
#inspired by Nichols and Thompson 1985


#create df that calculates proportion of organisms with planktonic larvae
abund_trait_sample_sum_plank <- abund_trait %>% 
  group_by(sample_date,station_code,dispersal) %>% 
  summarize(total_cpue = sum(mean_cpue)) %>% 
  #convert long to wide
  pivot_wider(names_from = dispersal, values_from = total_cpue) %>% 
  #calculate proportion
  mutate(plankton_prop = P/(N+P)
         ,total_ct = N + P
         )
  
#plot data
(plot_plankton_prop <- ggplot(abund_trait_sample_sum_plank, aes(x = total_ct, y = plankton_prop))+
    geom_point()+
    facet_grid("station_code")
  )

#Plots of proportion of organisms with planktonic larvae (except clams) vs total clams---------------


#create df that calculates proportion of organisms with planktonic larvae
abund_trait_sample_sum_plank_no_clam <- abund_trait_noclam %>% 
  group_by(sample_date,station_code,dispersal) %>% 
  summarize(total_cpue = sum(mean_cpue)) %>% 
  #convert long to wide
  pivot_wider(names_from = dispersal, values_from = total_cpue) %>% 
  #calculate proportion
  mutate(plankton_prop = P/(N+P)
         ,total_ct = N + P
  )

#create df that calculates total clams by sample
abund_trait_sample_sum_clams <- abund_trait %>% 
  filter(organism_code %in% clam) %>% 
  group_by(sample_date,station_code) %>% 
  summarize(clam_cpue = sum(mean_cpue))

#combine prop plankton and total clams dfs
plank_clam <- abund_trait_sample_sum_plank_no_clam %>% 
  left_join(abund_trait_sample_sum_clams)

#plot count data
# (plot_plankton_ct_noclam <- ggplot(plank_clam, aes(x = clam_cpue, y = total_ct))+
#     geom_point()+
#     facet_grid("station_code")
# )
#prop plot below is better

#plot count data
# (plot_plankton_count_noclam <- ggplot(plank_clam, aes(x = clam_cpue, y = P))+
#     geom_point()+
#     facet_grid("station_code")
# )
#because of outliers it is just harder to see pattern compared to looking at prop plot below

#plot prop data
(plot_plankton_prop_noclam <- ggplot(plank_clam, aes(x = clam_cpue, y = plankton_prop))+
    geom_point()+
    facet_grid("station_code")
)
#plot makes sense but I wonder if ALL organisms (regardless of traits) are at low abundance when clams are high

#plot non-clam cpue vs clam cpue
(plot_total_organisms_noclam <- ggplot(plank_clam, aes(x = clam_cpue, y = total_ct))+
    geom_point()+
    facet_grid("station_code")
)
#yes, at high clam densities everything else is low regardless of traits

#Plots of proportion of organisms (except clams) in different feeding habits vs total clams---------------

#create df that calculates proportion of organisms that filter feed
abund_trait_sample_sum_filter_no_clam <- abund_trait_noclam %>% 
  group_by(sample_date,station_code,trophic_habit) %>% 
  summarize(total_cpue = sum(mean_cpue)) %>% 
  #convert long to wide
  pivot_wider(names_from = trophic_habit, values_from = total_cpue) %>%   
  #calculate proportion
  mutate(total_cpue = sum(across(C:S),na.rm = T)
         ,filter_prop = F/total_cpue
  )

#combine prop filter and total clams dfs
filter_clam <- abund_trait_sample_sum_filter_no_clam %>% 
  left_join(abund_trait_sample_sum_clams)


#plot filter prop data
(plot_filter_prop_noclam <- ggplot(filter_clam, aes(x = clam_cpue, y = filter_prop))+
    geom_point()+
    facet_grid("station_code")
)
#declines with increasing clams

#plot shredder prop data
(plot_shredder_prop_noclam <- ggplot(filter_clam, aes(x = clam_cpue, y = S/total_cpue))+
    geom_point()+
    facet_grid("station_code")
)
#declines with increasing clams

#plot deposit feeder prop data
(plot_shredder_prop_noclam <- ggplot(filter_clam, aes(x = clam_cpue, y = D/total_cpue))+
    geom_point()+
    facet_grid("station_code")
)
#increases with clams; basically at high densities only deposit feeders coexist with clams

#plot deposit feeder count data
(plot_shredder_count_noclam <- ggplot(filter_clam, aes(x = clam_cpue, y = D))+
    geom_point()+
    facet_grid("station_code")
)
#what are the deposit feeds that remain with high clam abundance (>15,000 clams)?

#Look at the taxa present at very high clam densities-------------

#create dataframe
high_clam <- abund_trait_sample_sum_clams %>% 
  filter(clam_cpue > 15000) 

high_clam_taxa <- abund_trait %>% 
  inner_join(high_clam)  %>% 
  filter(mean_cpue>0 & !(organism_code%in%clam))

#plot high clam abundance vs other organism abundance
(plot_high_clam_taxa <- ggplot(high_clam_taxa, aes(x=clam_cpue, y=mean_cpue))+
    geom_point()+
    facet_wrap("organism_code",scales = "free_y")
  
)
#at high clam abundances (which is really just high Potamocorbula abundances) virtually all other
#are present at fairly low abundances compared to 4550 (amphipod Sinocorophium alienense)



#community composition by station----
#community compostion by station, pre and post clam invasion----