#Drought traits project
#Benthic invertebrates
#distribution of traits through time
#Leela Dixit

#packages----
#install.packages("tidyverse")
library(tidyverse)
library(stringr)


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
traits <- read_csv("./benthic/data_output/benthic_common10_by_stn_trait_data_filled_06162025.csv") %>% 
  glimpse()

#categorical salinity data
sal <- read_csv("./benthic/data_output/psu_categories_stn_yr.csv")

#format trait data----

#only keep planktonic larvae and trophic habit info
traits_pt <- traits %>%
  select(prop_d28, prop_d4, prop_d7,
         organism_code,
         native,
         target_taxon_name,
         trait,
         trait_value) %>%
  #only keeping larva and trophic habit trait data
  filter(trait %in% c("larva", "feeding_position")) %>%
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

#sum station by larva
abund_trait_disp <- abund_trait %>% 
  #sum cpue within station by larva
  group_by(station_code,year_adjusted, season, larva) %>% 
  summarise(cpue = (mean_cpue),.groups ='drop') %>% 
  glimpse()

#sum station by trophic habit
abund_trait_tro <- abund_trait %>%
  #sum cpue within station by trophic habit
  group_by(station_code,year_adjusted, season, feeding_position) %>% 
  summarise(cpue = (mean_cpue),.groups ='drop') %>% 
  glimpse()

#sum station by larva and trophic habit
abund_trait_disp_tro <- abund_trait %>% 
  #sum cpue within station by larva and trophic habit
  group_by(station_code,year_adjusted, season, larva, feeding_position) %>% 
  summarise(cpue = (mean_cpue),.groups ='drop') %>% 
  glimpse()

#sum station by larva, th, and origin
abund_trait_disp_tro_orig <- abund_trait %>% 
  #sum cpue within station by larva, th, and origin
  group_by(station_code,year_adjusted, season, native, larva, feeding_position) %>% 
  summarise(cpue = (mean_cpue),.groups ='drop') %>% 
  glimpse()


#mean CPUE at different time scales per trait----

#larva
#sum by year and station for larva
abund_trait_disp_stn_yr <- abund_trait_disp %>% 
  group_by(station_code,year_adjusted, larva) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()

#sum by year and season for larva
abund_trait_disp_seas_yr <- abund_trait_disp %>% 
  group_by(year_adjusted, season, larva) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()

#summarize by year for larva
abund_trait_disp_yr <- abund_trait_disp %>% 
  group_by(year_adjusted, larva) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()

#trophic habit
#sum by year and station for trophic habit
abund_trait_tro_stn_yr <- abund_trait_tro %>% 
  group_by(station_code,year_adjusted, feeding_position) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()

#sum by year and season for trophic habit
abund_trait_tro_seas_yr <- abund_trait_tro %>% 
  group_by(year_adjusted, season, feeding_position) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()

#summarize by year for trophic habit
abund_trait_tro_yr <- abund_trait_tro %>% 
  group_by(year_adjusted, feeding_position) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()

#mean CPUE at different time scales by combo traits----

#larva and trophic habit
#summarize by year and station
abund_trait_disp_tro_stn_yr <- abund_trait_disp_tro %>% 
  group_by(station_code,year_adjusted, larva, feeding_position) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()

#summarize by year and season
abund_trait_disp_tro_seas_yr <- abund_trait_disp_tro %>% 
  group_by(year_adjusted, season, larva, feeding_position) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()

#summarize by year
abund_trait_disp_tro_yr <- abund_trait_disp_tro %>% 
  group_by(year_adjusted, larva, feeding_position) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()

#larva and origin
#summarize by year and station
abund_trait_disp_orig_stn_yr <- abund_trait_disp_tro_orig %>% 
  group_by(station_code,year_adjusted, native, larva) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()

#summarize by year and season
abund_trait_disp_orig_seas_yr <- abund_trait_disp_tro_orig %>% 
  group_by(year_adjusted, season, larva, native) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()

#summarize by year
abund_trait_disp_orig_yr <- abund_trait_disp_tro_orig %>% 
  group_by(year_adjusted, larva, native) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()

#trophic habit and origin
#summarize by year and station
abund_trait_tro_orig_stn_yr <- abund_trait_disp_tro_orig %>% 
  group_by(station_code,year_adjusted, native, feeding_position) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()

#summarize by year and season
abund_trait_tro_orig_seas_yr <- abund_trait_disp_tro_orig %>% 
  group_by(year_adjusted, season, feeding_position, native) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()

#summarize by year
abund_trait_tro_orig_yr <- abund_trait_disp_tro_orig %>% 
  group_by(year_adjusted, feeding_position, native) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()

#larva plots----

#absolute abundance stacked bar plot by year
(plot_abund_disp_yr_abs <- ggplot(abund_trait_disp_yr, aes(x = year_adjusted, y = cpue, fill = larva))+
   geom_bar(stat = "identity", color = "black"))

#relative abundance stacked bar plot by year
(plot_abund_disp_yr_rel <- ggplot(abund_trait_disp_yr, aes(x = year_adjusted, y = cpue, fill = larva))+
    geom_bar(stat = "identity", position = "fill", color = "black"))

#very sharp increase in planktonic larva after introduction of clams, but surprising to see it drop pretty quickly

#absolute abundance stacked bar plot by station and year
(plot_abund_disp_stn_yr_abs <- ggplot(abund_trait_disp_stn_yr, aes(x = year_adjusted, y = cpue, fill = larva))+
    geom_bar(stat = "identity", color = "black")+
    facet_grid(station_code~.))
#D7 has most planktonic larva, makes sense with clams

#relative abundance stacked bar plot by station and year
(plot_abund_disp_stn_yr_rel <- ggplot(abund_trait_disp_stn_yr, aes(x = year_adjusted, y = cpue, fill = larva))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(station_code~.))
#increase in P in all, but drops in all but D7

#absolute abundance stacked bar plot by season and year
(plot_abund_disp_seas_yr_abs <- ggplot(abund_trait_disp_seas_yr, aes(x = year_adjusted, y = cpue, fill = larva))+
    geom_bar(stat = "identity", color = "black")+
    facet_grid(season~.))

#relative abundance stacked bar plot by season and year
(plot_abund_disp_seas_yr_rel <- ggplot(abund_trait_disp_seas_yr, aes(x = year_adjusted, y = cpue, fill = larva))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(season~.))


#trophic habit plots----

#absolute abundance stacked bar plot by year
(plot_abund_tro_yr_abs <- ggplot(abund_trait_tro_yr, aes(x = year_adjusted, y = cpue, fill = feeding_position))+
   geom_bar(stat = "identity", color = "black"))

#relative abundance stacked bar plot by year
(plot_abund_tro_yr_rel <- ggplot(abund_trait_tro_yr, aes(x = year_adjusted, y = cpue, fill = feeding_position))+
    geom_bar(stat = "identity", position = "fill", color = "black"))

#filter feeders boom with clams, but decrease over time. looks like their introduction allowed shredders to come into the system

#absolute abundance stacked bar plot by station and year
(plot_abund_tro_stn_yr_abs <- ggplot(abund_trait_tro_stn_yr, aes(x = year_adjusted, y = cpue, fill = feeding_position))+
    geom_bar(stat = "identity", color = "black")+
    facet_grid(station_code~.))

#relative abundance stacked bar plot by station and year
(plot_abund_tro_stn_yr_rel <- ggplot(abund_trait_tro_stn_yr, aes(x = year_adjusted, y = cpue, fill = feeding_position))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(station_code~.))

#shredders are mostly in D4 and D28...

#absolute abundance stacked bar plot by season and year
(plot_abund_tro_seas_yr_abs <- ggplot(abund_trait_tro_seas_yr, aes(x = year_adjusted, y = cpue, fill = feeding_position))+
    geom_bar(stat = "identity", color = "black")+
    facet_grid(season~.))

#relative abundance stacked bar plot by season and year
(plot_abund_tro_seas_yr_rel <- ggplot(abund_trait_tro_seas_yr, aes(x = year_adjusted, y = cpue, fill = feeding_position))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(season~.))

#combo plots----

#trophic habit and larva
#absolute abundance stacked bar plot by year
(plot_abund_disp_tro_yr_abs <- ggplot(abund_trait_disp_tro_yr, aes(x = year_adjusted, y = cpue, fill = feeding_position))+
   geom_bar(stat = "identity", color = "black")+
   facet_grid(larva~.))

#relative abundance stacked bar plot by year
(plot_abund_disp_tro_yr_rel <- ggplot(abund_trait_disp_tro_yr, aes(x = year_adjusted, y = cpue, fill = feeding_position))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(larva~.))

#absolute abundance stacked bar plot by station and year
(plot_abund_disp_tro_stn_yr_abs <- ggplot(abund_trait_disp_tro_stn_yr, aes(x = year_adjusted, y = cpue, fill = feeding_position))+
   geom_bar(stat = "identity", color = "black")+
   facet_grid(station_code~larva))

#relative abundance stacked bar plot by station and year
(plot_abund_disp_tro_stn_yr_rel <- ggplot(abund_trait_disp_tro_stn_yr, aes(x = year_adjusted, y = cpue, fill = feeding_position))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(station_code~larva))

#absolute abundance stacked bar plot by season and year
(plot_abund_disp_tro_seas_yr_abs <- ggplot(abund_trait_disp_tro_seas_yr, aes(x = year_adjusted, y = cpue, fill = larva))+
    geom_bar(stat = "identity", color = "black")+
    facet_grid(season~feeding_position))

#relative abundance stacked bar plot by season and year
(plot_abund_disp_tro_seas_yr_rel <- ggplot(abund_trait_disp_tro_seas_yr, aes(x = year_adjusted, y = cpue, fill = feeding_position))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(season~larva))

#trophic habit and origin
#absolute abundance stacked bar plot by year
(plot_abund_tro_orig_yr_abs <- ggplot(abund_trait_tro_orig_yr, aes(x = year_adjusted, y = cpue, fill = feeding_position))+
    geom_bar(stat = "identity", color = "black")+
    facet_grid(native~.))

#relative abundance stacked bar plot by year
(plot_abund_tro_orig_yr_rel <- ggplot(abund_trait_tro_orig_yr, aes(x = year_adjusted, y = cpue, fill = feeding_position))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(native~.))

#absolute abundance stacked bar plot by station and year
(plot_abund_tro_orig_stn_yr_abs <- ggplot(abund_trait_tro_orig_stn_yr, aes(x = year_adjusted, y = cpue, fill = feeding_position))+
    geom_bar(stat = "identity", color = "black")+
    facet_grid(station_code~native))

#relative abundance stacked bar plot by station and year
(plot_abund_tro_orig_stn_yr_rel <- ggplot(abund_trait_tro_orig_stn_yr, aes(x = year_adjusted, y = cpue, fill = feeding_position))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(station_code~native))

#absolute abundance stacked bar plot by season and year
(plot_abund_tro_orig_seas_yr_abs <- ggplot(abund_trait_tro_orig_seas_yr, aes(x = year_adjusted, y = cpue, fill = feeding_position))+
    geom_bar(stat = "identity", color = "black")+
    facet_grid(season~native))

#relative abundance stacked bar plot by season and year
(plot_abund_tro_orig_seas_yr_rel <- ggplot(abund_trait_tro_orig_seas_yr, aes(x = year_adjusted, y = cpue, fill = feeding_position))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(season~native))

#larva and origin
#absolute abundance stacked bar plot by year
(plot_abund_disp_orig_yr_abs <- ggplot(abund_trait_disp_orig_yr, aes(x = year_adjusted, y = cpue, fill = larva))+
    geom_bar(stat = "identity", color = "black")+
    facet_grid(native~.))

#relative abundance stacked bar plot by year
(plot_abund_disp_orig_yr_rel <- ggplot(abund_trait_disp_orig_yr, aes(x = year_adjusted, y = cpue, fill = larva))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(native~.))

#absolute abundance stacked bar plot by station and year
(plot_abund_disp_orig_stn_yr_abs <- ggplot(abund_trait_disp_orig_stn_yr, aes(x = year_adjusted, y = cpue, fill = larva))+
    geom_bar(stat = "identity", color = "black")+
    facet_grid(station_code~native))

#relative abundance stacked bar plot by station and year
(plot_abund_disp_orig_stn_yr_rel <- ggplot(abund_trait_disp_orig_stn_yr, aes(x = year_adjusted, y = cpue, fill = larva))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(station_code~native))

#absolute abundance stacked bar plot by season and year
(plot_abund_disp_orig_seas_yr_abs <- ggplot(abund_trait_disp_orig_seas_yr, aes(x = year_adjusted, y = cpue, fill = larva))+
    geom_bar(stat = "identity", color = "black")+
    facet_grid(season~native))

#relative abundance stacked bar plot by season and year
(plot_abund_disp_orig_seas_yr_rel <- ggplot(abund_trait_disp_orig_seas_yr, aes(x = year_adjusted, y = cpue, fill = larva))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(season~native))


#no clams: sum CPUE by trait----

#sum station by larva
abund_trait_noclam_disp <- abund_trait_noclam %>%
  #sum cpue within station by origin
  group_by(station_code,year_adjusted, season, larva) %>% 
  summarise(cpue = (mean_cpue),.groups ='drop') %>% 
  glimpse()

#sum station by trophic habit
abund_trait_noclam_tro <- abund_trait_noclam %>%
  #sum cpue within station by origin
  group_by(station_code,year_adjusted, season, feeding_position) %>% 
  summarise(cpue = (mean_cpue),.groups ='drop') %>% 
  glimpse()

#summarize station data by larva, trophic habit, origin
abund_trait_noclam_disp_tro_orig <- abund_trait_noclam %>% 
  #sum cpue within station by origin, larva, trophic habit
  group_by(station_code,year_adjusted, season, native, larva, feeding_position) %>% 
  summarise(cpue = (mean_cpue),.groups ='drop') %>% 
  glimpse()

#no clams: mean CPUE by different time scales per trait----

#larva
#summarize by year and station for larva
abund_trait_noclam_disp_stn_yr <- abund_trait_noclam_disp %>% 
  group_by(station_code,year_adjusted, larva) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()

#summarize by year and season for larva
abund_trait_noclam_disp_seas_yr <- abund_trait_noclam_disp %>% 
  group_by(year_adjusted, season, larva) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()

#summarize by year for larva
abund_trait_noclam_disp_yr <- abund_trait_noclam_disp %>% 
  group_by(year_adjusted, larva) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()

#trophic habit
#summarize by year and station for trophic habit
abund_trait_noclam_tro_stn_yr <- abund_trait_noclam_tro %>% 
  group_by(station_code,year_adjusted, feeding_position) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()

#summarize by year and season for trophic habit
abund_trait_noclam_tro_seas_yr <- abund_trait_noclam_tro %>% 
  group_by(year_adjusted, season, feeding_position) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()

#summarize by year for trophic habit
abund_trait_noclam_tro_yr <- abund_trait_noclam_tro %>% 
  group_by(year_adjusted, feeding_position) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()


#no clams: mean CPUE by different time scales by combo trait----

#larva and trophic habit
#summarize by year and station
abund_trait_noclam_disp_tro_stn_yr <- abund_trait_noclam_disp_tro_orig %>% 
  group_by(station_code,year_adjusted, larva, feeding_position) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()

#summarize by year and season
abund_trait_noclam_disp_tro_seas_yr <- abund_trait_noclam_disp_tro_orig %>% 
  group_by(year_adjusted, season, larva, feeding_position) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()

#summarize by year
abund_trait_noclam_disp_tro_yr <- abund_trait_noclam_disp_tro_orig %>% 
  group_by(year_adjusted, larva, feeding_position) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()

#larva and origin
#summarize by year and station
abund_trait_noclam_disp_orig_stn_yr <- abund_trait_noclam_disp_tro_orig %>% 
  group_by(station_code,year_adjusted, larva, native) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()

#summarize by year and season
abund_trait_noclam_disp_orig_seas_yr <- abund_trait_noclam_disp_tro_orig %>% 
  group_by(year_adjusted, season, larva, native) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()

#summarize by year
abund_trait_noclam_disp_orig_yr <- abund_trait_noclam_disp_tro_orig %>% 
  group_by(year_adjusted, larva, native) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()

#trophic habit and origin
#summarize by year and station
abund_trait_noclam_tro_orig_stn_yr <- abund_trait_noclam_disp_tro_orig %>% 
  group_by(station_code,year_adjusted, native, feeding_position) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()

#summarize by year and season
abund_trait_noclam_tro_orig_seas_yr <- abund_trait_noclam_disp_tro_orig %>% 
  group_by(year_adjusted, season, native, feeding_position) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()

#summarize by year
abund_trait_noclam_tro_orig_yr <- abund_trait_noclam_disp_tro_orig %>% 
  group_by(year_adjusted, native, feeding_position) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  glimpse()


#no clam: larva plot----

#absolute abundance stacked bar plot by year
(plot_abund_noclam_disp_yr_abs <- ggplot(abund_trait_noclam_disp_yr, aes(x = year_adjusted, y = cpue, fill = larva))+
   geom_bar(stat = "identity", color = "black"))

#relative abundance stacked bar plot by year
(plot_abund_noclam_disp_yr_rel <- ggplot(abund_trait_noclam_disp_yr, aes(x = year_adjusted, y = cpue, fill = larva))+
    geom_bar(stat = "identity", position = "fill", color = "black"))

#absolute abundance stacked bar plot by station and year
(plot_abund_noclam_disp_stn_yr_abs <- ggplot(abund_trait_noclam_disp_stn_yr, aes(x = year_adjusted, y = cpue, fill = larva))+
    geom_bar(stat = "identity", color = "black")+
    facet_grid(station_code~.))

#relative abundance stacked bar plot by station and year
(plot_abund_noclam_disp_stn_yr_rel <- ggplot(abund_trait_noclam_disp_stn_yr, aes(x = year_adjusted, y = cpue, fill = larva))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(station_code~.))

#absolute abundance stacked bar plot by season and year
(plot_abund_noclam_disp_seas_yr_abs <- ggplot(abund_trait_noclam_disp_seas_yr, aes(x = year_adjusted, y = cpue, fill = larva))+
    geom_bar(stat = "identity", color = "black")+
    facet_grid(season~.))

#relative abundance stacked bar plot by season and year
(plot_abund_noclam_disp_seas_yr_rel <- ggplot(abund_trait_noclam_disp_seas_yr, aes(x = year_adjusted, y = cpue, fill = larva))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(season~.))


#no clam: trophic habit plot----
#absolute abundance stacked bar plot by year
(plot_abund_noclam_tro_yr_abs <- ggplot(abund_trait_noclam_tro_yr, aes(x = year_adjusted, y = cpue, fill = feeding_position))+
   geom_bar(stat = "identity", color = "black"))

#relative abundance stacked bar plot by year
(plot_abund_noclam_tro_yr_rel <- ggplot(abund_trait_noclam_tro_yr, aes(x = year_adjusted, y = cpue, fill = feeding_position))+
    geom_bar(stat = "identity", position = "fill", color = "black"))


#absolute abundance stacked bar plot by station and year
(plot_abund_noclam_tro_stn_yr_abs <- ggplot(abund_trait_noclam_tro_stn_yr, aes(x = year_adjusted, y = cpue, fill = feeding_position))+
    geom_bar(stat = "identity", color = "black")+
    facet_grid(station_code~.))

#relative abundance stacked bar plot by station and year
(plot_abund_noclam_tro_stn_yr_rel <- ggplot(abund_trait_noclam_tro_stn_yr, aes(x = year_adjusted, y = cpue, fill = feeding_position))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(station_code~.))

#absolute abundance stacked bar plot by season and year
(plot_abund_noclam_tro_seas_yr_abs <- ggplot(abund_trait_noclam_tro_seas_yr, aes(x = year_adjusted, y = cpue, fill = feeding_position))+
    geom_bar(stat = "identity", color = "black")+
    facet_grid(season~.))

#relative abundance stacked bar plot by season and year
(plot_abund_noclam_tro_seas_yr_rel <- ggplot(abund_trait_noclam_tro_seas_yr, aes(x = year_adjusted, y = cpue, fill = feeding_position))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(season~.))

#no clam: combo plots----

#trophic habit and larva
#absolute abundance stacked bar plot by year
(plot_abund_noclam_disp_tro_yr_abs <- ggplot(abund_trait_noclam_disp_tro_yr, aes(x = year_adjusted, y = cpue, fill = feeding_position))+
   geom_bar(stat = "identity", color = "black")+
   facet_grid(larva~.))

#relative abundance stacked bar plot by year
(plot_abund_noclam_disp_tro_yr_rel <- ggplot(abund_trait_noclam_disp_tro_yr, aes(x = year_adjusted, y = cpue, fill = feeding_position))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(larva~.))

#absolute abundance stacked bar plot by station and year
(plot_abund_noclam_disp_tro_stn_yr_abs <- ggplot(abund_trait_noclam_disp_tro_stn_yr, aes(x = year_adjusted, y = cpue, fill = feeding_position))+
    geom_bar(stat = "identity", color = "black")+
    facet_grid(station_code~larva))

#relative abundance stacked bar plot by station and year
(plot_abund_noclam_disp_tro_stn_yr_rel <- ggplot(abund_trait_noclam_disp_tro_stn_yr, aes(x = year_adjusted, y = cpue, fill = feeding_position))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(station_code~larva))

#absolute abundance stacked bar plot by season and year
(plot_abund_noclam_disp_tro_seas_yr_abs <- ggplot(abund_trait_noclam_disp_tro_seas_yr, aes(x = year_adjusted, y = cpue, fill = feeding_position))+
    geom_bar(stat = "identity", color = "black")+
    facet_grid(season~larva))

#relative abundance stacked bar plot by season and year
(plot_abund_noclam_disp_tro_seas_yr_rel <- ggplot(abund_trait_noclam_disp_tro_seas_yr, aes(x = year_adjusted, y = cpue, fill = feeding_position))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(season~larva))

#trophic habit and origin
#absolute abundance stacked bar plot by year
(plot_abund_noclam_tro_orig_yr_abs <- ggplot(abund_trait_noclam_tro_orig_yr, aes(x = year_adjusted, y = cpue, fill = feeding_position))+
    geom_bar(stat = "identity", color = "black")+
    facet_grid(native~.))

#relative abundance stacked bar plot by year
(plot_abund_noclam_tro_orig_yr_rel <- ggplot(abund_trait_noclam_tro_orig_yr, aes(x = year_adjusted, y = cpue, fill = feeding_position))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(native~.))

#absolute abundance stacked bar plot by station and year
(plot_abund_noclam_tro_orig_stn_yr_abs <- ggplot(abund_trait_noclam_tro_orig_stn_yr, aes(x = year_adjusted, y = cpue, fill = feeding_position))+
    geom_bar(stat = "identity", color = "black")+
    facet_grid(station_code~native))

#relative abundance stacked bar plot by station and year
(plot_abund_noclam_tro_orig_stn_yr_rel <- ggplot(abund_trait_noclam_tro_orig_stn_yr, aes(x = year_adjusted, y = cpue, fill = feeding_position))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(station_code~native))

#absolute abundance stacked bar plot by season and year
(plot_abund_noclam_tro_orig_seas_yr_abs <- ggplot(abund_trait_noclam_tro_orig_seas_yr, aes(x = year_adjusted, y = cpue, fill = feeding_position))+
    geom_bar(stat = "identity", color = "black")+
    facet_grid(season~native))

#relative abundance stacked bar plot by season and year
(plot_abund_noclam_disp_tro_seas_yr_rel <- ggplot(abund_trait_noclam_tro_orig_seas_yr, aes(x = year_adjusted, y = cpue, fill = feeding_position))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(season~native))

#larva and origin
#absolute abundance stacked bar plot by year
(plot_abund_noclam_disp_orig_yr_abs <- ggplot(abund_trait_noclam_disp_orig_yr, aes(x = year_adjusted, y = cpue, fill = larva))+
    geom_bar(stat = "identity", color = "black")+
    facet_grid(native~.))

#relative abundance stacked bar plot by year
(plot_abund_noclam_disp_orig_yr_rel <- ggplot(abund_trait_noclam_disp_orig_yr, aes(x = year_adjusted, y = cpue, fill = larva))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(native~.))

#absolute abundance stacked bar plot by station and year
(plot_abund_noclam_disp_orig_stn_yr_abs <- ggplot(abund_trait_noclam_disp_orig_stn_yr, aes(x = year_adjusted, y = cpue, fill = larva))+
    geom_bar(stat = "identity", color = "black")+
    facet_grid(station_code~native))

#relative abundance stacked bar plot by station and year
(plot_abund_noclam_disp_orig_stn_yr_rel <- ggplot(abund_trait_noclam_disp_orig_stn_yr, aes(x = year_adjusted, y = cpue, fill = larva))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(station_code~native))

#absolute abundance stacked bar plot by season and year
(plot_abund_noclam_disp_orig_seas_yr_abs <- ggplot(abund_trait_noclam_disp_orig_seas_yr, aes(x = year_adjusted, y = cpue, fill = larva))+
    geom_bar(stat = "identity", color = "black")+
    facet_grid(season~native))

#relative abundance stacked bar plot by season and year
(plot_abund_noclam_disp_orig_seas_yr_rel <- ggplot(abund_trait_noclam_disp_orig_seas_yr, aes(x = year_adjusted, y = cpue, fill = larva))+
    geom_bar(stat = "identity", position = "fill", color = "black")+
    facet_grid(season~native))


#Plots of proportion of organisms with planktonic larvae vs total organisms---------------
#inspired by Nichols and Thompson 1985


#create df that calculates proportion of organisms with planktonic larvae
abund_trait_sample_sum_plank <- abund_trait %>% 
  group_by(sample_date,station_code,larva) %>% 
  summarize(total_cpue = sum(mean_cpue)) %>% 
  #convert long to wide
  pivot_wider(names_from = larva, values_from = total_cpue) %>% 
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
  group_by(sample_date,station_code,larva) %>% 
  summarize(total_cpue = sum(mean_cpue)) %>% 
  #convert long to wide
  pivot_wider(names_from = larva, values_from = total_cpue) %>% 
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
  group_by(sample_date,station_code,feeding_position) %>% 
  summarize(total_cpue = sum(mean_cpue)) %>% 
  #convert long to wide
  pivot_wider(names_from = feeding_position, values_from = total_cpue) %>%   
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

#similar plot but by trophic habitat rather than taxon
(plot_high_clam_th <- ggplot(high_clam_taxa, aes(x=clam_cpue, y=mean_cpue))+
    geom_point()+
    facet_wrap("feeding_position"
               ,scales = "free_y"
               ,ncol = 1
               )
  
)


#community composition by station----
#trait composition by salinity category----

med_cat_str = c('F'='fresh','VL'='very low','L'='low','B'='brackish',"VB'='very brackish")

#join trait data with salinity data
trait_sal <- full_join(abund_trait, sal, by= "station_code")%>%
  as.character(trait_sal$pss_median_category)
trait_sal$median_cat_short=trait_sal$pss_median_category
trait_sal$median_cat_short <- str_replace_all(string=text, pattern=med_cat_str)

#larva for median salinity categories
(plot_abund_trait_sal <- ggplot(trait_sal, aes(x=pss_median_category, y=mean_cpue, fill=larva))+
  geom_bar(stat="identity")+
  facet_grid(station_code~.)+
  scale_x_discrete(limits=c('fresh', 'very low', 'low', 'brackish', 'very brackish')))

#trophic_habit for median salinity categories
(plot_abund_trait_sal <- ggplot(trait_sal, aes(x=pss_median_category, y=mean_cpue, fill=feeding_position))+
    geom_bar(stat="identity")+
    facet_grid(station_code~.)+
    scale_x_discrete(limits=c('fresh', 'very low', 'low','brackish', 'very brackish')))

(plot_abund_trait_sal <- ggplot(trait_sal, aes(x=pss_median_category, y=mean_cpue, fill=feeding_position))+
    geom_bar(stat="identity", position="fill")+
    scale_x_discrete(limits=c('fresh','very low', 'low','brackish', 'very brackish')))

#larva for mean salinity categories
(plot_abund_trait_sal <- ggplot(trait_sal, aes(x=pss_mean_category, y=mean_cpue, fill=larva))+
    geom_bar(stat="identity", position ="fill")+
    scale_x_discrete(limits=c('fresh', 'very low', 'low', 'brackish', 'very brackish')))

#trophic_habit for mean salinity categories
(plot_abund_trait_sal <- ggplot(trait_sal, aes(x=pss_mean_category, y=mean_cpue, fill=feeding_position))+
    geom_bar(stat="identity", position ="fill")+
    scale_x_discrete(limits=c('fresh', 'very low', 'low', 'brackish', 'very brackish')))

#no clam: trait composition by salinity category----

#join trait data with salinity data
trait_sal_noclam <- full_join(abund_trait_noclam, sal, by= "station_code")

#larva for median salinity categories
(plot_abund_trait_sal_noclam <- ggplot(trait_sal_noclam, aes(x=pss_median_category, y=mean_cpue, fill=larva))+
    geom_bar(stat="identity")+
    facet_grid(station_code~.)+
    scale_x_discrete(limits=c('fresh', 'very low', 'low', 'brackish', 'very brackish')))

#trophic_habit for median salinity categories
(plot_abund_trait_sal_noclam <- ggplot(trait_sal_noclam, aes(x=pss_median_category, y=mean_cpue, fill=feeding_position))+
    geom_bar(stat="identity")+
    facet_grid(station_code~.)+
    scale_x_discrete(limits=c('fresh', 'very low', 'low','brackish', 'very brackish')))

#larva for mean salinity categories
(plot_abund_trait_sal_noclam <- ggplot(trait_sal_noclam, aes(x=pss_mean_category, y=mean_cpue, fill=larva))+
    geom_bar(stat="identity", position ="fill")+
    scale_x_discrete(limits=c('fresh', 'very low', 'low', 'brackish', 'very brackish')))

#trophic_habit for mean salinity categories
(plot_abund_trait_sal_noclam <- ggplot(trait_sal_noclam, aes(x=pss_mean_category, y=mean_cpue, fill=feeding_position))+
    geom_bar(stat="identity", position ="fill")+
    scale_x_discrete(limits=c('fresh', 'very low', 'low', 'brackish', 'very brackish')))

#trait composition over time with salinity category----

(plot_abund_trait_sal <- ggplot(trait_sal, aes(x=year, y=mean_cpue, fill=larva))+
   geom_bar(stat="identity")+
   facet_grid(station_code~.)+
  geom_text(data=trait_sal, label=trait_sal$pss_median_category))
  
