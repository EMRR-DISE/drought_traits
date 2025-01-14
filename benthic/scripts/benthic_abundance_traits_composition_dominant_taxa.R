#Drought traits project
#Benthic inverts
#Abundances of taxa and distributions of traits

#to do list
#look at data by wet/dry periods

#packages
library(tidyverse)

#read in data------------------

#abundances for the non-rare taxa (n=64) by station and month
abund5 <- read_csv("./benthic/data_output/benthic_common5_abundances.csv")

#trait data for the dominant (n=14) taxa
trait_dom <- read_csv("./benthic/data_output/traits/benthic_dom_table_q.csv")

#environmental variables
env <- read_csv("./drought_variables/drought_variables.csv")

#format trait data------------------

#histogram of body size
hist(trait_dom$body_size)
#group into bins of 10 mm

#histogram of salinity tolerance
hist(trait_dom$salinity_tolerance_cont)
#group into bins of 10 PSU
#reconsider value for Cyprideis

trait <- trait_dom %>% 
  #bin continuous predictors
  mutate(
    body_size_cat = as.factor(case_when(body_size <= 10 ~ 1
                              ,body_size <= 20 ~ 2
                              ,body_size <= 30 ~ 3
                              ,body_size <= 40 ~ 4
                              ,body_size <= 50 ~ 5
                              ,body_size > 50 ~ 6
    ))
    ,salinity_tolerance_cat = as.factor(case_when(salinity_tolerance_cont <= 10 ~ 1
                               ,salinity_tolerance_cont <= 20 ~ 2
                               ,salinity_tolerance_cont <= 30 ~ 3
                               ,salinity_tolerance_cont <= 40 ~ 4
                               ,salinity_tolerance_cont <= 50 ~ 5
                               ,salinity_tolerance_cont <= 60 ~ 6
                               ,salinity_tolerance_cont > 60 ~ 7
    )),.after = salinity_tolerance_cont
    ,across(organism_code:native,as.factor)
  ) %>% 
  #drop original continuous data columns
  select(-c(body_size,salinity_tolerance_cont)) %>% 
  glimpse()
#probably should make all these columns factors


#Summarize abundances---------------

#format abundance data
abund5s <- abund5 %>% 
  mutate(
    #add season column
    season = as.factor(
      case_when(month==12 | month==1 | month==2 ~ "w"
                       ,month==3 | month==4 | month==5 ~ "sp"
                       ,month==6 | month==7 | month==8 ~ "su"
                       ,month==9 | month==10 | month==11 ~ "f"
    )),.after = month
    #add era column (1981-1986 (6), 1987-2000 (14),2001-2021 (21))
    ,era = case_when(year_adjusted < 1987 ~ 1
                     ,year_adjusted > 1986 & year_adjusted < 2001 ~ 2
                     ,year_adjusted > 2000 ~ 3
                     )
    ,organism_code = as.factor(organism_code)
  ) %>% 
  glimpse()

#create list of dominant taxa codes to use for filtering
dom_codes <- traits %>% 
  pull(organism_code)

#filter to only include the 14 dominant taxa 
abund_dom <- abund5s %>% 
  filter(organism_code %in% dom_codes) 

#how many unique taxa remaining?
taxa_remaining <- unique(abund_dom$organism_code)
#14 as expected

#summarize abundances by station, season, year
abund_dom_stn_seas <- abund_dom %>% 
  group_by(station_code,year_adjusted, season, organism_code) %>% 
  summarise(cpue = mean(mean_cpue),.groups ='drop')

#summarize abundances by station and year
abund_dom_stn_yr <- abund_dom %>% 
  group_by(station_code,year_adjusted, organism_code) %>% 
  summarise(cpue = mean(mean_cpue),.groups ='drop')

#summarize abundances by season and year
abund_dom_seas_yr <- abund_dom %>% 
  group_by(season,year_adjusted, organism_code) %>% 
  summarise(cpue = mean(mean_cpue),.groups ='drop') %>% 
  glimpse()

#summarize abundances by year
abund_dom_yr <- abund_dom %>% 
  group_by(year_adjusted, organism_code) %>% 
  summarise(cpue = mean(mean_cpue),.groups ='drop') %>% 
  glimpse()

#summarize abundances by era
abund_dom_era <- abund_dom %>% 
  #calculate mean across stations within months
  group_by(month,year_adjusted,era,organism_code) %>% 
  summarise(cpue = mean(mean_cpue)) %>% 
  #calculate mean across months within year
  group_by(year_adjusted,era,organism_code) %>% 
  summarise(cpue = mean(cpue)) %>% 
  #calculate mean across years within era
  group_by(era, organism_code) %>% 
  summarise(cpue = mean(cpue),.groups ='drop') %>% 
  #add trait data
  left_join(trait) %>%
  glimpse()

abund_dom_era_trait <- abund_dom_era %>% 
  #convert wide to long
  pivot_longer(cols = c(armoring:native),names_to = "trait", values_to = "score") %>%
  #calculate total abundance by trait value for each era
  group_by(era,trait,score) %>% 
  summarize(t_cpue = sum(cpue),.groups = 'drop') %>% 
  glimpse()


#Plot abundance time series------------------------------

#stacked bar plot showing composition for the three eras
(plot_abund_era <- ggplot(abund_dom_era, aes(x = era, y = cpue, fill = name))+
   geom_bar(stat = "identity", color = "black")
)
#lowest abundances in 2nd era
#major drop in A. stimpsoni
#shift from Corbicula to Potamocorbula
#increase in Gammarus, Sinocorophium, V. angustipenis

#stacked bar plot by year
(plot_abund_yr <- ggplot(abund_dom_yr, aes(x = year_adjusted, y = cpue, fill = organism_code))+
   geom_bar(stat = "identity", color = "black")
 )

#stacked bar plot by season and year
(plot_abund_seas_yr <- ggplot(abund_dom_seas_yr, aes(x = year_adjusted, y = cpue, fill = organism_code))+
    geom_bar(stat = "identity", color = "black")+
    facet_grid(season~.)
)

#stacked bar plot by station and year
(plot_abund_stn_yr <- ggplot(abund_dom_stn_yr, aes(x = year_adjusted, y = cpue, fill = organism_code))+
    geom_bar(stat = "identity", color = "black")+
    facet_grid(station_code~.)
)

#Plot trait distributions------------------------
#reference intro dates of non-natives; many occurred during this time series
#consider splitting each era into wet/day

#summarize trait abundances by era

#armoring:stacked bar plot showing composition for the three eras
(plot_era_armoring <- abund_dom_era_trait %>% 
   filter(trait=="armoring") %>% 
   ggplot(aes(x = era, y = t_cpue, fill = score))+
    geom_bar(stat = "identity", position = "fill") +
    ggtitle("armoring")
)
#shift to more N at cost of P
#surprising there isn't big increase in G but probably because just shifted from corbicula to potamocorbula

#dispersal:stacked bar plot showing composition for the three eras
(plot_era_dispersal <- abund_dom_era_trait %>% 
    filter(trait=="dispersal") %>% 
    ggplot(aes(x = era, y = t_cpue, fill = score))+
    geom_bar(stat = "identity", position = "fill") +
    ggtitle("dispersal")
)
#mostly decrease in P (Corbicula only) and increase in A (Potamocorbula and others)

#habit:stacked bar plot showing composition for the three eras
(plot_era_habit <- abund_dom_era_trait %>% 
    filter(trait=="habit") %>% 
    ggplot(aes(x = era, y = t_cpue, fill = score))+
    geom_bar(stat = "identity", position = "fill") +
    ggtitle("habit")
)
#decrease in T; increase in B and S

#reproduction:stacked bar plot showing composition for the three eras
(plot_era_reproduction <- abund_dom_era_trait %>% 
    filter(trait=="reproduction") %>% 
    ggplot(aes(x = era, y = t_cpue, fill = score))+
    geom_bar(stat = "identity", position = "fill") +
    ggtitle("reproduction")
)
#little change; A is just one taxon

#body_size_cat:stacked bar plot showing composition for the three eras
(plot_era_body_size_cat <- abund_dom_era_trait %>% 
    filter(trait=="body_size_cat") %>% 
    ggplot(aes(x = era, y = t_cpue, fill = score))+
    geom_bar(stat = "identity", position = "fill") +
    ggtitle("body_size_cat")
)
#decrease in 1 and 6 (probably mostly Corbicula), increase in 2 and 3 (only potamocorbula)
#4 is just Bothrioneurum


#salinity_tolerance_cat:stacked bar plot showing composition for the three eras
(plot_era_salinity_tolerance_cat <- abund_dom_era_trait %>% 
    filter(trait=="salinity_tolerance_cat") %>% 
    ggplot(aes(x = era, y = t_cpue, fill = score))+
    geom_bar(stat = "identity", position = "fill") +
    ggtitle("salinity_tolerance_cat")
)
#increase in 3 at cost to 4; neither clam is a 3
#reconsider the 7 for Cyprideis

#trophic_habit:stacked bar plot showing composition for the three eras
(plot_era_trophic_habit <- abund_dom_era_trait %>% 
    filter(trait=="trophic_habit") %>% 
    ggplot(aes(x = era, y = t_cpue, fill = score))+
    geom_bar(stat = "identity", position = "fill") +
    ggtitle("trophic_habit")
)
#decrease in CG; increase in CF and S; note that S is one species

#voltinism:stacked bar plot showing composition for the three eras
(plot_era_voltinism <- abund_dom_era_trait %>% 
    filter(trait=="voltinism") %>% 
    ggplot(aes(x = era, y = t_cpue, fill = score))+
    geom_bar(stat = "identity", position = "fill") +
    ggtitle("voltinism")
)
#more univoltine after clams; interesting because both clams multivoltine

#native:stacked bar plot showing composition for the three eras
(plot_era_native <- abund_dom_era_trait %>% 
    filter(trait=="native") %>% 
    ggplot(aes(x = era, y = t_cpue, fill = score))+
    geom_bar(stat = "identity", position = "fill") +
    ggtitle("native")
)
#large increase in proportion of non-natives through time





#maybe get help from Dave on this
#refer back to other examples using the purrr package



