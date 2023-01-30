# Pete Nelson, PhD
# Department of Water Resources
# purpose: refine/explore beach seine data from the DJFMP

# drought/trait study ----

library(tidyverse)
library(lubridate)
library(hms)
library(readxl)

# set up -----
# Clear everything except the beach seine data objects, 
# plus the names & location info.
rm(list= ls()[!(ls() %in% c("dt3", "dt4", "dt5", # raw data files
                            "f_dat", "f_stn",    # working files
                            "dt3_test"))])       # test fish data

# Save time by loading those raw data files ("dt3", "dt4", "dt5"):
dt3 <- read.csv("fish_data/dt3.csv") # beach seine data
dt4 <- read.csv("fish_data/dt4.csv") # fish names
dt5 <- read.csv("fish_data/dt5.csv") # locations (beach seine + other methods)

f_dat <- read_csv("fish_data/f_dat.csv") # prepped beach seine data
f_stn <- read_csv("fish_data/f_stn.csv")
dt3_test <- read_csv("fish_data/dt3_test.csv")

# refine data files -----
## fish data -----
f_dat <- tibble(dt3[, c(1:5, 7:11, 13, 18:22, 26, 28:29, 33, 48)]) %>% 
  mutate(across(c(1:3, 18:19), factor)) %>% 
  mutate(date = as_date(sample_date),
         time = as_hms(sample_time), # convert to time of day (pkg hms)
         .keep = "unused",
         .after = station_code) %>% 
  filter(volume < 200 | is.na(volume), # greater values are errors (n=14)
         do < 50 | is.na(volume) # greater values are errors (n=27)
         ) 

## save copy of the fish data
write_csv(f_dat, "fish_data/f_dat.csv")

# pivot beach seine data so each line reps a 
# single seine set
seines <- f_dat %>% 
  group_by(region_code, station_code, date, 
                  do, water_temp, turbidity, specific_conductance,
                  volume, iep_fish_code) %>% 
  summarise(total_abundance = sum(count)) %>% 
  pivot_wider(names_from = iep_fish_code,
                      values_from = total_abundance,
                      values_fill = 0)

## length of time series
history_seine_sta <- f_dat %>% 
  group_by(location, station_code) %>% 
  summarise(year_start = min(year(date)), 
            year_end = max(year(date)),
            n = n())
write_csv(history_seine_sta, "fish_data/history_seine_sta.csv")

history_loc <- f_dat %>% 
  group_by(location) %>% 
  summarise(ts_length = max(year(date)) - min(year(date)))
history_location <- history_loc %>% 
  filter(ts_length >= 20)
write_csv(history_loc, "fish_data/history_loc.csv")
ggplot(history_location) +
  aes(x = as.factor(location), y = ts_length) +
  ggtitle("DJFMP Beach Seine Data", 
          subtitle = "time series may not be continuous") +
  xlab("location") + ylab("monitoring time span (years)") +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

history_station <- f_dat %>% 
  group_by(station_code, year = year(date)) %>% 
  summarise(n = n()) %>% 
  group_by(station_code) %>% 
  summarise(number_years = n())
ggplot(history_station) +
  aes(x = as.factor(station_code), y = number_years) +
  geom_bar(stat = "identity")

# numerical importance by sp
# note: iep_fish_code=NA includes multiple invertebrates
spp <- f_dat %>% 
  group_by(iep_fish_code) %>% 
  summarise(n = sum(count)) %>% 
  drop_na(iep_fish_code) %>% 
  arrange(., desc(n))
  
write_csv(arrange(spp, desc(n)), "spp.csv")

spp_top <- spp[1:10,]

ggplot(spp_top) +
  aes(x = iep_fish_code, 
       y = n) +
  geom_bar(stat = "identity") # not very useful!

dt3_test %>% 
  select(iep_fish_code, count, fl) %>% 
  mutate(derived_wt = 0.00085 * fl^2.91,
         biomass = derived_wt * count)

dt3_test %>% 
  select(iep_fish_code, count, fl) %>% 
  arrange(iep_fish_code) %>% 
  mutate(derived_wt = case_when(iep_fish_code == "BLACRA" ~ 0.0012 * fl^2.89,
                                iep_fish_code == "CHISAL" ~ 0.0009 * fl^3.02,
                                iep_fish_code == "LARBAS" ~ 0.001 * fl^3.2,
                                iep_fish_code == "MISSIL" ~ 0.00085 * fl^2.91,
                                iep_fish_code == "WESMOS" ~ 0.00075 * fl^2.6),
         biomass = derived_wt * count)

## QAQC -----
# check volume, do, turbidity & specific conductance data
vol_freq <- dt3 %>% 
  group_by(volume) %>% 
  summarise(n = n()) %>% 
  drop_na(volume)
ggplot(vol_freq) +
  aes(x = volume) +
  geom_histogram() # volume>200 likely error! records removed above

do_freq <- dt3 %>% 
  group_by(do) %>% 
  summarise(n = n()) %>% 
  drop_na(do)
ggplot(do_freq) +
  aes(x = do) +
  geom_histogram() # do>50 likely error but what are the units?! records n=27 removed above
do_freq2 <- dt3 %>% 
  filter(do < 50) %>% 
  group_by(do) %>% 
  summarise(n = n()) %>% 
  drop_na(do)
ggplot(do_freq2) +
  aes(x = do) +
  geom_histogram() # one outlier; retained

turbidity_freq <- dt3 %>% 
  group_by(turbidity) %>% 
  summarise(n = n()) %>% 
  drop_na(turbidity)
ggplot(turbidity_freq) +
  aes(x = turbidity) +
  geom_histogram() # turbidity>500 rare but possible; nothing removed

specific_conductance_freq <- dt3 %>% 
  group_by(specific_conductance) %>% 
  summarise(n = n()) %>% 
  drop_na(specific_conductance)
ggplot(specific_conductance_freq) +
  aes(x = specific_conductance) +
  geom_histogram() # interesting pattern; nothing removed; 
# values>50K may reflect issues w using correct units or working in seawater

# check gear_condition_code, site_disturbance & alternate_site
gear_cond_freq <- dt3 %>% 
  group_by(gear_condition_code) %>% 
  summarise(n = n()) %>% 
  drop_na(gear_condition_code)
gear_cond_freq
ggplot(gear_cond_freq) +
  aes(x = as.factor(gear_condition_code), 
      y = n) +
  geom_bar(stat = "identity") # given as '9' in only one instance; nothing removed

site_dist_freq <- dt3 %>% 
  group_by(site_disturbance) %>% 
  summarise(n = n())
site_dist_freq
ggplot(site_dist_freq) +
  aes(x = as.factor(site_disturbance),
      y = n) +
  geom_bar(stat = "identity")

# site_disturbance noted 
dt3 %>% filter(site_disturbance == "Y") # n=9,136

# site_disturbance noted and alternate_site selected
dt3 %>% filter(site_disturbance == "Y" &
                 alternate_site == "Y") # n=53

# site_disturbance noted but alternate_site NOT selected
dt3 %>% filter(site_disturbance == "Y" &
                 alternate_site == "N") # n=9,083

location_use_freq <- dt3 %>% 
  group_by(location) %>% 
  summarise(n = n())
View(location_use_freq)
ggplot(location_use_freq) +
  aes(x = as.factor(location),
      y = n) +
  geom_bar(stat = "identity")
levels(as.factor(dt3$location)) # n=66
levels(as.factor(dt3$region_code)) # n=7
levels(as.factor(dt3$station_code)) # n=86

# to-do's -----
# Determine which regions or locations to include...
# Determine which species to select from the beach seine data...

# locations -----
seine_locations <- tibble(dt5) %>% 
  filter(method_code == "SEIN") %>% 
  mutate(lat = latitude, lon = longitude, .keep = "unused") %>% 
  select(-"method_code") %>%  # beach seine data only anyway
  arrange(desc(lat)) # sort by latitude, N to S
write_csv(seine_locations, "fish_data/seine_locations.csv")

# questions ----
# probably best data set for silversides, possibly tule perch & splittail, other spp?
# excellent for looking at community effects too? compare to MWTR?

# summary data ----
levels(f_dat$location) # too many! n=66
levels(f_dat$region_code) # 1:7 whatever these mean; using region 2 for now (n location = )
seine_r2 <- f_dat %>% filter(region_code == 2) # 12 locations; may want to restrict further
dim(seine_r2) # 113,335 x 21
range(seine_r2$volume, na.rm = T) # 0.6 to 135.0
median(seine_r2$volume, na.rm = TRUE) # 46.8
# suggest min volume = 20?! except early years (pre-2010) didn't estimate volume at all
# suggest instead assume all seines were roughly equivalent when estimating effort

# Oncorhynchus tshawytscha = CHISAL (Chinook Salmon)
# Hypomesus transpacificus = DELSME (Delta Smelt)
# Acipenser transmontanus = WHISTU (White Sturgeon)
# Hysterocarpus traskii = TULPER (Tule Perch)
# Morone saxatilis = STRBAS (Striped Bass; age-0, plus older?)
# Menidia audens = MISSIL; note that unidentified silversides are coded UNIATH
# Pogonichthys macrolepidotus = SPLITT (Splittail)

seine_R2 <- seine_r2 %>% 
  pivot_wider(id_cols = c(location, station_code, date, gear_condition_code,
                          do, water_temp, turbidity, specific_conductance, volume),
              names_from = iep_fish_code,
              values_from = count,
              values_fn = sum,
              values_fill = 0) %>% 
  mutate(year_wk = yearweek(date)) %>% 
  select(location, station_code, year_wk, date, 
         gear_condition_code,
         do, water_temp, turbidity, specific_conductance, volume, 
         CHISAL, DELSME, TULPER, STRBAS, MISSIL, SPLITT)
View(seine_R2)

seine_summary <- seine_R2 %>%
  group_by(year = year(date)) %>% 
  summarise(CHISAL = sum(CHISAL),
            DELSME = sum(DELSME),
            TULPER = sum(TULPER),
            STRBAS = sum(STRBAS),
            MISSIL = sum(MISSIL), # isn't there a semi-recent paper on silversides?
            SPLITT = sum(SPLITT))
# ^ some work yet to do to account for changing identifications for silversides
# and probably others. Also, what the hell happened in 1986? Not to mention 1985!
write_csv(seine_summary, "fish_data/seine_summary.csv")

# SCRATCH PAPER #####
## site_disturbance(NA, n=726,686; 'Y', n=9,136), drop_na(dt3, site_disturbance)
# volume = NA (n=42,361)
print(dt3 %>% filter(is.na(volume)), width = Inf)
drop_na(dt3, volume) # n=844,338
dt3 %>% 
  filter(is.na(volume)) %>% 
  filter(seine_length >= 1 | seine_width >= 1 | seine_depth >= 0.1)
# There are 168 cases where 1 or more seine dimensions are lacking and volume=NA.



# from Nick
library(tidyverse)
library(sf)
library(deltamapr)

#look at bounding box for delta regions
bdb <- st_bbox(R_EDSM_Subregions_1617P1)

(spst_reg<-ggplot()+
    #CDFW Delta waterways
    geom_sf(data= WW_Delta, fill= "skyblue3", color= "black")+
    #EDSM 2017-18 Phase 1 Strata
    geom_sf(data =R_EDSM_Subregions_1617P1, aes(fill=SubRegion,alpha=0.8))+
    #Box picks up a few unneeded sampling over in Taylor Slough 
    coord_sf( 
      xlim =c(-122.1,-121.2),
      ylim = c(37.75,38.6)
    )+
    ggtitle("R_EDSM_Subregions_1617P1")+
    theme_bw()
)
