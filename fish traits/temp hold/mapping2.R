# Pete Nelson, PhD
# Department of Water Resources
# purpose: map sites from the CDFW FMWT program

# load libraries
library(ggplot2)
library(sf)
library(ggmap)
library(ggsn)
library(readxl)
library(tidyverse)
library(deltamapr)

# select site data ----
index_stations <- read_csv("fish traits/fish_data/index_stations.csv")

# plot fmwt sites ----
fmwt_sf <- st_as_sf(index_stations,
                    coords = c("longitude", "latitude"),
                    crs = 4326)

fmwt_sf2 <- st_as_sf(index_stations %>% 
                       filter(study_area == "within"),
                     coords = c("longitude", "latitude"),
                     crs = 4326)
ggplot() +
  geom_sf(data = WW_Delta) + # from deltamapr
  geom_sf(data = fmwt_sf,
          color = "red",
          fill = "red",
          shape = 21,
          alpha = 0.5,
          size = 2,
          stroke = 1) +
  labs(title = "Fall Midwater Trawl Index Sites") +
  theme_bw()

ggplot() +
  geom_sf(data = WW_Delta) + # from deltamapr
  geom_sf(data = fmwt_sf2,
          color = "blue",
          fill = "blue",
          shape = 21,
          alpha = 0.5,
          size = 2,
          stroke = 1) +
  labs(title = "Fall Midwater Trawl Index Sites") +
  theme_bw()

# SCRATCH ###########################
dt5 <- read_csv("fish_data/dt5.csv")
mwtr_sites <- dt5 %>% filter(method_code == "MWTR" | method_code == "KDTR, MWTR")
write_csv(mwtr_sites, "fish_data/mwtr_sites.csv")
seine_sites <- dt5 %>% filter(method_code == "SEIN" | method_code == "SEINE")
write_csv(seine_sites, "fish_data/seine_sites.csv")

trawls_select <- read_csv("fish_data/trawls_select.csv")
fmwt <- trawls_select %>% 
  filter(month(sample_date) %in% c(9:12)) # limit to Sep-Dec

FMWT_station_area_locations

# mod site data -----
# midwater trawl and beach seine site data from fish_sites.R
mwtr_sf <- st_as_sf(mwtr_sites,
                    coords = c("longitude", "latitude"),
                    crs = 4326) # is WGS84 the right projection?
# fmwt_sf <- st_as_sf(read_xlsx("fish_data/FMWT_Station_Locations.xlsx",
                              sheet = "FMWT_station_area_locations"),
                    coords = c("longitude", "latitude"),
                    crs = 4326)
seine_sf <- st_as_sf(seine_sites,
                    coords = c("longitude", "latitude"),
                    crs = 4326) # is WGS84 the right projection?

# shape file of Delta waterways
waterways <- read_sf("hydro_delta_marsh.shp") # issues w this file

# plot mwtr sites ----
ggplot() +
  geom_sf(data = waterways) +
  geom_sf(data = mwtr_sf,
          color = "blue",
          fill = "blue",
          shape = 21,
          alpha = 0.5,
          size = 2,
          stroke = 1) +
  labs(title = "DJFMP Midwater Trawl Sites") +
  theme_bw()

# plot fmwt sites ----
ggplot() +
  geom_sf(data = waterways) +
  geom_sf(data = fmwt_sf,
          color = "purple",
          fill = "purple",
          shape = 21,
          alpha = 0.5,
          size = 2,
          stroke = 1) +
  labs(title = "Fall Midwater Trawl Sites") +
  theme_bw()

# plot beach seine sites ----
ggplot() +
  geom_sf(data = waterways) +
  geom_sf(data = seine_sf,
          color = "green",
          fill = "green",
          shape = 21,
          alpha = 0.5,
          size = 2,
          stroke = 1) +
  labs(title = "DJFMP Beach Seine Sites") +
  theme_bw()

# heatmap code ----
# Laura's heatmap code example:
# summarize count of samples in each region by season by years
site_counts<-clams_sites4%>%group_by(Region, Year, Season)%>%summarize(n=n())

# look at seasonal sample coverage fo EMP clam data
sites_plot<-site_counts%>%ggplot(aes(x=Year,y=Region))+geom_tile(aes(fill=n))+
  scale_fill_gradient(low="white",high="red")+
  geom_text(aes(label=round(n,1)),size=4)+
  ggtitle("EMP Clam Sampling By Season")+
  facet_grid(Season~.)+
  theme_classic()+
  theme(legend.position = "none")
sites_plot
ggsave("clam_seasonal_sampling_coverage.png", sites_plot,  width=14, height=8)

