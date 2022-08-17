# mapping DJFMP (fish) monitoring sites

library(ggplot2)
library(sf)
library(ggmap)
library(ggsn)
library(readxl)
library(tidyverse)

# DJFMP sites ----

djfmp <- read_xlsx("fish_data/DJFMP_site_locations.xlsx", sheet = "DJFMP_site_locations")
djfmp <- djfmp %>% 
  filter(latitude < 38.68 & 
           method_code != "KDTR") 
# removes sites upstream of Discovery Park (SR060E) and Kodiak trawl locations

djfmpsf <- st_as_sf(djfmp, 
                    coords = c("longitude", "latitude"),  
                    crs = 4326) # this is WGS 84!

waterways <- read_sf("fish_data/hydro_delta_marsh.shp")
# only the lower-most part of Sac R
waterways # note that these are polygons, not points, and use CRS NAD 83, not WGS 84!

ggplot() + 
  geom_sf(data = waterways) + 
  geom_sf(data = djfmpsf)

ggplot() + 
  geom_sf(data = waterways) + 
  geom_sf(data = djfmpsf, aes(color = method_code)) +
  theme_bw() 

## MWTR -----
ggplot() + 
  geom_sf(data = waterways) + 
  geom_sf(data = djfmpsf[djfmpsf$method_code == "MWTR",],
          color = "blue",
          fill = "green",
          shape = 21,
          alpha = 0.5,
          size = 2,
          stroke = 1) +
  annotate("point", # office location, why not?
           x = -121.5629, y = 38.5742,
           color = "red",
           fill = "purple",
           shape = 23, # or 18, solid diamond
           stroke = 1,
           size = 2) +
  labs(title = "Midwater Trawl sites (fishes)") +
  theme_bw()

## SEIN -----
ggplot() + 
  geom_sf(data = waterways) + 
  geom_sf(data = djfmpsf[djfmpsf$method_code == "SEIN",],
          color = "blue",
          fill = "blue",
          shape = 21,
          alpha = 0.5,
          size = 2,
          stroke = 1) +
  annotate("point", # office location, why not?
           x = -121.5629, y = 38.5742,
           color = "red",
           fill = "purple",
           shape = 23, # or 18, solid diamond
           stroke = 1,
           size = 2) +
  theme_bw()

# max lat for 'waterways' polygon = 38.67781, so limit beach seine sites
# to those at or south of this...
ggplot() + 
  geom_sf(data = waterways) + 
  geom_sf(data = djfmpsf[djfmpsf$method_code == "SEIN",],
          color = "blue",
          fill = "yellow",
          shape = 21,
          alpha = 0.5,
          size = 2,
          stroke = 1) +
  annotate("point", # office location, why not?
           x = -121.5629, y = 38.5742,
           color = "red",
           fill = "purple",
           shape = 23, # or 18, solid diamond
           stroke = 1,
           size = 2) +
  theme_bw()
