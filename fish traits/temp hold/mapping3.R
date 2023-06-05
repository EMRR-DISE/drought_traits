# sample locations
# Dave Bosworth, 14 Oct 2022

# global code & functions
# Load packages
library(tidyverse)
library(readxl)
library(sf)
library(leaflet)
library(here)

region_shape <- read_sf("spatial_files/region.shp")

# Check if we are in the correct working directory
i_am("Sample_Location_Map.Rmd")

# import & prep data
# Import station coordinates 
df_coord <- read_excel(path = "./Station_Metadata.xlsx", sheet = "Data")


# Prepare station coordinates for leaflet map
sf_coord <- df_coord %>% 
  select(
    taxon,
    program,
    station,
    latitude = lat_wgs84,
    longitude = long_wgs84
  ) %>% 
  drop_na(latitude, longitude) %>% 
  filter(program %in% c("EMP", "MWTR")) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# Import Region polygon
sf_region <- read_sf(here("spatial_files/region.shp"))

# Convert CRS of Region polygon to WGS84 so that it works with the leaflet map
sf_region_4326 <- st_transform(sf_region, crs = 4326)

# sampling locations map
# This interactive map allows for zooming and panning. Hover over a station marker to get more information about it. The blue polygon represents the region boundary we are considering for the spatial extent of the study.

# Define color palette for Surveys 
color_pal <- colorFactor(palette = "viridis", domain = sf_coord$taxon)

# Create map using leaflet
leaflet() %>% 
  addTiles() %>%
  addCircleMarkers(
    data = sf_coord,
    radius = 5,
    fillColor = ~color_pal(taxon),
    fillOpacity = 0.8,
    weight = 0.5,
    color = "black",
    opacity = 1,
    label = paste0("Station: ", sf_coord$station)
  ) %>% 
  addPolylines(data = sf_region_4326) %>%
  addLegend(
    position = "topright",
    pal = color_pal,
    values = sf_coord$taxon,
    title = "Taxon"
  )
