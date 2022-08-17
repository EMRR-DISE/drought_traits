library(ggplot2)
library(sf)
library(ggmap)
library(ggsn)
library(readxl)
library(tidyverse)
library(deltamapr)

ggplot() + 
  geom_sf(data = R_EDSM_Subregions_Mahardja) + 
  geom_sf(data = waterways) +
  geom_sf(data = djfmpsf,
          color = "blue",
          fill = "green",
          shape = 21,
          alpha = 0.5,
          size = 2,
          stroke = 1) +
  labs(title = "DJFMP sites (selected)") +
  theme_bw()

