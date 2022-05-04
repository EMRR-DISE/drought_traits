

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

