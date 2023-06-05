#Drought Synthesis
#Special Studies Project
#Create customized shapefile of Bay-Delta from deltamapr shapefile
#Use the shapefile to filter stations spatially

#required packages
library(tidyverse) #suite of data science tools
library(sf) #working with spatial data
library(deltamapr) #Bay-Delta shape files

#chose this version of the shapefile because it included all the areas we need
#and splits out Grizzly Bay from Suisun Marsh
R_EDSM_Subregions_1617P1 

#look at coordinate reference system (CRS) of regions and basemap
st_crs(R_EDSM_Subregions_1617P1) #NAD83 / UTM zone 10N which is EPSG = 26910
st_crs(WW_Delta) #NAD83 which is EPSG = 4269

#change CRS of WW_Delta to EPSG = 26910
#Note: the difference is so subtle is probably doesn't even matter
WW_Delta_26910 <- st_transform(WW_Delta, crs = 26910)

#look at bounding box for delta regions
bdb <- st_bbox(R_EDSM_Subregions_1617P1)

#make map
(map_region_all<-ggplot()+
    #CDFW Delta waterways
    geom_sf(data = WW_Delta_26910, fill= "skyblue3", color= "black")+
    #EDSM 2017-18 Phase 1 Strata
    geom_sf(data = R_EDSM_Subregions_1617P1, aes(fill=SubRegion), alpha=0.8)+
    #add title
    ggtitle("R_EDSM_Subregions_1617P1")+
    theme_bw()
)

#filter out some unneeded subregions
region_focal <- R_EDSM_Subregions_1617P1 %>% 
  filter(!grepl("Napa", SubRegion)
         & SubRegion!="Suisun Marsh" 
         & SubRegion!="East San Pablo Bay"
         )
  
#remake map with reduced subregions  
(map_region_focal<-ggplot()+
    #CDFW Delta waterways
    geom_sf(data= WW_Delta_26910, fill= "skyblue3", color= "black")+
    #reduced region
    geom_sf(data =region_focal, aes(fill=SubRegion), alpha=0.8)+
    #add title
    ggtitle("Focal Region")+
    theme_bw()
)
#ggsave(file = "RegionMap.png",type ="cairo-png",width=20, units="in",dpi=300)

# Dissolve region shapefile to just the outside perimeter, removing subregions
region_focal_diss <- region_focal %>% 
  # Add a 0.5 meter buffer to remove slivers within perimeter
  st_buffer(0.5) %>%
  # Dissolve subregions
  st_union()

#write the shapefile of just the outside perimeter
write_sf(region_focal_diss, "spatial_files/region.shp")

