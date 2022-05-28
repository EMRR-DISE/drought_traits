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

#change CRS to WGS84 (4326) which is what most (all?) station coordinates will be in
#Note: the difference is so subtle is probably doesn't even matter
regions_4326 <- st_transform(R_EDSM_Subregions_1617P1, crs = 4326)
WW_Delta_4326 <- st_transform(WW_Delta, crs = 4326)

#look at bounding box for delta regions
bdb <- st_bbox(regions_4326)

#make map
(map_region_all<-ggplot()+
    #CDFW Delta waterways
    geom_sf(data= WW_Delta_4326, fill= "skyblue3", color= "black")+
    #EDSM 2017-18 Phase 1 Strata
    geom_sf(data =regions_4326, aes(fill=SubRegion,alpha=0.8))+
    #add title
    ggtitle("R_EDSM_Subregions_1617P1")+
    theme_bw()
)

#filter out some unneeded subregions
region_focal <- regions_4326 %>% 
  filter(!grepl("Napa", SubRegion)
         & SubRegion!="Suisun Marsh" 
         & SubRegion!="East San Pablo Bay"
         )
  
#remake map with reduced subregions  
(map_region_focal<-ggplot()+
    #CDFW Delta waterways
    geom_sf(data= WW_Delta_4326, fill= "skyblue3", color= "black")+
    #reduced region
    geom_sf(data =region_focal, aes(fill=SubRegion,alpha=0.8))+
    #add title
    ggtitle("Focal Region")+
    theme_bw()
)
#ggsave(file = "RegionMap.png",type ="cairo-png",width=20, units="in",dpi=300)


#write the shapefile
#st_write(region_focal, "spatial_files/region.shp")
#got some warning messages

  
  
  
  
  
  
  
