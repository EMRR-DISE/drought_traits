library(ggplot2)
library(sf)
library(ggmap)
library(ggsn)
library(readxl)
library(tidyverse)
library(deltamapr)

ggplot() + 
  geom_sf(data = R_EDSM_Subregions_Mahardja) + 
  geom_sf(data = waterways) + # can't find 'waterways' any more!
  geom_sf(data = djfmpsf,
          color = "blue",
          fill = "green",
          shape = 21,
          alpha = 0.5,
          size = 2,
          stroke = 1) +
  labs(title = "DJFMP sites (selected)") +
  theme_bw()

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

