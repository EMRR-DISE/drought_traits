#Benthic Inverts
#look at changes in abundances through time 
#for taxa present in at least 10% of each of the three long term stations
#start with total abundances
#then look at abundances by trait (eg, native vs nonnative, size)

#packages
library(tidyverse)

#read in data
benthic <- read_csv("./benthic/data_output/benthic_common10_abundances_by_station.csv") %>% 
  glimpse()

#taxa <- unique(benthic$organism_code) #n = 66 as expected

#create subsets for each station----------

d28 <- benthic %>% 
  filter(station_code == "D28A-L" & mean_cpue>0) %>% 
  arrange(organism_code)

d4 <- benthic %>% 
  filter(station_code == "D4-L"& mean_cpue>0)

d7 <- benthic %>% 
  filter(station_code == "D7-C"& mean_cpue>0) %>% 
  glimpse()

#plot panel of plots showing changes in abundance through time of organisms at each station---------

#D28
(plot_d28 <- ggplot(d28,aes(sample_date,mean_cpue))+
   geom_line()+
   facet_wrap(vars(organism_code)
              ,scales="free_y"
              )+
   ggtitle("D28")
)
#ggsave(plot_d28,filename="./benthic/figures/benthic_10_plus_abundances_d28.png",dpi=300, width = 12, height = 8, units = "in")


#D7
(plot_d7 <- ggplot(d7,aes(sample_date,mean_cpue))+
    geom_line()+
    facet_wrap(vars(organism_code)
               ,scales="free_y"
    )+
    ggtitle("D7") 
)
#ggsave(plot_d7,filename="./benthic/figures/benthic_10_plus_abundances_d7.png",dpi=300, width = 12, height = 8, units = "in")


#D4
(plot_d4 <- ggplot(d4,aes(sample_date,mean_cpue))+
    geom_line()+
    facet_wrap(vars(organism_code)
               ,scales="free_y"
    )+
    ggtitle("D4") 
)
#ggsave(plot_d4,filename="./benthic/figures/benthic_10_plus_abundances_d4.png",dpi=300, width = 12, height = 8, units = "in")


#plot abundances for each taxon through time combining abundances across stations--------------
#there is a nice mix of taxa that increase through time, decrease through time, and fluctuate through time
#NOTE: when interpreting this graph, keep in mind that if a given taxon was not present in at least 10% of samples
#at a given station, it was dropped from that station. In other words, a value for a taxon might appear to be zero for a station but may be  
#greater than that in reality

benthic_comb <- benthic %>% 
  group_by(organism_code,sample_date) %>% 
  summarize(mean_cpue_tot=sum(mean_cpue), .groups = 'drop') %>% 
  glimpse()

(plot_stn_comb <- ggplot(benthic_comb,aes(sample_date,mean_cpue_tot))+
    #geom_point()+
    geom_line()+
    facet_wrap(vars(organism_code)
               ,scales="free_y"
    ) 
)


