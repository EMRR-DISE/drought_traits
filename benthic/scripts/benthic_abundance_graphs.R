#Benthic Inverts
#look at changes in abundances through time 
#for taxa present in at least 10% of each of the three long term stations

#packages
library(tidyverse)

#read in data
benthic <- read_csv("./benthic/data_output/benthic_common10_abundances_by_station.csv") %>% 
  glimpse()

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
   geom_bar(stat="identity")+
   facet_wrap(vars(organism_code)
              ,scales="free_y"
              )+
   ggtitle("D28")
   
)

#D7
(plot_d7 <- ggplot(d7,aes(sample_date,mean_cpue))+
    geom_bar(stat="identity")+
    facet_wrap(vars(organism_code)
               ,scales="free"
    )+
    ggtitle("D7") 
    
)

#D4
(plot_d4 <- ggplot(d4,aes(sample_date,mean_cpue))+
    geom_bar(stat="identity")+
    facet_wrap(vars(organism_code)
               ,scales="free_y"
    )+
    ggtitle("D4") 
)
  
#once we get the plots working properly, maybe we do anomaly plots to better show who increased and decreased




