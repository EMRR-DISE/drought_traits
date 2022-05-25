####Zooplankton data manipulation for Special Studies Drought Synthesis
####Author: Laura Twardochleb

#### Startup commands #######################################################################################
setwd("~/IEP_drought_synthesis/Special Studies/drought_traits")

# install Zooper package
options(repos = c(
  sbashevkin = 'https://sbashevkin.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))

install.packages('zooper')
library(zooper)
library(tidyverse)

########## download zoop community data ########################################################################################################

########### mesozoops
#account for changes in taxonomic resolution over time
meso_time_consistent<-Zoopsynther(Data_type="Community", Sources = c("EMP"), Size_class = "Meso", Date_range = c("1975-01-01", "2022-01-01"), Time_consistency = TRUE)
unique(meso_time_consistent$Taxname)

########### all zoops
zoops<-Zoopsynther(Data_type="Community", Sources = c("EMP"), Size_class = c("Micro", "Meso", "Macro"), Date_range = c("1975-01-01", "2022-01-01"), Time_consistency = TRUE)
unique(zoops$Taxname)

#remove undersampled taxa- for analyses utilizing abundance data
zoops2<-filter(zoops, Undersampled==FALSE)
unique(zoops2$Taxname)

######### explore metadata before further data cleaning ######################################################################################################################

#summarize stations by date
zoops_summary<-zoops2%>%group_by(Latitude, Longitude, Year, Station)%>%summarize(N_dates=length(unique(Date)), N_samples=length(unique(SampleID)))

######### cleaning steps ########################################################################################################################





