#Special Studies Drought Synthesis
#Environmental Monitoring Program data
#Benthic Invertebrate Survey data
#Pull trait data from WoRMS online database

#steps
#create named vector of all AphiaIDs
#feed this into the wm_attr_data() function
#

#filter to just size "measurementType"
#then unnest dataframe (does this need to be done multiple times?)
#for size could probably just go with largest value for each taxon instead of dealing with nested data

# Load required packages -----------------
library(worrms)

# Examples from package documentation ---------

ex1 <- wm_attr_data(id = 1040874)
ex2 <- wm_attr_data_(id = c(127160, 126436))


  
  
  
  
