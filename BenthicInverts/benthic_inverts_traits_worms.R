#Special Studies Drought Synthesis
#Environmental Monitoring Program data
#Benthic Invertebrate Survey data
#Pull trait data from WoRMS online database

#steps
#create named vector of all AphiaIDs
#feed this into the wm_attr_data() function
#filter dataset to just desired trait
#do a bunch of unnesting of this poorly structured dataset

#filter to just size "measurementType"

# Load required packages -----------------
library(worrms)

# Examples from package documentation ---------

ex1 <- wm_attr_data(id = 1040874)
ex2 <- wm_attr_data_(id = c(127160, 126436))

#unnesting
test <- ex1 %>% 
  filter(measurementTypeID==15) %>% 
  unnest(cols = children, names_repair = "universal") %>% 
  unnest(cols = children) %>% 
  unnest(cols = children, names_repair = "universal") %>% 
  unnest(children)

  
  
  
  
