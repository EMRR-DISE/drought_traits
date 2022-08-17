# Pete Nelson, PhD
# Department of Water Resources
# purpose: acquire white sturgeon data from the SF Bay Study data set

# drought/trait study ----
# load white sturgeon data from the Bay Study

library(readxl)
library(tidyverse)
library(janitor)
sturgeon <- tibble(read_excel("fish_data/Bay Study sturgeon/Bay Study sturgeon data.xlsx", 
                    sheet = "WHISTU"))

# ReadMe file suggests that "...for comparing years only the series 1 stations 
# (1980+) and the months most sampled should be used. These are Feb-Oct for 
# otter trawl and Apr-Oct for midwater trawl." Easy to select series 1, but no
# (?) indication re months. F-ing Access!

# query used: TotalCatch by Species

sturg <- sturgeon %>% 
  clean_names() %>% 
  filter(series == "1") %>% 
  select(year, survey, station, net, tow, total_catch, bay, chan_shoal)

write_csv(sturg, "fish_data/sturg.csv")

sturg <- read_csv("fish_data/sturg.csv")
sturg %>% group_by(year = year) %>% summarise(WHISTU = sum(total_catch))
