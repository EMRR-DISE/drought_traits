### Effects of Drought on SF Bay-Delta ###
## quantifying environmental predictabilty ##
## ts of American River water temperatures ##
## 
## Peter Nelson, peter.nelson@water.ca.gov
## Dept Water Resources, and
## Institute of Marine Science, UCSC

library(tidyverse)
library(lubridate)
library(tibbletime)
library(envPred)

afd <- as_tibble(read.csv(file.choose(), header = TRUE))

afd <- afd %>% 
  mutate(date = mdy(date))

n_states <- 11

# based on Colwell 1974 Predictability, constancy and contingency of periodic phenomena
# Ecology 55:1148-1153
fct <- envPred:::colwell_stats 
fct(afd$temp, afd$date, n_states) 
# c is constancy, m is contingency, c+m=p is predictability

dat <- env_stats(afd$temp,
                 afd$date,
                 n_states = 11,
                 delta = 1,
                 is_uneven = FALSE,
                 interpolate = FALSE,
                 show_warns = TRUE,
                 noise_method = 'spectrum')

gg_envpred(dat, type = "detrended")
gg_envpred(dat, type = "spectral")

# calculate monthly means
afd_m <- afd %>% 
  mutate(year = year(date), month = month(date)) %>% 
  group_by(year, month) %>% 
  summarise(mean_temp = mean(temp)) # calculate

plot(afd_m)

library(ggplot2)
library(plotly)