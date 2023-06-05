# DJFMP sites and site histories
library(tidyverse)


# Save time by loading raw data files, as needed ("dt3", "dt4", "dt5"):
dt3 <- read.csv("fish_data/dt3.csv") # beach seine data
dt4 <- read.csv("fish_data/dt4.csv") # fish names
dt5 <- read.csv("fish_data/dt5.csv") # locations (beach seine + other methods)

f_dat <- read_csv("fish_data/f_dat.csv")
f_stn <- read_csv("fish_data/f_stn.csv")
dt3_test <- read_csv("fish_data/dt3_test.csv")