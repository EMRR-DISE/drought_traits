# Pete Nelson, PhD
# Department of Water Resources
# project: drought traits (special studies)
# purpose: complete fourth-corner & RLQ tutorial
# from: Dray et al. 2014
# created: 2023-05-23
# last modified: 2023-06-05

# getting started -----
library(tidyverse)
library(ade4)

# load data ------
## L df ----
# fish occurrence data with years prior to 1975 + 1979 removed bc environmental data incomplete for these
fish <- read_csv("fish traits/fish_data/fmwt1.csv") %>% # derived from fmwt.R, collapsed striper data
  filter(year >= "1975" & year != "1979") %>% # no (env) data these years
  column_to_rownames(var = "year")

## R df ----
# environmental data for each year included in fish data
# replace Phos 2021 datum (NA) with mean of all previous years
temp <- read_csv("fish traits/fish_data/drought_variables.csv")
temp[55,10] <- mean(temp$Phos, na.rm = T) # replace NA w mean Phos for 2021
fenv <- temp %>% 
  filter(year >= "1975" & year != "1979" & year != "2022") %>% 
  column_to_rownames(var = "year")

## Q df ----
# trait data for each fish sp
names <- c("am.shad", "p.herring", "threadfin", "p.anchovy", "d.smelt", "striped.b", "chinook", "splittail", "longfin")
ftrait <- as.data.frame(read_csv("fish traits/fish_data/ftrait.csv")) %>% 
  select(-"species")
rownames(ftrait) <- names
ftrait <- ftrait %>% 
  select("origin", "life_hist", "habitat",
         "lmax", "fecundity", "life_span", "therm_tol") # order the variables w categorical first

# check that the dimensions of the LQR tables match
dim(fish) # L df, 46 years and 9 spp
dim(fenv) # R df, each year per each of the environmental variables
dim(ftrait) # Q df, each spp described by 7 traits (fecundity, habitat, lmax, etc)

# RLQ -----

## separate analysis for each table ----
afcL.fish <- # 'L' table of species abundance by year
  dudi.coa(fish,   # correspondence analysis appl to spp table
           scannf = F)
score(afcL.fish)

# acpR.fish <- # 'R' table of environmental conditions by year
#  dudi.hillsmith(fenv, # environ data include both quantitative & categorical variables, thus dudi.hillsmith
#    row.w = afcL.fish$lw,
#    scannf = F,
#    nf = 2)

acpR.fish <-
  dudi.pca(fenv[,6:12], # Hill & Smith code isn't working so try pca on numeric environmental data
           row.w = afcL.fish$lw,
           scannf = F,
           nf = 2)
score(acpR.fish)

# acpQ.fish <- # 'Q' table of traits by species
#  dudi.hillsmith(  # trait variables include numeric and categorical data
#    ftrait, 
#    row.w = afcL.fish$cw,
#    scannf = F)

acpQ.fish <- # 'Q' table of traits by species
  dudi.pca(  # limit trait variables to numeric data
  ftrait[,4:7], 
  row.w = afcL.fish$cw,
  scannf = F)
score(acpQ.fish)

## combine spp-env-traits -----
rlq.fish <-
  rlq(acpR.fish, afcL.fish, acpQ.fish,
      scannf = F)

# RLQ analysis finds coefficients in $c1 to obtain a linear combo of traits (spp scores in $1Q) and coefficients (in $l1) to obtain a linear combo of env variables (site scores in $1R). The covariance btwn these 2 set scores is maximized and equal to the square root of the corresponding eigenvalue.

## plot results ----
plot(rlq.fish)

# Plot figures separately as follows:
par(mfrow = c(1, 3)) 
s.arrow(rlq.fish$l1) 
s.arrow(rlq.fish$c1) 
s.label(rlq.fish$lQ, boxes = FALSE)
