# Pete Nelson, PhD
# Department of Water Resources
# project: drought traits (special studies)
# purpose: complete fourth-corner & RLQ analyses of fish-trait data, this time with a more restricted set of environmental variables and a larger number of fish species
# from: Dray et al. 2014
# created: 2023-06-30
# last modified: 2023-10-12

# getting started -----
library(tidyverse)
library(ade4)

# load data ------
## L df ----
# fish occurrence data with years prior to 1975 + 1979 removed bc environmental data incomplete for these
fish <- read_csv("fish traits/fish_data/fmwt.csv") %>% # derived from fmwt.R, collapsed striper data
  filter(year >= "1975" & year != "1979" & year != "2022") %>% # no (env) data these years
  column_to_rownames(var = "year") %>% 
  select(american_shad, pacific_herring, threadfin_shad, northern_anchovy, delta_smelt,
         striped_bass, chinook_salmon, splittail, longfin_smelt, yellowfin_goby,
         white_sturgeon, white_catfish, topsmelt, jacksmelt, shiner_perch, white_croaker,
         channel_catfish, starry_flounder, plainfin_midshipman)

## R df ----
# environmental data for each year included in fish data
# replace Phos 2021 datum (NA) with mean of all previous years
temp <- read_csv("fish traits/fish_data/drought_variables.csv")
temp[55,10] <- mean(temp$Phos, na.rm = T) # replace NA w mean Phos for 2021

### revisions ----
# Modified original environmental data set to limit some of the redundant or less-than-useful variables. 
fenv <- temp %>% 
  filter(year >= "1975" & year != "1979" & year != "2022") %>% 
  select(c(year, drought_year, water_year_sac, inflow_annual_cfs:Temperature)) %>% 
  mutate(water_year_sac = as_factor(water_year_sac)) %>% 
  relocate(water_year_sac, .before = drought_year) %>% 
  column_to_rownames(var = "year")
rm(temp)

## Q df ----
# trait data for each fish sp

ftrait <- readRDS("fish traits/fish_data/ftrait.rds")

ftrait <- ftrait %>% 
  mutate_at(c("origin", "life_hist", "residency", "habitat"), as.factor) %>% 
  mutate_at(c("fecundity", "life_span", "l_mat", "l_max", "therm_tol"), as.numeric)
glimpse(ftrait)

# check that the dimensions of the LQR tables match
dim(fish) # L df, 46 years and 9 spp; quantitative--PCA is fine
dim(fenv) # R df, each year per each of the environmental variables; multiple factorial variables + quantitative
dim(ftrait) # Q df, each spp described by 7 traits (fecundity, habitat, l_max, etc); like fenv, mixed

# RLQ -----

## fish ----
# Works fine because entirely quantitative, so used correspondence analysis.
afcL.fish <- # 'L' table of species abundance by year
  dudi.coa(fish,   # correspondence analysis appl to spp table
           scannf = F)
score(afcL.fish)

## environment ----
# Because the environmental data includes both quantitative and categorical variables, should use dudi.hillsmith() (dudi.mixed() if ordered variables). 
acpR.fish <- # 'R' table of environmental conditions by year
  dudi.hillsmith(fenv, # environ data include both quantitative & categorical variables, thus dudi.hillsmith
                 row.w = afcL.fish$lw,
                 scannf = F,
                 nf = 2)
score(acpR.fish)

## traits -----
# Again, should be able to use dudi.hillsmith() because trait data include both quant and categorical data, but doesn't work.

acpQ.fish <- # 'Q' table of traits by species
  dudi.hillsmith(  # trait variables include numeric and categorical data
    ftrait[,2:10], # left the spp names out
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
par(mfrow = c(1, 1))

# It's important to see how the individual parts (ie cov(traits, env)^2 = var(traits) x var(env) x cor(traits, env)^2) of the compromise are considered. Hence, compare the RLQ analysis to the separate analyses which maximize independently the structure of the trait (pca traits), structure of the environment (Hill-Smith analysis environmental variables) and correlation (correspondence analysis of the sites-spp table). Comparisons provided summary function:
summary(rlq.fish)

# Fourth-corner ----
# Analysis tests associations btwn individual traits and environmental variables. To obtain test w correct type I error, results of model 2 (permutation of sites, ie rows) and 4 (permutation of spp, ie columns) should be combined. While the former version of ade4 required three steps (tests w the two models and combination of the results), the combined approach can now be generated by setting the modeltype argument to 6. Note that we used a very high number of repetitions (nrepet <- 49999) to have enough power in corrected tests. This time-consuming and could be modified to speed up the different analyses (eg nrepet <- 999, the default setting).

nrepet <- 49999 # takes a couple minutes
four.comb.fish <- fourthcorner(
  fenv, fish, ftrait[,2:10],
  modeltype = 6,
  p.adjust.method.G = "none",
  p.adjust.method.D = "none",
  nrepet = nrepet
) # gets error msg: "'tabQ' must contain only numeric values or factors" but ftrait[,2:10] does!

# SCRATCH ----
nrepet <- 999 # few reps for working things out
temp <- ftrait %>% 
  mutate(name_abr = as_factor(name_abr))
four.comb.fish <- fourthcorner(
  fenv, fish, temp,
  modeltype = 2,
  p.adjust.method.G = "none",
  p.adjust.method.D = "none",
  nrepet = nrepet
) # gets error msg: "'tabQ' must contain only numeric values or factors" but ftrait[,2:10] does!

# When you plot the results, blue cells correspond to negative significant relationships while red cells correspond to positive ones (modify using argument col). In this example, there are some associations btwn categorical traits and quantitative environmental variables which can be measured in three different ways (Legendre et al 1997). These three methods correspond to three possible values of the stat argument in the plot and print functions:
# stat="D2": association is measured btwn quantitative variable and each category separately. A correlation coefficient is used to indicate the strength of the association between the given category and the small or large values of the quantitative variable.
# stat="G": the association btwn the quantitative variable and the whole categorical variable is measured by a global statsistic (F)
# stat="D": association is estimated btwn the quantitative variable and each category separately by a measure of the within-group homogeneity. The strength of the association is indicated by the dispersion of the values of the quantitative variable for a given category.
# In the rest of the tutorial, the focus is on the D2 statistic. The correction of p-values by a sequential procedure leads to significant associations if the maximal p-value is lower than alpha=0.05. 

# There seem to be no significant relationships in the fish/env/trait data sets!

plot(four.comb.fish, alpha = 0.05, stat = "D2")

# Now, adjust p-values for multiple comparisons using the fdr method using the p.adjust.4thcorner function (assuming nrepet set to 49999, though I got the same results with nrepet=9999).

four.comb.fish.adj <-
  p.adjust.4thcorner(
    four.comb.fish,
    p.adjust.method.G = "fdr",
    p.adjust.method.D = "fdr"
  )

# Adjusted p-values can be obtained directly using the fourthcorner function:
fourthcorner(fenv, fish, ftrait,
             modeltype = 6,
             p.adjust.method.G = "fdr",
             p.adjust.method.D = "fdr",
             nrepet = nrepet)

plot(four.comb.fish.adj,
     alpha = 0.05,
     stat = "D2")

# 4th+RLQ ----

## trait X env -----

# First evaluate the global significance of the traits-environment relationships using test based on total inertia of the RLQ analysis:
testrlq.fish <- 
  randtest(rlq.fish,
           modeltype = 6,
           nrepet = nrepet)
testrlq.fish

plot(testrlq.fish)

# Total inertia of RLQ analysis is equal to the S(RLQ) multivariate statistic defined in Dray & Legrende (2008). this stat is returned by fourthcorner2 function:
Srlq <- fourthcorner2(
  fenv, fish, ftrait,
  modeltype = 6,
  p.adjust.method.G = "fdr",
  nrepet = nrepet
)
Srlq$trRLQ

# Both approaches can be combined if RLQ scores are used to represent traits and environmental variables on a biplot. Then, significant associations revealed by the 4thcorner approach can be represented using segments (blue for neg, red for pos, see argument col). Only traits and environmental variables that have at least one significant association are reprresented. Here, we apply this method using adjusted p-values for multiple comparisons and alpha=0.05.

plot(four.comb.fish.adj, 
     x.rlq = rlq.fish, 
     alpha = 0.05,
     stat = "D2",
     type = "biplot")

# Another approach is provided by the fourthcorner.rlq function and consists of testing directly the links btwn RLQ axes and traits (typetest = "Q.axes") or environmental variables (typetest = "R.axes").

testQaxes.comb.fish <-
  fourthcorner.rlq(rlq.fish,
                   modeltype = 6,
                   typetest = "Q.axes",
                   nrepet = nrepet,
                   p.adjust.method.G = "fdr",
                   p.adjust.method.D = "fdr")

testRaxes.comb.fish <-
  fourthcorner.rlq(rlq.fish,
                   modeltype = 6,
                   typetest = "R.axes",
                   nrepet = nrepet,
                   p.adjust.method.G = "fdr",
                   p.adjust.method.D = "fdr")

print(testQaxes.comb.fish, stat = "D")
print(testRaxes.comb.fish, stat = "D")

# Present result in table format with colors to indicate significance:
par(mfrow = c(1, 2))
plot(testQaxes.comb.fish,
     alpha = 0.05,
     type = "table",
     stat = "D2")
plot(testRaxes.comb.fish,
     alpha = 0.05,
     type = "table",
     stat = "D2")

# Significance with axes can also be reported on the factorial map of RLQ analysis. Here, significant associations with the first axis are in blue, with the 2nd axis in orange, and both axes in green (variables w no significant association in black):

par(mfrow = c(1, 2))
plot(testQaxes.comb.fish,
     alpha = 0.05,
     type = "biplot",
     stat = "D2",
     col = c("black", "blue", "orange", "green"))
plot(testRaxes.comb.fish,
     alpha = 0.05,
     type = "biplot",
     stat = "D2",
     col = c("black", "blue", "orange", "green"))
par(mfrow = c(1, 1))

