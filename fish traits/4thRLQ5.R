# Pete Nelson, PhD
# Department of Water Resources
# project: drought traits (special studies)
# purpose: complete fourth-corner & RLQ analyses of fish-trait data, this time with a FURTHER restricted set of environmental variables and a larger number of fish species
# from: Dray et al. 2014
# created: 2023-06-30
# last modified: 2024-02-20

# getting started -----
library(tidyverse)
library(ade4)
library(magrittr)

# load data ------
## L df ----
# fish occurrence data with years prior to 1975 + 1979 removed bc environmental data incomplete for these
fish <- read_rds("fish traits/fish_data/fish.rds") %>% print()
glimpse(fish)

## R df ----
# environmental data for each year included in fish data
# reverse environmental data to merely quant + factorial (no longer ordered)
fenv <- read_rds("fish traits/fish_data/fenv.rds") %>% 
  mutate(drought_year_trunc = factor(drought_year_trunc, ordered = F),
         water_year_sac = factor(water_year_sac, ordered = F)) %>% 
  select(c(drought_year_trunc, inflow_annual_cfs, Temperature, Salinity)) 
# trying to FURTHER reduce number of variables
glimpse(fenv)

## Q df ----
# trait data for each fish sp
# combo of quantitative and factorial variables
ftrait <- read_rds("fish traits/fish_data/ftrait.rds") %>% 
  select(c(origin, residency, diet, reproduction, fecundity, l_max))
# trying to FURTHER reduce number of variables
glimpse(ftrait)

# check that the dimensions of the LQR tables match
dim(fish) # L df, 46 years and 19 spp; quantitative--PCA is fine
dim(fenv) # R df, each year per each of the 10 environmental variables; multiple factorial variables + quantitative
dim(ftrait) # Q df, each spp described by 14 traits (fecundity, habitat, l_max, etc); like fenv, mixed

# RLQ -----

## L: fish ----
# Works fine because entirely quantitative, so used correspondence analysis.
(dudi.L <- # 'L' table of species abundance by year
   dudi.coa(fish,   # correspondence analysis appl to spp table
            scannf = F,
            nf = 2))

score(dudi.L) 

## R: environment ----
# Because the environmental data includes ordered categorical variables, should use dudi.mix(), but this doesn't allow for weighting rows
(dudi.R <- # 'R' table of environmental conditions by year
   dudi.hillsmith(fenv,
                  scannf = F, # display eigenvalue barplot?; 12 column weights ($cw)
                  nf = 2,
                  row.w = dudi.L$lw))

score(dudi.R)

## Q: traits -----
# Use dudi.hillsmith() because trait data include both quant and categorical data

(dudi.Q <- # 'Q' table of traits by species
   dudi.hillsmith(  # trait variables include numeric and categorical data
     ftrait, 
     row.w = dudi.L$cw, # weight based on previous corr analysis (species)
     scannf = F))
score(dudi.Q)

## combine spp-env-traits -----
# "RLQ analysis performs a double inertia analysis of two arrays (R and Q) with a link expressed by a contingency table (L). The rows of L correspond to the rows of R and the columns of L correspond to the rows of Q. [checks out]"

rlq.fish <-
  rlq(dudi.R, dudi.L, dudi.Q,
      scannf = F,
      nf = 2)
# trouble here if using dudi.mix() for R: "Non equal row weights"

dudi.R # 46 numeric row weights
dudi.L # 46 numeric row weights
dudi.Q # 19 numeric row weights!

dim(fish) # L 46 rows (years), 19 columns (spp)
dim(fenv) # R 46 rows (years), 6 columns (env variables)
dim(ftrait) # Q 19 rows (spp), 11 columns (traits)

# RLQ analysis finds coefficients in $c1 to obtain a linear combo of traits (spp scores in $1Q) and coefficients (in $l1) to obtain a linear combo of env variables (site scores in $1R). The covariance btwn these 2 set scores is maximized and equal to the square root of the corresponding eigenvalue.

## plot results ----
plot(rlq.fish)
summary(rlq.fish)
randtest(rlq.fish)
fourthcorner.rlq(rlq.fish, type = "Q.axes")
fourthcorner.rlq(rlq.fish, type = "R.axes")

### env v year ----
# mod fenv for plotting
fenv.p <- rownames_to_column(fenv, "year")

# create df environmental ordination for plotting
env <- rownames_to_column(rlq.fish$mR, "year") %>% 
  left_join(., fenv.p,
            by = "year")

ggplot(env,
       aes(x=NorS1, y=NorS2)) +
  geom_text(aes(color = drought_year_trunc,
                label = year)) +
  ggtitle("PCA: Environmental Conditions") +
  theme_bw()

# basis...
ggplot(env,
       aes(x=NorS1, y=NorS2)) +
  geom_point() +
  ggtitle("PCA: Environmental Conditions") +
  theme_bw()

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
# nrepet <- 10000 # faster

four.comb.fish_mod6 <- fourthcorner(
  fenv, fish, ftrait,
  modeltype = 6, # Dray and Legendre (2008) and ter Braak et al. (20012) showed that all models 
  # except model 6 have inflated type I error
  p.adjust.method.G = "none",
  p.adjust.method.D = "none",
  nrepet = nrepet
)

# When you plot the results, blue cells correspond to negative significant relationships while red cells 
# correspond to positive ones (modify using argument col). In this example, there are some associations 
# btwn categorical traits and quantitative environmental variables which can be measured in three different 
# ways (Legendre et al 1997). These three methods correspond to three possible values of the stat argument 
# in the plot and print functions:
# stat="D2": association is measured btwn quantitative variable and each category separately. A correlation 
# coefficient is used to indicate the strength of the association between the given category and the small 
# or large values of the quantitative variable.
# stat="G": the association btwn the quantitative variable and the whole categorical variable is measured by 
# a global statistic (F)
# stat="D": association is estimated btwn the quantitative variable and each category separately by a measure 
# of the within-group homogeneity. The strength of the association is indicated by the dispersion of the 
# values of the quantitative variable for a given category.
# In the rest of the tutorial, the focus is on the D2 statistic. The correction of p-values by a sequential 
# procedure leads to significant associations if the maximal p-value is lower than alpha=0.05. 

plot(four.comb.fish_mod6, alpha = 0.05, stat = "D2")

# Now, adjust p-values for multiple comparisons using the fdr method using the p.adjust.4thcorner function 
# (assuming nrepet set to 49999, though I got the same results with nrepet=9999).

four.comb.fish.6.adj <-
  p.adjust.4thcorner(
    four.comb.fish_mod6,
    p.adjust.method.G = "fdr",
    p.adjust.method.D = "fdr"
  )

# When you plot the results, blue cells correspond to negative significant relationships while red cells 
# correspond to positive ones (modify using argument col). In this example, there are some associations 
# btwn categorical traits and quantitative environmental variables which can be measured in three different 
# ways (Legendre et al 1997). These three methods correspond to three possible values of the stat argument 
# in the plot and print functions:
# stat="D2": association is measured btwn quantitative variable and each category separately. A correlation 
# coefficient is used to indicate the strength of the association between the given category and the small 
# or large values of the quantitative variable.
# stat="G": the association btwn the quantitative variable and the whole categorical variable is measured by 
# a global statistic (F)
# stat="D": association is estimated btwn the quantitative variable and each category separately by a measure 
# of the within-group homogeneity. The strength of the association is indicated by the dispersion of the 
# values of the quantitative variable for a given category.
# In the rest of the tutorial, the focus is on the D2 statistic. The correction of p-values by a sequential 
# procedure leads to significant associations if the maximal p-value is lower than alpha=0.05. 

plot(four.comb.fish_mod6, alpha = 0.05, stat = "D2")

# Now, adjust p-values for multiple comparisons using the fdr method using the p.adjust.4thcorner function 
# (assuming nrepet set to 49999, though I got the same results with nrepet=9999).

four.comb.fish.6.adj <-
  p.adjust.4thcorner(
    four.comb.fish_mod6,
    p.adjust.method.G = "fdr",
    p.adjust.method.D = "fdr"
  )

plot(four.comb.fish.6.adj,
     alpha = 0.05,
     stat = "D2")

# Adjusted p-values can be obtained directly using the fourthcorner function:
(fourthcorner.mod6 <- 
    fourthcorner(fenv, fish, ftrait,
                 modeltype = 6,
                 p.adjust.method.G = "fdr",
                 p.adjust.method.D = "fdr",
                 nrepet = nrepet))

# 4th+RLQ ----

## trait X env -----

# First evaluate the global significance of the traits-environment relationships using test based on total inertia of the RLQ analysis:
(testrlq.fish <- 
   randtest(rlq.fish,
            modeltype = 6,
            nrepet = nrepet))

plot(testrlq.fish)

# Total inertia of RLQ analysis is equal to the S(RLQ) multivariate statistic defined in Dray & Legrende (2008). this stat is returned by fourthcorner2 function:
Srlq <- fourthcorner2(
  fenv, fish, ftrait,
  modeltype = 6,
  p.adjust.method.G = "fdr",
  nrepet = nrepet
)
Srlq$trRLQ

# Both approaches can be combined if RLQ scores are used to represent traits and environmental variables on a biplot. Then, significant associations revealed by the 4thcorner approach can be represented using segments (blue for neg, red for pos, see argument col). Only traits and environmental variables that have at least one significant association are represented. Here, we apply this method using adjusted p-values for multiple comparisons and alpha=0.05.

plot(four.comb.fish.6.adj,
     x.rlq = rlq.fish,
     alpha = 1,
     stat = "D2",
     type = "biplot",
     main = "test")

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

# SCRATCH #######