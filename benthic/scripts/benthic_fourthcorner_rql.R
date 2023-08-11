#Special Studies Drought Synthesis
#Environmental Monitoring Program data
#Benthic Invertebrate Survey data

# Load required packages -----------------

library(tidyverse) #suite of data science tools
library(janitor) #used to quickly clean up column names
library(ade4) #running fourth corner analyses
library(vegan) #multivariate analysis

# Load data sets ----------------
#l df
abund <- read_csv("./benthic/data_output/benthic_table_l.csv") %>%
  column_to_rownames(var = "year_adjusted")

#r df
temp <- read_csv("fish traits/fish_data/drought_variables.csv") #just took from fish for now
temp[55,10] <- mean(temp$Phos, na.rm = T) # replace NA w mean Phos for 2021
env <- temp %>%
  filter(year >= "1981" & year != "2022")%>% 
  select(c(year, drought_year, water_year_sac, inflow_annual_cfs:Temperature)) %>% 
  mutate(water_year_sac = as_factor(water_year_sac)) %>% 
  relocate(water_year_sac, .before = drought_year) %>% 
  column_to_rownames(var = "year")

#q df
trait <- read_csv("./benthic/data_output/traits/benthic_table_q.csv") %>%
  select(-organism_code)

#check dimensions
dim(abund) #39 x 64 correspondance
dim(env) #41 x 9 all quantitative -- PCA
dim(trait) #64 x 3 hillsmith, quantitative and factor

#correspondance for abundance data
afcL.benthic <- 
  dudi.coa(abund, scannf = F)
summary(afcL.benthic) 
#total inertia 1.107

#hill-smith for environmental data
acpR.benthic <- 
  dudi.hillsmith(env, 
                 row.w = afcL.benthic$lw,
                 scannf = F,
                 nf = 2)
score(acpR.benthic)

#hill-smith again for trait data
acpQ.benthic <- # 'Q' table of traits by species
  dudi.hillsmith(  # trait variables include numeric and categorical data
    trait, 
    row.w = afcL.benthic$cw,
    scannf = F)
score(acpQ.benthic)

# Format data for analysis ----------
#maybe should look at distribution of abundances
#Bocard et al 2011: CA is a method adapted to the analysis of species abundance data without pre-transformation

#turn first column contain the years into row names
abundf <- abund %>% 
  remove_rownames %>% 
  column_to_rownames(var="year_adjusted")

#try fourth root transformation of data
#see decostand function in vegan package for common transformations
#aft <- (abundf)^(1/4)
#aft2 <- sqrt(abundf)

# Run correspondence analysis on abundance data set ---------------

#run in ade4
abundf_coa <- dudi.coa(abundf, scannf = F, nf=2)
#the first two eigenvalue bars are much higher than others
summary(abundf_coa)

#run in vegan
#NOTE: eigenvalues are the same in both scalings (1 vs 2)
#The scaling affects the eigenvectors to be drawn but not the eigenvalues.
abundf_cca <- cca(abundf)
summary(abundf_cca) # default scaling 2 (site scores)
summary(abundf_cca, scaling=1) #species scores

#scree plot
#These eigenvalues cannot be interpreted as "variance explained" as cleanly as in the case of PCA, but 
#they can instead be explained as the correlation coefficient between species scores and sample scores
#values over 0.6 indicate a very strong gradient in the data
screeplot(abundf_cca, bstick = TRUE, npcs = length(abundf_cca$CA$eig))
#broken stick indicates the first three axes are worth interpreting (ie, red dots within gray bars)
#my first axis is only about 0.36, so probably not that strong of a gradient in the data
#probably because asian clams dominate regardless of year 


#draw the CA biplots; compare the two scalings
par(mfrow = c(1, 2))
# Scaling 1: sites are centroids of species
plot(abundf_cca,
     scaling = 1,
     main = "CA abundances - biplot scaling 1"
)
# Scaling 2 (default): species are centroids of sites
plot(abundf_cca, main = "CA abundances - biplot scaling 2")










