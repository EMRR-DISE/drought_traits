#Special Studies Drought Synthesis
#Environmental Monitoring Program data
#Benthic Invertebrate Survey data

# Load required packages -----------------

library(tidyverse) #suite of data science tools
library(janitor) #used to quickly clean up column names
library(ade4) #running fourth corner analyses
library(vegan) #multivariate analysis

# Load data sets ----------------

abund <- read_csv("./BenthicInverts/Benthic_TableL.csv")


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










