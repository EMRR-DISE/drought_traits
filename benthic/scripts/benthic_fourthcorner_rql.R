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
  column_to_rownames(var = "year_adjusted") %>% 
  #drop columns for three higher level taxa
  select(-c("turbellarian sp. A","mermithid sp. A","Actinolaiminae sp. A")) %>% 
  glimpse()

#r df
temp <- read_csv("fish traits/fish_data/drought_variables.csv") %>%  #just took from fish for now
  #clean up column names
  clean_names() %>% 
  glimpse()

temp[55,10] <- mean(temp$phos, na.rm = T) # replace NA w mean Phos for 2021

envn <- temp %>%
  filter(year >= "1981" & year != "2022" & year!="2004" & year!="2005")%>% 
  #let's just start with a subset of variables, focusing on continuous ones (ie, no factors)
  #also let's drop flow because highly correlated with salinity
  select(
    year
    #, drought_year
    #, water_year_sac
    #, inflow_annual_cfs
    , nitrate
    , ammonia
    , phos
    , salinity
    , secchi
    , temperature
    ) %>% 
  #mutate(water_year_sac = as_factor(water_year_sac)) %>% 
  #relocate(water_year_sac, .before = drought_year) %>% 
  column_to_rownames(var = "year") %>% 
  glimpse()

#q df
trait <- read_csv("./benthic/data_output/traits/benthic_table_q.csv") %>%
  select(-organism_code) %>% 
  #drop rows for three higher level taxa
  filter(!(target_taxon_name=="Turbellaria" | target_taxon_name=="Actinolaiminae"|target_taxon_name=="Mermithidae")) %>% 
  column_to_rownames(var = "target_taxon_name") %>% 
  glimpse()

#check dimensions
dim(abund) #39 x 61 correspondance
dim(envn) #39 x 6 all quantitative -- PCA
dim(trait) #61 x 2 hillsmith, quantitative and factor

#correspondance for abundance data
afcL.benthic <- 
  dudi.coa(abund, scannf = F)
summary(afcL.benthic) 
#total inertia 1.107

#start with PCA for environmental data
acpR.benthic <- 
  dudi.pca(envn, 
                 row.w = afcL.benthic$lw,
                 scannf = F,
                 nf = 2 #leave this out for now to see all eigenvalues
           )
score(acpR.benthic)

#hill-smith for environmental data
#acpR.benthic <- 
#  dudi.hillsmith(envn, 
#                  row.w = afcL.benthic$lw,
#                  scannf = F,
#                  nf = 2)
# score(acpR.benthic)

#hill-smith again for trait data
acpQ.benthic <- # 'Q' table of traits by species
  dudi.hillsmith(  # trait variables include numeric and categorical data
    trait, 
    row.w = afcL.benthic$cw,
    scannf = F)
score(acpQ.benthic)

#RQL analysis-------

#build model
rlq.benthic <- rlq(acpR.benthic, afcL.benthic, acpQ.benthic,
                 scannf = FALSE)

#plot results
plot(rlq.benthic)

#look at results summary
summary(rlq.benthic)

#plot subset of graphs
par(mfrow = c(1, 3))
s.arrow(rlq.benthic$l1)
s.arrow(rlq.benthic$c1)
s.label(rlq.benthic$lQ, boxes = FALSE)


#Fourth corner analysis--------------

#build model
#this is version without adjustment of pvalues for multiple comparisons
nrepet <- 49999 
four.comb.benthic <- fourthcorner(envn, abund,
                                trait, modeltype = 6, p.adjust.method.G = "none",
                                p.adjust.method.D = "none", nrepet = nrepet)

#plot results
par(mfrow = c(1, 1))
plot(four.comb.benthic, alpha = 0.05, stat = "D2")
#no significant relationships

#rerun model with adjustment of pvalues
#not much point though given that analysis w/o pvalue adjustment had no sign. comparisons
four.comb.benthic_padj <- fourthcorner(envn, abund,
                                  trait, modeltype = 6, p.adjust.method.G = "fdr",
                                  p.adjust.method.D = "fdr", nrepet = nrepet)
plot(four.comb.benthic_padj, alpha = 0.05, stat = "D2")

#combined RQL and Fourth Corner---------------

testrlq.benthic <- randtest(rlq.benthic, modeltype = 6, nrepet = nrepet)
testrlq.benthic
#Model 2 sign. but Model 4 not (p = 0.1)

plot(testrlq.benthic)

#The total inertia of RLQ analysis is equal to the SRLQ multivariate statistic defined in Dray and
#Legendre (2008). This statistic is returned by the fourthcorner2 function
Srlq <- fourthcorner2(envn,abund,trait,
                      modeltype = 6, p.adjust.method.G = "fdr", nrepet = nrepet)
Srlq$trRLQ 

#biplot
#plot should be devoid of relationship lines
plot(four.comb.benthic_padj, x.rlq = rlq.benthic, alpha = 0.05,
     stat = "D2", type = "biplot")
#yep just shows the traits and env predictors

#Another approach is provided by the fourthcorner.rlq function and consists in testing directly the
#links between RLQ axes and traits (typetest="Q.axes") or environmental variables (typetest="R.axes").
testQaxes.comb.benthic <- fourthcorner.rlq(rlq.benthic, modeltype = 6,
                                         typetest = "Q.axes", nrepet = nrepet, p.adjust.method.G = "fdr",
                                         p.adjust.method.D = "fdr")
testRaxes.comb.benthic <- fourthcorner.rlq(rlq.benthic, modeltype = 6,
                                         typetest = "R.axes", nrepet = nrepet, p.adjust.method.G = "fdr",
                                         p.adjust.method.D = "fdr")
print(testQaxes.comb.benthic, stat = "D")
#no significant pvalues
#AxcR1 / body_size is close before adjustment (p = 0.07)

print(testRaxes.comb.benthic, stat = "D")
#none are significant but four are close

#Results can be represented using a table with colors indicating significance :
par(mfrow = c(1, 2))
plot(testQaxes.comb.benthic, alpha = 0.05, type = "table",
     stat = "D2")
plot(testRaxes.comb.benthic, alpha = 0.05, type = "table",
     stat = "D2")
#no significant relationships to show

# #Significance with axes can also be reported on the factorial map of RLQ analysis. Here, significant
# associations with the first axis are represented in blue, with the second axis in orange, with both axes in
# green (variables with no significant association are in black):
  par(mfrow = c(1, 2))
plot(testQaxes.comb.benthic, alpha = 0.05, type = "biplot",
     stat = "D2", col = c("black", "blue", "orange", "green"))
plot(testRaxes.comb.benthic, alpha = 0.05, type = "biplot",
     stat = "D2", col = c("black", "blue", "orange", "green"))

# Format data for analysis ----------
#maybe should look at distribution of abundances
#Bocard et al 2011: CA is a method adapted to the analysis of species abundance data without pre-transformation

#try fourth root transformation of data
#see decostand function in vegan package for common transformations
#aft <- (abund)^(1/4)
#aft2 <- sqrt(abund)

# Run correspondence analysis on abundance data set ---------------

#run in ade4
abund_coa <- dudi.coa(abund, scannf = F, nf=2)
#the first two eigenvalue bars are much higher than others
summary(abund_coa)

#run in vegan
#NOTE: eigenvalues are the same in both scalings (1 vs 2)
#The scaling affects the eigenvectors to be drawn but not the eigenvalues.
abund_cca <- cca(abund)
summary(abund_cca) # default scaling 2 (site scores)
summary(abund_cca, scaling=1) #species scores

#scree plot
#These eigenvalues cannot be interpreted as "variance explained" as cleanly as in the case of PCA, but 
#they can instead be explained as the correlation coefficient between species scores and sample scores
#values over 0.6 indicate a very strong gradient in the data
screeplot(abund_cca, bstick = TRUE, npcs = length(abund_cca$CA$eig))
#broken stick indicates the first three axes are worth interpreting (ie, red dots within gray bars)
#my first axis is only about 0.36, so probably not that strong of a gradient in the data
#probably because asian clams dominate regardless of year 


#draw the CA biplots; compare the two scalings
par(mfrow = c(1, 2))
# Scaling 1: sites are centroids of species
plot(abund_cca,
     scaling = 1,
     main = "CA abundances - biplot scaling 1"
)
# Scaling 2 (default): species are centroids of sites
plot(abund_cca, main = "CA abundances - biplot scaling 2")










