#Special Studies Drought Synthesis
#Environmental Monitoring Program data
#Benthic Invertebrate Survey data

# To do list--------------
#make sure all categorical traits and env. variables are coded correctly as factors
#run versions of analysis with other metrics of drought in place of salinity (eg, inflow)
#make sure envn_winter and envn_lag1_winter have correct dimensions
#try different types of lags
  #For analyses by year, try 2 year lag instead of one year lag
  #for analyses by season, could try matching a season's abundance with envn from same season, one earlier, two earlier, etc
#consider look at dominant inverts by season

# Load required packages -----------------

library(tidyverse) #suite of data science tools
library(janitor) #used to quickly clean up column names
library(ade4) #running fourth corner analyses
library(vegan) #multivariate analysis

# Load and format trait data -----------------

#non-rare benthic invert traits
trait <- read_csv("./benthic/data_output/traits/benthic_table_q.csv") %>%
  select(-organism_code) %>% 
  mutate(native = as.factor(native)) %>% 
  #drop rows for three higher level taxa
  filter(!(target_taxon_name=="Turbellaria" | target_taxon_name=="Actinolaiminae"|target_taxon_name=="Mermithidae")) %>% 
  column_to_rownames(var = "target_taxon_name") %>% 
  glimpse()

#dominant benthic invert traits
trait_dominant <- read_csv("./benthic/data_output/traits/benthic_dom_table_q.csv") 

#format dominant benthic invert traits data frame
trait_dom <- trait_dominant %>%
  select(-organism_code) %>% 
  column_to_rownames(var = "name") %>%
  #make categorical traits into factors
  mutate(across(c(
    armoring
    ,dispersal
    ,habit
    ,reproduction
    ,trophic_habit
    ,voltinism
    ,native
  ),factor)) %>% 
  glimpse()

# Load and format abundances ----------------

#Non-rare taxa: Abundances by year
#can just subset this to get Table L for dominant 14 taxa
abund <- read_csv("./benthic/data_output/benthic_table_l.csv") %>%
  column_to_rownames(var = "year_adjusted") %>% 
  #drop columns for three higher level taxa
  select(-c("turbellarian sp. A","mermithid sp. A","Actinolaiminae sp. A")) %>% 
  glimpse()

#Create dominant benthic invert abundances data frame
#it will be a subset of columns from non-rare taxa abundances data frame 

#create vector of dominant taxa names
dom_names <- trait_dominant %>% 
  pull(name)

#format abundance table
#use select(all_of()) to just keep the columns with names in the vector of 14 dominant taxa
abund_dom <- abund %>% 
  select(all_of(dom_names))

#Abundances by season
abund_fall <- read_csv("./benthic/data_output/benthic_table_l_fall.csv") %>%
  column_to_rownames(var = "year_adjusted") %>% 
  #drop columns for three higher level taxa
  select(-c("turbellarian sp. A","mermithid sp. A","Actinolaiminae sp. A"))

abund_spring <- read_csv("./benthic/data_output/benthic_table_l_spring.csv") %>%
  column_to_rownames(var = "year_adjusted") %>% 
  #drop columns for three higher level taxa
  select(-c("turbellarian sp. A","mermithid sp. A","Actinolaiminae sp. A"))

abund_summer <- read_csv("./benthic/data_output/benthic_table_l_summer.csv") %>%
  column_to_rownames(var = "year_adjusted") %>% 
  #drop columns for three higher level taxa
  select(-c("turbellarian sp. A","mermithid sp. A","Actinolaiminae sp. A"))

abund_winter <- read_csv("./benthic/data_output/benthic_table_l_winter.csv") %>%
  column_to_rownames(var = "year_adjusted") %>% 
  #drop columns for three higher level taxa
  select(-c("turbellarian sp. A","mermithid sp. A","Actinolaiminae sp. A")) 

# Load and format environmental variables ----------------

#read in data with all variables
#NOTE: this is a draft version of variables; will replace with refined one later
temp <- read_csv("./drought_variables/drought_variables.csv") %>%  
  #clean up column names
  clean_names() %>% 
  glimpse()

# replace NA w mean Phos for 2021
temp[55,12] <- mean(temp$phos, na.rm = T) 

#format data frame to match benthic invert abundances data frame
envn <- temp %>%
  #just keep the years for which we have benthic invert abundance data
  filter(year >= "1981" & year != "2022" & year!="2004" & year!="2005")%>% 
  #make drought_year_trunc a factor
  mutate(drought_year_trunc = as.factor(drought_year_trunc)) %>% 
  #let's just start with a subset of variables, focusing on continuous ones (ie, no factors)
  #also let's drop flow because highly correlated with salinity
  select(
    year
    , drought_year_trunc #added this later to adjust analyses, post uninteresting results
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

#have to remove 2021 from the winter season analysis as there were no samples that year
envn_winter <- temp %>%
  filter(year >= "1981" & year != "2022" & year!="2004" & year!="2005" & year!="2021")%>% 
  #let's just start with a subset of variables, focusing on continuous ones (ie, no factors)
  #also let's drop flow because highly correlated with salinity
  #make drought_year_trunc a factor
  mutate(drought_year_trunc = as.factor(drought_year_trunc)) %>% 
  select(
    year
    , drought_year_trunc #added this in later, post uninteresting results
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

#past work with clams has shown a one year lag in changes to abundances and distributions
#create new column with year shifted to one year later to match the abundance data for one year later
#eg, the year 1980 will be 1981 in new lag column so it will match with abundances for 1981
envn_lag1 <- temp %>%
  mutate(
    #add column with one year lag
    year_lag1 = year + 1,.after = year
    #make drought_year_trunc a factor
    ,drought_year_trunc = as.factor(drought_year_trunc)
    ) %>% 
  #filter out lag years that won't match abundances because data aren't available
  filter(year_lag1 >= "1981" & year_lag1 < "2022" & year_lag1 !="2004" & year_lag1 !="2005")%>% 
  select(
    year_lag1
    , drought_year_trunc #added this in later, results were initially interesting for year_lag but we added it to other analyses as well
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
  column_to_rownames(var = "year_lag1") %>% 
  glimpse()

#adjust lag environmental data for winter season analysis, to remove the year 2021
envn_lag1_winter <- temp %>%
  mutate(
    #add column with one year lag
    year_lag1 = year + 1,.after = year
    #make drought_year_trunc a factor
    ,drought_year_trunc = as.factor(drought_year_trunc)
  ) %>% 
  #filter out lag years that won't match abundances because data aren't available
  filter(year_lag1 >= "1981" & year_lag1 < "2022" & year_lag1 !="2004" & year_lag1 !="2005" & year_lag1 !="2021")%>% 
  select(
    year_lag1
    , drought_year_trunc #added this in later, results were initially interesting for year_lag but we added it to other analyses as well
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
  column_to_rownames(var = "year_lag1") %>% 
  glimpse()


# Analysis: Non-rare taxa, annual, no time lag --------------------

#check dimensions
dim(abund) #39 x 61 correspondance
dim(envn) #39 x 7 hillsmith, quantitative and factor
dim(trait) #61 x 2 hillsmith, quantitative and factor

#correspondance for abundance data
afcL.benthic <- 
  dudi.coa(abund, scannf = F)
summary(afcL.benthic) 
#total inertia 1.107

#hill-smith for environmental data
acpR.benthic <-
 dudi.hillsmith(envn,
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

#RQL analysis

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


#Fourth corner analysis

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

#combined RQL and Fourth Corner

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
#5 are right at p=0.05

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

#non-rare benthic: Format data for analysis ----------
#maybe should look at distribution of abundances
#Bocard et al 2011: CA is a method adapted to the analysis of species abundance data without pre-transformation

#try fourth root transformation of data
#see decostand function in vegan package for common transformations
#aft <- (abund)^(1/4)
#aft2 <- sqrt(abund)

#non-rare benthic: Run correspondence analysis on abundance data set ---------------

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


# Analysis: Dominant taxa, annual, no time lag --------------------

#check dimensions
dim(abund_dom) #39 x 14 correspondance
dim(envn) #39 x 7 hillsmith, quantitative and factor
dim(trait_dom) #14 x 9 hillsmith, quantitative and factor

#correspondance for abundance data
afcL.benthic_dom <- 
  dudi.coa(abund_dom, scannf = F)
summary(afcL.benthic_dom) 
#total inertia 1.006, which is a little worse than the non-rare taxa analysis with more taxa but fewer traits

#hill-smith for environmental data
acpR.benthic_dom <-
 dudi.hillsmith(envn,
                 row.w = afcL.benthic_dom$lw,
                 scannf = F,
                 nf = 2)
score(acpR.benthic_dom)

#hill-smith for trait data
acpQ.benthic_dom <- # 'Q' table of traits by species
  dudi.hillsmith(  # trait variables include numeric and categorical data
    trait_dom, 
    row.w = afcL.benthic_dom$cw,
    scannf = F)
score(acpQ.benthic_dom)

#RQL analysis

#build model
rlq.benthic_dom <- rlq(acpR.benthic_dom, afcL.benthic_dom, acpQ.benthic_dom,
                   scannf = FALSE)

#plot results
plot(rlq.benthic_dom)

#look at results summary
summary(rlq.benthic_dom)

#plot subset of graphs
par(mfrow = c(1, 3))
s.arrow(rlq.benthic_dom$l1)
s.arrow(rlq.benthic_dom$c1)
s.label(rlq.benthic_dom$lQ, boxes = FALSE)

#Fourth corner analysis

#build model
#this is version without adjustment of pvalues for multiple comparisons
nrepet <- 49999 
four.comb.benthic_dom <- fourthcorner(envn, abund_dom,
                                  trait_dom, modeltype = 6, p.adjust.method.G = "none",
                                  p.adjust.method.D = "none", nrepet = nrepet)

#plot results
par(mfrow = c(1, 1))
plot(four.comb.benthic_dom, alpha = 0.05, stat = "D2")
#a few sig results
four.comb.benthic_dom

#rerun model with adjustment of pvalues
four.comb.benthic_padj_dom <- fourthcorner(envn, abund_dom,
                                       trait_dom, modeltype = 6, p.adjust.method.G = "fdr",
                                       p.adjust.method.D = "fdr", nrepet = nrepet)
plot(four.comb.benthic_padj_dom, alpha = 0.05, stat = "D2") 
#no sig results

#combined RQL and Fourth Corner

testrlq.benthic_dom <- randtest(rlq.benthic_dom, modeltype = 6, nrepet = nrepet)
testrlq.benthic_dom
#Model 2 sign. but Model 4 not (p = 0.76)

plot(testrlq.benthic_dom)

#The total inertia of RLQ analysis is equal to the SRLQ multivariate statistic defined in Dray and
#Legendre (2008). This statistic is returned by the fourthcorner2 function
Srlq_dom <- fourthcorner2(envn,abund_dom,trait_dom,
                      modeltype = 6, p.adjust.method.G = "fdr", nrepet = nrepet)
Srlq_dom$trRLQ 

#biplot
#plot should be devoid of relationship lines
plot(four.comb.benthic_padj_dom, x.rlq = rlq.benthic_dom, alpha = 0.05,
     stat = "D2", type = "biplot")
#yep just shows the traits and env predictors

#Another approach is provided by the fourthcorner.rlq function and consists in testing directly the
#links between RLQ axes and traits (typetest="Q.axes") or environmental variables (typetest="R.axes").
testQaxes.comb.benthic_dom <- fourthcorner.rlq(rlq.benthic_dom, modeltype = 6,
                                           typetest = "Q.axes", nrepet = nrepet, p.adjust.method.G = "fdr",
                                           p.adjust.method.D = "fdr")
testRaxes.comb.benthic_dom <- fourthcorner.rlq(rlq.benthic_dom, modeltype = 6,
                                           typetest = "R.axes", nrepet = nrepet, p.adjust.method.G = "fdr",
                                           p.adjust.method.D = "fdr")
print(testQaxes.comb.benthic_dom, stat = "D")
#no significant pvalues

print(testRaxes.comb.benthic_dom, stat = "D")
#phos significant, nitrate/ammonia/salinity/temperature close 

#Results can be represented using a table with colors indicating significance :
par(mfrow = c(1, 2))
plot(testQaxes.comb.benthic_dom, alpha = 0.05, type = "table",
     stat = "D2")
plot(testRaxes.comb.benthic_dom, alpha = 0.05, type = "table",
     stat = "D2")
#only phos significant

# #Significance with axes can also be reported on the factorial map of RLQ analysis. Here, significant
# associations with the first axis are represented in blue, with the second axis in orange, with both axes in
# green (variables with no significant association are in black):
par(mfrow = c(1, 2))
plot(testQaxes.comb.benthic_dom, alpha = 0.05, type = "biplot",
     stat = "D2", col = c("black", "blue", "orange", "green"))
plot(testRaxes.comb.benthic_dom, alpha = 0.05, type = "biplot",
     stat = "D2", col = c("black", "blue", "orange", "green"))

# Analysis: Dominant taxa, annual, one year lag --------------------

#check dimensions
dim(abund_dom) #39 x 14 correspondance
dim(envn_lag1) #39 x 7 hillsmith, quantitative and factor
dim(trait_dom) #14 x 9 hillsmith, quantitative and factor

#hill-smith for environmental data
acpR.benthic_dom_lag <-
  dudi.hillsmith(envn_lag1,
                 row.w = afcL.benthic_dom$lw,
                 scannf = F,
                 nf = 2)
score(acpR.benthic_dom_lag)

#RQL analysis

#build model
rlq.benthic_dom_lag <- rlq(acpR.benthic_dom_lag, afcL.benthic_dom, acpQ.benthic_dom,
                       scannf = FALSE)

#plot results
plot(rlq.benthic_dom_lag)

#look at results summary
summary(rlq.benthic_dom_lag)

#plot subset of graphs
par(mfrow = c(1, 3))
s.arrow(rlq.benthic_dom_lag$l1)
s.arrow(rlq.benthic_dom_lag$c1)
s.label(rlq.benthic_dom_lag$lQ, boxes = FALSE)

#Fourth corner analysis

#build model
#this is version without adjustment of pvalues for multiple comparisons
nrepet <- 49999 
four.comb.benthic_dom_lag <- fourthcorner(envn_lag1, abund_dom,
                                      trait_dom, modeltype = 6, p.adjust.method.G = "none",
                                      p.adjust.method.D = "none", nrepet = nrepet)

#plot results
par(mfrow = c(1, 1))
plot(four.comb.benthic_dom_lag, alpha = 0.05, stat = "D2")
#a few sig results
four.comb.benthic_dom_lag
#a few sig results!

#rerun model with adjustment of pvalues
four.comb.benthic_padj_dom_lag <- fourthcorner(envn_lag1, abund_dom,
                                           trait_dom, modeltype = 6, p.adjust.method.G = "fdr",
                                           p.adjust.method.D = "fdr", nrepet = nrepet)
plot(four.comb.benthic_padj_dom_lag, alpha = 0.05, stat = "D2") 
#no significant results

#combined RQL and Fourth Corner

testrlq.benthic_dom_lag <- randtest(rlq.benthic_dom_lag, modeltype = 6, nrepet = nrepet)
testrlq.benthic_dom_lag
#Model 2 sig. but Model 4 not (p = 0.53)

plot(testrlq.benthic_dom_lag)

#The total inertia of RLQ analysis is equal to the SRLQ multivariate statistic defined in Dray and
#Legendre (2008). This statistic is returned by the fourthcorner2 function
Srlq_dom_lag <- fourthcorner2(envn_lag1,abund_dom,trait_dom,
                          modeltype = 6, p.adjust.method.G = "fdr", nrepet = nrepet)
Srlq_dom_lag$trRLQ 

#biplot
#plot should be devoid of relationship lines
plot(four.comb.benthic_padj_dom_lag, x.rlq = rlq.benthic_dom_lag, alpha = 0.05,
     stat = "D2", type = "biplot")
#yep just shows the traits and env predictors

#Another approach is provided by the fourthcorner.rlq function and consists in testing directly the
#links between RLQ axes and traits (typetest="Q.axes") or environmental variables (typetest="R.axes").
testQaxes.comb.benthic_dom_lag <- fourthcorner.rlq(rlq.benthic_dom_lag, modeltype = 6,
                                               typetest = "Q.axes", nrepet = nrepet, p.adjust.method.G = "fdr",
                                               p.adjust.method.D = "fdr")
testRaxes.comb.benthic_dom_lag <- fourthcorner.rlq(rlq.benthic_dom_lag, modeltype = 6,
                                               typetest = "R.axes", nrepet = nrepet, p.adjust.method.G = "fdr",
                                               p.adjust.method.D = "fdr")
print(testQaxes.comb.benthic_dom_lag, stat = "D")
#no significant pvalues

print(testRaxes.comb.benthic_dom_lag, stat = "D")
#a few significant

#Results can be represented using a table with colors indicating significance :
par(mfrow = c(1, 2))
plot(testQaxes.comb.benthic_dom_lag, alpha = 0.05, type = "table",
     stat = "D2")
plot(testRaxes.comb.benthic_dom_lag, alpha = 0.05, type = "table",
     stat = "D2")

# #Significance with axes can also be reported on the factorial map of RLQ analysis. Here, significant
# associations with the first axis are represented in blue, with the second axis in orange, with both axes in
# green (variables with no significant association are in black):
par(mfrow = c(1, 2))
plot(testQaxes.comb.benthic_dom_lag, alpha = 0.05, type = "biplot",
     stat = "D2", col = c("black", "blue", "orange", "green"))
plot(testRaxes.comb.benthic_dom_lag, alpha = 0.05, type = "biplot",
     stat = "D2", col = c("black", "blue", "orange", "green"))

# Analysis: Non-rare taxa, seasonal, no time lag --------------------

#check dimensions
dim(abund_winter) #39 x 61 correspondance
dim(envn) #39 x 7 hillsmith, quantitative and factor
dim(trait) #61 x 2 hillsmith, quantitative and factor

#for winter only, will remove 2021 from environmental table (table r)
envn_winter <- envn[-c(2021)]

#correspondance for abundance data

afcL.benthic_fall <- 
  dudi.coa(abund_fall, scannf = F)
summary(afcL.benthic_fall)
#total inertia: 1.59
afcL.benthic_spring <- 
  dudi.coa(abund_spring, scannf = F)
summary(afcL.benthic_spring)
#total inertia: 1.527
afcL.benthic_summer <- 
  dudi.coa(abund_summer, scannf = F)
summary(afcL.benthic_summer)
#total inertia: 1.31
afcL.benthic_winter <- 
  dudi.coa(abund_winter, scannf = F)
summary(afcL.benthic_winter)
#total inertia: 1.777

#hill-smith for environmental data
acpR.benthic_fall <-
  dudi.hillsmith(envn,
                 row.w = afcL.benthic_fall$lw,
                 scannf = F,
                 nf = 2)
score(acpR.benthic_fall)

acpR.benthic_spring <-
  dudi.hillsmith(envn,
                 row.w = afcL.benthic_spring$lw,
                 scannf = F,
                 nf = 2)
score(acpR.benthic_spring)

acpR.benthic_summer <-
  dudi.hillsmith(envn,
                 row.w = afcL.benthic_summer$lw,
                 scannf = F,
                 nf = 2)
score(acpR.benthic_summer)

#code for winter not working; need to check dimensions
acpR.benthic_winter <-
  dudi.hillsmith(envn_winter,
                 row.w = afcL.benthic_winter$lw,
                 scannf = F,
                 nf = 2)
score(acpR.benthic_winter)

#hill-smith for trait data
acpQ.benthic_fall <- # 'Q' table of traits by species
  dudi.hillsmith(  # trait variables include numeric and categorical data
    trait, 
    row.w = afcL.benthic_fall$cw,
    scannf = F)
score(acpQ.benthic_fall)
acpQ.benthic_spring <- # 'Q' table of traits by species
  dudi.hillsmith(  # trait variables include numeric and categorical data
    trait, 
    row.w = afcL.benthic_spring$cw,
    scannf = F)
score(acpQ.benthic_spring)
acpQ.benthic_summer <- # 'Q' table of traits by species
  dudi.hillsmith(  # trait variables include numeric and categorical data
    trait, 
    row.w = afcL.benthic_summer$cw,
    scannf = F)
score(acpQ.benthic_summer)
acpQ.benthic_winter <- # 'Q' table of traits by species
  dudi.hillsmith(  # trait variables include numeric and categorical data
    trait, 
    row.w = afcL.benthic_winter$cw,
    scannf = F)
score(acpQ.benthic_winter)

#RQL analysis

#build model
rlq.benthic_fall <- rlq(acpR.benthic_fall, afcL.benthic_fall, acpQ.benthic_fall,
                           scannf = FALSE)
rlq.benthic_spring <- rlq(acpR.benthic_spring, afcL.benthic_spring, acpQ.benthic_spring,
                        scannf = FALSE)
rlq.benthic_summer <- rlq(acpR.benthic_summer, afcL.benthic_summer, acpQ.benthic_summer,
                        scannf = FALSE)
#winter not running
rlq.benthic_winter <- rlq(acpR.benthic_winter, afcL.benthic_winter, acpQ.benthic_winter,
                        scannf = FALSE)

#plot results
plot(rlq.benthic_fall)
plot(rlq.benthic_spring)
plot(rlq.benthic_summer)
plot(rlq.benthic_winter) #winter not running

#look at results summary
summary(rlq.benthic_fall)

#plot subset of graphs
par(mfrow = c(1, 3))
s.arrow(rlq.benthic_fall$l1)
s.arrow(rlq.benthic_fall$c1)
s.label(rlq.benthic_fall$lQ, boxes = FALSE)
par(mfrow = c(1, 3))
s.arrow(rlq.benthic_spring$l1)
s.arrow(rlq.benthic_spring$c1)
s.label(rlq.benthic_spring$lQ, boxes = FALSE)
par(mfrow = c(1, 3))
s.arrow(rlq.benthic_summer$l1)
s.arrow(rlq.benthic_summer$c1)
s.label(rlq.benthic_summer$lQ, boxes = FALSE)
par(mfrow = c(1, 3)) #winter not running
s.arrow(rlq.benthic_winter$l1)
s.arrow(rlq.benthic_winter$c1)
s.label(rlq.benthic_winter$lQ, boxes = FALSE)

#Fourth corner analysis

#build model
#this is version without adjustment of pvalues for multiple comparisons
nrepet <- 49999 
four.comb.benthic_fall <- fourthcorner(envn, abund_fall,
                                          trait, modeltype = 6, p.adjust.method.G = "none",
                                          p.adjust.method.D = "none", nrepet = nrepet)
four.comb.benthic_spring <- fourthcorner(envn, abund_spring,
                                       trait, modeltype = 6, p.adjust.method.G = "none",
                                       p.adjust.method.D = "none", nrepet = nrepet)
four.comb.benthic_summer <- fourthcorner(envn, abund_summer,
                                       trait, modeltype = 6, p.adjust.method.G = "none",
                                       p.adjust.method.D = "none", nrepet = nrepet)
four.comb.benthic_winter <- fourthcorner(envn_winter, abund_winter,
                                       trait, modeltype = 6, p.adjust.method.G = "none",
                                       p.adjust.method.D = "none", nrepet = nrepet)

#plot results
par(mfrow = c(1, 1))
plot(four.comb.benthic_fall, alpha = 0.05, stat = "D2")#nothing significant, some close
four.comb.benthic_fall#no sig results
par(mfrow = c(1, 1))
plot(four.comb.benthic_spring, alpha = 0.05, stat = "D2")#some sig results
four.comb.benthic_spring#some sig, one close
par(mfrow = c(1, 1))
plot(four.comb.benthic_summer, alpha = 0.05, stat = "D2")#nothing significant, some close
four.comb.benthic_summer#no sig results, some close
par(mfrow = c(1, 1))
plot(four.comb.benthic_winter, alpha = 0.05, stat = "D2")#nothing significant, some close
four.comb.benthic_winter#no sig results, one close


#rerun model with adjustment of pvalues
four.comb.benthic_padj_fall <- fourthcorner(envn, abund_fall,
                                               trait, modeltype = 6, p.adjust.method.G = "fdr",
                                               p.adjust.method.D = "fdr", nrepet = nrepet)
plot(four.comb.benthic_padj_fall, alpha = 0.05, stat = "D2") 
#no significant results
four.comb.benthic_padj_spring <- fourthcorner(envn, abund_spring,
                                            trait, modeltype = 6, p.adjust.method.G = "fdr",
                                            p.adjust.method.D = "fdr", nrepet = nrepet)
plot(four.comb.benthic_padj_spring, alpha = 0.05, stat = "D2")
#no sig results
four.comb.benthic_padj_summer <- fourthcorner(envn, abund_summer,
                                            trait, modeltype = 6, p.adjust.method.G = "fdr",
                                            p.adjust.method.D = "fdr", nrepet = nrepet)
plot(four.comb.benthic_padj_summer, alpha = 0.05, stat = "D2")
#no sig results
four.comb.benthic_padj_winter <- fourthcorner(envn_winter, abund_winter,
                                            trait, modeltype = 6, p.adjust.method.G = "fdr",
                                            p.adjust.method.D = "fdr", nrepet = nrepet)
plot(four.comb.benthic_padj_winter, alpha = 0.05, stat = "D2")
#no sig results

#combined RQL and Fourth Corner

testrlq.benthic_fall <- randtest(rlq.benthic_fall, modeltype = 6, nrepet = nrepet)
testrlq.benthic_fall
plot(testrlq.benthic_fall)
#Model 2 sig. but Model 4 not (p = 0.11)
testrlq.benthic_spring <- randtest(rlq.benthic_spring, modeltype = 6, nrepet = nrepet)
testrlq.benthic_spring
plot(testrlq.benthic_spring)
#model 2 sig, model 4 close p=0.11
testrlq.benthic_summer <- randtest(rlq.benthic_summer, modeltype = 6, nrepet = nrepet)
testrlq.benthic_summer
plot(testrlq.benthic_summer)
#model 2 sig, model 4 close p=0.07
testrlq.benthic_winter <- randtest(rlq.benthic_winter, modeltype = 6, nrepet = nrepet)
testrlq.benthic_winter
plot(testrlq.benthic_winter)
#model 2 sig


#The total inertia of RLQ analysis is equal to the SRLQ multivariate statistic defined in Dray and
#Legendre (2008). This statistic is returned by the fourthcorner2 function
Srlq_fall <- fourthcorner2(envn,abund_fall,trait,
                              modeltype = 6, p.adjust.method.G = "fdr", nrepet = nrepet)
Srlq_fall$trRLQ 
Srlq_spring <- fourthcorner2(envn,abund_spring,trait,
                           modeltype = 6, p.adjust.method.G = "fdr", nrepet = nrepet)
Srlq_spring$trRLQ 
Srlq_summer <- fourthcorner2(envn,abund_summer,trait,
                           modeltype = 6, p.adjust.method.G = "fdr", nrepet = nrepet)
Srlq_summer$trRLQ 
Srlq_winter <- fourthcorner2(envn_winter,abund_winter,trait,
                           modeltype = 6, p.adjust.method.G = "fdr", nrepet = nrepet)
Srlq_winter$trRLQ 

#biplot
#plot should be devoid of relationship lines
plot(four.comb.benthic_padj_fall, x.rlq = rlq.benthic_fall, alpha = 0.05,
     stat = "D2", type = "biplot")
plot(four.comb.benthic_padj_spring, x.rlq = rlq.benthic_spring, alpha = 0.05,
     stat = "D2", type = "biplot")
plot(four.comb.benthic_padj_summer, x.rlq = rlq.benthic_summer, alpha = 0.05,
     stat = "D2", type = "biplot")
plot(four.comb.benthic_padj_winter, x.rlq = rlq.benthic_winter, alpha = 0.05,
     stat = "D2", type = "biplot")
#yep just shows the traits and env predictors

#Another approach is provided by the fourthcorner.rlq function and consists in testing directly the
#links between RLQ axes and traits (typetest="Q.axes") or environmental variables (typetest="R.axes").
testQaxes.comb.benthic_fall <- fourthcorner.rlq(rlq.benthic_fall, modeltype = 6,
                                                   typetest = "Q.axes", nrepet = nrepet, p.adjust.method.G = "fdr",
                                                   p.adjust.method.D = "fdr")
testRaxes.comb.benthic_fall <- fourthcorner.rlq(rlq.benthic_fall, modeltype = 6,
                                                   typetest = "R.axes", nrepet = nrepet, p.adjust.method.G = "fdr",
                                                   p.adjust.method.D = "fdr")
print(testQaxes.comb.benthic_fall, stat = "D")
#no significant pvalues

print(testRaxes.comb.benthic_fall, stat = "D")
#no significant pvalues

#Results can be represented using a table with colors indicating significance :
par(mfrow = c(1, 2))
plot(testQaxes.comb.benthic_fall, alpha = 0.05, type = "table",
     stat = "D2")
plot(testRaxes.comb.benthic_fall, alpha = 0.05, type = "table",
     stat = "D2")

# #Significance with axes can also be reported on the factorial map of RLQ analysis. Here, significant
# associations with the first axis are represented in blue, with the second axis in orange, with both axes in
# green (variables with no significant association are in black):
par(mfrow = c(1, 2))
plot(testQaxes.comb.benthic_fall, alpha = 0.05, type = "biplot",
     stat = "D2", col = c("black", "blue", "orange", "green"))
plot(testRaxes.comb.benthic_fall, alpha = 0.05, type = "biplot",
     stat = "D2", col = c("black", "blue", "orange", "green"))

# Analysis: Non-rare taxa, seasonal, one year lag --------------------

#check dimensions
dim(abund_winter) #38 x 61 correspondance (for winter)
dim(envn_lag1_winter) #38 x 7 all hillsmith, quantitative and factor
dim(trait) #61 x 2 hillsmith, quantitative and factor

#correspondance for abundance data, same as non-lag seasonal analysis

afcL.benthic_fall <- 
  dudi.coa(abund_fall, scannf = F)
summary(afcL.benthic_fall)
#total inertia: 1.59
afcL.benthic_spring <- 
  dudi.coa(abund_spring, scannf = F)
summary(afcL.benthic_spring)
#total inertia: 1.527
afcL.benthic_summer <- 
  dudi.coa(abund_summer, scannf = F)
summary(afcL.benthic_summer)
#total inertia: 1.31
afcL.benthic_winter <- 
  dudi.coa(abund_winter, scannf = F)
summary(afcL.benthic_winter)
#total inertia: 1.777

#hill-smith for environmental data
acpR.benthic_fall_lag <- 
  dudi.hillsmith(envn_lag1, 
           row.w = afcL.benthic_fall$lw,
           scannf = F,
           nf = 2 #leave this out for now to see all eigenvalues
  )
score(acpR.benthic_fall_lag)

acpR.benthic_spring_lag <- 
  dudi.hillsmith(envn_lag1, 
           row.w = afcL.benthic_spring$lw,
           scannf = F,
           nf = 2 #leave this out for now to see all eigenvalues
  )
score(acpR.benthic_spring_lag)

acpR.benthic_summer_lag <- 
  dudi.hillsmith(envn_lag1, 
           row.w = afcL.benthic_summer$lw,
           scannf = F,
           nf = 2 #leave this out for now to see all eigenvalues
  )
score(acpR.benthic_summer_lag)

acpR.benthic_winter_lag <- 
  dudi.hillsmith(envn_lag1_winter, 
           row.w = afcL.benthic_winter$lw,
           scannf = F,
           nf = 2 #leave this out for now to see all eigenvalues
  )
score(acpR.benthic_winter_lag)


#hill-smith for trait data, same as non-lag seasonal analysis
acpQ.benthic_fall <- # 'Q' table of traits by species
  dudi.hillsmith(  # trait variables include numeric and categorical data
    trait, 
    row.w = afcL.benthic_fall$cw,
    scannf = F)
score(acpQ.benthic_fall)
acpQ.benthic_spring <- # 'Q' table of traits by species
  dudi.hillsmith(  # trait variables include numeric and categorical data
    trait, 
    row.w = afcL.benthic_spring$cw,
    scannf = F)
score(acpQ.benthic_spring)
acpQ.benthic_summer <- # 'Q' table of traits by species
  dudi.hillsmith(  # trait variables include numeric and categorical data
    trait, 
    row.w = afcL.benthic_summer$cw,
    scannf = F)
score(acpQ.benthic_summer)
acpQ.benthic_winter <- # 'Q' table of traits by species
  dudi.hillsmith(  # trait variables include numeric and categorical data
    trait, 
    row.w = afcL.benthic_winter$cw,
    scannf = F)
score(acpQ.benthic_winter)

#RQL analysis

#build model
rlq.benthic_fall_lag <- rlq(acpR.benthic_fall_lag, afcL.benthic_fall, acpQ.benthic_fall,
                        scannf = FALSE)
rlq.benthic_spring_lag <- rlq(acpR.benthic_spring_lag, afcL.benthic_spring, acpQ.benthic_spring,
                          scannf = FALSE)
rlq.benthic_summer_lag <- rlq(acpR.benthic_summer_lag, afcL.benthic_summer, acpQ.benthic_summer,
                          scannf = FALSE)
rlq.benthic_winter_lag <- rlq(acpR.benthic_winter_lag, afcL.benthic_winter, acpQ.benthic_winter,
                          scannf = FALSE)

#plot results
plot(rlq.benthic_fall_lag)
plot(rlq.benthic_spring_lag)
plot(rlq.benthic_summer_lag)
plot(rlq.benthic_winter_lag)

#look at results summary
summary(rlq.benthic_fall_lag)

#plot subset of graphs
par(mfrow = c(1, 3))
s.arrow(rlq.benthic_fall_lag$l1)
s.arrow(rlq.benthic_fall_lag$c1)
s.label(rlq.benthic_fall_lag$lQ, boxes = FALSE)
par(mfrow = c(1, 3))
s.arrow(rlq.benthic_spring_lag$l1)
s.arrow(rlq.benthic_spring_lag$c1)
s.label(rlq.benthic_spring_lag$lQ, boxes = FALSE)
par(mfrow = c(1, 3))
s.arrow(rlq.benthic_summer_lag$l1)
s.arrow(rlq.benthic_summer_lag$c1)
s.label(rlq.benthic_summer_lag$lQ, boxes = FALSE)
par(mfrow = c(1, 3))
s.arrow(rlq.benthic_winter_lag$l1)
s.arrow(rlq.benthic_winter_lag$c1)
s.label(rlq.benthic_winter_lag$lQ, boxes = FALSE)

#Fourth corner analysis

#build model
#this is version without adjustment of pvalues for multiple comparisons
nrepet <- 49999 
four.comb.benthic_fall_lag <- fourthcorner(envn_lag1, abund_fall,
                                       trait, modeltype = 6, p.adjust.method.G = "none",
                                       p.adjust.method.D = "none", nrepet = nrepet)
four.comb.benthic_spring_lag <- fourthcorner(envn_lag1, abund_spring,
                                         trait, modeltype = 6, p.adjust.method.G = "none",
                                         p.adjust.method.D = "none", nrepet = nrepet)
four.comb.benthic_summer_lag <- fourthcorner(envn_lag1, abund_summer,
                                         trait, modeltype = 6, p.adjust.method.G = "none",
                                         p.adjust.method.D = "none", nrepet = nrepet)
four.comb.benthic_winter_lag <- fourthcorner(envn_lag1_winter, abund_winter,
                                         trait, modeltype = 6, p.adjust.method.G = "none",
                                         p.adjust.method.D = "none", nrepet = nrepet)

#plot results
par(mfrow = c(1, 1))
plot(four.comb.benthic_fall_lag, alpha = 0.05, stat = "D2")#nothing significant, some close
four.comb.benthic_fall_lag#some sig results
par(mfrow = c(1, 1))
plot(four.comb.benthic_spring_lag, alpha = 0.05, stat = "D2")#some sig results
four.comb.benthic_spring_lag#some sig
par(mfrow = c(1, 1))
plot(four.comb.benthic_summer_lag, alpha = 0.05, stat = "D2")#nothing significant, some close
four.comb.benthic_summer_lag#some sig results
par(mfrow = c(1, 1))
plot(four.comb.benthic_winter_lag, alpha = 0.05, stat = "D2")#nothing significant, some close
four.comb.benthic_winter_lag#some sig results


#rerun model with adjustment of pvalues
four.comb.benthic_padj_fall_lag <- fourthcorner(envn_lag1, abund_fall,
                                            trait, modeltype = 6, p.adjust.method.G = "fdr",
                                            p.adjust.method.D = "fdr", nrepet = nrepet)
plot(four.comb.benthic_padj_fall_lag, alpha = 0.05, stat = "D2") 
#2 significant results
four.comb.benthic_padj_spring_lag <- fourthcorner(envn_lag1, abund_spring,
                                              trait, modeltype = 6, p.adjust.method.G = "fdr",
                                              p.adjust.method.D = "fdr", nrepet = nrepet)
plot(four.comb.benthic_padj_spring_lag, alpha = 0.05, stat = "D2")
#no sig results
four.comb.benthic_padj_summer_lag <- fourthcorner(envn_lag1, abund_summer,
                                              trait, modeltype = 6, p.adjust.method.G = "fdr",
                                              p.adjust.method.D = "fdr", nrepet = nrepet)
plot(four.comb.benthic_padj_summer_lag, alpha = 0.05, stat = "D2")
#no sig results
four.comb.benthic_padj_winter_lag <- fourthcorner(envn_lag1_winter, abund_winter,
                                              trait, modeltype = 6, p.adjust.method.G = "fdr",
                                              p.adjust.method.D = "fdr", nrepet = nrepet)
plot(four.comb.benthic_padj_winter_lag, alpha = 0.05, stat = "D2")
#no sig results

#season/lag benthic: combined RQL and Fourth Corner

testrlq.benthic_fall_lag <- randtest(rlq.benthic_fall_lag, modeltype = 6, nrepet = nrepet)
testrlq.benthic_fall_lag
plot(testrlq.benthic_fall_lag)
#Model 2 sig
testrlq.benthic_spring_lag <- randtest(rlq.benthic_spring_lag, modeltype = 6, nrepet = nrepet)
testrlq.benthic_spring_lag
plot(testrlq.benthic_spring_lag)
#model 2 sig
testrlq.benthic_summer_lag <- randtest(rlq.benthic_summer_lag, modeltype = 6, nrepet = nrepet)
testrlq.benthic_summer_lag
plot(testrlq.benthic_summer_lag)
#model 2 sig
testrlq.benthic_winter_lag <- randtest(rlq.benthic_winter_lag, modeltype = 6, nrepet = nrepet)
testrlq.benthic_winter_lag
plot(testrlq.benthic_winter_lag)
#model 2 sig


#The total inertia of RLQ analysis is equal to the SRLQ multivariate statistic defined in Dray and
#Legendre (2008). This statistic is returned by the fourthcorner2 function
Srlq_fall_lag <- fourthcorner2(envn_lag1,abund_fall,trait,
                           modeltype = 6, p.adjust.method.G = "fdr", nrepet = nrepet)
Srlq_fall_lag$trRLQ 
Srlq_spring_lag <- fourthcorner2(envn_lag1,abund_spring,trait,
                             modeltype = 6, p.adjust.method.G = "fdr", nrepet = nrepet)
Srlq_spring_lag$trRLQ 
Srlq_summer_lag <- fourthcorner2(envn_lag1,abund_summer,trait,
                             modeltype = 6, p.adjust.method.G = "fdr", nrepet = nrepet)
Srlq_summer_lag$trRLQ 
Srlq_winter_lag <- fourthcorner2(envn_lag1_winter,abund_winter,trait,
                             modeltype = 6, p.adjust.method.G = "fdr", nrepet = nrepet)
Srlq_winter_lag$trRLQ 

#biplot
#plot should be devoid of relationship lines
plot(four.comb.benthic_padj_fall_lag, x.rlq = rlq.benthic_fall_lag, alpha = 0.05,
     stat = "D2", type = "biplot")
  #uh oh, this one has 2 relationship lines?

plot(four.comb.benthic_padj_spring_lag, x.rlq = rlq.benthic_spring_lag, alpha = 0.05,
     stat = "D2", type = "biplot")
plot(four.comb.benthic_padj_summer_lag, x.rlq = rlq.benthic_summer_lag, alpha = 0.05,
     stat = "D2", type = "biplot")
plot(four.comb.benthic_padj_winter_lag, x.rlq = rlq.benthic_winter_lag, alpha = 0.05,
     stat = "D2", type = "biplot")
#yep just shows the traits and env predictors

# #Another approach is provided by the fourthcorner.rlq function and consists in testing directly the
# #links between RLQ axes and traits (typetest="Q.axes") or environmental variables (typetest="R.axes").
# testQaxes.comb.benthic_fall <- fourthcorner.rlq(rlq.benthic_fall, modeltype = 6,
#                                                 typetest = "Q.axes", nrepet = nrepet, p.adjust.method.G = "fdr",
#                                                 p.adjust.method.D = "fdr")
# testRaxes.comb.benthic_fall <- fourthcorner.rlq(rlq.benthic_fall, modeltype = 6,
#                                                 typetest = "R.axes", nrepet = nrepet, p.adjust.method.G = "fdr",
#                                                 p.adjust.method.D = "fdr")
# print(testQaxes.comb.benthic_fall, stat = "D")
# #no significant pvalues
# 
# print(testRaxes.comb.benthic_fall, stat = "D")
# #no significant pvalues
# 
# #Results can be represented using a table with colors indicating significance :
# par(mfrow = c(1, 2))
# plot(testQaxes.comb.benthic_fall, alpha = 0.05, type = "table",
#      stat = "D2")
# plot(testRaxes.comb.benthic_fall, alpha = 0.05, type = "table",
#      stat = "D2")
# 
# # #Significance with axes can also be reported on the factorial map of RLQ analysis. Here, significant
# # associations with the first axis are represented in blue, with the second axis in orange, with both axes in
# # green (variables with no significant association are in black):
# par(mfrow = c(1, 2))
# plot(testQaxes.comb.benthic_fall, alpha = 0.05, type = "biplot",
#      stat = "D2", col = c("black", "blue", "orange", "green"))
# plot(testRaxes.comb.benthic_fall, alpha = 0.05, type = "biplot",
#      stat = "D2", col = c("black", "blue", "orange", "green"))



# Analysis: Non-rare taxa, annual, one year lag

#check dimensions
dim(abund) #39 x 61 correspondance
dim(envn_lag1) #39 x 7 hillsmith, quantitative and factor
dim(trait) #61 x 2 hillsmith, quantitative and factor

#hill-smith for envn
acpR.benthic_nonrare_lag <- 
  dudi.pca(envn_lag1, 
           row.w = afcL.benthic$lw,
           scannf = F,
           nf = 2 #leave this out for now to see all eigenvalues
  )
score(acpR.benthic_nonrare_lag1)

#RQL analysis

#build model
rlq.benthic_nonrare_lag <- rlq(acpR.benthic_nonrare_lag, afcL.benthic, acpQ.benthic,
                           scannf = FALSE)

#plot results
plot(rlq.benthic_nonrare_lag)

#look at results summary
summary(rlq.benthic_nonrare_lag)

#plot subset of graphs
par(mfrow = c(1, 3))
s.arrow(rlq.benthic_nonrare_lag$l1)
s.arrow(rlq.benthic_nonrare_lag$c1)
s.label(rlq.benthic_nonrare_lag$lQ, boxes = FALSE)

#Fourth corner analysis

#build model
#this is version without adjustment of pvalues for multiple comparisons
nrepet <- 49999 
four.comb.benthic_nonrare_lag <- fourthcorner(envn_lag1, abund,
                                          trait, modeltype = 6, p.adjust.method.G = "none",
                                          p.adjust.method.D = "none", nrepet = nrepet)

#plot results
par(mfrow = c(1, 1))
plot(four.comb.benthic_nonrare_lag, alpha = 0.05, stat = "D2")
#a few sig results
four.comb.benthic_nonrare_lag
#a few sig results!

#rerun model with adjustment of pvalues
four.comb.benthic_padj_nonrare_lag <- fourthcorner(envn_lag1, abund,
                                               trait, modeltype = 6, p.adjust.method.G = "fdr",
                                               p.adjust.method.D = "fdr", nrepet = nrepet)
plot(four.comb.benthic_padj_nonrare_lag, alpha = 0.05, stat = "D2") 
#no significant results

#non-rare year_lag benthic: combined RQL and Fourth Corner

testrlq.benthic_nonrare_lag <- randtest(rlq.benthic_nonrare_lag, modeltype = 6, nrepet = nrepet)
testrlq.benthic_nonrare_lag
#Model 2 sig. but Model 4 not (p = 0.53)

plot(testrlq.benthic_nonrare_lag)

#The total inertia of RLQ analysis is equal to the SRLQ multivariate statistic defined in Dray and
#Legendre (2008). This statistic is returned by the fourthcorner2 function
Srlq_nonrare_lag <- fourthcorner2(envn_lag1,abund,trait,
                              modeltype = 6, p.adjust.method.G = "fdr", nrepet = nrepet)
Srlq_nonrare_lag$trRLQ 

#biplot
#plot should be devoid of relationship lines
plot(four.comb.benthic_padj_nonrare_lag, x.rlq = rlq.benthic_nonrare_lag, alpha = 0.05,
     stat = "D2", type = "biplot")
#yep just shows the traits and env predictors

#Another approach is provided by the fourthcorner.rlq function and consists in testing directly the
#links between RLQ axes and traits (typetest="Q.axes") or environmental variables (typetest="R.axes").
testQaxes.comb.benthic_nonrare_lag <- fourthcorner.rlq(rlq.benthic_nonrare_lag, modeltype = 6,
                                                   typetest = "Q.axes", nrepet = nrepet, p.adjust.method.G = "fdr",
                                                   p.adjust.method.D = "fdr")
testRaxes.comb.benthic_nonrare_lag <- fourthcorner.rlq(rlq.benthic_nonrare_lag, modeltype = 6,
                                                   typetest = "R.axes", nrepet = nrepet, p.adjust.method.G = "fdr",
                                                   p.adjust.method.D = "fdr")
print(testQaxes.comb.benthic_nonrare_lag, stat = "D")
#no significant pvalues

print(testRaxes.comb.benthic_nonrare_lag, stat = "D")
#4 significant

#Results can be represented using a table with colors indicating significance :
par(mfrow = c(1, 2))
plot(testQaxes.comb.benthic_nonrare_lag, alpha = 0.05, type = "table",
     stat = "D2")
plot(testRaxes.comb.benthic_nonrare_lag, alpha = 0.05, type = "table",
     stat = "D2")

# #Significance with axes can also be reported on the factorial map of RLQ analysis. Here, significant
# associations with the first axis are represented in blue, with the second axis in orange, with both axes in
# green (variables with no significant association are in black):
par(mfrow = c(1, 2))
plot(testQaxes.comb.benthic_nonrare_lag, alpha = 0.05, type = "biplot",
     stat = "D2", col = c("black", "blue", "orange", "green"))
plot(testRaxes.comb.benthic_nonrare_lag, alpha = 0.05, type = "biplot",
     stat = "D2", col = c("black", "blue", "orange", "green"))
