#Special Studies Drought Synthesis
#Explore Dray et al 2014 example analysis

#required packages
library(ade4) #package by paper authors

#look at data sets-------------------
#these are all included in the ade4 package

data("aravo")

#species abundance matrix
species <- aravo$spe

#species traits
traits <- aravo$traits

#environmental predictors
env <- aravo$env

#RQL----------

#correspondence analysis of species table
afcL.aravo <- dudi.coa(aravo$spe, scannf = T)
summary(afcL.aravo)
#total inertia: 4.214


#principle components analysis of traits data
#works because all traits are quantitative
acpQ.aravo <- dudi.pca(aravo$traits, row.w = afcL.aravo$cw,
                       scannf = FALSE)

#Hill-Smith analysis of environmental variables
#used this because env variables are mix of quantitative and qualitative variables
acpR.aravo <- dudi.hillsmith(aravo$env, row.w = afcL.aravo$lw,
                             scannf = FALSE)

#run the RQL analysis
#grabs coefficients (in $c1) to obtain linear combinations of traits (species scores in $lQ)
#grabs coefficients (in $l1) to get linear combinations of env variables (site scores in $lR)
rlq.aravo <- rlq(acpR.aravo, afcL.aravo, acpQ.aravo,
                 scannf = FALSE)

#RQL summary
summary(rlq.aravo)

#look at species scores (AxcQ1 and AxcQ2)
rlq.aravo$lQ

#look at site scores (AxcR1 and AxcR2)
rlq.aravo$lR

#view set of plots from RQL
plot(rlq.aravo)

#view these plots separately
par(mfrow = c(1, 3))
s.arrow(rlq.aravo$l1) #env
s.arrow(rlq.aravo$c1) #traits
s.label(rlq.aravo$lQ, boxes = FALSE) #species

#fourth corner analysis--------------------

#nrepet <- 49999
nrepet <- 999 #runs faster
four.comb.aravo <- fourthcorner(aravo$env, aravo$spe,
                                aravo$traits, modeltype = 6, p.adjust.method.G = "none",
                                p.adjust.method.D = "none", nrepet = nrepet)

#adjust p values for multiple comparisons
four.comb.aravo.adj <- p.adjust.4thcorner(four.comb.aravo,
                                          p.adjust.method.G = "fdr", p.adjust.method.D = "fdr")

#Note that, adjusted p-values can be obtained directly using the fourthcorner function:
fourthcorner(aravo$env, aravo$spe, aravo$traits, modeltype = 6,
             p.adjust.method.G = "fdr", p.adjust.method.D = "fdr",
             nrepet = nrepet)

#plot results with pvalues not adjusted
par(mfrow = c(1, 1))
plot(four.comb.aravo, alpha = 0.05, stat = "D2")

#plot results with pvalues adjusted
par(mfrow = c(1, 1))
plot(four.comb.aravo.adj, alpha = 0.05, stat = "D2")
#note: probably shows fewer sign. associations because used fewer nrepet

#RQL and fourth corner combined------------

#multivariate test applied to evaluate the global significance of the traits-environment
#relationships. This test is based on the total inertia of the RLQ analysis
testrlq.aravo <- randtest(rlq.aravo, modeltype = 6, nrepet = nrepet)

#summary of test results
testrlq.aravo
#results are highly significant
#indicates global relationship between env and traits

#look at example with partial RLQ
#effect of year and habitat removed to look at effect of grazing
#on plant traits (see Wesuls et al 2012)

#look at data
data(piosphere)

#four tables

#veg: site by species abundance
veg <- piosphere$veg

#species traits
traits <- piosphere$traits

#env variables
env <- piosphere$env

#habitat: vector of habitat types for each site
habitat<-as.data.frame(piosphere$habitat)

#RQL analysis
afcL <- dudi.coa(log(piosphere$veg + 1), scannf = FALSE)
acpR <- dudi.pca(piosphere$env, scannf = FALSE, row.w = afcL$lw)
acpQ <- dudi.hillsmith(piosphere$traits, scannf = FALSE, row.w = afcL$cw)
rlq1 <- rlq(acpR, afcL, acpQ, scannf = FALSE)

#partial RQL
wrlq1 <- wca(rlq1, fac = piosphere$habitat, scannf = FALSE)
wrlq1
plot(wrlq1)




