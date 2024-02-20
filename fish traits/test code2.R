# Pete Nelson, PhD
# Department of Water Resources
# project: 4th corner / RLQ demo and exploration
# purpose: complete fourth-corner & RLQ demo script from Dray 2013 A tutorial to perform fourth-corner and RLQ analyses in R (unpublished?)
# created: 2023-11-11
# last modified: 

# getting started -----
library(tidyverse)
library(ade4)
library(vegan)
library(ggvegan)
data(aravo)

aravo
glimpse(aravo)
dim(aravo$spe) # 75 plots eval for as many as 82 plant spp
dim(aravo$traits) # 82 plant spp eval for each of 8 plant traits
dim(aravo$env) # 75 plots eval for each of 6 environmental variables

# RLQ ---------------
## analyze each table ---------

# Apply correspondence analysis to the species table, hillsmith function to the environmental table (because includes both quantitative and categorical variables), principal components analysis to traits (these all quantitative). 

afcL.aravo <- dudi.coa(aravo$spe, scannf = F) 
acpR.aravo <- dudi.hillsmith(aravo$env, row.w = afcL.aravo$lw, scannf = FALSE) 
acpQ.aravo <- dudi.pca(aravo$traits, row.w = afcL.aravo$cw, scannf = FALSE)

# Note that the RLQ analysis requires that trait and environmental variables be weighted by the sites and spp weights derived from the previous correspondence analysis. For this reason, you can't included ordered factors for either trait or environmental data; this would require the use of dudi.mix() which doesn't allow for the inclusion of weighted rows.

rlq.aravo <- rlq(acpR.aravo, afcL.aravo, acpQ.aravo, scannf = FALSE)
plot(rlq.aravo)
s.arrow(rlq.aravo$l1) # bottom, 2nd from left; environment
s.arrow(rlq.aravo$c1) # bottom, middle; traits
s.label(rlq.aravo$lQ, boxes = F) # top right; species (note use of labels, no arrows)

# SCRATCH ####################
# 2 0.75 c4s, first pieces on pitch 1 of Serenity, 1 no 2 C4