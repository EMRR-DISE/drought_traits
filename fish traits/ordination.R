# Pete Nelson, PhD
# Department of Water Resources
# project: drought traits (special studies)
# purpose: explore other ways to look at the data...

# created: 2024-02-08

# getting started -----
library(tidyverse)
library(vegan)
library(ggplot2)

# read fish-drought-year data -----
## L df ----
# fish occurrence data with years prior to 1975 + 1979 removed bc environmental data incomplete for these

fish <- read_rds("fish traits/fish_data/fish.rds")

fish_rel <- read_rds("fish traits/fish_data/fish.rds") %>% 
  decostand(., method = "total") %>% # convert to relative abundance
  print()

## R df ----
# environmental data for each year included in fish data
# reverse environmental data to merely quant + factorial (no longer ordered)
fenv <- read_rds("fish traits/fish_data/fenv.rds") %>% 
  mutate(drought_year_trunc = factor(drought_year_trunc, ordered = F),
         water_year_sac = factor(water_year_sac, ordered = F)) %>% 
  select(-c(drought_year, Nitrate, Salinity, Secchi)) %>% # trying to reduce number of variables
  rownames_to_column(., "year")

glimpse(fenv)

## Q df ----
# trait data for each fish sp
# combo of quantitative and factorial variables
ftrait <- read_rds("fish traits/fish_data/ftrait.rds") %>% 
  select(-c(troph_se, life_span, fish_vul))
# trying to reduce number of variables
glimpse(ftrait)

# check that the dimensions of the LQR tables match
dim(fish_rel) # L df, 46 years and 19 spp; quantitative--PCA is fine
dim(fenv) # R df, each year per each of the 10 environmental variables; multiple factorial variables + quantitative
dim(ftrait) # Q df, each spp described by 14 traits (fecundity, habitat, l_max, etc); like fenv, mixed

# fish abund NMDS ----
nmds <- metaMDS(fish_rel, # abundance modified to "relative"
                distance = "bray", 
                k = 3,
                maxit = 999,
                trymax = 250,
                wascores = T)

nmds1 <- metaMDS(fish, # abundance NOT modified to "relative"
                 distance = "bray",
                 k = 3,
                 maxit = 999,
                 trymax = 250,
                 wascores = T)

# fish abund CA ----
cca <- cca(fish)
cca1 <- cca(fish_rel)  

# fish abund DCA -----
# detrended correspondence analysis
dca <- decorana(fish)
dca1 <- decorana(fish_rel)

# fish abund CCorA -----
# canonical correlation analysis
CCorA(fish)

# plots ----
# wo type=t, years=circles, spp=crosses
plot(nmds, type = "t") # using relative abundance
plot(nmds1, type = "t") # straight abundance

plot(cca, type = "t")
plot(cca1, type = "t")

plot(dca, type = "t") # no separation for years!
plot(dca1, type = "t") # 'years' little different, but nice clusters for spp

# NMDS scores
year <- as.data.frame(scores(nmds)$sites) %>% 
  rownames_to_column(., "year") %>% 
  tibble() %>% 
  left_join(., fenv[,1:3], by = "year")

spp <- as.data.frame(scores(nmds)$species) %>% 
  rownames_to_column(., "species") %>% 
  tibble()


year %>% 
  ggplot(.,
         aes(x = NMDS1, y = NMDS2, color = drought_year_trunc)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("blue", "green", "orange", "red")) +
  theme_bw()
# Fairly tight cluster to the right but lots of scatter. Dry years are concentrated in that cluster, but there are plenty of wet years in the same group. Weird.

spp %>% 
  ggplot(.,
         aes(x = NMDS1, y = NMDS2)) +
  geom_point(size = 3) +
  theme_bw()

# SCRATCH ################
biplot(out, "ob")                 # Two plots of objects
biplot(out, "v", cex=c(0.7,0.6))  # Two plots of variables
biplot(out, "ov", cex=c(0.7,0.6)) # Four plots (2 for objects, 2 for variables)
biplot(out, "b", cex=c(0.7,0.6))  # Two biplots
biplot(out, xlabs = NA, plot.axes = c(3,5))    # Plot axes 3, 5. No object names
biplot(out, plot.type="biplots", xlabs = NULL) # Replace object names by numbers
