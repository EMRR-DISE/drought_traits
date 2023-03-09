# Pete Nelson, PhD
# Department of Water Resources
# project: drought traits (special studies)
# purpose: analysis of FMWT (fish) data
# created: 2022-09-02
# last modified: 2022-12-06

# get started -----
rm(list= ls()[!(ls() %in% c("fmwt1", "spp_occ",    # indices & freq of occurrence
                            "index_stations",
                            "year_types"))])  # station location

library(tidyverse)
library(readxl)
library(scales)

pal_yrtype <- c("Critical" = "#FDE333", "Dry" = "#53CC67", "Below Normal" = "#009B95","Above Normal" = "#00588B", "Wet" = "#4B0055")
show_col(pal_yrtype)
pal_yrtype2 <- c("CR" = "#FDE333", "DY" = "#53CC67", "BN" = "#009B95","AN" = "#00588B", "WT" = "#4B0055")
show_col(pal_yrtype2)

# if necessary -----
fmwt1 <- read_csv("fish/fish_data/fmwt1.csv") # derived from fmwt.R, collapsed striper data
index_stations <- read_csv("fish/fish_data/index_stations.csv") # from fmwt.R
spp_occ <- read_csv("fish/fish_data/spp_occ.csv")
year_types <- read_excel("fish/fish_data/yeartypes.xlsx")

# ordination ----
library(vegan)

# remove years with no data and 'year' column from data set
data <- fmwt1 %>% 
  filter(year != "1974" & year != "1979") %>% # remove years 1974 & 1979, no fmwt data
  rename(c(Am.Shad = american_shad,
           Chinook = chinook_salmon,
           D.Smelt = delta_smelt,
           Longfin = longfin_smelt,
           Anchovy = northern_anchovy,
           Herring = pacific_herring,
           Splittail = splittail,
           Str.Bass = striped_bass,
           Threadfin = threadfin_shad)) %>%
  left_join(year_types, keep = FALSE) %>% 
  select(-c(year, drought, shortterm, sprNDOI, yr_type))

## correspondence analysis ----
(CA <- cca(data[,1:9]))
ordiplot(CA)

## detrended correspondence analysis ----
(DCA <- decorana(data[,1:9]))
(DCA2 <- decorana(log1p(data[,1:9]))) # causes years to cluster tightly relative to spp; not informative
ordiplot(DCA)
# length of first axis, transformed or not, is <3 SD, so data should be analyzed by linear (PCA, RDA), not unimodal, ordination methods. See https://www.davidzeleny.net/anadat-r/doku.php/en:ordination#linear_or_unimodal_ordination_method for details
# Zeleny recommends transforming spp composition data using the Hellinger standardization

### plot with ggplot ----
data.scores <- as_tibble(scores(DCA, "sites"))
data.scores$year <- fmwt1[-c(8,13),]$year # add year but remove 1974 and 1979 (no data)
data.scores$yr_type <- data$yr_type2 # add year type

spp.scores <- as_tibble(scores(DCA, "species")) # use vegan scores function to extract spp scores, convert to df
spp.scores$species <- c("Am.Shad", "Chinook", "D.Smelt", "Longfin", "Anchovy",
                        "Herring", "Splittail", "Str.Bass", "Threadfin")
head(spp.scores)  #look at the data

# simple DCA plot of the years 1967-2021
ggplot() +
  geom_point(data = data.scores, 
             aes(x = DCA1, y = DCA2),
             size = 3) +
  theme_bw()

# color the points by year type
ggplot() +
  geom_point(data = data.scores, 
             aes(x = DCA1, y = DCA2,
                 color = data$yr_type2),
             size = 3) +
  scale_color_manual(values = pal_yrtype2) +
  theme_bw()

# add species
ggplot() +
  geom_point(data = data.scores, 
             aes(x = DCA1, y = DCA2,
                 color = data$yr_type2),
             size = 3) +
  geom_text(data = spp.scores, 
            aes(x = DCA1, y = DCA2, label = species), 
            alpha = 0.8) +
  xlim(-1.5, 1.5) +
  scale_color_manual(values = pal_yrtype2) +
  theme_bw()

## NMDS ----
nmds <- metaMDS(data[,1:9], # spp data only
                noshare = T, 
                autotransform = FALSE, 
                trymax = 500)
plot(nmds, type = "t")

### plot with ggplot ----
data.scores <- as_tibble(scores(nmds, "sites"))
data.scores$year <- fmwt1[-c(8,13),]$year # add year but remove 1974 and 1979 (no data)
data.scores$yr_type <- data$yr_type2 # add year type


spp.scores <- as_tibble(scores(nmds, "species")) # use vegan scores function to extract spp scores, convert to df
spp.scores$species <- c("Am.Shad", "Chinook", "D.Smelt", "Longfin", "Anchovy",
                        "Herring", "Splittail", "Str.Bass", "Threadfin")
head(spp.scores)  #look at the data

# simple nmds plot of the years 1967-2021
ggplot() +
  geom_point(data = data.scores, 
             aes(x = NMDS1, y = NMDS2),
             size = 3) +
  theme_bw()

# color the points by year type
ggplot() +
  geom_point(data = data.scores, 
             aes(x = NMDS1, y = NMDS2,
                 color = yr_type),
             size = 3) +
  scale_color_manual(values = pal_yrtype2) +
  theme_bw()

# add species
ggplot() +
  geom_point(data = data.scores, 
             aes(x = NMDS1, y = NMDS2,
                 color = yr_type),
             size = 3) +
  geom_text(data = spp.scores, 
            aes(x = NMDS1, y = NMDS2, label = species), 
            alpha = 0.8) +
  # expand x-axis to accomodate longfin
  xlim(-2, 2) +
  scale_color_manual(values = pal_yrtype2) +
  theme_bw()


####
# SCRATCH ####
####
data(varespec) # columns 44: spp; rows 24: sites; data are vegetation cover values
data(varechem) # 24 by 14; soil characteristics of the 24 sites

test <- as_tibble(varespec) %>%
  mutate(sites = factor(rep(c("grassland", "quarry"), each = 12)),
        obs = factor(LETTERS[1:24])) %>% 
  relocate(sites, obs)

ord <- metaMDS(varespec)
plot(ord, type = "t")
## Fit environmental variables
ef <- envfit(ord, varechem)
ef
plot(ef, p.max = 0.05) # show effect of environmental variables on ordination

ord <- metaMDS(test[,3:46])
