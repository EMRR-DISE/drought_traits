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
fmwt1 <- read_csv("fish_data/fmwt1.csv") # derived from fmwt.R, collapsed striper data
index_stations <- read_csv("fish_data/index_stations.csv") # from fmwt.R
spp_occ <- read_csv("fish_data/spp_occ.csv")
year_types <- read_excel("fish_data/yeartypes.xlsx") %>% select(-...1)

# NMDS ----

library(vegan)

data <- rename(fmwt1,
               c(Am.Shad = american_shad,
                 Chinook = chinook_salmon,
                 Delta.Smelt = delta_smelt,
                 Longfin = longfin_smelt,
                 Anchovy = northern_anchovy,
                 Herring = pacific_herring,
                 Splittail = splittail,
                 Striped.Bass = striped_bass,
                 Threadfin = threadfin_shad)) %>% 
  left_join(year_types, keep = FALSE) %>% 
  select(-c(drought, shortterm, sprNDOI, yr_type)) %>% 
  filter(year >= 1970) %>% 
  filter(year != 1974 & year != 1979)  # remove years 1974 & 1979, no fmwt data

nmds <- metaMDS(data[,2:10], # spp data only
                noshare = T, 
                autotransform = FALSE, 
                trymax = 500)
plot(nmds, type = "t")

## plot with ggplot ----
data.scores <- as.tibble(scores(nmds, "sites"))
data.scores$year <- data$year
data.scores$yr_type <- data$yr_type2

spp.scores <- as.tibble(scores(nmds, "species")) # use vegan scores function to extract spp scores, convert to df
spp.scores$species <- c("Am.Shad", "Chinook", "Delta.Smelt", "Longfin", "Anchovy",
                        "Herring", "Splittail", "Striped.Bass", "Threadfin")
head(spp.scores)  #look at the data

# simple nmds plot of the years 1970-2021
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
  geom_text(data = spp.scores, 
            aes(x = NMDS1, y = NMDS2, label = species), 
            alpha = 0.8) +
  geom_point(data = data.scores, 
             aes(x = NMDS1, y = NMDS2,
                 color = yr_type),
             size = 3) +
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
