# Laura's effort mapping (tile/heat map)
library(tidyverse)

# function to define seasons based on 'SampleDate'
library(lubridate)
get_season <- function(SampleDate){
  numeric.date <- 100 * month(SampleDate) + 
    day(SampleDate)
  # input seasons upper limits in form MMDD in the "break =" option:
  cuts <- base::cut(numeric.date, breaks = c(0, 319, 0620, 0921, 1220, 1231))
  # rename resulting groups (could've been done within cut(...levels=) 
  # if "winter" wasn't double)
  levels(cuts) <- c("winter", "spring", "summer", "fall", "winter")
  return(cuts)
}

# sample counts ----
## MWTR -----
# summarize count of samples in each region by season by years
# NOTE: lines in dt2 don't equate with unique trawls, so
# 'effort' here (site_counts_mwtr) needs further refinement...
site_counts_mwtr <- dt2 %>% 
  filter(MethodCode == "MWTR") %>% 
  group_by(Region = RegionCode, 
           Year = year(SampleDate),
           Season = get_season(SampleDate)) %>% 
  summarize(n = n())

## beach seine ----
site_counts_beach_seine <- seines %>% 
  group_by(region = region_code,
           year = year(date),
           season = get_season(date)) %>% 
  summarise(n = n())

## sturgeon -----
sites_counts_sturgeon <- sturg %>% 
  group_by(station = station,
           year = year,
           survey = survey) %>% 
  summarise(n = n())

# plots -----
## MWTR -----
# look at seasonal sample coverage for DJFMP MWTR data
sites_mwtr_plot <- site_counts_mwtr %>% 
  ggplot(aes(x = Year, y = Region)) + 
  geom_tile(aes(fill = n)) +
  scale_fill_gradient(low = "white", high = "red") +
  geom_text(aes(label = round(n, 1)), size = 4) +
  ggtitle("Midwater Trawl Pelagic Fish Sampling By Season") +
  facet_grid(Season ~ .) +
  theme_classic() +
  theme(legend.position = "none")

sites_mwtr_plot

ggsave("figures/mwtr_seasonal_sampling_coverage.jpg", 
       sites_mwtr_plot,
       width = 14, height = 8)

## beach seine ----
# seasonal sample coverage for DJFMP beach seine data
sites_seine_plot <- site_counts_beach_seine %>% 
  ggplot(aes(x = year, y = region)) + 
  geom_tile(aes(fill = n)) +
  scale_fill_gradient(low = "white", high = "red") +
  geom_text(aes(label = round(n, 1)), size = 4) +
  ggtitle("Beach Seine Fish Sampling By Season") +
  facet_grid(season ~ .) +
  theme_classic() +
  theme(legend.position = "none")

sites_seine_plot

ggsave("figures/sites_seine_plot.jpg", 
       sites_seine_plot,
       width = 14, height = 8)

## sturgeon ----
# coverage for wht sturgeon data;
# no season information & subbed survey
survey_sturgeon_plot <- sites_counts_sturgeon %>% 
  ggplot(aes(x = year, y = station)) + 
  geom_tile(aes(fill = n)) +
  scale_fill_gradient(low = "white", high = "red") +
  geom_text(aes(label = round(n, 1)), size = 4) +
  ggtitle("White Sturgeon Sampling By Survey") +
  facet_grid(survey ~ .) +
  theme_classic() +
  theme(legend.position = "none")

survey_sturgeon_plot

ggsave("figures/survey_sturgeon_plot.jpg", 
       survey_sturgeon_plot,
       width = 14, height = 8)

# original (clam) code -----
#summarize count of samples in each region by season by years
site_counts <- clams_sites4 %>% 
  group_by(Region, Year, Season) %>% 
  summarize(n = n())

#look at seasonal sample coverage fo EMP clam data
sites_plot <- site_counts %>% 
  ggplot(aes(x = Year, y = Region)) + 
  geom_tile(aes(fill = n)) +
  scale_fill_gradient(low = "white", high = "red") +
  geom_text(aes(label = round(n, 1)), size = 4) +
  ggtitle("EMP Clam Sampling By Season") +
  facet_grid(Season ~ .) +
  theme_classic() +
  theme(legend.position = "none")
sites_plot
ggsave("clam_seasonal_sampling_coverage.png", 
       sites_plot,
       width=14, height=8)
