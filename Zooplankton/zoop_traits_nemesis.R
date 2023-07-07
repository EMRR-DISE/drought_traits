# Special Studies Drought Synthesis
# Purpose: Pull zooplankton trait data from nemesis website
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

# Load packages
library(tidyverse)
library(rvest)
library(glue)
library(here)
library(conflicted)

# Declare package conflict preferences 
conflicts_prefer(dplyr::filter())

# Check if we are in the correct working directory
i_am("Zooplankton/zoop_traits_nemesis.R")

# Import csv file with our target zooplankton taxa
target_zoop <- read_csv(here("Zooplankton/zoop_taxonomy.csv"))

# Add unique nemesis species numbers for our target taxa
target_zoop_c <- target_zoop %>% 
  transmute(
    target_taxon_name = Taxon,
    target_taxon_level = Rank,
    nemesis_sp_num = case_match(
      target_taxon_name,
      "Acartiella sinensis" ~ "-212",
      "Daphnia" ~ "-210",
      "Eurytemora affinis affinis" ~ "85863",
      "Hyperacanthomysis longirostris" ~ "682631",
      "Pseudodiaptomus forbesi" ~ "-218",
      "Sinocalanus doerrii" ~ "-222",
      "Tortanus" ~ "-224",
      .default = NA_character_
    )
  )

# Scrape trait information from the third table each species' webpage
df_traits <- target_zoop_c %>% 
  # Remove taxa without records in nemesis
  drop_na(nemesis_sp_num) %>% 
  mutate(
    html = map(nemesis_sp_num, ~ read_html(glue("https://invasions.si.edu/nemesis/species_summary/{.x}"))),
    lit_taxon_name = map_chr(html, ~ html_element(.x, "h1") %>% html_text()),
    trait_tbl = map(html, ~ html_elements(.x, "table") %>% chuck(3) %>% html_table())
  ) %>% 
  select(-c(nemesis_sp_num, html)) %>% 
  unnest(trait_tbl) %>% 
  # Rename the columns from the third table
  rename(
    trait_group = X1,
    trait_value = X2,
    citation = X3
  )

# Keep just the desired traits and format for export
df_traits_f <- df_traits %>% 
  # Remove trait values equal to "None"
  filter(trait_value != "None") %>% 
  # Keep the Temperature, Salinity, and Length traits, but not related to reproduction
  mutate(
    trait_group = case_match(
      trait_group,
      "Maximum Length (mm)" ~ "maximum body length",
      "Maximum Salinity (‰)" ~ "salinity maximum",
      "Minimum Salinity (‰)" ~ "salinity minimum",
      "Maximum Temperature (ºC)" ~ "thermal maximum",
      "Minimum Temperature (ºC)" ~ "thermal minimum",
      .default = NA_character_
    )
  ) %>% 
  filter(!is.na(trait_group)) %>% 
  # Convert trait values to numeric and add columns for trait units,
  # lit_taxon_level, and lit_taxon_type
  mutate(
    trait_value = as.numeric(trait_value),
    trait_unit = case_when(
      str_detect(trait_group, "length$") ~ "mm",
      str_detect(trait_group, "^salinity") ~ "PSU",
      str_detect(trait_group, "^thermal") ~ "degrees C"
    ),
    lit_taxon_level = "Species",
    lit_taxon_type = case_when(
      lit_taxon_name == target_taxon_name ~ "target",
      target_taxon_level == "Genus" ~ "species within genus",
      TRUE ~ "congener"
    ),
    # Add back "_UnID" to the target taxa names to be consistent with other
    # trait data
    target_taxon_name = if_else(
      target_taxon_level == "Genus", 
      paste0(target_taxon_name, "_UnID"),
      target_taxon_name
    )
  ) %>% 
  # Rearrange columns
  select(
    starts_with("target"),
    starts_with("lit"),
    citation,
    starts_with("trait")
  ) %>% 
  arrange(target_taxon_name)

# Export trait data from nemesis
df_traits_f %>% write_csv(here("Zooplankton/zoop_traits_nemesis.csv"), na = "")

