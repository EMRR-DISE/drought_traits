# Special Studies Drought Synthesis
# Purpose: Update zooplankton taxonomy for non-rare taxa, and search for
  # synonyms from the WoRMS database
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

# Taxonomy last updated 4/26/2023

# Load packages
library(tidyverse)
library(worrms)
library(here)
library(conflicted)

# Declare package conflict preferences 
conflicts_prefer(dplyr::filter())

# Check if we are in the correct working directory
i_am("Zooplankton/zoop_taxonomy_update.R")

# Import zooplankton taxonomic names from Table L and format for worms query
zoop_taxa <- read_csv(here("Zooplankton/ZoopTableL.csv")) %>% 
  names() %>% 
  str_subset("Year", negate = TRUE) %>% 
  str_remove("_UnID$")

# Use worrms package to check accepted names and update higher taxonomy
df_zoop_taxa_records <- 
  wm_records_names(zoop_taxa, fuzzy = FALSE, marine_only = FALSE) %>% 
  list_rbind() %>% 
  distinct()

# One name is not in df_zoop_taxa_update
missing_taxa <- zoop_taxa[!(zoop_taxa %in% unique(df_zoop_taxa_records$scientificname))]

# Use the TAXAMATCH algorithm to search for this missing name
df_zoop_taxamatch <- 
  wm_records_taxamatch(missing_taxa) %>% 
  list_rbind() %>% 
  distinct()

# Combine taxa tables
df_zoop_taxa_all <- bind_rows(df_zoop_taxa_records, df_zoop_taxamatch)

# One name from zoop_taxa is corrected in the WoRMS database
df_zoop_taxa_all %>% 
  distinct(scientificname, valid_name, status) %>% 
  filter(scientificname != valid_name)

# Redo search for 1 corrected species because classification is for old species
df_zoop_taxa_correct <- df_zoop_taxa_all %>% 
  filter(scientificname != valid_name) %>% 
  pull(valid_name) %>% 
  wm_records_name(fuzzy = FALSE)

# Add info for corrected species and format for export
df_zoop_taxa_f <- df_zoop_taxa_all %>% 
  filter(scientificname == valid_name) %>% 
  bind_rows(df_zoop_taxa_correct) %>% 
  # just keep the needed columns
  select(
    AphiaID = valid_AphiaID,
    Taxon = valid_name,
    status,
    rank,
    kingdom,
    phylum,
    class,
    order,
    family,
    genus
  ) %>% 
  rename_with(str_to_title, matches("^[[:lower:]].+", ignore.case = FALSE)) %>% 
  # add source column
  mutate(Source = "worms")

# Export updated taxonomic info for zooplankton
write_csv(df_zoop_taxa_f, here("Zooplankton/zoop_taxonomy.csv"))

# Search for all synonyms for the zooplankton taxa on WoRMS
df_zoop_taxa_syn <- wm_synonyms_(df_zoop_taxa_f$AphiaID)
# 3 indicate "no content", which means those taxa have no synonyms

# Clean up synonym data set
df_zoop_taxa_syn_c <- df_zoop_taxa_syn %>% 
  # just keep the needed columns
  select(
    AphiaID = valid_AphiaID,
    Taxon = valid_name,
    AphiaID_Syn = AphiaID,
    Synonym = scientificname,
    status,
    rank,
    kingdom,
    phylum,
    class,
    order,
    family,
    genus
  ) %>% 
  rename_with(str_to_title, matches("^[[:lower:]].+", ignore.case = FALSE)) %>% 
  # add source column
  mutate(Source = "worms")

# Export synonym info for zooplankton
write_csv(df_zoop_taxa_syn_c, here("Zooplankton/zoop_synonyms.csv"))

