# Special Studies Drought Synthesis
# Purpose: Pull zooplankton trait data from WoRMS online database
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

# Load packages
library(tidyverse)
library(worrms)
library(here)
library(conflicted)

# Declare package conflict preferences 
conflicts_prefer(dplyr::filter())

# Check if we are in the correct working directory
i_am("Zooplankton/zoop_traits_worms.R")

# Import csv file with our target zooplankton taxa
target_zoop <- read_csv(here("Zooplankton/zoop_taxonomy.csv"))

# Create dataframe of AphiaID's and associated info for zooplankton taxa to be
# used in WoRMS search
target_zoop_c <- target_zoop %>% 
  drop_na(AphiaID) %>% 
  distinct(AphiaID, Taxon, Rank, Genus)

# Run AphiaID's from target_zoop through the function that grabs the trait data
# from WoRMS
zoop_traits <- target_zoop_c %>% 
  pull(AphiaID) %>% 
  wm_attr_data_(id = .)
# No data for: 149771, 104108, 148370, 234062, 157683, 148379

# Add taxonomic info to trait data
zoop_traits_c <- zoop_traits %>% 
  mutate(AphiaID = as.numeric(AphiaID)) %>% 
  left_join(target_zoop_c %>% select(-Genus), by = join_by(AphiaID)) %>% 
  rename(
    target_aphia_id = AphiaID,
    target_taxon_name = Taxon,
    target_taxon_level = Rank
  ) %>% 
  relocate(target_taxon_name, target_taxon_level, .after = target_aphia_id) %>% 
  # Duplicate target taxon info as the literature taxon info, since they were
  # exact matches
  mutate(
    lit_aphia_id = target_aphia_id,
    lit_taxon_name = target_taxon_name,
    lit_taxon_level = target_taxon_level,
    lit_taxon_type = "target",
    .after = target_taxon_level
  )

# Look at taxa without data
zoop_nodata <- target_zoop_c %>% filter(!AphiaID %in% unique(zoop_traits_c$target_aphia_id))

# For genus level taxa, search all species level taxa within genus for traits
zoop_nodata_gen_allsp <- zoop_nodata %>% 
  filter(Rank == "Genus") %>%
  drop_na(AphiaID) %>% 
  distinct(AphiaID) %>% 
  pull(AphiaID) %>% 
  wm_children_(id = ., marine_only = FALSE)

# Create dataframe with valid_AphiaID's and associated info from
  # zoop_nodata_gen_allsp for trait search
nodata_gen_allsp_valid <- zoop_nodata_gen_allsp %>% 
  drop_na(valid_AphiaID) %>% 
  distinct(id, valid_AphiaID, valid_name, rank) 
  
# First search for traits using valid_AphiaID provided by wm_children_ search
zoop_traits_gen_valid <- nodata_gen_allsp_valid %>% 
  pull(valid_AphiaID) %>% 
  wm_attr_data_(id = .)

# Add taxonomic info to trait data
zoop_traits_gen_valid_c <- zoop_traits_gen_valid %>% 
  mutate(AphiaID = as.numeric(AphiaID)) %>% 
  select(-id) %>% 
  left_join(nodata_gen_allsp_valid, by = join_by(AphiaID == valid_AphiaID)) %>%
  # Define AphiaID, name, and rank as literature derived since they aren't the
  # exact target taxa
  rename(
    lit_aphia_id = AphiaID,
    lit_taxon_name = valid_name,
    lit_taxon_level = rank
  ) %>% 
  mutate(lit_taxon_type = "species within genus") %>% 
  relocate(lit_taxon_name, lit_taxon_level, lit_taxon_type, .after = lit_aphia_id) %>%
  mutate(id = as.numeric(id)) %>% 
  # Add target taxa information joining with the id column
  left_join(target_zoop_c %>% select(-Genus), by = join_by(id == AphiaID)) %>% 
  # Define AphiaID, name, and rank as the target taxa
  rename(
    target_aphia_id = id,
    target_taxon_name = Taxon,
    target_taxon_level = Rank
  ) %>%
  relocate(target_aphia_id, target_taxon_name, target_taxon_level)
  
# Look for valid_AphiaIDs without traits and search using their unaccepted AphiaIDs
zoop_traits_gen_notvalid <- zoop_nodata_gen_allsp %>% 
  filter(valid_AphiaID != AphiaID | is.na(valid_AphiaID)) %>% 
  filter(!valid_AphiaID %in% unique(zoop_traits_gen_valid_c$lit_aphia_id)) %>% 
  drop_na(AphiaID) %>% 
  distinct(AphiaID) %>% 
  pull(AphiaID) %>% 
  wm_attr_data_(id = .)
# No data

# For species level taxa, search congeners for traits
zoop_nodata_sp_cong <- zoop_nodata %>% 
  filter(Rank == "Species") %>% 
  drop_na(Genus) %>% 
  distinct(Genus) %>% 
  pull(Genus) %>% 
  wm_children_(name = ., marine_only = FALSE)

# Create dataframe with valid_AphiaID's and associated info from
  # zoop_nodata_sp_cong for trait search
nodata_sp_cong_valid <- zoop_nodata_sp_cong %>% 
  drop_na(valid_AphiaID) %>% 
  distinct(id, valid_AphiaID, valid_name, rank) 

# First search for traits using valid_AphiaID provided by wm_children_ search
zoop_traits_sp_cong_valid <- nodata_sp_cong_valid %>% 
  pull(valid_AphiaID) %>% 
  wm_attr_data_(id = .)
# No data

# Next search for traits using unaccepted AphiaID provided by wm_children_ search
zoop_traits_sp_cong_notvalid <- zoop_nodata_sp_cong %>% 
  filter(valid_AphiaID != AphiaID) %>%
  drop_na(AphiaID) %>% 
  distinct(AphiaID) %>% 
  pull(AphiaID) %>% 
  wm_attr_data_(id = .)
# No data

# Again, look at taxa without data
zoop_nodata2 <- target_zoop_c %>% 
  filter(!AphiaID %in% unique(c(zoop_traits_c$target_aphia_id, zoop_traits_gen_valid_c$target_aphia_id)))
# 3 taxa left, we'll look at their synonyms

# Import csv file with synonyms for our target zooplankton taxa
target_zoop_syn <- read_csv(here("Zooplankton/zoop_synonyms.csv"))

# Search for traits of synonyms of three taxa without data
zoop_traits_syn <- target_zoop_syn %>% 
  filter(AphiaID %in% unique(zoop_nodata2$AphiaID)) %>% 
  drop_na(AphiaID_Syn) %>% 
  distinct(AphiaID_Syn) %>% 
  pull(AphiaID_Syn) %>% 
  wm_attr_data_(id = .)
# No data for synonyms

# Combine trait data for genus-level search to all other trait data
zoop_traits_all <- bind_rows(zoop_traits_c, zoop_traits_gen_valid_c)

# Look at the type of trait data in resulting output dataframe
zoop_traits_all %>% count(measurementTypeID, measurementType)

# Almost all records are for Body size (TypeID = 15), so we'll focus on those
zoop_bsize <- zoop_traits_all %>% 
  filter(measurementTypeID == 15) %>% 
  # drop unnecessary columns
  select(-c(source_id, AphiaID_Inherited, CategoryID)) %>% 
  # change the id column to row number so each row is unique, which
  # helps with the unnesting function below
  mutate(id = row_number())

# Create function to extract measurements from the children list column and
  # restructure them for iterative extraction
# The df_nxt argument is a dummy variable to allow for the use of accumulate() on
  # one input at a time
extract_child <- function(df, df_nxt) {
  df %>% 
    # using hoist() since most of the elements in each children list column are duplicates
    hoist(
      children,
      measurementType_2 = "measurementType",
      measurementValue_2 = "measurementValue",
      children_2 = "children"
    ) %>% 
    pivot_wider(
      names_from = measurementType_2,
      values_from = measurementValue_2,
      names_repair = "unique"
    ) %>% 
    select(-children) %>% 
    rename(children = children_2) %>% 
    unnest(children)
}

# Unnest the children list column 6 times since that is the furthest level of
  # nesting, saving each intermediate result because records are lost along the way
zoop_bsize_un <-
  accumulate(1:6, extract_child, .init = zoop_bsize) %>% 
  # remove the children columns from all elements so left_join works correctly
  map(~ select(.x, -children)) %>%
  # left join all list elements together
  reduce(left_join)

# Clean up traits data set
zoop_bsize_c1 <- zoop_bsize_un %>% 
  mutate(
    life_stage = if_else(!is.na(`Life stage`), `Life stage`, `Life stage...20`),
    trait_value = as.numeric(measurementValue)
  ) %>% 
  # Only keep maximum lengths for adults
  filter(
    life_stage == "adult",
    Type == "maximum",
    Dimension == "length"
  ) %>% 
  select(
    starts_with("target_"),
    starts_with("lit_"),
    citation = reference,
    trait_group = measurementType,
    trait_value,
    trait_unit = Unit,
    Gender
  )

# Add taxa without trait data in WoRMS to traits data set to be explicit about
  # taxa with missing data - 4 taxa with missing traits
zoop_bsize_missing <- target_zoop_c %>% 
  filter(!AphiaID %in% unique(zoop_bsize_c1$target_aphia_id)) %>% 
  select(
    target_aphia_id = AphiaID,
    target_taxon_name = Taxon,
    target_taxon_level = Rank
  )

zoop_bsize_f <- bind_rows(zoop_bsize_c1, zoop_bsize_missing) %>% 
  mutate(
    # Combine information in life_stage, Type, and Dimension columns
    trait_group = "maximum adult body length",
    # Add back "_UnID" to the target taxa names to be consistent with other
    # trait data
    target_taxon_name = if_else(
      target_taxon_level == "Genus", 
      paste0(target_taxon_name, "_UnID"),
      target_taxon_name
    )
  ) %>% 
  arrange(target_taxon_name)

# Export trait data from WoRMS
zoop_bsize_f %>% write_csv(here("Zooplankton/zoop_traits_worms_size.csv"), na = "")

