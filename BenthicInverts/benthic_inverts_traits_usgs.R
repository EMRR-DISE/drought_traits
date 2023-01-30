#A Database of Lotic Invertebrate Traits for North America
#https://pubs.usgs.gov/ds/ds187/

#to do list
#read through metadata to decide which columns of database are useful to me (most probably aren't)
#then cut down the database before looking for traits for my taxa
#should look up synonyms for my target taxa, and maybe also outdated families

#load packages
library(tidyverse)
library(janitor)

#read in USGS data
traits <- read_tsv("https://pubs.usgs.gov/ds/ds187/htodcs/InvertTraitsTable_v1.txt") %>% 
  clean_names()

citations <- read_tsv("https://pubs.usgs.gov/ds/ds187/htodcs/InvertTraitsCitations_v1.txt")

metadata <- read_tsv("https://pubs.usgs.gov/ds/ds187/htodcs/InvertTraitsFields_v1.txt")

#save a copy of the online files 
#write_csv(traits, "./BenthicInverts/usgs_trait_database/InvertTraitsTable_v1.csv")
#write_csv(metadata, "./BenthicInverts/usgs_trait_database/InvertTraitsFields_v1.csv")
#write_csv(citations, "./BenthicInverts/usgs_trait_database/InvertTraitsCitations_v1.csv")

#read in taxonomy info for my target taxa
target <- read_csv("./BenthicInverts/benthic_inverts_taxa_common_5_updated_2023-01-26.csv")

#explore target taxa-----------------

#how many cases of multiple species per genus?
target_cong <- target %>% 
  group_by(genus) %>% 
  summarize(count=n()) %>% 
  arrange(-count) %>% 
  filter(!is.na(genus) & count>1)
#6 genera; one with three species, the rest with two


#species level matches---------------------- 
#this is a quick check, there could be matches that didn't join because of 
#subtle differences in spelling or synonyms
#what will match: exact species names, exact family and higher names
#what won't match: genera because USGS puts "spp." after the genus name in this column
#can still match theirs to mine using the genus column (see step below)
match_sp <- target %>% 
  select(organism_code,taxon) %>% 
  left_join(traits) %>% 
  add_column(taxon_level = "species") %>% 
  relocate(taxon_level, .after = organism_code) %>% 
  #remove non-matching taxa
  filter(!is.na(trait_record_id))
#31 matches 

#create df with just the organism code and trait record
#will be used to filter out duplicates in genus df
match_sp_f <- match_sp %>% 
  select(organism_code,trait_record_id)

#are there any missing trait record IDs? 
#seems like there shouldn't be
trait_rid_na <- traits %>% 
  filter(is.na(trait_record_id))
#as expected, no missing record IDs
#so reasonable column for filtering out non-matches with my target taxa

#look at how many records by species
match_sp_ct <- match_sp %>% 
  group_by(taxon) %>% 
  summarise(records = n()) %>% 
  arrange(-records)
#11 species


#genus level matches---------------------- 
match_gn <- target %>% 
  select(organism_code,genus) %>% 
  filter(!is.na(genus)) %>% 
  left_join(traits) %>% 
  add_column(taxon_level = "genus") %>% 
  relocate(taxon_level, .after = organism_code) %>% 
  #remove non-matching taxa
  filter(!is.na(trait_record_id))
#125 genus level matches so quite a bit better than species level matches

#remove duplicates already present in the species level df
match_gn_remain <- match_gn %>% 
  anti_join(match_sp_f)
#looks like it worked; removed 31 rows as expected based on species df

#create df with just the organism code and trait record
#will be used to filter out duplicates in family df
#note that all the species level matches are baked in here too which is good
match_gn_f <- match_gn %>% 
  select(organism_code,trait_record_id)

#look at how many records by genus
match_gn_ct <- match_gn %>% 
  group_by (genus) %>% 
  summarize(records = n()) %>% 
  arrange(-records)
#20 genera; most hits for chironomids, amphipods, annelids

#family level matches---------------
match_fm <- target %>% 
  select(organism_code,family) %>% 
  filter(!is.na(family)) %>% 
  left_join(traits) %>% 
  add_column(taxon_level = "family") %>% 
  relocate(taxon_level, .after = organism_code) %>% 
  filter(!is.na(trait_record_id))
#1807 family level matches 

#remove duplicates already present in the species and genus level dfs
match_fm_remain <- match_fm %>% 
  anti_join(match_gn_f)
#I was expecting 1807 -  125 = 1682 but instead got more (1742)
#could be difference between my classification and theirs on which 
#genera are in which family
#compare genus and family matching below in combined data set

#look at how many records by family
match_fm_ct <- match_fm %>% 
  group_by (family) %>% 
  summarize(records = n()) %>% 
  arrange(-records)
#14 families; most records for annelids and chironomids

#combine the sp, genus, and family level dfs
target_taxa_traits <- bind_rows(match_sp,match_gn_remain,match_fm_remain) %>% 
  remove_empty(which="cols") %>% 
  rename(lit_genus = genus
         , lit_family = family
         , lit_taxon_name = taxon
         , lit_taxon_level = taxon_level
         )
#drops 9 empty columns

#double check for duplicate combos of organism code and trait id
#should be same number of rows as original df
ttt_dup <- target_taxa_traits %>% 
  distinct(organism_code,trait_record_id)
#1867, which is what I expect (ie, no duplicated combos)

#create version of target taxon taxonomy to add back in
target_sub <- target %>% 
  select(organism_code
         , target_taxon_name = taxon
         , target_taxon_level = rank
         , target_genus = genus
         , target_family = family
  )

#add target taxa info with trait df; should join by organism code
ttt_exp <- left_join(target_taxa_traits, target_sub) %>% 
  #add column to determine if family matches between mine and theirs
  mutate(family_comp = if_else(target_family == lit_family, 1, 0)) %>% 
  relocate(c(target_taxon_name:target_family,family_comp),.after = organism_code)

#look closer at non-matching family names
ttt_fam <- ttt_exp %>% 
  filter(family_comp==0) %>% 
  distinct(target_family,lit_family) %>% 
  arrange(target_family)
#there are 6 cases in which the family I recently got from WoRMS
#doesn't match the one from the USGS database; last modified 2016

#also look at matching family names
#look closer at non-matching family names
ttt_famc <- ttt_exp %>% 
  filter(family_comp==1) %>% 
  distinct(target_family,lit_family) %>% 
  arrange(target_family)
#14 family matches

#extract selected traits for target taxa-----------
#start with body size
#next look at the following: salin_fresh, salin_brackish, salin_salt
#thermal_pref, min_temp_reported, max_temp_reported,
# thermal_comments, turbidity
#note: max_themal_temp was dropped earlier because all NA

trait_size <- ttt_exp %>%
  select(organism_code:study_longitude
         , measured_length
  ) %>% 
  #filter any row with NA for all traits
  #filter(if_any(c(max_body_size:body_shape_case), complete.cases))
  filter(!is.na(measured_length)) %>% 
  arrange(organism_code)
#98 observations total, including genus and family level size estimates

#look closer at body size data
trait_size_sp <- trait_size %>% 
  filter(lit_taxon_level=="species")
#only 6 species level size estimates
#could just keep the largest value for each species
#note: we don't know if these measurements are for adults (likely not)

trait_size_sp_u <- trait_size_sp %>% 
  distinct(target_taxon_name)
#only four species


trait_temp_mx <- ttt_exp %>%
  select(organism_code:study_longitude
         , max_temp_reported
  ) %>% 
  #filter any row with NA for all traits
  #filter(if_any(c(max_body_size:body_shape_case), complete.cases))
  filter(!is.na(max_temp_reported)) %>% 
  arrange(organism_code)
#15 observations total, including genus and family level estimates

#look closer at body size data
trait_temp_mx_sp <- trait_temp_mx %>% 
  filter(lit_taxon_level=="species")
#only 1 species level size estimates




#write file containing the trait data for our target taxa-----

#write_csv()





