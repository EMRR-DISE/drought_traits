#Special Studies Drought Synthesis
#Environmental Monitoring Program data
#Benthic Invertebrate Survey data
#Automating taxonomy updating for non-rare taxa
#ie, those in at least 5% of samples for three focal stations

#taxonomy last updated 3/27/2023

#Nick Rasmussen, nicholas.rasmussen@water.ca.gov

# Load required packages -----------------
#installed worms, worrms, ritis, and taxize from github
#these versions seems to work better, particularly worms

#library(worms) 
#searches WoRMS online database; uses plyr which conflicts with tidyverse; load only with functions
library(janitor) #used to quickly clean up column names
library(taxize) #updating taxonomy using a suite of online databases
library(tidyverse) #suite of data science tools
#library(worrms) #didn't work well; updating taxonomy using worms database
#library(ritis) #didn't work well; searches ITIS online database
#ultimately, I used worms for WoRMS and taxize for ITIS
#note: don't load 'worms' here (see code below) because 
#dependencies don't play nice with tidyverse

# Read in the data-----------------------

#read in organism key list published on EDI
#https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=1036&revision=2
benthic_spp <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.1036.2&entityid=d0f0dd3c1835fe5b669342f8c8e77024") %>% 
  clean_names()

#read in list of organism codes for all taxa found in at least 5% of samples across all three stations (n=64)
common5 <- read_csv("./benthic/data_output/benthic_common5_codes_2023-01-25.csv") %>%
  #convert from data frame back to a vector of organism codes
  pull(organism_code)

#read in list of organism codes for all taxa found in at least 10% of samples in each of three stations
#very similar to common5 list; adds three new taxa and drop one of the 5% taxa (n =)
common10 <- read_csv("./benthic/data_output/benthic_common10_prop_stations.csv") %>%
  #convert from data frame back to a vector of organism codes
  pull(organism_code)

#Clean up organism key dataframe-------------------

benthic_spp_format <- benthic_spp %>% 
  #rename some columns
  rename(class = class_level
         ,order = order_level
         ,family = family_level
         ,genus1 = genus
         ,species1 = species
  ) %>%
  #look for cases of Unknown as unknown
  #just one present in dataframe
  #filter(if_any(class:species, ~ . == 'unknown')) %>% 
  mutate(
    #change one case of "unknown" to "Unknown"
    across(c(class:species1), ~as.character(gsub("unknown", "Unknown", .)))
    #change Mermithid to Unknown and then sp. A to Mermithid sp. A
    ,genus = case_when(genus1 == "Mermithid" ~ "Unknown", TRUE ~ genus1)
    ,species = case_when(organism_code == 1290 ~ "mermithid sp. A"
                         #note that some of higher taxonomy for this parastic isopod is wrong (eg, Annelida)
                         #confirmed that the familiy is correct at least, which is used in name correction below
                         ,organism_code == 4337 ~ "bopyrid sp. A" 
                         ,organism_code == 4504 ~ "podocerid sp. A"
                         ,organism_code == 1200 ~ "nematode sp."
                         #change Synidotea laevidorsalis to S. laticauda (following CALNEMO)
                         ,organism_code ==4310 ~ "laticauda"
                         #change Sinelobus sp. A to S. standfordi
                         #CALNEMO says it's S. cf. stanfordi so use S. stanfordi traits with caution
                         ,organism_code == 4270 ~ "stanfordi"
                         , TRUE ~ species1)
  ) %>% 
  select(organism_code:family,genus:species, common_name)

#Determine how many taxa in whole data set are IDed to species vs morphospecies--------------------------
#note that in some cases there are names in the genus column that aren't genera
#"Nudibranch" just describes the order but is in the genus column

#focus on "species" column and filter by whether "sp." character string is present
benthic_spp_morpho <- benthic_spp_format %>% 
  #NOTE: need to include the space after "sp." or it filters out species names that include "sp" for some reason
  filter(grepl(c('sp. |Sp. |Unknown'), species)) %>% 
  arrange(phylum,class, order,family,genus,species)
#202 of 478 spp are morphospecies; 43% of all species

#Taxa IDed to species
benthic_spp_true <- benthic_spp_format %>% 
  filter(!grepl('sp. |Sp. |Unknown|No catch',species))%>% 
  arrange(phylum,class, order,family,genus,species)
#275 of 478 are IDed to species; 57% of all species


#add column to taxonomy data set that combines genus and specific epithet--------
#this is tricky because many taxa are morphospecies

#nearly all issues with species names can be resolved by dropping "Unknown "
#a few others needed customized solutions
# Making data frame with existing strings and their replacement
#there is an isopod with just "sp. A" in species column can't just find and replace though 
#because there are lots of others that also contain "sp. A"
tr <- data.frame(target = c("Unknown ","No catch No catch"),
                 replacement = c("", "No catch"))

# Making the named replacement vector from tr
replacements <- c(tr$replacement)
names(replacements) <- c(tr$target)

benthic_spp_names <- benthic_spp_format %>% 
  #concatonate genus and species columns and separate them with a space
  #also keep original two columns
  unite("species_name1",genus:species,sep=" ",remove=F) %>%  
  #drop "Unknown" from species_name strings
  #a few taxa need species names edited individually to make sense
  mutate('species_name' = str_replace_all(species_name1,pattern = replacements)) %>% 
  select(organism_code:family,genus,species,species_name,common_name) %>% 
  arrange(phylum,class, order,family,genus,species)

#write file for Leela to use
#note that file path is outdated
#write_csv(benthic_spp_names,"./BenthicInverts/nmds/BenthicInverts_Taxonomy_NameLabels.csv")

#Determine how many non-rare taxa are IDed to species vs morphospecies--------------------------
#after removing species that are in fewer than 5% or 10% of samples

#5%: filter the taxonomy dataset using organism codes from data set with only taxa that are in at least 5% of samples
benthic_spp_names_5 <-benthic_spp_names %>% 
  #filter to keep only taxa in at least 5% of samples using organism codes
  filter(organism_code %in% common5) 
#64 taxa as expected

#5%: focus on "species" column and filter by whether "sp." character string is present
benthic_spp_5_morpho <- benthic_spp_names_5 %>% 
  #NOTE: need to include the space after "sp." or it filters out species names that include "sp" for some reason
  filter(grepl(c('sp. |Sp. |Unknown'), species))
#13 of 64 spp are morphospecies; 22% of all species
#all of these are IDed to genus except three (one subfamily, one class, one family)

#5%: Taxa IDed to species
benthic_spp_5_true <- benthic_spp_names_5 %>% 
  filter(!grepl('sp. |Sp. |Unknown|No catch',species))
#51 of 64 are IDed to species; 78% of all species

#NOTE: would need to read in a csv with list of taxa in at least 10% of sample for the code below to run
#10%: filter the taxonomy dataset using organism codes from data set with only taxa that are in at least 10% of samples
benthic_spp_names_10 <-benthic_spp_names %>%
  #filter to keep only taxa in at least 10% of samples using organism codes
  filter(organism_code %in% common10)
#n=66 as expected

#10%: focus on "species" column and filter by whether "sp." character string is present
benthic_spp_10_morpho <- benthic_spp_names_10 %>% 
  #NOTE: need to include the space after "sp." or it filters out species names that include "sp" for some reason
 filter(grepl(c('sp. |Sp. |Unknown|unknown'), species))
#12 of 66 spp are morphospecies; 18% of all species
#10 of 12 are IDed to genus

#10%: Taxa IDed to species
benthic_spp_10_true <- benthic_spp_names_10 %>% 
  filter(!grepl('sp. |Sp. |Unknown|unknown|No catch',species))
#54 of 66 are IDed to species; 82% of all species

#automate taxonomy updates for 5% taxa: check WoRMS using worms package-------------
#likely the most updated taxonomy for benthic inverts
#for taxa not found in WoRMS will then check ITIS 
#previously ran this for 5% taxa; now running for 10% taxa by station

#create list of taxa IDed to species
specieslist <- benthic_spp_5_true %>% 
  pull(species_name)

#create list of taxa not IDed to species
#most are genus, a few are higher taxonomic levels
#across entire original taxonomy file, some taxa only IDed to phylum
#so wrote this code accordingly, even though it's overkill for my subset here
taxonlist <- benthic_spp_5_morpho %>% 
  mutate(taxon_lowest = case_when(genus!="Unknown" ~ genus
                                  ,family!="Unknown" ~ family
                                  ,order!="Unknown" ~ order
                                  ,class!="Unknown" ~ class
                                  ,TRUE ~ phylum
  ))

#make list of taxa not IDed to species
txlist <- taxonlist %>% 
  pull(taxon_lowest)

#make list with all taxa in it (those IDed to species and those not)
full_list <- c(specieslist,txlist)

#create df for all 64 taxa that contains both the organism code and the name searched in database---------

#for df with species level id, make new df with just organism_code and species_name
sp_code <- benthic_spp_5_true %>% 
  select(organism_code
         ,name = species_name)

#for df without species level id, make new df with just organism_code and taxon_lowest
all_code <- taxonlist %>% 
  select(organism_code
         ,name = taxon_lowest) %>% 
  #now add the df containing species level IDs
  bind_rows(sp_code)

#use worms package to check accepted names and update higher taxonomy
#github repo:https://github.com/janhoo/worms/
#documentation: https://www.rdocumentation.org/packages/worms/versions/0.2.2/topics/wormsbynames
library(worms)

#version of package from CRAN only matched 25 of 50 species
#wormsbyname failed to match many species I know are in database
#Github version worked better; matched 49 of 50 species
#ids = T means output will include search name
#match = T taxon_names that could not retrieved will be retried with wormsbymatchnames
#if not using match=T sometimes returns subspecies, varieties, etc instead of species
#but it may also return a similar taxon for a taxon truly not in worms 
#eg, finds Isocystis for Isocypris which is wrong

worms_records <- worms::wormsbynames(full_list, ids=T,match=T)

#IMPORTANT: the classification provided is for invalid names provided not the valid names
#need to rerun this using the valid names
#found 49 of 50 species; missing only Mooreobdella microstoma, which I confirmed is not in WoRMS
#found 14 of 15 higher level taxa; missing Isocypris, which I confirmed is not in WoRMS
#Note that using fuzzy matching (ie match=T) provides genus and species which I want 
#but it also provides an incorrect match for Isocypris (Isocystis)
#if I don't use fuzzy matching, I get names that usually include subspecies, forms, etc which I don't want
#Turbellaria was found but is not an accepted name; did not return an accepted name

#examine how well the search worked across all input taxa
worms_records_outcome <- worms_records %>% 
  #add column indicating whether exact match between input and output names
  mutate(outcome = case_when(
    #input name is accepted name
    name == valid_name ~ "accepted"
    #input name is not in online database
    ,is.na(scientificname) ~ "missing"
    #input name found and replaced with accepted name
    ,name == scientificname & scientificname!=valid_name ~ "corrected"
    #input name not found but a similar, and possibly incorrect, name was found
    #NOTE: I later realized there is a match_type column that indicates whether match is "exact" or "near_2"
    #,name!=scientificname & !is.na(scientificname) ~ "questionable"
    ,match_type!="exact" ~ "questionable"
    #input name found but isn't the accepted name
    ,name == scientificname & status!="accepted" ~ "old"
  )
           ) %>% 
  arrange(outcome)
#accepted = 56, corrected = 6, missing = 1, old = 1, questionable = 1
#need to redo search for corrected species because classification is for old species
#no need to redo search for accepted
#no point in redoing search for missing, old, or wrong

#create df that can match organism codes, input names, and AphiaIDs
tax_key1 <- worms_records_outcome %>% 
  select(name,valid_name) 
#join by name
tax_key <- left_join(tax_key1,all_code) %>% 
  #now drop name because name in worms_records_corrected is the valid name rather than the input name
  select(-name)

#create subset with just the taxa that were found in WoRMS and the names correctly updated
species_corrected <- worms_records_outcome %>% 
  #filter to just the corrected taxa
  filter(outcome == "corrected") %>% 
  pull(valid_name)
#six taxa as expected

#rerun the WoRMS search on the corrected species
worms_records_corrected <- worms::wormsbynames(species_corrected, ids=T,match=T) %>% 
  #not sure if I'll need this column but adding it back in just in case
  add_column(outcome="corrected")

#add organism codes to corrected species; will join by valid_name
corrected_codes <- left_join(worms_records_corrected,tax_key)

#add organism codes to main worms dataframe; will match by name
worms_codes <- left_join(all_code,worms_records_outcome)

#format the worms output
worms_format <- worms_codes %>%
  #drops outdated rows for corrected species
  filter(outcome!="corrected") %>% 
  #add rows with updated info for corrected species
  bind_rows(corrected_codes) %>%   
  #remove incorrectly matched info for Isocypris
  #need to include is.na() part so I don't accidentally drop Mooreobdella microstoma
  filter(scientificname != "Isocystis" | is.na(scientificname)) %>% 
  add_row(name = "Isocypris") %>% 
  #just keep the needed columns
  select(organism_code
         ,aphia_id = AphiaID
         ,taxon = name
         ,status
         ,rank
         ,kingdom:genus
  ) %>% 
  #add column indicating source
  add_column(source = "worms") %>% 
  arrange(status)
#everything now looks good with three exceptions
#Turbellaria which is an old name with no current replacement; ask Betsy
#two taxa not in WoRMS; will check ITIS next
#Melanoides tuberculata has temporary name with weird formatting including [] for order 

#take a closer look at non-matches
worms_unmatch <- worms_format %>% 
  filter(is.na(status))
#two taxa

#make list of taxa not matched by WoRMS
#will check these in ITIS
check_itis <- worms_format %>% 
  filter(is.na(kingdom)) %>% 
  pull(taxon)
#Note: format of output will be different than worms package

#automate taxonomy updates for 10% taxa by station: check WoRMS using worms package-------------
#likely the most updated taxonomy for benthic inverts
#for taxa not found in WoRMS will then check ITIS 
#previously ran this for 5% taxa; now running for 10% taxa by station

#create list of taxa IDed to species
specieslist10 <- benthic_spp_10_true %>% 
  pull(species_name)

#create list of taxa not IDed to species
#most are genus, a few are higher taxonomic levels
#across entire original taxonomy file, some taxa only IDed to phylum
#so wrote this code accordingly, even though it's overkill for my subset here
taxonlist10 <- benthic_spp_10_morpho %>% 
  mutate(taxon_lowest = case_when(genus!="Unknown" ~ genus
                                  ,family!="Unknown" ~ family
                                  ,order!="Unknown" ~ order
                                  ,class!="Unknown" ~ class
                                  ,TRUE ~ phylum
  ))

#make list of taxa not IDed to species
txlist10 <- taxonlist10 %>% 
  pull(taxon_lowest)

#make list with all taxa in it (those IDed to species and those not)
full_list10 <- c(specieslist10,txlist10)

#create df for all 66 taxa (for the 10% group) that contains both the organism code and the name searched in database---------

#for df with species level id, make new df with just organism_code and species_name
sp_code10 <- benthic_spp_10_true %>% 
  select(organism_code
         ,name = species_name)

#for df without species level id, make new df with just organism_code and taxon_lowest
all_code10 <- taxonlist10 %>% 
  select(organism_code
         ,name = taxon_lowest) %>% 
  #now add the df containing species level IDs
  bind_rows(sp_code10)

#use worms package to check accepted names and update higher taxonomy
#github repo:https://github.com/janhoo/worms/
#documentation: https://www.rdocumentation.org/packages/worms/versions/0.2.2/topics/wormsbynames
library(worms)

#version of package from CRAN only matched 25 of 50 species
#wormsbyname failed to match many species I know are in database
#Github version worked better; matched 49 of 50 species
#ids = T means output will include search name
#match = T taxon_names that could not retrieved will be retried with wormsbymatchnames
#if not using match=T sometimes returns subspecies, varieties, etc instead of species
#but it may also return a similar taxon for a taxon truly not in worms 
#eg, finds Isocystis for Isocypris which is wrong

worms_records10 <- worms::wormsbynames(full_list10, ids=T,match=T)

#IMPORTANT: the classification provided is for invalid names provided not the valid names
#need to rerun this using the valid names
#found 49 of 50 species; missing only Mooreobdella microstoma and Stylurus olivaceus, which I confirmed are not in WoRMS
#found 14 of 15 higher level taxa; missing Isocypris, which I confirmed is not in WoRMS
#Note that using fuzzy matching (ie match=T) provides genus and species which I want 
#but it also can provides an incorrect matches (eg, previously matched Isocypris as Isocystis)
#if I don't use fuzzy matching, I get names that usually include subspecies, forms, etc which I don't want
#Turbellaria was found but is not an accepted name; did not return an accepted name

#examine how well the search worked across all input taxa
worms_records_outcome10 <- worms_records10 %>% 
  #add column indicating whether exact match between input and output names
  mutate(outcome = case_when(
    #input name is accepted name
    name == valid_name ~ "accepted"
    #input name is not in online database
    ,is.na(scientificname) ~ "missing"
    #input name found and replaced with accepted name
    ,name == scientificname & scientificname!=valid_name ~ "corrected"
    #input name not found but a similar, and possibly incorrect, name was found
    #NOTE: I later realized there is a match_type column that indicates whether match is "exact" or "near_2"
    #,name!=scientificname & !is.na(scientificname) ~ "questionable"
    ,match_type!="exact" ~ "questionable"
    #input name found but isn't the accepted name
    ,name == scientificname & status!="accepted" ~ "old"
  )
  ) %>% 
  arrange(outcome)
#accepted = 57, corrected = 6, missing = 2, old = 1
#need to redo search for corrected species because classification is for old species
#no need to redo search for accepted
#no point in redoing search for missing, old, or wrong

#create df that can match organism codes, input names, and AphiaIDs
tax_key1_10 <- worms_records_outcome10 %>% 
  select(name,valid_name) 
#join by name
tax_key10 <- left_join(tax_key1_10,all_code10) %>% 
  #now drop name because name in worms_records_corrected is the valid name rather than the input name
  select(-name)

#create subset with just the taxa that were found in WoRMS and the names correctly updated
species_corrected10 <- worms_records_outcome10 %>% 
  #filter to just the corrected taxa
  filter(outcome == "corrected") %>% 
  pull(valid_name)
#six taxa as expected

#rerun the WoRMS search on the corrected species
worms_records_corrected10 <- worms::wormsbynames(species_corrected10, ids=T,match=T) %>% 
  #not sure if I'll need this column but adding it back in just in case
  add_column(outcome="corrected")

#add organism codes to corrected species; will join by valid_name
corrected_codes10 <- left_join(worms_records_corrected10,tax_key10)

#add organism codes to main worms dataframe; will match by name
worms_codes10 <- left_join(all_code10,worms_records_outcome10)

#format the worms output
worms_format10 <- worms_codes10 %>%
  #drops outdated rows for corrected species
  filter(outcome!="corrected") %>% 
  #add rows with updated info for corrected species
  bind_rows(corrected_codes10) %>%   
  #just keep the needed columns
  select(organism_code
         ,aphia_id = AphiaID
         ,taxon = name
         ,status
         ,rank
         ,kingdom:genus
  ) %>% 
  #add column indicating source
  add_column(source = "worms") %>% 
  arrange(status)
#everything now looks good with three exceptions
#Turbellaria which is an old name with no current replacement; ask Betsy
#two taxa not in WoRMS; will check ITIS next
#Melanoides tuberculata has temporary name with weird formatting including [] for order 

#take a closer look at non-matches
worms_unmatch10 <- worms_format10 %>% 
  filter(is.na(status))
#two taxa

#make list of taxa not matched by WoRMS
#will check these in ITIS
check_itis10 <- worms_format10 %>% 
  filter(is.na(kingdom)) %>% 
  pull(taxon)
#Note: format of output will be different than worms package

#automate taxonomy updates: check WoRMS using worrms package (poor results)--------------------
#Github repo: https://github.com/ropensci/worrms
#documentation: https://rdrr.io/cran/worrms/
#SUMMARY: functions in this package aren't working very well
#not producing many matches even in cases when I know it should
#package doesn't seem very well maintained

#function that locates records for individual species throws Error 400
#https://github.com/ropensci/worrms/issues/29
#if there are spaces in the character string (ie, between genus and species)
#replacing space with '+' fixes this 
#wm_records_name(name = c('Gammarus daiberi')) #Error: (400) Bad Request
#wm_records_name(name = c('Gammarus+daiberi')) #works as expected

#use sister function that works on multiple species names
#sort of works but doesn't match as many species as worms function
#replacing space with '+' didn't help
#records_worrms <- wm_records_names(name = specieslist)
#records_worrms_fzy <- wm_records_taxamatch(name = specieslist)
#wm_records_taxamatch was much slower and didn't produce better matching

#prints latin name corresponding to ID #
#wm_id2name(id = c(397131)) #single record version works
#wm_id2name_(id = c(397131,148669)) #multiple record version works

#print ID# from name
#get an error if spaces in name
#wm_name2id(name = "Lumbriculus variegatus") #Error 400
#wm_name2id(name = "Lumbriculus+variegatus") #works
#wm_name2id_(name = c("Lumbriculus+variegatus","Girardia+tigrina")) #surprisingly works

#automate taxonomy updates:check ITIS using RITIS package (poor results)-------------------
#Github repo: https://github.com/ropensci/ritis
#documentation: only detailed documentation is for old version
#https://www.rdocumentation.org/packages/ritis/versions/0.5.4
#SUMMARY: not very helpful because most functions only seems to work on one record at a time

#to query multiple scientific names, use terms()
#search_scientific() only works on one species at a time

#records_ritis <- terms(query=c(specieslist), "scientific")

#a bit slow but seems to work fine; found most of the species
#however, taxonomy not as up to date as WoRMS, unsurprisingly
#probably best to get as much info from WoRMS as possible
#then fill gaps with ITIS as needed

#just search ITIS for taxa not found in WoRMS search above

#records_ritis_gaps <- terms(query=c(check_itis), "scientific")

#works but output not everything I want; missing higher level taxonomy
#probably best approach is to now grab the TSNs and use function to get higher taxonomy
#for larger data sets, might need to add a step that checks for valid/accepted name for TSN
#also need to combine tibbles before we can create list of all TSNs

#this function will pull full taxonomy but only works for a single TSN
#hierarchy_full(tsn = 69450)

#try using taxize package instead because it should be more powerful 
#ie, functions to work with species lists


#automate taxonomy updates for 5% taxa group:check ITIS using taxize package------------------- 
#github repo: https://github.com/ropensci/taxize
#documentation: https://docs.ropensci.org/taxize/articles/taxize.html
#book: https://books.ropensci.org/taxize/

#determine if the names not found in WoRMS are considered accepted names in ITIS
#accepted = T only returns results for accepted names
#accepted = F returns both accepted and unaccepted names
tsn_gaps <- get_tsn(check_itis,accepted = T)
accept_gaps <- lapply(as.character(tsn_gaps),itis_acceptname)
#for both taxa, acceptedname = NA  meaning the input name is the accepted name

#search ITIS for taxa not in WoRMS
taxize_records_gaps <- classification(check_itis, db = 'itis')

#convert nested dataframe to one data frame and then combine with WoRMS data

#create vector of column names from WoRMS df
#will be used to filter columns from ITIS df
taxheader = names(worms_format)

#make tibbles for ITIS matches
itis_tibble <- taxize_records_gaps %>% 
  map(as_tibble)

#convert from nested dataframe to single dataframe
itis_format <- enframe(itis_tibble,name="name") %>% 
  mutate(
    #make a new vector that provides name of rank of taxon searched for (eg, genus or species)
    #uses the last() function to grab the most specific rank 
    rank = map_chr(value,~last(pull(.x,rank)))
    #convert from long to wide, drop the id column
    ,value = map(value,~pivot_wider(.x, id_cols=-id, names_from = rank,values_from = name) %>% 
                   #subset the columns to just those in the WoRMS dataframe
                   select(any_of(taxheader)) )
  ) %>% 
  #unnest the tibbles
  unnest(cols = value) %>% 
  #add column indicating status = accepted; manually confirmed above these two are accepted names
  #also add column indicating taxonomy source (ie, ITIS)
  add_column(status = "accepted"
             ,source = "itis")
#need to capitalize rank names

#add organism codes to itis records; match by name
itis_codes <- left_join(itis_format,all_code) %>% 
  rename(taxon = name)

#replace missing taxa info with ITIS info
all_format <- worms_format %>% 
  #drop the WoRMS rows filled with NAs
  anti_join(itis_codes,by='taxon') %>% 
  #add the rows from ITIS with info
  bind_rows(itis_codes) %>% 
  mutate(
    #make all rank names consistently title case
    rank = str_to_title(rank)
    #drop note from online database about status of taxonomic name as unassigned
    #only applied to a single order name (ie, for Melanoides tuberculata)
    #NOTE: need fixed=T to turn off regex, which has specific use for []
    ,across(c(kingdom:genus), ~as.character(gsub("[unassigned] ", "", .,fixed=T)))
    ) %>% 
  #reorder columns
  select(organism_code,aphia_id,taxon,source,status:genus) %>% 
  arrange(kingdom,phylum, class, order, genus) %>% 
  glimpse()

#write a file containing the updated taxonomy
#write_csv(all_format,"./benthic/data_output/benthic_common5_taxonomy_2023-03-27.csv")

#automate taxonomy updates for 10% taxa group:check ITIS using taxize package------------------- 
#github repo: https://github.com/ropensci/taxize
#documentation: https://docs.ropensci.org/taxize/articles/taxize.html
#book: https://books.ropensci.org/taxize/

#determine if the names not found in WoRMS are considered accepted names in ITIS
#accepted = T only returns results for accepted names
#accepted = F returns both accepted and unaccepted names
tsn_gaps10 <- get_tsn(check_itis10,accepted = T)
accept_gaps <- lapply(as.character(tsn_gaps10),itis_acceptname)
#for both taxa, acceptedname = NA  meaning the input name is the accepted name

#search ITIS for taxa not in WoRMS
taxize_records_gaps10 <- classification(check_itis10, db = 'itis')

#convert nested dataframe to one data frame and then combine with WoRMS data

#create vector of column names from WoRMS df
#will be used to filter columns from ITIS df
taxheader10 = names(worms_format10)

#make tibbles for ITIS matches
itis_tibble10 <- taxize_records_gaps10 %>% 
  map(as_tibble)

#convert from nested dataframe to single dataframe
itis_format10 <- enframe(itis_tibble10,name="name") %>% 
  mutate(
    #make a new vector that provides name of rank of taxon searched for (eg, genus or species)
    #uses the last() function to grab the most specific rank 
    rank = map_chr(value,~last(pull(.x,rank)))
    #convert from long to wide, drop the id column
    ,value = map(value,~pivot_wider(.x, id_cols=-id, names_from = rank,values_from = name) %>% 
                   #subset the columns to just those in the WoRMS dataframe
                   select(any_of(taxheader)) )
  ) %>% 
  #unnest the tibbles
  unnest(cols = value) %>% 
  #add column indicating status = accepted; manually confirmed above these two are accepted names
  #also add column indicating taxonomy source (ie, ITIS)
  add_column(status = "accepted"
             ,source = "itis")
#need to capitalize rank names

#add organism codes to itis records; match by name
itis_codes10 <- left_join(itis_format10,all_code10) %>% 
  rename(taxon = name)

#replace missing taxa info with ITIS info
all_format10 <- worms_format10 %>% 
  #drop the WoRMS rows filled with NAs
  anti_join(itis_codes10,by='taxon') %>% 
  #add the rows from ITIS with info
  bind_rows(itis_codes10) %>% 
  mutate(
    #make all rank names consistently title case
    rank = str_to_title(rank)
    #drop note from online database about status of taxonomic name as unassigned
    #only applied to a single order name (ie, for Melanoides tuberculata)
    #NOTE: need fixed=T to turn off regex, which has specific use for []
    ,across(c(kingdom:genus), ~as.character(gsub("[unassigned] ", "", .,fixed=T)))
  ) %>% 
  #reorder columns
  select(organism_code,aphia_id,taxon,source,status:genus) %>% 
  arrange(kingdom,phylum, class, order, genus) %>% 
  glimpse()

#write a file containing the updated taxonomy
#write_csv(all_format10,"./benthic/data_output/benthic_common10_by_stn_taxonomy_2024-12-30.csv")


#look for synonyms of target taxa on worms----------------
#use wm_synonyms_() from worrms
#note there are two taxa not in worms so this obviously won't generate synonyms for them

#create vector of aphia ids
worms_aphia <- all_format %>% 
  #drop the NAs (ie, taxa not on worms)
  filter(!is.na(aphia_id)) %>% 
  pull(aphia_id)

#search for all synonyms for the 62 (of 64 total) taxa on WoRMS
worms_syn <- worrms::wm_synonyms_(id=worms_aphia)
#346 rows
#18 warnings: 
#17 indicate "no content", which means those taxa have no synonyms
#one warning is that a function used by worrms is deprecated
#note: all matches are exact matches because we used the aphid IDs (not names)

#create version of target taxa data set with just organism code and aphia ID
worms_aphia_code <- all_format %>% 
  select(organism_code,aphia_id)

#clean up synonym data set
worms_syn_format <- worms_syn %>% 
  #just keep the needed columns
  select(aphia_id = valid_AphiaID
         ,taxon = valid_name
         ,aphia_id_syn = AphiaID
         ,synonym = scientificname
         ,status
         ,rank
         ,kingdom:genus
         ) %>% 
  #add organism code
  left_join(worms_aphia_code) %>% 
  #move organism code to front of df
  relocate(organism_code,.before = aphia_id) %>% 
  #add source column
  add_column(source = "worms",.after = "taxon")


#look for synonyms of target taxa on ITIS-----------------
#just need two that aren't in worms

#search for synonyms based on species names
#this will create a list for each taxon
syn_itis <- synonyms(check_itis,db="itis")
#one synonym for M. microstoma and none for Isocypris

#make tibbles from lists
itis_tibble_syn <- syn_itis %>% 
  map(as_tibble)

#convert from nested dataframe to single dataframe
itis_format_syn <- enframe(itis_tibble_syn,name="name") %>% 
  #unnest the tibbles
  unnest(cols = value) 

#make vector of synonym TSNs
itis_tsn_syn <- itis_format_syn %>% 
  pull(syn_tsn)

#get full taxonomy for synonyms using TSNs
taxize_records_gaps_syn <- classification(itis_tsn_syn, db = 'itis')
#doesn't provide full taxonomy for synonyms like worms does
#I guess just use taxonomy for accepted name and replace the genus

#pull row from accepted taxonomy df
#find and replace the genus 
#then add this modified row to the worms synonyms file
itis_syn <- all_format %>% 
  filter(organism_code==2970) %>% 
  add_column(
    aphia_id_syn = NA
    ,synonym = "Dina microstoma") %>% 
  relocate(c(aphia_id_syn,synonym),.after=source) %>% 
  mutate(status = case_when(organism_code==2970~"unaccepted")
         ,genus = case_when(organism_code==2970~"Dina")
         )

#add ITIS synonym to worms synonyms
synonyms_all <- bind_rows(itis_syn,worms_syn_format)

#write synonyms file
#write_csv(synonyms_all,"./benthic/data_output/benthic_common5_synonyms_2023-03-27.csv")

# Look at summaries of different taxonomic levels -----------
#summarize lowest rank for each taxon
#ideally these would all be species for rounding up traits but they aren't
summary_taxon <-all_format %>% 
  group_by(rank) %>% 
  summarize(count = n())
#of 64 taxa, 50 are species, 11 are genera, 3 are higher

#explore diversity at higher taxonomic levels
summary_phylum <-all_format %>% 
  group_by(phylum) %>% 
  summarize(count = n())
#5 phyla, mostly annelids, molluscs, and arthropods

summary_class <-all_format %>% 
  group_by(class) %>% 
  summarize(count = n())
#12 classes, mostly Malacostraca (crustaceans) and Clitellata (worms)

summary_order <-all_format %>% 
  group_by(order) %>% 
  summarize(count = n())
#25 order (4 taxa have no order listed); mostly tubificids and amphipods

#experiment with searching all species using taxize instead of starting with worms---------
#made good progress but didn't complete this
#may be better in short term to stick with above approach
#check WoRMS using worms and get rest from ITIS using taxize

#look at one species; I know name in data set is not the accepted name on WoRMS

#names_res_ex <- gnr_resolve("Pisidium casertanum"
#https://resolver.globalnames.org/data_sources
#can specify database(s); 9 = WoRMS
#,data_source_ids =  9) %>% arrange(score)

#shows results for a bunch of databases, including WoRMS, but output doesn't indicate that 
#this isn't the current accepted name on WoRMS (it's Euglesa casertana)
#also doesn't provide any sort of ID #s for any of these records
#so overall this function isn't very useful for my purposes

#for taxa not found in WoRMS, next check ITIS
#get_tsn() may not find every species; for those not matched, an NA is returned
#NAs will prevent itis_acceptname from working, so have to remove NAs between steps
#try all 50 species in my list
#accepted = F returns both accepted and unaccepted names
tsn_all <- get_tsn(specieslist,accepted = F)
#found 43 of 50 species but only prints the numbers without the names
#remove species that didn't match (ie, NA results)

#accepted = T returns only accepted names
tsn_accept <- get_tsn(specieslist,accepted = T)
#finds 42 of 50 species, so one name was an unaccepted name
#"Sparganophilus eiseni" is unaccepted name; WoRMS already caught this one

#create data frame with input species names, all TSNs, and accepted TSNs
tsn_comp <- as.data.frame(cbind(specieslist,tsn_all, tsn_accept)) %>%
  #add column that compares the two tsn columns
  mutate(diff = case_when(tsn_all == tsn_accept ~ F, (is.na(tsn_all) & is.na(tsn_accept) ~ F), TRUE ~ T)) %>% 
  glimpse()

#generate list of species not found in ITIS
itis_fail <- tsn_comp %>% 
  filter(is.na(tsn_all) & is.na(tsn_accept)) %>% 
  pull(specieslist)
#need to figure out why these taxa failed
#may need to either fix spelling, update name manually, or try other DB

#create list of TSNs that were found in ITIS but aren't for accepted names
#probably best to only run itis_acceptname for taxa we know weren't accepted
#running for all species would take a while and wouldn't be worth it
tsn_unaccept <- tsn_comp %>% 
  filter(diff == T) %>% 
  pull(tsn_all) %>% 
  glimpse()
#one taxon and output is chr type

#find accepted TSN for unaccepted TSN
#this output will be lists
#need to figure out how to extract "acceptedtsn" 
# probably don't need "acceptedname" because classification() will grab it
accepted_now <- lapply(as.character(tsn_unaccept),itis_acceptname)

#looks for all species in list and returns output as lists by species
#output includes nameUsage (eg, valid, invalid), scientificName, tsn
#maybe this is all I need as starting point
#figure out how to create three sets of species:
#no ITIS match at all (tibble: 0 x 0)
#ITIS match but not valid/accepted (nameUsage != valid | accepted)
#ITIS valid/accepted (nameUsage = valid | accepted)
#if way to do this easily, would be simplier than approach above
#also should only keep first hit; subsequent hits seem to all be subspecies
itis_test <- itis_terms(query=specieslist,"scientific")

#try making the list into a tibble
#itis_tibble <- as_tibble(itis_test)
#! Tibble columns must have compatible sizes.
#makes sense that this doesn't work I guess

# filter to keep tibbles for species with nameUsage=invalid
invalid <- keep(itis_test, ~ any(c("invalid","unaccepted") %in% .x$nameUsage)) 
#kept the one case
#gives a warning for each of the species that had no ITIS match, which is fine

#create list with just species with valid/accepted names
valid <- keep(itis_test, ~ any(c("valid","accepted") %in% .x$nameUsage)) 
#kept the 42 species with valid names

#try to filter out the species that had no match and create a vector of their names



#get the Worms ID
#accepted = F returns both accepted and unaccepted names
#ask=F eliminates interactive name selection; means could be multiple rows per species
#tried specifying rows=1 but wasn't working
#found 25 of 50, which seems wrong
#some very common taxa that I know are in WORMS did not match

#worms_id2 <- get_wormsid(specieslist,accepted = F,ask=F)


