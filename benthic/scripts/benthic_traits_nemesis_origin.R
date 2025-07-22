#Special Studies Drought Synthesis
#Environmental Monitoring Program data
#Benthic Invertebrate Survey data
#non-native status of taxa
#provided by Sharon Shiba based on NEMESIS/CalNEMO database

#Nick Rasmussen, nicholas.rasmussen@water.ca.gov

#To do list-----------------

#Changes made upstream of this script in taxonomy script based on CalNEMO: 
#Sinelobus sp. A to Sinelobus stanfordi
#Synidotea laevidorsalis to S. laticauda

#should add estimated year of introduction for non-natives
#useful because some of the important taxa arrived during the time series we're analyzing
#also need to pair this trait data with literature references (largely Cohen and Carlton 1995)

# Load required packages -----------------

library(janitor) #used to quickly clean up column names
library(tidyverse) #suite of data science tools
library(readxl) #reading xlsx files
library(EDIutils) #download EDI data
#library(patchwork) #make figure panels

# Read in the data-----------------------

#read in non-native status file
benthic_aliens <- read_excel("./benthic/data_input/traits_nemesis/IEP_BenthicOrganismList2021_w_Intro_status_From Sharon Shiba.xlsx") %>% 
  clean_names() %>% 
  glimpse()

#read in my list of focal taxa (n = 64, present in at least 5% of samples)
#these names have been updated as needed (mostly using WoRMS) 
focal_taxa <- read_csv("./benthic/data_output/benthic_common5_taxonomy_2023-03-27.csv") %>% 
  glimpse()

#read in list of all taxa found in the three focal stations (n=249)
#these names haven't been updated, just used the ones from EMP
all_taxa <- read_csv("./benthic/data_output/benthic_relative_abundances.csv")

#read in the whole CPUE data set to make list of all taxa present in monitoring program

#list all data files from EMP benthic inverts EDI package
benthic_pkg <- read_data_entity_names(packageId = "edi.1036.4")

#benthic invert CPUE, 1975-2021
#data have been converted to CPUE (organisms/m2)
#replicate grabs have been averaged for each site visit
#all non-occurrence (zero) data for a site visit has been removed
#Nick: samples with no organisms at all are probably included as "No catch"
benthic_invert_cpue <- read_csv(read_data_entity(packageId = "edi.1036.4", entityId= benthic_pkg$entityId[2])) %>% 
  clean_names() %>% 
  glimpse()

# Format data in prep for combining ---------------------

#focal taxa data
focal_taxa_format <- focal_taxa %>% 
  select(organism_code
         , taxon_worms = taxon) %>% 
  glimpse()

#all taxa data
all_taxa_format <- all_taxa %>% 
  select(organism_code
         ,common_5perc
         ,species_name) %>% 
  glimpse()

#whole EMP monitoring program data
emp_format <- benthic_invert_cpue %>% 
  distinct(organism_code)
#420 taxa

#NEMESIS data
aliens_format <- benthic_aliens %>% 
  select(organism_code
         , genus
         , species
         , origin = status_in_cal_nemo
         , comments) %>% 
  glimpse()

#take a closer look at those with a non-native status
aliens <- aliens_format %>% 
  filter(!is.na(origin))
#72 taxa

#look at categories for origin
aliens_cat <- aliens %>% 
  group_by(origin) %>% 
  count() %>% 
  glimpse()
#nearly all simply say introduced; all other categories are singletons

# Filter data set to just my focal taxa ------------------

aliens_focal <- left_join(focal_taxa_format,aliens_format) %>% 
  glimpse()
#one cryptogenic species; maybe just include as introduced or native since only one

#format for taxonomic comparison
focal_taxa_comp <- aliens_focal %>% 
  separate(col = taxon_worms, into = c("genus_worms","species_worms"), sep = " ", remove = F) %>% 
  rename(genus_emp = genus, species_emp = species) %>% 
  mutate(genus_match = case_when(genus_emp == genus_worms ~ 1, TRUE ~ 0)
         ,species_match = case_when((species_emp == species_worms |species_emp == "sp. A") ~ 1, TRUE ~ 0)
         ,taxon_match = case_when((genus_match==1 & species_match==1) ~ 1, TRUE ~ 0)
         ) %>% 
  arrange(taxon_match,comments)

#create subset of taxa with taxonomic discrepancies or other issues to examine
focal_taxa_issue <- focal_taxa_comp %>% 
  filter(taxon_match==0 | !is.na(comments))
#most discrepancies are simply from me updating names using worms so they're fine as is
#Sharon indicates that Laonome calida should be Laonome cf. calida, so use literature data with caution
#she also labels as Sinelobus sp. A as Sinelobus cf. stanfordi, which is better than just genus level but again use species data with caution
#she labeled Manayunkia speciosa as cryptogenic; maybe just label as invasive for simplicity (following Cohen and Carlton 1995)
#she labeled Synidotea laevidorsalis as S. laticauda. both are valid names
#unclear if ours is S. laevidorsalis or S. laticauda but NEMESIS suggests probably S. laticauda so let's use that

#changes to make here: 
#Manayunkia speciosa change from cryptogenic to non-native
#assume Sinelobus sp. A is Sinelobus stanfordi and change from Introduced? to Introduced

#changes to make upstream in taxonomy script: 
#Sinelobus sp. A to Sinelobus stanfordi
#Synidotea laevidorsalis to S. laticauda

# Format the data set filtered to just my target taxa ---------------
#NOTE: assuming any taxon not specificly IDed as non-native is native
#clearly some of these taxa left as native could be non-native, particularly the ones not IDed to species

aliens_focal_format <- aliens_focal %>% 
  mutate(
    #change one case of Introduced? and Cryptogenic to Introduced (following Cohen and Carlton 1995 which might be outdated)
    #fill NAs with Native
    native = case_when((origin == "Introduced?" | origin == "Cryptogenic" | origin == "Introduced") ~ "0"
                        , is.na(origin) ~ "1")
    ) %>% 
  select(-c(origin,comments)) %>% 
  arrange(organism_code) %>% 
  glimpse()

#export the complete file
#write_csv(aliens_focal_format,"./benthic/data_output/traits/benthic_traits_nemesis_origin_taxa64.csv")

#match up all 249 taxa with the origin info ----------------

aliens_all <- left_join(all_taxa_format,aliens_format) %>% 
  glimpse()
#one cryptogenic species; maybe just include as introduced or native since only one

# Format the data set filtered to just the 249 taxa ---------------

aliens_all_format <- aliens_all %>% 
  mutate(
    #change one case of Introduced? and Cryptogenic to Introduced (following Cohen and Carlton 1995 which might be outdated)
    #fill NAs with Native
    native = case_when((origin == "Introduced?" | origin == "Cryptogenic" | origin == "Introduced") ~ "0"
                       , is.na(origin) ~ "1"),.after=common_5perc
  ) %>% 
  arrange(native,-common_5perc) %>% 
  glimpse()

#export the complete file
#write_csv(aliens_all_format,"./benthic/data_output/traits/benthic_traits_nemesis_origin_taxa249.csv")
#NOTE: there is an NA in the "native" column for "Needs review"

#format the full data set---------------------------

full_taxa <- emp_format %>% 
  left_join(benthic_aliens)

#look at all unique status names
unique(full_taxa$status_in_cal_nemo)
#NA               "Cryptogenic"    "Introduced"     "Introduced?"    "?"              "Needs review"  "Boundary issue"

#look at all cases that aren't NA or "Introduced"
full_taxa_unk <- full_taxa %>% 
  filter(status_in_cal_nemo!="Introduced" & !is.na(status_in_cal_nemo))
#all of these alternative statuses are singletons
#we will follow same approach used above for dealing with Cryptogenic and Introduced? (make Introduced)
#for "?" we will assume it is non-native because probably sp. 1 like in NEMESIS
#for "boundary issue" we will assume introduced because described as such in Cohen and Carlton 1995
#for "needs review" we will assume native; worms lists this species as terrestrial so maybe misIDed

full_taxa_format <- full_taxa %>% 
  mutate(
    #clean up origin column so everything is native or introduced
    #everything will be considered introduced except NA and "Needs review"
    native = case_when((is.na(status_in_cal_nemo) | status_in_cal_nemo == "Needs review") ~ "1"
                       ,TRUE ~ "0")
  ) %>% 
  select(organism_code
         ,native
         ,phylum:common_name
         ,origin = status_in_cal_nemo
         ,comments
         ) %>% 
  glimpse()

#create version with just organism code and native columns
#most of other columns either aren't needed or are redundant with what is in the destination df
full_taxa_truc <- full_taxa_format %>% 
  select(organism_code,native)

#add info about native vs nonnative to cpue dataset
emp_origin <- benthic_invert_cpue %>% 
  left_join(full_taxa_truc)

#filter the EMP abundance data to just the nonnatives
emp_invader <- emp_origin %>% 
  filter(native==0) %>% 
  glimpse()
#note: there are 70 non-natives

#filter the EMP abundance data to just the natives
emp_native <- emp_origin %>% 
  filter(native==1) %>% 
  glimpse()
#could consider plotting the native composition too, even doing a faceted plot with native/non-native
#tried it and there is a lot more class level diversity and taxa generally within natives

#summarize abundance data to show first detection of a taxon within a given year
emp_invader_yr <- emp_invader %>% 
  #organize data so first detection of a taxon within a year is first row
  arrange(organism_code, sample_date) %>% 
  #now group by the organism code
  group_by(organism_code, year) %>% 
  #keep only the first record of each taxon within year
  slice(1) 

#are there any invaders who disappeared after arriving?
emp_invader_extinct <- emp_invader %>% 
  #organize data by organism code and date
  arrange(organism_code, sample_date) %>%
  group_by(organism_code) %>% 
  slice_tail(n=1) %>% 
  arrange(sample_date)

#create subset of taxa that haven't been seen in past five years
emp_invader_extinct_old <- emp_invader_extinct %>% 
  filter(year < 2019) %>% 
  arrange(organism_code) %>% 
  select(sample_date:month
         ,common_name
         ,organism_code:native) %>% 
  glimpse()
#are any of these taxa in our trait data set (probably not)?

#3394 Neoamphitrite sp. A just showed up a couple times; not much known
#https://invasions.si.edu/nemesis/species_summary/67932

#4330 Asellus hilgendorfii  quite abundant at times in the past in low salinity zone
#https://invasions.si.edu/nemesis/species_summary/-263 no idea where it went

#4390 Sphaeroma quoianum has broad salinity tolerance (5-40 PSU)
#https://invasions.si.edu/nemesis/species_summary/92340 burrows in wood so maybe not caught in ponar

#4580 Monocorophium uenoi seems to have broad salinity tolerance based on collection localities
#https://invasions.si.edu/nemesis/species_summary/-244

#4715 Stenothoe valida
#https://invasions.si.edu/nemesis/species_summary/206574 seems to generally stay in saltier areas

#4940 Eriocheir sinensis (mitten crab)
#https://invasions.si.edu/nemesis/species_summary/99058 disappeared

#6640 Tritia obsoleta
#https://invasions.si.edu/nemesis/species_summary/74111 probalby mostly in the saltier areas

#6850 Gemma gemma
#https://invasions.si.edu/nemesis/species_summary/81511 not clear what happened to it

#6999 Styela clava
#https://invasions.si.edu/nemesis/species_summary/159337 probably in saltier areas mostly

#make a list of the organism codes for the extinct ones
extinct <- emp_invader_extinct_old %>% 
  pull(organism_code)

#look at which years those extinct taxa were present
extinct_time_series <- emp_origin %>% 
  filter(organism_code %in% extinct) %>% 
  #sum mean CPUE by year
  group_by(organism_code,year,genus, species, common_name) %>% 
  summarize(total_cpue = sum(mean_cpue),.groups = 'drop')

#plot time series for each taxon
(plot_extinct_time_series <- ggplot(extinct_time_series,aes(year,total_cpue))+
  geom_line()+
  geom_point()+
    facet_grid(organism_code~., scales = "free_y")
)

#count invader spp by year
emp_invader_yr_ct <- emp_invader_yr %>% 
  group_by(year) %>% 
  summarise(invaders = n_distinct(organism_code))

#count invader spp by year and class
emp_invader_yr_tx <- emp_invader_yr %>% 
  group_by(year,class_level) %>% 
  summarise(invaders = n_distinct(organism_code),.groups = 'drop')

#plot total invaders by year
(plot_invader_count_yr <- ggplot(emp_invader_yr_ct, aes(x = year, y = invaders)) +
  geom_line()+
  geom_point()+
  scale_x_continuous(limits = c(1974,2024))
)
#increasing trend in nonnative taxa detected over time series as expected

#determine which classes have most taxa so we can set order of them in stacked bar plot
emp_invader_class_ct <- emp_invader %>% 
  distinct(organism_code,class_level) %>% 
  group_by(class_level) %>% 
  count() %>% 
  arrange(n)

emp_invader_order <- emp_invader_class_ct %>% 
  pull(class_level)

emp_invader_yr_tx$class_level <- factor(emp_invader_yr_tx$class_level,levels = emp_invader_order)

#plot taxonomic composition of invaders through time
(plot_invader_count_yr_tx_stack_bar <- ggplot(emp_invader_yr_tx, aes(x = year, y = invaders, fill = class_level))+
    geom_bar(stat = 'identity', color = "black")+
    scale_x_continuous(limits = c(1974,2024))+
  labs(fill="Class", x = "Year", y="Number of Non-native Taxa")
    )

# ggsave(plot = plot_invader_count_yr_tx_stack_bar #tell ggsave which plot to export
#        , filename = "benthic/figures/benthic_all_emp_number_comp_invasive_timeline.png" #provide the name for the image file
#        , width = 8, height =6, units = "in" #dimensions of the exported image
#        , dpi = 300 #resolution of the image (dots per inch)
# )

#create figure panel
#plot_invader_count_yr/plot_invader_count_yr_tx_stack_bar
#I guess we don't need the line plot because the stacked bar plot shows the total count too

#are there invasive taxa that have disappeared?

#show same stacked bar plot with natives
  
