#Special Studies Drought Synthesis
#Environmental Monitoring Program data
#Benthic Invertebrate Survey data
#scrape trait data for non-native species from NEMESIS website

#To do list--------
#will need to manually clean up the "citation" column which has various notes in it
#should read text of webpages for additional traits. 
#For example, there may be data on reproduction in the Ecology:General section

#packages-----------
library(tidyverse) #suite of data science tools
library(rvest) #scraping info from the internet
library(purrr) #map functions for scraping multiple webpages at once

#read in data----------

#file with NEMESIS record links for each taxon
#in_database: Y = yes, N = no, S = placeholder record only
nemesis_links <- read_csv("./benthic/data_input/traits_nemesis/benthic_nemesis_links.csv")

#examples----------

#scraping multiple tables from one webpage 
#https://stackoverflow.com/questions/55092329/extract-table-from-webpage-using-r

#scraping tables from multiple webpages
#https://stackoverflow.com/questions/68659860/r-scrape-tables-from-multiple-unique-urls

#step by step how to scrape html table
#https://dcl-wrangle.stanford.edu/rvest.html

#webscraping with R manual
#https://webscraping-tures.github.io/rvest3.html

#single webpage: scrape table by specifying which table number on the webpage---------------
#I think the number and order of tables on each webpage is standardized
#so this should work for all webpages

#specify URL
url <-  "https://invasions.si.edu/nemesis/species_summary/81746"

#scrape the third table of the webpage and format it a bit
df <- url %>% 
  #read all the html for the webpage into R
  read_html() %>% 
  #scrapes all the tables on the webpage
  #use html_elements() instead of html_element() because there are multiple tables
  #and the first table isn't the one I want
  html_elements("table") %>% 
  #turn the html into tibbles
  #creates list of 6 tibbles because there isn't just one table
  html_table() %>%   
  #just keep the third table of the webpage
  chuck(3) %>% 
  #create better column headers
  rename(trait = X1
         ,value = X2
         ,citation = X3
  ) %>% 
  #add column with url
  #will use this column later to add organism codes and taxon names
  add_column(link = url) %>% 
  glimpse()

#single webpage: scrape table using CSS selector----------
#this approach won't work across all webpages

#grabbed the html element that represents the table
#right click on table and then inspect
#click elements until the whole table and only the table are highlighted
#right click that html line and then copy > CSS Selector
css_selector <- "table.table:nth-child(18)"

#this approach won't work across webpages though 
#because tables don't have the same name
#examples from other pages
#table.table:nth-child(16)
#table.table:nth-child(14)

test_table <- url %>% 
  #read all the html for the webpage into R
  read_html() %>% 
  #scrapes all the tables on the webpage
  #would be nice if we could just specify the table I want
  html_element(css = css_selector) %>% 
  #turn the html into a tibble
  html_table() 
  
#scrape specific table from multiple webpages----------------------

tables_all <- nemesis_links %>% 
  #one species wasn't on NEMESIS because it's actually cryptogenic
  drop_na(link) %>% 
  #create a new column with nested dataframe
  mutate(
    wptables = map(
      link,
      #read the html from all the webpages based on URL in "link" column
      ~ read_html(.x) %>% 
        #grab just the tables (multiple per webpage)
        html_elements(css = "table") %>% 
        #convert html to tibbles
        html_table()
    ),
    # check if any of the elements in df_data are empty and remove them
    #there are two species with placeholder webpages but no info yet
    check_df_data = map_lgl(wptables, is_empty)
  ) %>% 
  #remove the two taxa with empty lists
  filter(!check_df_data) %>% 
  #just keep the third table
  mutate(trait_tables = map(wptables, ~ chuck(.x, 3))) %>% 
  #just keep the needed columns
  select(organism_code
         ,taxon_worms
         ,trait_tables
         ,link
  ) %>% 
  #unnest the trait tables
  unnest(cols = c(trait_tables)) %>% 
  #rename the columns
  rename(trait = X1
         ,value = X2
         ,citation = X3
  ) %>% 
  mutate(
    #within value column, change "None" to NA
    value = case_when(value=="None"~NA, TRUE~value)
    #make value column numeric
         ,value=as.numeric(value)
         ) %>% 
  glimpse()

#Filter and format trait data-------------- 

#look at unique "traits" 
unique(tables_all$trait)

#look at number of available records per trait
tables_all_sum <- tables_all %>% 
  group_by(trait) %>% 
  count() %>% 
  arrange(-n)

#traits to keep
traits_keep <- c("Maximum Length (mm)","Maximum Width (mm)" ,"Maximum Temperature (ºC)", "Maximum Salinity (‰)")

#filter data set to keep just the desired traits and then format data for export
tables_all_filt <- tables_all %>% 
  filter(trait %in% traits_keep) %>%
  mutate(
    #rename traits
    trait_group = case_when((trait == "Maximum Length (mm)" | trait == "Maximum Width (mm)")~"body_size_max"
                              ,trait == "Maximum Temperature (ºC)" ~ "thermal_max"
                              ,trait == "Maximum Salinity (‰)" ~ "salinity_max"
    ) 
    #create a column for the trait units
    ,trait_unit = case_when((trait == "Maximum Length (mm)" | trait == "Maximum Width (mm)")~"mm"
                            ,trait == "Maximum Temperature (ºC)" ~ "C"
                            ,trait == "Maximum Salinity (‰)" ~ "PSU"
                            )
         #create some additional new columns 
        ,target_taxon_level = "species"
         #lit_taxon_name does actually sometimes differ from the worms name
         #so should probably pull the names from the webpages
         #worth noting though that there will be differences between taxon from NEMESIS and lit cited therein as well
         ,lit_taxon_name = taxon_worms
         ,lit_taxon_level = "species"
         ,lit_taxon_type = "target"
         ,lit_taxon_type_ord = "1"
         ,database = "NEMESIS"
         ) %>% 
  select(organism_code
         ,target_taxon_name = taxon_worms
         ,target_taxon_level
         ,lit_taxon_name
         ,lit_taxon_level
         ,lit_taxon_type
         ,lit_taxon_type_ord
         ,database
         ,citation
         ,trait_group
         ,trait_value = value
         ,trait_unit
         ,link
         ) %>% 
  glimpse()
  
#write the file-----------
#write_csv(tables_all_filt,"./benthic/data_output/traits/benthic_traits_nemesis_size&physiology.csv")




