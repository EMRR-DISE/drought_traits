
#Notes----------
#tried to use the SelectorGadget Chrome extension to scrape the 
#specific table but it selects all the tables (which I don't want)

#packages-----------
library(tidyverse)
library(rvest)

#read in data----------

#file with NEMESIS record links for each taxon
#in_database: Y = yes, N = no, S = placeholder record only
nemesis_links <- read_csv("./BenthicInverts/calnemo/benthic_nemesis_links.csv")

#examples----------

#scraping multiple tables from one webpage 
#https://stackoverflow.com/questions/55092329/extract-table-from-webpage-using-r

#scraping tables from multiple webpages
#https://stackoverflow.com/questions/68659860/r-scrape-tables-from-multiple-unique-urls

#step by step how to scrape html table
#https://dcl-wrangle.stanford.edu/rvest.html

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
  .[[3]] %>% 
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
  
#scrape table from multiple webpages----------------------

#start with simple example with only two URLs
url2 <-  c("https://invasions.si.edu/nemesis/species_summary/81746","https://invasions.si.edu/nemesis/species_summary/-82")

#create vector of all URLs
url_all <- nemesis_links %>% 
  pull(link)

#function for scraping tables from multiple webpages
#is it possible to just grab the third table from each webpage instead of grabbing all tables?
read_my_urls <- function(url){

    url2 <- read_html(url)

     Procdetail <- url2 %>% html_nodes("table") %>%
html_table (fill=T) %>% .[[3]] 
 Procdetail
}

#run the function on the set of webpage URLs
my_scraped_data <- lapply(url2, read_my_urls)

#need to go into the list of tables for each webpage and grab the third table






