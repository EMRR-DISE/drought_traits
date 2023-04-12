
#Notes----------
#tried to use the SelectorGadget Chrome extension to scrape the 
#specific table but it selects all the tables (which I don't want)

#packages-----------
library(tidyverse) #suite of data science tools
library(rvest) #scraping info from the internet
library(purrr) #map functions for scraping multiple webpages at once

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
  #[3] would format it as a list, [[3]] formats it as a df (what we want)
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

#go to each URL
#read the html
#turn the tables into tibbles
#keep the third table only
#format the column names
#add the URL as a column
#combine all tables into one table
#join with df with organism codes and taxon names


#start with simple example with only two URLs
url2 <-  c("https://invasions.si.edu/nemesis/species_summary/81746","https://invasions.si.edu/nemesis/species_summary/-82")

#create vector of all URLs
#note that all URLs are the same except for the webpage specific number at the end
# prefix = https://invasions.si.edu/nemesis/species_summary/
url_all <- nemesis_links %>% 
  pull(link)

#try to scrape the specific table from all webpages
#eventually replace ulr2 with url_all
tables_all <- url2 %>% 
  #read the html from all the webpages
  map(read_html) %>% 
  #grab just the tables (multiple per webpage)
  map(html_elements,css="table") %>% 
  #convert html to tibbles
  map(html_table)  
  #just keep the third table 
  map(.[[3]]) #this doesn't work

#example
pages %>% 
  map(html_node, css = "a.navbar-brand") %>% 
  map_chr(html_text)

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







