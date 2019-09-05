
## horsens.R
## Massimo G Losinno
## July 2019
## Scrape Horsens city council referater

# rm(list = ls())
# setwd("C:/Users/musse/Aarhus universitet/Tim Dennis Runck - CROW_FAR/First_Repository_CROW_FAR/data_collection_code")

library(rvest)
library(RCurl)
library(stringr)
library(data.table)
library(XML)



# Create directories to save files if don't exist
if(!dir.exists("../data_archive")) dir.create("../data_archive")
if(!dir.exists("../data_archive/horsens_archive")) dir.create("../data_archive/horsens_archive")

#set base url for looping -- this is more of a 'start' page for this task
start_url <- "https://horsens.dk/Politik#PolitiskeUdvalg"
extra_url <- "https://horsens.dk/Politik/Byraadet/50"


months <- c("januar", "februar", "marts", "april", "maj", "juni", "juli", "august", "september", "oktober", "november", "december")


dfs <- list() #list for dataframes for each meeting
dates <- "Not a date" #creating a date count to distenguish the first scrape


#Creating the url to get links for each of the committees
first_url <- read_html(start_url)

#Getting links for each committees' site
links <- first_url %>% html_nodes('.teaser__link') %>% html_attr("href")

links <- links[11:18]
links <- paste0("https://horsens.dk", links)
links <- c(links, extra_url)

for(cl in 1:length(links)){

  print(paste0("Working on com nr: ", cl, " / ", length(links)))
  
  
  com_url <- read_html(links[cl])
  

  #Grapping links for each meeting
  meeting_links <- com_url %>% html_nodes("p") %>% html_text(trim = T)

  html_link <- read_html(links[cl])
  

  
  meeting <- htmlParse(html_link)
  
  ref <- xpathSApply(doc = meeting, 
                     path = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'chevron-right', ' ' ))]", 
                     xmlValue)
  