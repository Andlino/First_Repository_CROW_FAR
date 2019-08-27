## ballerup.R
## Massimo G Losinno
## August 2019
## Scrape Ballerup city council referater

# rm(list = ls())
# setwd("C:/Users/musse/Aarhus universitet/Tim Dennis Runck - CROW_FAR/First_Repository_CROW_FAR/data_collection_code")

library(RCurl)
library(stringr)
library(XML)
library(rvest)
library(data.table)
library(curl)




# Create directories to save files if don't exist
if(!dir.exists("../data_archive")) dir.create("../data_archive")
if(!dir.exists("../data_archive/ballerup")) dir.create("../data_archive/ballerup_archive")


#Getting the main url link in
master_url <- "https://ballerup.dk/dagsorden-og-referat?from_date%5Bvalue%5D%5Bdate%5D=01-01-2007&to_date%5Bvalue%5D%5Bdate%5D=31-12-2017&os2web_meetings_tax_committee=All&page="

dates <- "Not a date" #creating a date count to distenguish the first scrape

dfs <- list() #list for dataframes for each meeting



for (lc in 0:36) {
  
  print(paste("Working on page", lc + 1, "out of", 37))
  
  link_url <- paste0(master_url, lc)
  link_url <- read_html(link_url)
  
  #Creating a list of links to all committees
  meet_links <- link_url %>% html_nodes('caption~ tbody .views-field-field-os2web-meetings-committee a') %>% html_attr("href")
  meet_links <- paste0("https://ballerup.dk", meet_links)
  meet_dates <- link_url %>% html_nodes('caption~ tbody .date-display-single') %>% html_text(trim = T)
  meet_dates <- str_extract(meet_dates, "[[:digit:]]*/[[:digit:]]*/[[:digit:]]*")
  meet_dates <- gsub("/", "-", meet_dates)
  
  
  for (mc in 1:length(meet_links)) {
    
    
    date  <- meet_dates[mc]
    
    dates <- str_c(c(dates, date))
    date <- make.unique(dates, "-")[length(dates)]
    ref_date <- dates[length(dates)]
    
    file.name <- paste0("../data_archive/ballerup_archive/", date, ".RData")
    
    #Check to see if the pdf is already downloaded
    if (file.exists(file.name)){
      #if archived file exists, load it instead of downloading again
      load(file.name)
    } else {
      
      ok <- FALSE
      problem <- FALSE
      count <- 1
      
      #Reading the html for the meeting, leads to a link to the meeting
      while (ok == FALSE) {
        meeting <- tryCatch({                  
          read_html(meet_links[mc])
        },
        error = function(e) {problm <- TRUE
        Sys.sleep(2)
        e
        }
        )
        if ("error" %in% class(meeting)) {
          print(paste("Problem with link", mc))
          count <- count + 1
          
        }else {
          if(problem == TRUE ){print(paste("Problem with", yc, "fixed"))}
          ok <- TRUE
        }
        if(count == 10){
          break}}
      
      meeting <- htmlParse(meeting)
      #Saving the xml for the meeting
      meeting <- saveXML(meeting)
      save(meeting, file = file.name)
    }
    
    
    meet_url <- read_html(meeting)
    
    meeting <- htmlParse(meeting)
    
    items <- xpathSApply(doc = meeting, 
                       path = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'accordion__heading__title', ' ' ))]", 
                       xmlValue)
    items <- str_squish(items)
    
    
    ref <- xpathSApply(doc = meeting, 
                         path = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'panel', ' ' ))]", 
                         xmlValue)
    ref <- str_squish(ref)
    
    luk_check <- grep("[[:digit:]]* LUKKET", items)
    if (length(luk_check) >= 1) {
      ref <- ref[-luk_check]
      items <- items[-luk_check]
      }
    
    if(length(items) == 0 & length(ref) == 0){
      dates <- dates[-length(dates)]
      next
    }
    
    #assemble dataframe
    df <- data.frame(
      city = "Ballerup",
      date = ref_date,
      agenda_no = 1:length(items),
      title = items,
      referat = ref,
      stringsAsFactors = F
    )
    
    #put that dataframe into our list of dataframes
    dfs[[length(dfs) + 1]] <- df    
    
  }#End of meeting loop
}#End of page loop
      
      
#put together all dataframes into one big one
out <- as.data.frame(rbindlist(dfs))

#save final data for later
save(out, file = '../data_archive/favrskov_13-17.RData')
      
