## syddjurs.R
## Massimo G Losinno
## August 2019
## Scrape Syddjurs city council referater

# rm(list = ls())
# setwd("C:/Users/musse/Aarhus universitet/Tim Dennis Runck - CROW_FAR/First_Repository_CROW_FAR/data_collection_code")

library(rvest)
library(RCurl)
library(stringr)
library(data.table)
library(pdftools)


# Create directories to save files if don't exist
if(!dir.exists("../data_archive")) dir.create("../data_archive")
if(!dir.exists("../data_archive/syddjurs")) dir.create("../data_archive/syddjurs_archive")


dfs <- list() #list for dataframes for each meeting

dates <- "Not a date" #creating a date count to distenguish the first scrape

#Getting the main url link in for the specific dates
master_url <- "https://www.syddjurs.dk/dagsorden-og-referat?from_date%5Bvalue%5D%5Bdate%5D=01-01-2007&to_date%5Bvalue%5D%5Bdate%5D=31-12-2017&os2web_meetings_tax_committee=All"
master_url <- getURL(master_url) 
master_url <- read_html(master_url)

#Specifying how many sites to loop over
limit <- master_url %>% html_nodes('.pager-current') %>% html_text(trim = T)
limit <- as.numeric(str_extract(limit, "[[:digit:]]*$"))

master_url <- "https://www.syddjurs.dk/dagsorden-og-referat?from_date%5Bvalue%5D%5Bdate%5D=01-01-2007&to_date%5Bvalue%5D%5Bdate%5D=31-12-2017&os2web_meetings_tax_committee=All&page="


#Starting main loop over the different pages
for(x in 1:limit) {
  print(paste("Working on page", x, "out of 91"))
  
  url <- paste0(master_url, x)  
  url <- getURL(url) 
  url <- read_html(url)
  
  #Getting the dates, links and names for the meetings of the page
  links <- url %>% html_nodes('.view-filters+ .view-content .views-field-field-os2web-meetings-committee a') %>% html_attr("href")
  links <- paste0("https://www.syddjurs.dk", links)
  
  meet_dates <- url %>% html_nodes('.view-filters+ .view-content .date-display-single') %>% html_text(trim = T)
  meet_dates <- sub("[[:space:]][[:digit:]]*[[:punct:]][[:digit:]]*", "", meet_dates)
  
  
  
  #Starting secondary loop over the different meetings
  for (y in 1:length(links)) {

    dates <- str_c(c(dates, meet_dates[y]))
    date <- make.unique(dates, "-")[length(dates)]
    ref_date <- dates[length(dates)]
    
    
    file.name <- paste0("../data_archive/syddjurs_archive/", date, ".RData")
    

        if (file.exists(file.name)){
      #if archived file exists, load it instead of downloading again
      load(file.name)
      
    } else {

      ok <- FALSE
      problem <- FALSE
      count <- 1
      
      #Reading the html for the first meeting
      while (ok == FALSE) {
        meeting <- tryCatch({                  
          getURL(links[y])
        },
        error = function(e) {problm <- TRUE
        Sys.sleep(2)
        e
        }
        )
        if ("error" %in% class(meeting)) {
          print(paste("Problem with link", y))
          count <- count + 1
          
        } else {
          if(problem == TRUE ){print(paste("Problem with", y, "fixed"))}
          ok <- TRUE
        }
        if(count == 5){
          break
        }
        
      }
      
      if(count == 5){
        print(paste("Skipping meeting", y, "on page", x))
        next}
      
      save(meeting, file = file.name)
    }
    
    
    meet_url <- tryCatch({                  
        read_html(meeting) 
    },
    error = function(e) {problm <- TRUE
    Sys.sleep(2)
    e
    }
    )
    if ("error" %in% class(meet_url)) {
        meet_url <- read_html(meeting, options = "HUGE")}    
    
    ref <- meet_url %>% html_nodes('.views-field-title') %>% html_text(trim = T)
    items <- meet_url %>% html_nodes('.agenda-bullet-list') %>% html_text(trim = T)
    
    if(length(items) == 0 & length(ref) == 0){
      print(paste("Skipping meeting", y, "on page", x))
      next}
    
    item_check <- grep("Lukket punkt|LUKKET", items)
    if(length(item_check) >= 1){
      items <- items[-item_check]
      ref <- ref[-item_check]
    
      if(length(items) == 0 & length(ref) == 0){
        print(paste("Skipping meeting", y, "on page", x))
        next}
      }
    
    ref <- str_squish(ref)
    
    ref <- sub("^Punkt [[:digit:]]* ", "", ref)
    items <- sub("^Punkt [[:digit:]]* ", "", items)
    
    df <- data.frame(
      city = "Syddjurs",
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
save(out, file = '../data_archive/syddjurs_11-17.RData')
  
  