## hedensted.R
## Massimo G Losinno
## August 2019
## Scrape Hedensted city council referater

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
if(!dir.exists("../data_archive/hedensted")) dir.create("../data_archive/hedensted_archive")

#list of months to convert dates later
months <- c("januar", "februar", "marts", "april", "maj", "juni", "juli", "august", "september", "oktober", "november", "december")


#Getting the main url link in
master_url <- "https://www.hedensted.dk/politik/dagsordener-og-referater"
master_url <- read_html(master_url)

dates <- "Not a date" #creating a date count to distenguish the first scrape

dfs <- list() #list for dataframes for each meeting

#Creating a list of links to all committees
master_links <- master_url %>% html_nodes('.main-text a') %>% html_attr("href")
master_links <- master_links[-12]
master_links <- master_links[-10]
com_links <- paste0(master_links, "-2014-17")


for(cc in 1:length(com_links)){
  
  print(paste0("Working on committee ", cc, " out of ", length(com_links)))
  
  if(cc == 1){com_links[cc] <- "https://www.hedensted.dk/politik/byraad-og-udvalg/byraad/byraadet-2014-2017"}
  if(cc > 7){com_links[cc] <- sub("-2014-17$", "", com_links[cc])}
  
  ok <- FALSE
  problem <- FALSE
  count <- 1
  
  #Getting a list of years for the chosen committee
  while (ok == FALSE) {
    com_url <- tryCatch({                  
      read_html(com_links[cc])
    },
    error = function(e) {problm <- TRUE
    Sys.sleep(2)
    e
    }
    )
    if ("error" %in% class(com_url)) {
      print(paste("Problem with link", cc))
      count <- count + 1
      
    } else {
      if(problem == TRUE ){print(paste("Problem with", cc, "fixed"))}
      ok <- TRUE
    }
    if(count == 10){
      break
    }
    
  }


meet_links <- com_url %>% html_nodes('.search__filter__soptions a') %>% html_attr("href")

meet_dates <- com_url %>% html_nodes('.search__filter__soptions a') %>% html_text(trim = T) 
meet_dates <- gsub("[.]", "", str_extract(meet_dates, "^[[:digit:]]*[.] [[:alpha:]]* [[:digit:]]*"))

if(cc > 7){
  
  date_check <- grep("2018|2019", meet_dates)
meet_links <- meet_links[-date_check]  
meet_dates <- meet_dates[-date_check]  
}


for (mc in 1:length(meet_links)) {
  
  date <- meet_dates[mc] 

  month <- str_extract(date,"[[:digit:]]* [[:alpha:]]*")
  month <- sub("[[:digit:]]* ", "", month)
  month <- grep(month, months)
  if(month < 10){month <- paste0("0", month)}
  if(nchar(str_extract(date, "^[[:digit:]]*")) == 1){date <- paste0("0", date)}
  date <- sub(" [[:alpha:]]* " , paste0("-", month, "-"), date)
  
  dates <- str_c(c(dates, date))
  date <- make.unique(dates, "-")[length(dates)]
  ref_date <- dates[length(dates)]
  
  
  file.name <- paste0("../data_archive/hedensted_archive/", date, ".RData")
  
  
  #Check if the file already exists for the meeting
  if (file.exists(file.name)){
    #if archived file exists, load it instead of downloading again
    load(file.name)
  } else {

    junior_url <- meet_links[mc]
    
    ok <- FALSE
    problem <- FALSE
    count <- 1
    
    #Reading the html for the meeting
    while (ok == FALSE) {
      meeting <- tryCatch({                  
        getURL(junior_url)
      },
      error = function(e) {problm <- TRUE
      Sys.sleep(2)
      e
      }
      )
      if ("error" %in% class(meeting)) {
        print(paste("Problem with link", mc))
        count <- count + 1
        
      } else {
        if(problem == TRUE ){print(paste("Problem with", mc, "fixed"))}
        ok <- TRUE
      }
      if(count == 5){
        break
      }}
    
    if(count == 5){
      print(paste("Skipping meeting", mc, "in committee", cc))
      next
    }
    
    
    save(meeting, file = file.name)
  }
  
  
  meeting_url <- read_html(meeting) 
  
  
  
  items <- meeting_url %>% html_nodes('.title') %>% html_text(trim = T) 
  
  ref <- meeting_url %>% html_nodes('.extended') %>% html_text(trim = T)
  
  if(length(items) == 0 & length(ref) == 0){
    print(paste("Skipping meeting", mc, "in committee", cc))
    next
  }
  
  ref <- str_squish(ref)
  items <- str_squish(items)
  
  
  df <- data.frame(
    city = "Hedensted",
    date = ref_date,
    agenda_no = 1:length(items),
    title = items,
    referat = ref,
    stringsAsFactors = F
  )
  
  #put that dataframe into our list of dataframes
  dfs[[length(dfs) + 1]] <- df 
  
  
 }#End of meeting loop
}#End of committee loop
  



#put together all dataframes into one big one
out <- as.data.frame(rbindlist(dfs))

#save final data for later
save(out, file = '../data_archive/hedensted_14-17.RData')

