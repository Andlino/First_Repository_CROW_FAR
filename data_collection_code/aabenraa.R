## aabenraa.R
## Massimo G Losinno
## August 2019
## Scrape Aabenraa city council referater

# rm(list = ls())
# setwd("C:/Users/musse/Aarhus universitet/Tim Dennis Runck - CROW_FAR/First_Repository_CROW_FAR/data_collection_code")

library(rvest)
library(RCurl)
library(stringr)
library(data.table)
library(XML)


# Create directories to save files if don't exist
if(!dir.exists("../data_archive")) dir.create("../data_archive")
if(!dir.exists("../data_archive/aabenraa_archive")) dir.create("../data_archive/aabenraa_archive")

#set base url for looping -- this is more of a 'start' page for this task
start_url <- "https://www.aabenraa.dk/politik-og-dialog/dagsordener-og-referater/"

dfs <- list() #list for dataframes for each meeting
dates <- "Not a date" #creating a date count to distenguish the first scrape

months <- c("januar", "februar", "marts", "april", "maj", "juni", "juli", "august", "september", "oktober", "november", "december")

start_url <- read_html(start_url)

com_links <- start_url %>% html_nodes('p+ p a:nth-child(1)') %>% html_attr("href")

com_links <- paste0("https://www.aabenraa.dk", com_links)


for(cc in 1:length(com_links)){
  
  print(paste0("Working on committee ", cc, " out of ", length(com_links)))
  
  ok <- FALSE
  problem <- FALSE
  count <- 1
  
  #Getting a list of years for the chosen committee
  while (ok == FALSE) {
    meet_url <- tryCatch({                  
      read_html(com_links[cc])
    },
    error = function(e) {problm <- TRUE
    Sys.sleep(2)
    e
    })
    if ("error" %in% class(meet_url)) {
      print(paste("Problem with link", cc))
      count <- count + 1
      } else {
      if(problem == TRUE ){print(paste("Problem with", cc, "fixed"))}
      ok <- TRUE
    }
    if(count == 10){
      break}}
  

meet_links <- meet_url %>% html_nodes('.answer a') %>% html_attr("href")
meet_dates <- meet_url %>% html_nodes('.answer a') %>% html_text(trim = T) 

checker <- grep("2018|2019", meet_dates)
if(length(checker) >= 1){
  meet_links <- meet_links[-grep("2018|2019", meet_dates)]
  meet_dates <- meet_dates[-grep("2018|2019", meet_dates)]
  if(length(meet_links) == 0){
    print(paste0("Nothing before 2018 in com ", cc))
    next}}

meet_links <- paste0("https://www.aabenraa.dk", meet_links)  
meet_dates <- sub("[.]", "", meet_dates)

mc_seq <- seq(from = 0, to = length(meet_links), by = 5)

 for (mc in 1:length(meet_links)) {
  
   print_check <- grep(paste0("^", mc, "$"), mc_seq)
   if(length(print_check) == 1){  print(paste0("Working on meeting ", mc, " out of ", length(meet_links)))}
     
  
  date  <- meet_dates[mc]
  
  month <- str_extract(date,"[[:digit:]]* [[:alpha:]]*")
  month <- sub("[[:digit:]]* ", "", month)
  month <- grep(month, months)
  if(month < 10){month <- paste0("0", month)}
  if(as.numeric(str_extract(date, "^[[:digit:]]*")) < 10){date <- paste0("0", date)}
  date <- sub(" [[:alpha:]]* " , paste0("-", month, "-"), date)
  
  dates <- str_c(c(dates, date))
  date <- make.unique(dates, "-")[length(dates)]
  ref_date <- dates[length(dates)]
  
  file.name <- paste0("../data_archive/aabenraa_archive/", date, ".RData")
  
  if (file.exists(file.name)){
    #if archived file exists, load it instead of downloading again
    load(file.name)
    
  } else {
    
    ok <- FALSE
    problem <- FALSE
    count <- 1
    
    #Reading the html for the meeting
    while (ok == FALSE) {
      html_link <- tryCatch({                  
        read_html(meet_links[mc])
      },
      error = function(e) {problm <- TRUE
      Sys.sleep(2)
      e
      }
      )
      if ("error" %in% class(html_link)) {
        print(paste("Problem with link", mc))
        count <- count + 1
        
      } else {
        if(problem == TRUE ){print(paste("Problem with", mc, "fixed"))}
        ok <- TRUE
      }
      if(count == 10){
        break
      }}
    
    meeting <- htmlParse(html_link)
    #Saving the xml for the meeting
    meeting <- saveXML(meeting)
    save(meeting, file = file.name)
  }

  meeting <- htmlParse(meeting)
  
  ref <- xpathSApply(doc = meeting, 
                     path = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'answer', ' ' ))]", 
                     xmlValue)
  
  items <- xpathSApply(doc = meeting, 
                     path = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'question', ' ' ))]//span", 
                     xmlValue)
  
  ref <- str_squish(ref)
  items <- str_squish(items)

  
  #assemble dataframe
  df <- data.frame(
      city = "Aabenraa",
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
save(out, file = '../data_archive/aabenraa_12-17.RData')

