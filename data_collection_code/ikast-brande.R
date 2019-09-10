## ikast-brande.R
## Massimo G Losinno
## September 2019
## Scrape Ikast-Brande city council referater

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
if(!dir.exists("../data_archive/ikast-brande_archive")) dir.create("../data_archive/ikast-brande_archive")

#set base url for looping -- this is more of a 'start' page for this task
start_url <- "http://esdh.ikast-brande.dk/ProFile/Udvalgsbehandling/Webudvalg/ProfileWebMeeting.nsf/AllAgendas"

#list of months to convert dates later
months <- c("januar", "februar", "marts", "april", "maj", "juni", "juli", "august", "september", "oktober", "november", "december")


dfs <- list() #list for dataframes for each meeting
dates <- "Not a date" #creating a date count to distenguish the first scrape

start_url <- read_html(start_url)

com_links <- start_url %>% html_nodes('a') %>% html_attr("href")
com_links <- com_links[-1:-2]
com_links <- paste0("http://esdh.ikast-brande.dk", com_links)


for(cc in 1:length(com_links)){
  
  print(paste0("Working on committee ", cc, " out of ", length(com_links)))
  
  ok <- FALSE
  problem <- FALSE
  count <- 1
  
  #Getting a list of years for the chosen committee
  while (ok == FALSE) {
    year_url <- tryCatch({                  
      read_html(com_links[cc])
    },
    error = function(e) {problm <- TRUE
    Sys.sleep(2)
    e
    }
    )
    if ("error" %in% class(year_url)) {
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
  
  year_check <- year_url %>% html_nodes('td') %>% html_text(trim = T)
  years_links <- year_url %>% html_nodes('a') %>% html_attr("href")
  
  years_links <- years_links[grep(paste0(str_extract(com_links[cc], "Expand=[[:digit:]]*"), "[.]"), years_links)]
  
  year_check <- unique(str_extract(year_check, "[[:digit:]][[:digit:]][[:digit:]][[:digit:]]"))[-1]
  year_check <- grep("2019|2018", year_check)
  if(length(year_check)  >= 1){years_links <- years_links[-year_check]
  
  if(length(years_links) == 0){
    print(paste0("Nothing before 2018 in com ", cc))
    next}}
  
  years_links <- paste0("http://esdh.ikast-brande.dk", years_links)
  
  
  for(yc in 1:length(years_links)){
    
    print(paste("Working on year", yc, "out of", length(years_links)))
    
    ok <- FALSE
    problem <- FALSE
    count <- 1
    
    #Reading the html for the first meeting
    while (ok == FALSE) {
      com_url <- tryCatch({                  
        read_html(years_links[yc])
      },
      error = function(e) {problm <- TRUE
      Sys.sleep(2)
      e
      }
      )
      if ("error" %in% class(com_url)) {
        print(paste("Problem with link", yc))
        count <- count + 1
        
      } else {
        if(problem == TRUE ){print(paste("Problem with", yc, "fixed"))}
        ok <- TRUE
      }
      if(count == 10){
        break
      }
      
    }
  
    
    meet_dates <- com_url %>% html_nodes('.txt') %>% html_text(trim = T) 
    meet_dates <- sub("[.]", "", str_extract(meet_dates, "[[:digit:]]*[.] [[:alpha:]]* [[:digit:]]*$"))
    
    
    meet_links <- com_url %>% html_nodes('.txt') %>% html_attr("href")
    meet_links <- paste0("http://esdh.ikast-brande.dk", meet_links)

    
    for (mc in 1:length(meet_links)) {
      
      
      date <- meet_dates[mc] 
      
      month <- str_extract(date,"[[:digit:]]* [[:alpha:]]*")
      month <- sub("[[:digit:]]* ", "", month)
      month <- grep(month, months, ignore.case = T)
      if(month < 10){month <- paste0("0", month)}
      if(nchar(str_extract(date, "^[[:digit:]]*")) == 1){date <- paste0("0", date)}
      date <- sub(" [[:alpha:]]* " , paste0("-", month, "-"), date)
      
      dates <- str_c(c(dates, date))
      date <- make.unique(dates, "-")[length(dates)]
      ref_date <- dates[length(dates)]
      
      
      
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
        
        
      
      
      
      meeting_url <- read_html(meeting) 
      
      
      
      items <- meeting_url %>% html_nodes('a') %>% html_text(trim = T) 
      
      if(length(items) == 0){
        print(paste("Skipping meeting", mc, "in committee", cc))
        next
      }
      
      items <- sub("^[[:digit:]]*[[:punct:]] *", "", items)
      items <- sub(" [(][[:alpha:]]*[)]$", "", items)
      
      item_links <- meeting_url %>% html_nodes('a') %>% html_attr("href")
      item_links <- paste0("http://esdh.ikast-brande.dk", item_links)
      
      refs <- list()

      for (ic in 1:length(item_links)) {
      
      
        file.name <- paste0("../data_archive/ikast-brande_archive/", date, " - item ", ic, ".RData")
      
      
        #Check if the file already exists for the meeting
      if (file.exists(file.name)){
        #if archived file exists, load it instead of downloading again
        load(file.name)
      } else {
      
        ok <- FALSE
        problem <- FALSE
        count <- 1
        
        #Reading the html for the meeting
        while (ok == FALSE) {
          item <- tryCatch({                  
            getURL(item_links[ic])
          },
          error = function(e) {problm <- TRUE
          Sys.sleep(2)
          e
          }
          )
          if ("error" %in% class(item)) {
            print(paste("Problem with link", mc))
            count <- count + 1
            
          } else {
            if(problem == TRUE ){print(paste("Problem with", ic, "fixed"))}
            ok <- TRUE
          }
          if(count == 5){
            break
          }}
        
        if(count == 5){
          print(paste("Skipping item", ic))
          next
        }    
  
      save(item, file = file.name)
      }
        
        item_url <- read_html(item) 
        
        ref <- item_url %>% html_nodes('.MsoNormalTable p') %>% html_text(trim = T) 
        ref <- paste(str_squish(ref),collapse=" ")
        refs[[length(refs) + 1]] <- gsub("[(]Åben[)]", "", ref)
        
      }#End of item loop
        
       refs <- unlist(refs) 
       
       items <- str_squish(items)
       
       
       #assemble dataframe
       df <- data.frame(
         city = "Ikast-Brande",
         date = ref_date,
         agenda_no = 1:length(items),
         title = items,
         referat = refs,
         stringsAsFactors = F
       )
       
       #put that dataframe into our list of dataframes
       dfs[[length(dfs) + 1]] <- df 
       
       
    }#End of meeting loop
  }#End of year loop
}#End of committee loop


#put together all dataframes into one big one
out <- as.data.frame(rbindlist(dfs))

#save final data for later
save(out, file = '../data_archive/ikast-brande_10-17.RData')
        