## hvidovre_14-17.R
## Massimo G Losinno
## August 2019
## Scrape Hvidovre city council referater

# rm(list = ls())
# setwd("C:/Users/musse/Aarhus universitet/Tim Dennis Runck - CROW_FAR/First_Repository_CROW_FAR/data_collection_code")

library(rvest)
library(RCurl)
library(stringr)
library(data.table)
library(XML)


# Create directories to save files if don't exist
if(!dir.exists("../data_archive")) dir.create("../data_archive")
if(!dir.exists("../data_archive/hvidovre_archive")) dir.create("../data_archive/hvidovre_archive")

#set base url for looping -- this is more of a 'start' page for this task
start_url <- "https://hvidovre.dk/Politik/dagsordener-og-referater-fra-kommunalbestyrelsen-og-udvalg/politiske-udvalg-2014-17"

dfs <- list() #list for dataframes for each meeting
dates <- "Not a date" #creating a date count to distenguish the first scrape
years <- c("2014", "2015", "2016", "2017")
months <- c("januar", "februar", "marts", "april", "maj", "juni", "juli", "august", "september", "oktober", "november", "december")

start_url <- read_html(start_url)

com_links <- start_url %>% html_nodes('.linkBoxHeader a') %>% html_attr("href")
com_links <- paste0("https://hvidovre.dk", com_links)


for(cc in 1:length(com_links)){
  
  print(paste0("Working on committee ", cc, " out of ", length(com_links)))
 
  
  for(yc in 1:length(years)){  
    
    print(paste0("Working on year ", years[yc]))
    
    year_link <-  paste0(com_links[cc], "?y=", years[yc])
  
  
    ok <- FALSE
    problem <- FALSE
    count <- 1
    
    #Getting a list of years for the chosen committee
    while (ok == FALSE) {
      year_url <- tryCatch({                  
        read_html(year_link)
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
      }}
    
    
    meet_links <- year_url %>% html_nodes('.esdhtitlecell a') %>% html_attr("href")

    if(length(meet_links) == 0){next}
    
    meet_dates <- year_url %>% html_nodes('.esdhtitlecell a') %>% html_text(trim = T) 
    meet_links <- meet_links[grep("Referat", meet_dates)]
    meet_links <- paste0("https://hvidovre.dk", meet_links)
    
    for(mc in 1:length(meet_links)){
      
      #print(paste("Working on meeting", mc, "out of", length(meet_links)))
      
      date <- meet_dates[mc] 
      date <- sub("[.]", "", str_extract(date, "[[:digit:]]*[.] [[:alpha:]]* [[:digit:]]*$"))
      
      month <- str_extract(date,"[[:digit:]]* [[:alpha:]]*")
      month <- sub("[[:digit:]]* ", "", month)
      month <- grep(month, months)
      if(month < 10){month <- paste0("0", month)}
      if(as.numeric(str_extract(date, "^[[:digit:]]*")) < 10){date <- paste0("0", date)}
      date <- sub(" [[:alpha:]]* " , paste0("-", month, "-"), date)
      
      dates <- str_c(c(dates, date))
      date <- make.unique(dates, "-")[length(dates)]
      ref_date <- dates[length(dates)]
      
      file.name <- paste0("../data_archive/hvidovre_archive/", date, ".RData")
      
      if (file.exists(file.name)){
        #if archived file exists, load it instead of downloading again
        load(file.name)
        
      } else {
        
        ok <- FALSE
        problem <- FALSE
        count <- 1
        
        #Reading the html for the meeting
        while (ok == FALSE) {
          meeting <- tryCatch({                  
            getURL(meet_links[mc])
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
          if(count == 10){
            print(paste("Skipping meeting", mc, years[yc]))
            next
          }}
        
        
        save(meeting, file = file.name)
      }
    
      meeting_url <- tryCatch({                  
        read_html(meeting) 
      },
      error = function(e) {problm <- TRUE
      Sys.sleep(2)
      e
      }
      )
      if ("error" %in% class(meeting_url)) {
        meeting_url <- read_html(meeting, options = "HUGE")} 
      
      
      items <- meeting_url %>% html_nodes('.esdhOverviewLink') %>% html_text(trim = T) 
      
      ref <- meeting_url %>% html_nodes('.articleText') %>% html_text(trim = T)
      ref <- str_squish(ref)
      
      ref <- sub(paste("^.*",items[1]), "", ref) 
      ref <- paste0(" ", items[1], ref)
      
      refs <- list()
      for(rc in 1:length(items)){
        if(rc < length(items)){
          
          if(grepl("[(]", items[rc]) == T & grepl("[)]", items[rc]) == F){items[rc] <- sub("[(].*", "", items[rc])}
          if(grepl("[(]", items[rc + 1]) == T & grepl("[)]", items[rc + 1]) == F){items[rc + 1] <- sub("[(].*", "", items[rc + 1])}
          
          temp_ref    <- str_extract(ref, paste("(?=", items[rc],"?).*", items[rc + 1]))
          temp_ref <- sub(items[rc + 1], "", temp_ref)
          refs[[rc]] <- temp_ref
        }else{
          refs[[rc]] <- str_extract(ref, paste("(?=", items[rc],"?).*"))}
      }
      refs <- unlist(refs) 
      
      
      df <- data.frame(
        city = "Hvidovre",
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
save(out, file = '../data_archive/hvidovre_14-17.RData')
