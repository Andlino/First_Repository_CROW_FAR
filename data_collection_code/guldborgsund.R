## guldborgsund.R
## Massimo G Losinno
## August 2019
## Scrape Guldborgsund city council referater

# rm(list = ls())
# setwd("C:/Users/musse/Aarhus universitet/Tim Dennis Runck - CROW_FAR/First_Repository_CROW_FAR/data_collection_code")

library(rvest)
library(RCurl)
library(stringr)
library(data.table)
library(pdftools)


# Create directories to save files if don't exist
if(!dir.exists("../data_archive")) dir.create("../data_archive")
if(!dir.exists("../data_archive/guldborgsund_archive")) dir.create("../data_archive/guldborgsund_archive")

#set base url for looping -- this is more of a 'start' page for this task
start_url <- "https://www.guldborgsund.dk/da/Dagsorden_og_referater"

years <- as.character(10:17)
months <- c("januar", "februar", "marts", "april", "maj", "juni", "juli", "august", "september", "oktober", "november", "december")

dfs <- list() #list for dataframes for each meeting
dates <- "Not a date" #creating a date count to distenguish the first scrape

start_url <- read_html(start_url)

com_links <- start_url %>% html_nodes('#left-col li a') %>% html_attr("href")
com_links <- com_links[-12:-15]
com_links <- paste0("https://www.guldborgsund.dk", com_links)

for(cc in 1:length(com_links)){
  
  print(paste0("Working on committee ", cc, " out of ", length(com_links)))

  
  for(yc in 1:length(years)){  
    
    print(paste0("Working on year 20", years[yc]))
    
    year_link <-  paste0(com_links[cc], "?year=20", years[yc])
    
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

  
  meet_links <- year_url %>% html_nodes('h3 a') %>% html_attr("href")
  if(length(meet_links) == 0){next}
  
  check <- year_url %>% html_nodes('h3 a') %>% html_text(trim = T) 
  meet_links <- meet_links[grep("Referat", check)]
  meet_links <- paste0("https://www.guldborgsund.dk", meet_links)
  
  meet_dates <- year_url %>% html_nodes('.meetings-date') %>% html_text(trim = T) 
  
  for(mc in 1:length(meet_links)){
    
    print(paste("Working on meeting", mc, "out of", length(meet_links)))
  
    
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
    
    file.name <- paste0("../data_archive/guldborgsund_archive/", date, ".RData")
    
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
          break
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
        
    
    items <- meeting_url %>% html_nodes('.meeting-bullet-link h2') %>% html_text(trim = T) 
    items <- sub("Beslutningssag: ", "", items)
    
    
    ref <- meeting_url %>% html_nodes('#sagsfremstillingContainer') %>% html_text(trim = T)
    if(length(ref) < length(items)){
        
        ref <- meeting_url %>% html_nodes('.paragraf , h2') %>% html_text(trim = T)
        
        ref <- paste(ref,collapse=" ")
        ref <- str_squish(ref)
        
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
        
    }else{
        ref <- sub("^INDSTILLING\n", "", ref)
        refs <- str_squish(ref)
    }
    
    
    
    df <- data.frame(
      city = "Guldborgsund",
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
save(out, file = '../data_archive/guldborgsund_10-17.RData')


    
    
    
    