## koege.R
## Massimo G Losinno
## August 2019
## Scrape Køge city council referater

# rm(list = ls())
# setwd("C:/Users/musse/Aarhus universitet/Tim Dennis Runck - CROW_FAR/First_Repository_CROW_FAR/data_collection_code")

library(rvest)
library(RCurl)
library(stringr)
library(data.table)
library(pdftools)


# Create directories to save files if don't exist
if(!dir.exists("../data_archive")) dir.create("../data_archive")
if(!dir.exists("../data_archive/koege_archive")) dir.create("../data_archive/koege_archive")

#set base url for looping -- this is more of a 'start' page for this task
start_url <- "https://www.koege.dk/byraad-udvalg/Dagsordener-og-referater.aspx"


dfs <- list() #list for dataframes for each meeting
dates <- "Not a date" #creating a date count to distenguish the first scrape

start_url <- read_html(start_url)

com_links <- start_url %>% html_nodes('#ctl00_cphPageContentMain_pagecontentmain_1_txtText a') %>% html_attr("href")
com_links <- paste0("https://www.koege.dk", com_links)
com_links2 <- com_links[11]
com_links <- com_links[-10:-11]

second_url <- read_html(com_links2)
com_links2 <- second_url %>% html_nodes('#ctl00_cphPageContentMain_pagecontentmain_1_txtText a') %>% html_attr("href")
com_links <- c(com_links, paste0("https://www.koege.dk", com_links2))

for(cc in 1:length(com_links)){
  
  print(paste0("Working on committee ", cc, " out of ", length(com_links)))
  

  com_url <- read_html(com_links[cc])
  
  meet_links <- com_url %>% html_nodes('.esdhtitlecell a') %>% html_attr("href")
  meet_dates <- com_url %>% html_nodes('.esdhdatecell a') %>% html_text(trim = T) 
  
  if(length(grep("2018|2019", meet_dates)) >= 1){
  meet_links <- meet_links[-grep("2018|2019", meet_dates)]
  meet_dates <- meet_dates[-grep("2018|2019", meet_dates)]
  }
  meet_links <- paste0("https://www.koege.dk", meet_links)
  
  meet_dates <- str_extract(meet_dates, "^[[:digit:]]*-[[:digit:]]*-[[:digit:]]*")

  
  for(mc in 1:length(meet_links)){
    
    print(paste("Working on meeting", mc, "out of", length(meet_links)))
    
    
    date <- meet_dates[mc] 
    
  
    dates <- str_c(c(dates, date))
    date <- make.unique(dates, "-")[length(dates)]
    ref_date <- dates[length(dates)]
    
    file.name <- paste0("../data_archive/koege_archive/", date, ".RData")
    
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
    
    
    meeting_url <- read_html(meeting) 
    
    items <- meeting_url %>% html_nodes('.esdhOverviewLink') %>% html_text(trim = T) 
    
    if(length(items) >= 1){
    
    ref <- meeting_url %>% html_nodes('h3 , .esdhRightContent') %>% html_text(trim = T)
    ref <- ref[-1:-2]
    
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
    refs <- sub("[[:digit:]]*[.] $", "", refs)
    
    
      df <- data.frame(
      city = "Koege",
      date = ref_date,
      agenda_no = 1:length(items),
      title = items,
      referat = refs,
      stringsAsFactors = F
    )
    
    #put that dataframe into our list of dataframes
    dfs[[length(dfs) + 1]] <- df 
    }else{dates <- dates[-length(dates)]}
    
  }#End of meeting loop
}#End of committee loop


#put together all dataframes into one big one
out <- as.data.frame(rbindlist(dfs))

#save final data for later
save(out, file = '../data_archive/koege_14-17.RData')





    