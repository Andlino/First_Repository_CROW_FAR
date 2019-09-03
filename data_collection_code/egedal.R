
## egedal.R
## Massimo G Losinno
## July 2019
## Scrape Egedal city council referater

# rm(list = ls())
# setwd("C:/Users/musse/Aarhus universitet/Tim Dennis Runck - CROW_FAR/First_Repository_CROW_FAR/data_collection_code")

library(rvest)
library(RCurl)
library(stringr)
library(data.table)
library(XML)


# Create directories to save files if don't exist
if(!dir.exists("../data_archive")) dir.create("../data_archive")
if(!dir.exists("../data_archive/egedal_archive")) dir.create("../data_archive/egedal_archive")


#Getting the main url link in
com_links <- c("www.egedalkommune.dk/polweb/firstagenda/committee_158082/agendas.html", "www.egedalkommune.dk/polweb/firstagenda/committee_160204/agendas.html", "www.egedalkommune.dk/polweb/firstagenda/committee_159489/agendas.html", "www.egedalkommune.dk/polweb/firstagenda/committee_187354/agendas.html", "www.egedalkommune.dk/polweb/firstagenda/committee_78358/agendas.html", "www.egedalkommune.dk/polweb/firstagenda/committee_187683/agendas.html", "http://www.egedalkommune.dk/polweb/firstagenda/committee_78169/agendas.html", "www.egedalkommune.dk/polweb/firstagenda/committee_78360/agendas.html", "www.egedalkommune.dk/polweb/firstagenda/committee_78170/agendas.html", "www.egedalkommune.dk/polweb/firstagenda/committee_78356/agendas.html", "www.egedalkommune.dk/polweb/firstagenda/committee_78359/agendas.html", "http://www.egedalkommune.dk/polweb/firstagenda/committee_78373/agendas.html")

dates <- "Not a date" #creating a date count to distenguish the first scrape

dfs <- list() #list for dataframes for each meeting



for(cc in 1:length(com_links)){
  
  print(paste0("Working on committee ", cc, " out of ", length(com_links)))
  
  com_url2 <- getURL(com_links[cc])
  com_url <- read_html(com_url2)
  
  meet_links <- com_url %>% html_nodes('a') %>% html_attr("href")
  meet_links <-  paste0(sub("agendas.html$", "", com_links[cc]), meet_links)
  
  meet_dates <- com_url %>% html_nodes('a') %>% html_text(trim = T)
  meet_dates <- str_extract(meet_dates, "^[[:digit:]]*-[[:digit:]]*-[[:digit:]]*")
  
  
  for (mc in 1:length(meet_links)) {
    
    dates <- str_c(c(dates, meet_dates[mc]))
    date <- make.unique(dates, "-")[length(dates)]
    ref_date <- dates[length(dates)]
    
    
    file.name <- paste0("../data_archive/egedal_archive/", date, ".RData")
    
    
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
          getURL(meet_links[mc])
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
          if(problem == TRUE ){print(paste("Problem with", lf, "fixed"))}
          ok <- TRUE
        }
        if(count == 10){
          break
        }
        
      }
      
      
      save(meeting, file = file.name)
    }
  
  
    meet_url <- read_html(meeting)
    
    
    # Extract transcriptions and items
    ref <- meet_url %>% html_nodes('#sagsfremstillingContainer') %>% html_text(trim = T)  
    items <- meet_url %>% html_nodes('h3 a') %>% html_text(trim = T)
    
    if(length(ref) == 0 & length(items) != 0){
    ref <- meet_url %>% html_nodes('b , li , span , h3 a , div') %>% html_text(trim = T)  
    
    ref <- str_squish(ref)
    ref <- paste(ref,collapse=" ")
    ref <- paste0(" ", ref)
    
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
    ref <- unlist(refs)  
    
    }else{
    
    if(length(items) == 0 & length(ref) == 0){next}
    
    item_check <- grep("Lukket|lukket|LUKKET", items)
    if(length(item_check) >= 1){items <- items[-item_check]}
    item_check2 <- grep("^[[:digit:]]*[.]", items)
    items <- items[item_check2]
    }
    
    
    items <- gsub("Ã¸", "ø", items)
    items <- gsub("Ã¦", "æ", items)
    items <- gsub("Ã¥", "å", items)
    items <- gsub("Ã…", "Å", items)
    items <- gsub("Ã˜", "Ø", items)
    items <- gsub("Ã†", "Æ", items)
    
    ref <- gsub("Ã¸", "ø", ref)
    ref <- gsub("Ã¦", "æ", ref)
    ref <- gsub("Ã¥", "å", ref)
    ref <- gsub("Ã…", "Å", ref)
    ref <- gsub("Ã˜", "Ø", ref)
    ref <- gsub("Ã†", "Æ", ref)
    
    ref <- str_squish(ref)
    
    df <- data.frame(
      city = "Egedal",
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
save(out, file = '../data_archive/egedal_14-17.RData')
