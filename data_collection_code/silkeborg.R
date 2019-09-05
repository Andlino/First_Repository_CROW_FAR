

## silkeborg.R
## Massimo G Losinno
## July 2019
## Scrape Silkeborg city council referater

# rm(list = ls())
# setwd("C:/Users/musse/Aarhus universitet/Tim Dennis Runck - CROW_FAR/First_Repository_CROW_FAR/data_collection_code")

library(rvest)
library(RCurl)
library(stringr)
library(data.table)
library(XML)
library(pdftools)



# Create directories to save files if don't exist
if(!dir.exists("../data_archive")) dir.create("../data_archive")
if(!dir.exists("../data_archive/silkeborg_archive")) dir.create("../data_archive/silkeborg_archive")

#set base url for looping -- this is more of a 'start' page for this task
start_url <- "https://silkeborg.dk/Politik/Dagsordener-og-beslutninger-2019/Dagsordener-og-beslutninger-"

years <- c("2017", "2016", "2015")


months <- c("januar", "februar", "marts", "april", "maj", "juni", "juli", "august", "september", "oktober", "november", "december")


dfs <- list() #list for dataframes for each meeting
dates <- "Not a date" #creating a date count to distenguish the first scrape


for (yc in 1:length(years)) {
  
  print(paste0("Working on year ", years[yc]))
  
  main_url <- paste0(start_url, years[yc])
  
  
  #Creating the url to get links for each of the meetings in the year yc
  main_url <- read_html(main_url)
  com_links <- main_url %>% html_nodes('#layout-main a') %>% html_attr("href")
  com_links <- paste0("https://silkeborg.dk", com_links)
  
  
  for(cl in 1:length(com_links)){
    
    print(paste0("Working on com nr: ", cl, " / ", length(com_links)))
    
    com_url <- read_html(com_links[cl])
    
    #Grapping links for each meeting
    meeting_links <- com_url %>% html_nodes("#layout-main a") %>% html_attr("href")
    meeting_links <- paste0("https://www.silkeborg.dk", meeting_links)
    
    #Getting dates for each meeting
    meeting_dates <- com_url %>% html_nodes("#layout-main a") %>% html_text(trim = T)
    
    meeting_dates <- str_extract(meeting_dates,"^[[:digit:]]*[[:punct:]] [[:alnum:]]* [[:digit:]]*")
    meeting_dates <- sub("[[:punct:]]", "", meeting_dates)
    
    
    for(mc in 1:length(meeting_links)){
      
      print(paste0("Working on meeting nr: ", mc, " / ", length(meeting_links)))
     
      
      #Creating the url to get links for each of the meeting
      meeting_url <- read_html(meeting_links[mc])
      
      items <- meeting_url %>% html_nodes(".ESDH-agenda-bullets strong") %>% html_text(trim = T)
      
      if(length(items) >= 1){
        nos <- 1:length(items)
        
       
      #get date for the specific meeting
      date <- meeting_dates[mc]
      month <- str_extract(date,"[[:digit:]]* [[:alpha:]]*")
      month <- sub("[[:digit:]]* ", "", month)
      month <- grep(month, months)
      if(month < 10){month <- paste0("0", month)}
      date <- sub(" [[:alpha:]]* " , paste0("-", month, "-"), date)
      dates <- str_c(c(dates, date))
      date <- make.unique(dates, "-")[length(dates)]
      ref_date <- dates[length(dates)]
    

      file.name <- paste0("../data_archive/silkeborg_archive/", date, ".PDF")
  
      
      #if archived file exists, load it instead of downloading again
      if (!file.exists(file.name)){

        pdf_link <- meeting_url %>% html_nodes("#phmain_1_phcentercontent_0_hlAgenda") %>% html_attr("href")
        pdf_link <- paste0("https://www.silkeborg.dk", pdf_link)
        
        download.file(pdf_link, 
                      destfile = file.name, 
                      method = "auto", quiet = T, mode = "wb",
                      cacheOK = TRUE, extra = getOption("download.file.extra"))
      }
      
      
      agenda <- pdf_text(file.name)
      agenda <- paste(agenda,collapse="")
      ref <- str_squish(agenda)
      ref <- gsub("[[:punct:]]Offentlig[[:punct:]] ", "", ref)
      ref <- gsub("[[:punct:]]Fortrolig[[:punct:]] ", "", ref)
      
      items2 <- paste(nos, items)
      ref <- str_extract(ref, paste0("Side ", "[[:digit:]] ", items2[1], ".*"))
      ref <- sub(paste0("Side ", "[[:digit:]] "), "", ref)
      
            
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
      
      
      
      #assemble dataframe
      df <- data.frame(
        city = "Silkeborg",
        date = ref_date,
        agenda_no = nos,
        title = items,
        referat = refs,
        stringsAsFactors = F
      )
      
      #put that dataframe into our list of dataframes
      dfs[[length(dfs) + 1]] <- df 
          
     
  }}
  
  }
    
}


#put together all dataframes into one big one
out <- as.data.frame(rbindlist(dfs))

#save final data for later
save(out, file = '../data_archive/silkeborg_15-17.RData')




