## frederikshavn.R
## Massimo G Losinno
## July 2019
## Scrape Frederikshavn city council referater for 2007 - 2016

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
if(!dir.exists("../data_archive/frederikshavn_archive_2007-2016")) dir.create("../data_archive/frederikshavn_archive_2007-2016")


  #set base url for looping -- this is more of a 'start' page for this task
  start_url <- "https://stadsarkiv.frederikshavn.dk/politiske-moedereferater/"


dfs <- list() #list for dataframes for each meeting
dates <- "Not a date" #creating a date count to distenguish the first scrape

start_url <- read_html(start_url)

com_links <- start_url %>% html_nodes('.nav-card__link') %>% html_attr("href")
com_links <- paste0("https://stadsarkiv.frederikshavn.dk", com_links)
second_url <- read_html(com_links[12])

com_links <- com_links[-12:-16]

com_links2 <- second_url %>% html_nodes('.content__rte a') %>% html_attr("href")
com_links2 <- paste0("https://stadsarkiv.frederikshavn.dk", com_links2)

com_links <- c(com_links, com_links2)

com_links <- gsub("ø", "%C3%B8", com_links)
com_links <- gsub("æ", "%C3%A6", com_links)

for(cc in 1:length(com_links)){
  
  print(paste0("Working on committee ", cc, " out of ", length(com_links)))
  
    com_url <- read_html(com_links[cc])
  
    if(cc == 1){com_url <- com_url %>% html_nodes('.content__rte a') %>% html_attr("href")
    com_url <- read_html(paste0("https://stadsarkiv.frederikshavn.dk", com_url[1]))}
    
  
  
  years_links <- com_url %>% html_nodes('.content__rte a') %>% html_attr("href")
  years_links <- paste0("https://stadsarkiv.frederikshavn.dk", years_links)
  
  years_links <- gsub("ø", "%C3%B8", years_links)
  years_links <- gsub("æ", "%C3%A6", years_links)
  
  for(yc in 1:length(years_links)){  
    
    print(paste0("Working on year ", yc, " out of ", length(years_links)))
    
    ok <- FALSE
    problem <- FALSE
    count <- 1
    
    #Getting a list of years for the chosen committee
    while (ok == FALSE) {
      year_url <- tryCatch({                  
        read_html(years_links[yc])
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
    
    
    meet_links <- year_url %>% html_nodes('.list-page__item-content') %>% html_attr("href")
    if(length(meet_links) == 0){next}
    meet_links <- paste0("https://stadsarkiv.frederikshavn.dk", meet_links)
    
    meet_dates <- year_url %>% html_nodes('.document-list__item-inner--title') %>% html_text(trim = T) 
    
    if(length(grep("samlet|med", meet_dates)) >= 1 & length(grep("samlet|med", meet_dates)) != length(meet_dates)){
      meet_links <- meet_links[-grep("samlet|med", meet_dates)]
      meet_dates <- meet_dates[-grep("samlet|med", meet_dates)]
    }
    
    if(length(grep("[[:digit:]]*-[[:digit:]]*-[[:digit:]]*", meet_dates)) != length(meet_dates)){
        meet_links <- meet_links[grep("[[:digit:]]*-[[:digit:]]*-[[:digit:]]*", meet_dates)]
        meet_dates <- meet_dates[grep("[[:digit:]]*-[[:digit:]]*-[[:digit:]]*", meet_dates)]
    }
    
    
        
    meet_name <- meet_dates
    
    meet_dates2 <- str_extract(meet_dates, "[[:digit:]]*-[[:digit:]]*-[[:digit:]]* ")
    
    date_check <- grep("[[:digit:]]", meet_dates2)
    if(length(date_check) == 0){meet_dates2 <- str_extract(meet_dates, "[[:digit:]]*-[[:digit:]]*-[[:digit:]]*")}
    
    na_check <- meet_dates2[is.na(meet_dates2)]
    if(length(na_check) >= 1){
        meet_links <- meet_links[complete.cases(meet_dates2)]
        meet_name <- meet_name[complete.cases(meet_dates2)]
        meet_dates2 <- meet_dates2[complete.cases(meet_dates2)]
    }
            
    for(mc in 1:length(meet_links)){
      
      print(paste("Working on meeting", mc, "out of", length(meet_links)))
      
    date <- meet_dates2[mc]
    date <- str_extract(date, "[[:digit:]]*-[[:digit:]]*-[[:digit:]]*")
    
    if(as.numeric(str_extract(date, "^[[:digit:]]*")) < 100 & as.numeric(str_extract(date, "[[:digit:]]*$")) < 100){
      date <- sub("[[:digit:]]*$", paste0("20", str_extract(date, "[[:digit:]]*$")), date)
    }else{if(as.numeric(str_extract(date, "^[[:digit:]]*")) > 100){
      date_year <- str_extract(date, "^[[:digit:]]*")
      date_month <- str_extract(date, "-[[:digit:]]*-")
      date <- paste0(str_extract(date, "[[:digit:]]*$"), date_month, date_year)
      }}
      
    
    dates <- str_c(c(dates, date))
    date <- make.unique(dates, "-")[length(dates)]
    ref_date <- dates[length(dates)]
    
    file.name <- paste0("../data_archive/frederikshavn_archive_2007-2016/", date, ".PDF")
    
    #Check to see if the pdf is already downloaded
    if (!file.exists(file.name)){
    
      
      download.file(meet_links[mc], 
                    destfile = file.name, 
                    method = "auto", quiet = T, mode = "wb",
                    cacheOK = TRUE, extra = getOption("download.file.extra"))
    }  
    
    ref <- pdf_text(file.name)
    ref <- paste(ref,collapse=" ")
    ref <- str_squish(ref)
    
    check <- grep("[[:alnum:]]", ref)
    if(length(check) == 0){next}
    
    #assemble dataframe
    df <- data.frame(
      city = "Frederikshavn",
      date = ref_date,
      title = meet_name[mc],
      referat = ref,
      stringsAsFactors = F
    )
    
    #put that dataframe into our list of dataframes
    dfs[[length(dfs) + 1]] <- df 
    
    
    
    }#End of meeting loop
  Sys.sleep(3) }#End of year loop
}#End of committee loop  

    
    
#put together all dataframes into one big one
out <- as.data.frame(rbindlist(dfs))

#save final data for later
save(out, file = '../data_archive/frederikshavn_07-16.RData')
