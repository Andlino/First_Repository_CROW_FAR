## hvidovre_07-13.R
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
library(pdftools)


# Create directories to save files if don't exist
if(!dir.exists("../data_archive")) dir.create("../data_archive")
if(!dir.exists("../data_archive/hvidovre_archive_07-13")) dir.create("../data_archive/hvidovre_archive_07-13")

#set base url for looping -- this is more of a 'start' page for this task
start_url <- "https://hvidovre.dk/referatarkiv/"

dfs <- list() #list for dataframes for each meeting
dates <- "Not a date" #creating a date count to distenguish the first scrape

start_url <- read_html(start_url)

com_links <- start_url %>% html_nodes('br+ a') %>% html_attr("href")
com_links <- paste0("https://hvidovre.dk", com_links)

for(cc in 1:length(com_links)){
  
  print(paste0("Working on committee ", cc, " out of ", length(com_links)))
  
  com_url <- read_html(com_links[cc])

  years_links <- com_url %>% html_nodes('br+ a') %>% html_attr("href")
  
  years_check <- grep("2007|2008|2009|2010|2011|2012|2013", years_links)
  if(length(years_check) == 0){
    print(paste("No meetings in committee", cc))
    next}
  
  years_links <- years_links[grep("2007|2008|2009|2010|2011|2012|2013", years_links)]
  years_links <- paste0("https://hvidovre.dk", years_links)

  years <- str_extract(years_links, "2007|2008|2009|2010|2011|2012|2013")
  
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
    
    
    meet_links <- year_url %>% html_nodes('br+ a') %>% html_attr("href")
    
    if(length(meet_links) == 0){next}
    
    meet_dates <- year_url %>% html_nodes('br+ a') %>% html_text(trim = T) 
    meet_links <- paste0("https://hvidovre.dk", meet_links)
    
    for(mc in 1:length(meet_links)){
      
      #print(paste("Working on meeting", mc, "out of", length(meet_links)))
      
      date <- meet_dates[mc] 
      date <- str_extract(date, "[[:digit:]][[:digit:]][[:digit:]][[:digit:]]$")
      date <- paste0(str_extract(date,"^[[:digit:]][[:digit:]]"), "-", str_extract(date,"[[:digit:]][[:digit:]]$"), "-", years[yc])
      
      dates <- str_c(c(dates, date))
      date <- make.unique(dates, "-")[length(dates)]
      ref_date <- dates[length(dates)]
    
      file.name <- paste0("../data_archive/hvidovre_archive_07-13/", date, ".PDF")
      
      #Check to see if the pdf is already downloaded
      if (!file.exists(file.name)){
        
        ok <- FALSE
        problem <- FALSE
        count <- 1
        
        #Getting a list of years for the chosen committee
        while (ok == FALSE) {
          meet_url <- tryCatch({                  
            read_html(meet_links[mc])
          },
          error = function(e) {problm <- TRUE
          Sys.sleep(2)
          e
          }
          )
          if ("error" %in% class(meet_url)) {
            print(paste("Problem with link", mc))
            count <- count + 1
            
          } else {
            if(problem == TRUE ){print(paste("Problem with", mc, "fixed"))}
            ok <- TRUE
          }
          if(count == 10){
            break
          }}
        
        pdf_link <- meet_url %>% html_nodes('br+ a') %>% html_attr("href")        
        pdf_link <- paste0("https://hvidovre.dk", pdf_link)
       # pdf_checker <- sub("/referatarkiv/", "", pdf_link)
        #pdf_link <- pdf_link[grep("ref", pdf_checker)]
        pdf_link <- pdf_link[grep(paste0("ref", meet_dates[mc] ,".pdf"), pdf_link)]
        if(length(pdf_link) == 0){
          pdf_link <- meet_url %>% html_nodes('br+ a') %>% html_attr("href")        
          pdf_link <- paste0("https://hvidovre.dk", pdf_link)
          pdf_link <- pdf_link[grep(paste0(meet_dates[mc] ,".pdf"), pdf_link)]
        if(length(pdf_link) == 0){next}
        }
        
        
        download.file(pdf_link[1], 
                      destfile = file.name, 
                      method = "auto", quiet = T, mode = "wb",
                      cacheOK = TRUE, extra = getOption("download.file.extra"))
      }  
      
      agenda <- pdf_text(file.name)
      agenda_checker <- paste(agenda ,collapse=" ")
      if(nchar(agenda_checker) < 500){
        print(paste("Meeting", mc, "cant be read"))
        next}
      
      items <- str_extract_all(agenda, "Pkt.nr. [[:digit:]]*")
      items <- unlist(items[2])
      if(length(items) == 0){
        items <- str_extract_all(agenda, "[[:digit:]]*[.] *[[:alpha:]][[:alnum:]]*")
        items <- unlist(items[2])
        items <- str_squish(items)}
      
      if(length(items) == 0){next}
      
      
      ref <- str_squish(agenda)
      ref <- paste(ref,collapse=" ")
      
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
save(out, file = '../data_archive/hvidovre_07-13.RData')
      
    
