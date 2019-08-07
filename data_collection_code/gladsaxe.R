## gladsaxe.R
## Massimo G Losinno
## August 2019
## Scrape Gladsaxe city council referater

# rm(list = ls())
# setwd("C:/Users/musse/Aarhus universitet/Tim Dennis Runck - CROW_FAR/First_Repository_CROW_FAR/data_collection_code")

library(rvest)
library(RCurl)
library(stringr)
library(data.table)
library(pdftools)


# Create directories to save files if don't exist
if(!dir.exists("../data_archive")) dir.create("../data_archive")
if(!dir.exists("../data_archive/gladsaxe_archive")) dir.create("../data_archive/gladsaxe_archive")

#set base url for looping -- this is more of a 'start' page for this task
start_url <- "http://www2.gladsaxe.dk/ProFile/ProfileWebMeeting.nsf/AllAgendas"


dfs <- list() #list for dataframes for each meeting
dates <- "Not a date" #creating a date count to distenguish the first scrape

start_url <- read_html(start_url)

com_links <- start_url %>% html_nodes('a') %>% html_attr("href")
com_links <- com_links[-1:-4]
com_links <- paste0("http://www2.gladsaxe.dk", com_links)


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
  
  
  years_links <- year_url %>% html_nodes('td>a') %>% html_attr("href")

  years_links <- years_links[grep(paste0("Expand=", cc + 1), years_links)]
  
  years_links <- paste0("http://www2.gladsaxe.dk", years_links)
    
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
    
      meet_links <- com_url %>% html_nodes('td+ td a:nth-child(1)') %>% html_attr("href")
      names <- com_url %>% html_nodes('td+ td a:nth-child(1)') %>% html_text(trim = T) 
      
      meet_links <- meet_links[grep("Referat", names)]
      names <- names[grep("^[[:alpha:]]", names)]
      meet_links <- paste0("http://www2.gladsaxe.dk", meet_links)
      
      meet_dates <- com_url %>% html_nodes('.txt') %>% html_text(trim = T) 
      meet_dates <- meet_dates[grep("Referat", names)]
      meet_dates <- gsub("[.]", "-", str_extract(meet_dates, "[[:digit:]]*[.][[:digit:]]*[.][[:digit:]]*$"))
      
      
      for (mc in 1:length(meet_links)) {
      

      date  <- meet_dates[mc]
      if(as.numeric(str_extract(date, "^[[:digit:]]*")) < 10){date <- paste0("0", date)}
      
      dates <- str_c(c(dates, date))
      date <- make.unique(dates, "-")[length(dates)]
      ref_date <- dates[length(dates)]
      
      
      
      ok <- FALSE
      problem <- FALSE
      count <- 1
      
      #Reading the html for the first meeting
      while (ok == FALSE) {
          meeting_url <- tryCatch({                  
              read_html(meet_links[mc])
          },
          error = function(e) {problm <- TRUE
          Sys.sleep(2)
          e
          }
          )
          if ("error" %in% class(meeting_url)) {
              print(paste("Problem with link", mc))
              count <- count + 1
              
          }else {
              if(problem == TRUE ){print(paste("Problem with", yc, "fixed"))}
              ok <- TRUE
          }
          if(count == 10){
              break}}
      
      
      items <- meeting_url %>% html_nodes('a') %>% html_text(trim = T) 
      items <- items[-1]    
      
      items_links  <- meeting_url %>% html_nodes('a') %>% html_attr("href")
      items_links <- items_links[-1]
      check <- grep("pdf$", items_links)
      if(length(check) >= 1){
          items <- items[-grep("pdf$", items_links)]
          items_links <- items_links[-grep("pdf$", items_links)]
           }
      
      refs <- list()
      
      if(length(items_links) >= 1){
          items_links <- paste0("http://www2.gladsaxe.dk", items_links)
          for(ic in 1:length(items_links)){
      
      file.name <- paste0("../data_archive/gladsaxe_archive/", date, " item ", ic, ".RData")
      if (file.exists(file.name)){
          #if archived file exists, load it instead of downloading again
          load(file.name)
      } else {
          
          
          ok <- FALSE
          problem <- FALSE
          count <- 1
          
          #Reading the html for the first meeting
          while (ok == FALSE) {
              item <- tryCatch({                  
                  getURL(items_links[ic])
              },
              error = function(e) {problm <- TRUE
              Sys.sleep(2)
              e
              }
              )
              if ("error" %in% class(item)) {
                  print(paste("Problem with link", ic))
                  count <- count + 1
                  
              } else {
                  if(problem == TRUE ){print(paste("Problem with", ic, "fixed"))}
                  ok <- TRUE
              }
              if(count == 10){
                  break
              }
              
          }
          
          save(item, file = file.name)
      }
      
      item_url <- read_html(item) 
      
      ref <- item_url %>% html_nodes('.MsoNormal') %>% html_text(trim = T)
      ref <- paste(ref,collapse=" ")
      ref <- str_squish(ref)
      refs[[length(refs)+1]] <- ref
      
      }
      
      refs <- unlist(refs)    
      
      #assemble dataframe
      df <- data.frame(
          city = "Gladsaxe",
          date = ref_date,
          agenda_no = 1:length(items),
          title = items,
          referat = refs,
          stringsAsFactors = F
      )
      
      #put that dataframe into our list of dataframes
      dfs[[length(dfs) + 1]] <- df 
      } 
      
      }#End meeting loop
  }#End year loop
}#End committee loop

      



#put together all dataframes into one big one
out <- as.data.frame(rbindlist(dfs))

#save final data for later
save(out, file = '../data_archive/gladsaxe_09-17.RData')

    