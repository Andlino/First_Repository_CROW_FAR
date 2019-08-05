## holbaek.R
## Massimo G Losinno
## August 2019
## Scrape Holbæk city council referater

# rm(list = ls())
# setwd("C:/Users/musse/Aarhus universitet/Tim Dennis Runck - CROW_FAR/First_Repository_CROW_FAR/data_collection_code")


library(rvest)
library(RCurl)
library(stringr)
library(data.table)
library(XML)




# Create directories to save files if don't exist
if(!dir.exists("../data_archive")) dir.create("../data_archive")
if(!dir.exists("../data_archive/holbaek_archive")) dir.create("../data_archive/holbaek_archive")

#set base url for looping -- this is more of a 'start' page for this task
first_url <- "https://holbaek.dk/politik/referater-2014-2017/"


count <- 1
dfs <- list() #list for dataframes for each meeting
dates <- "Not a date" #creating a date count to distenguish the first scrape

year <- c("17", "16", "15", "14")


#Creating the url to get links for each of the meetings
first_url <- read_html(first_url)


#Getting the links and dates for the meetings from the city council 
com_links <- first_url %>% html_nodes('.calltoactiontext a') %>% html_attr("href")


for(cc in 1:length(com_links)){
  
  print(paste0("Working on committee ", cc, " out of ", length(com_links)))
  
  com_url <- read_html(com_links[cc])
    
  meet_links <- com_url %>% html_nodes('a') %>% html_attr("href")  
    
  meet_dates <- com_url %>% html_nodes('a') %>% html_text(trim = T)
  meet_dates <- sub(" [[:digit:]]*:[[:digit:]]*:[[:digit:]]*$", "", meet_dates)
  
  for (mc in 1:length(meet_links)) {
    
    print(paste("Working on meeting", mc, "out of", length(meet_links)))
  
  
     dates <- str_c(c(dates, meet_dates[mc]))
     date <- make.unique(dates, "-")[length(dates)]
     ref_date <- dates[length(dates)]
     
     file.name <- paste0("../data_archive/holbaek_archive/", date, ".RData")

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
           getURL(sub("agendas[.]html", meet_links[mc], com_links[cc]), encoding="UTF16")
         },
         error = function(e) {problm <- TRUE
         Sys.sleep(2)
         e
         }
         )
         if ("error" %in% class(meeting)) {
           print(paste("Problem with link", lc))
           count <- count + 1
           
         } else {
           if(problem == TRUE ){print(paste("Problem with", lc, "fixed"))}
           ok <- TRUE
         }
         if(count == 10){
           break
         } }
       save(meeting, file = file.name) }
        
       
       meet_url <- read_html(meeting)
       
       #Get agenda items
       items <- meet_url %>% html_nodes('body > a') %>% html_text(trim = T) 
       items <- items[grep("^[[:digit:]]*[.] ", items)]   
         
       if(length(items) >= 1){
         #Creating list of number for items
         nos <- 1:length(items)
         
         #Get transcriptions
         ref <- meet_url %>% html_nodes('body :nth-child(1)') %>% html_text(trim = T)
         ref <- ref[-grep("^$", ref)]
         ref <- paste(ref,collapse=" ")
         
         
         refs <- list()
         for(rc in 1:length(items)){
           if(rc < length(items)){
             
             if(grepl("[(]", items[rc]) == T & grepl("[)]", items[rc]) == F){items[rc] <- sub("[(].*", "", items[rc])}
             if(grepl("[(]", items[rc + 1]) == T & grepl("[)]", items[rc + 1]) == F){items[rc + 1] <- sub("[(].*", "", items[rc + 1])}
               
               if(grepl("[)]", items[rc]) == T & grepl("[(]", items[rc]) == F){items[rc] <- sub("[)].*", "", items[rc])}
               if(grepl("[)]", items[rc + 1]) == T & grepl("[(]", items[rc + 1]) == F){items[rc + 1] <- sub("[)].*", "", items[rc + 1])}
             
             temp_ref    <- str_extract(ref, paste("(?=", items[rc],"?).*", items[rc + 1]))
             temp_ref <- sub(items[rc + 1], "", temp_ref)
             refs[[rc]] <- temp_ref
           }else{
             refs[[rc]] <- str_extract(ref, paste("(?=", items[rc],"?).*"))}
         }
         refs <- unlist(refs)        
         
         refs <- gsub("Ã¸", "ø", refs)
         refs <- gsub("Ã~", "Ø", refs)
         refs <- gsub("Ã¥", "å", refs)
         refs <- gsub("Ã.", "Å", refs)
         refs <- gsub("Ã©", "æ", refs)
         refs <- gsub("Ã¦", "æ", refs)
         
         
         items <- gsub("Ã¸", "ø", items)
         items <- gsub("Ã~", "Ø", items)
         items <- gsub("Ã¥", "å", items)
         items <- gsub("Ã.", "Å", items)
         items <- gsub("Ã©", "æ", items)
         items <- gsub("Ã¦", "æ", items)
         
         
         
          #assemble dataframe
         df <- data.frame(
           city = "Holbæk",
           date = ref_date,
           agenda_no = nos,
           title = items,
           referat = refs,
           stringsAsFactors = F
         )
         
         #put that dataframe into our list of dataframes
         dfs[[length(dfs) + 1]] <- df}
       
       
     }}

     
     #put together all dataframes into one big one
     out <- as.data.frame(rbindlist(dfs))
     
     #save final data for later
     save(out, file = '../data_archive/holbaek_14-17.RData')
     