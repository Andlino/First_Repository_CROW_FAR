## frederiksberg.R
## Matt W. Loftis
## July 2019
## Scrape Frederiksberg city council referater

rm(list = ls())
# setwd("C:/Users/musse/Aarhus universitet/Tim Dennis Runck - CROW_FAR/First_Repository_CROW_FAR/data_collection_code")

library(stringr)
library(XML)
library(data.table)
library(curl)
library(pdftools)
library(readtext)

archive.folder <- "../data_archive/copenhagen_archive/"

# get list of downloaded search pages
file.list <- list.files(archive.folder)

name_check <- "not a name"
date_check <- "not a date"
dfs <- list() #list for dataframes for each meeting

# empty list for dataframes
df.after <- list()



for (file in file.list) {  
  
  
  meeting_name <- str_extract(file, "^[[:digit:]]*-[[:digit:]]*-[[:digit:]]*.*item")

  
  date <- str_extract(file, "^[[:digit:]]*-[[:digit:]]*-[[:digit:]]*")
  if(date != date_check){
      name_check <- "not a name"
      date_check <- date
      }
  
  check <- grep(meeting_name, name_check)
  if(!length(check) > 0){
    
      
  print(paste("Working on:", sub(" item", "", meeting_name)))
  
  name_check <- str_c(c(name_check, meeting_name))
    

  meeting_numbers <- grep(meeting_name, file.list)
  

  refs <- list()
  items <- list()
  nos <- list()
  
  for(mc in 1:length(meeting_numbers)){
  
      meeting <- file.list[meeting_numbers[mc]]

      
      
      item_n <- str_extract(meeting, "[[:digit:]]*[[:punct:]][[:alpha:]]*$")
      item_n <- as.numeric(str_extract(item_n, "[[:digit:]]*"))
      
      type <- str_extract(meeting, "[[:punct:]][[:alpha:]]*$")
  
  
  if(type == ".RData"){
    
    load(paste0(archive.folder, meeting))
    
      
     
      agenda <- htmlParse(agenda)
    
    ref <- xpathSApply(doc = agenda, 
                       path = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'sec-inner', ' ' ))]", 
                       xmlValue)
    
    if(length(ref) == 0){
      
        ref <- xpathSApply(doc = agenda, 
                         path = "//div/p[.]", 
                         xmlValue)
      
        if(length(ref) == 0){ next}
        
        
      item_s <- grep(paste0("^",item_n), ref)
      item <- ref[item_s[1]]
      item <- str_squish(item)
      
      ref <- paste(ref, collapse = "")
      ref <- str_squish(ref)
      
      
    }else{
      ref <- str_squish(ref[5])
      ref <- sub("Del Tilgængelighed Udskriv Del på Facebook Linkedin share Del på Twitter E-mail ", "", ref)    
    
      item <- xpathSApply(doc = agenda, 
                     path = "//h1", 
                     xmlValue)
      
      item_s <- grep(paste0("^",item_n), item)
      item <- item[item_s[1]]
      item <- str_squish(item)
      }
    
    
  }else{
    if(type == ".PDF"){
    
    agenda <- pdf_text(paste0(archive.folder, meeting))
    agenda <- paste(agenda,collapse="")
    ref <- str_squish(agenda)
    
    item <- str_extract(ref, paste0("^", item_n, ".*[(][[:digit:]]|^", item_n, ".*[(][[:punct:]]"))
    item <- sub("[(][[:digit:]]$", "", item)
    item <- sub("[(][)]$", "", item)
    
      }else{
    
    if(type == ".DOC"){
      
      agenda <- readtext(paste0(archive.folder, meeting))
      agenda <- paste(agenda$text,collapse="")
      ref <- str_squish(agenda)
      
      item <- str_extract(ref, paste0("^", item_n, ".*[(][[:digit:]]|^", item_n, ".*[(][[:punct:]]"))
      item <- sub("[(][[:digit:]]$", "", item)
      item <- sub("[(][)]$", "", item)
      
      
  }
  } 
  }
   refs[[mc]] <- ref
   items[[mc]] <- item        
   nos[[mc]] <- item_n  
   }
  
  if(length(refs) == 0){ next}
  
  refs <- unlist(refs)
  items <- unlist(items)
  nos <- unlist(nos)
  
  df <- data.frame(
    city = "Copenhagen",
    date = date,
    agenda_no = nos,
    title = items,
    referat = refs,
    stringsAsFactors = F
  )
  
  df <-df[order(df$agenda_no),]
  
  
  
  #put that dataframe into our list of dataframes
  dfs[[length(dfs) + 1]] <- df
  
  }}
  
out <- as.data.frame(rbindlist(dfs))
save(out, file = '../data_archive/copenhagen_07-17.RData')
