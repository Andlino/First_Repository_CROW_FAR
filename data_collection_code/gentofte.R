
## gentofte.R
## Massimo G Losinno
## August 2019
## Scrape Gentofte city council referater

# rm(list = ls())
# setwd("C:/Users/musse/Aarhus universitet/Tim Dennis Runck - CROW_FAR/First_Repository_CROW_FAR/data_collection_code")

library(RCurl)
library(stringr)
library(XML)
library(rvest)
library(data.table)
library(curl)
library(pdftools)
library(readtext)

# Create directories to save files if don't exist
if(!dir.exists("../data_archive")) dir.create("../data_archive")
if(!dir.exists("../data_archive/gentofte_archive")) dir.create("../data_archive/gentofte_archive")


#Getting the main url link in
first_url <- "http://www.gentofte.dk/da/Indflydelse-,-a-,-politik/Dagsordener_og_referater?q&subject=Alle&from=01-01-2007&to=30-12-2017"
first_url <- getURL(first_url) 
first_url <- read_html(first_url)

dates <- "Not a date" #creating a date count to distenguish the first scrape

dfs <- list() #list for dataframes for each meeting

#Creating a list of links to all meetings
meetings <- first_url %>% html_nodes('#meetings a') %>%  html_attr("href")
meeting_name <- grep("Referat", first_url %>% html_nodes('#meetings a') %>% html_text(trim = T))
meetings <- meetings[meeting_name]
meetings <- paste0("http://www.gentofte.dk", meetings)

type <- first_url %>% html_nodes('#meetings li') %>% html_text(trim = T)
type <- type[meeting_name]
type <- gsub("[[:punct:]]", "", str_extract(type, "[[:punct:]][[:alpha:]]*[[:punct:]]"))

dates <- first_url %>% html_nodes('.date') %>% html_text(trim = T) 
  td <- first_url %>% html_nodes('td') %>% html_text(trim = T) 
  td <- td[seq(2,length(td),2)]
 dates <- [grep("Referat", td)]
 dates2 <- make.unique(dates, "-")
  
for(mc in 1:length(meetings)){


  if(type[mc] == "PDF"){
  
    file.name <- paste0("../data_archive/gentofte_archive/", dates2[mc], ".PDF")  
        
    if (!file.exists(file.name)){
      
        #Saving the pdf files
        download.file(meetings[mc], 
                      destfile = file.name, 
                      method = "auto", quiet = T, mode = "wb",
                      cacheOK = TRUE, extra = getOption("download.file.extra"))
      
      agenda <- pdf_text(file.name)
      agenda <- paste(agenda,collapse="")
      ref <- str_squish(agenda)
    }
    }else{  
    
  
  
  }else{if(type[mc] == "DOC"){
    
    file.name <- paste0("../data_archive/gentofte_archive/", dates2[mc], ".DOC")  
    
    if (!file.exists(file.name)){
      
      #Saving the doc files
     download.file(meetings[mc], 
                  destfile = file.name, 
                  method = "auto", quiet = FALSE, mode = "wb",
                  cacheOK = TRUE, extra = getOption("download.file.extra"))
  
    
    agenda <- readtext(file.name)
    agenda <- paste(agenda$text,collapse="")
    ref <- str_squish(agenda)
      }
    
    
  
  }else{if(type[mc] == "DOCX"){
    
    file.name <- paste0("../data_archive/gentofte_archive/", dates2[mc], ".DOCX")  
    
    if (!file.exists(file.name)){
      
    #Saving the docx files
    download.file(meetings[mc], 
                  destfile = file.name, 
                  method = "auto", quiet = FALSE, mode = "wb",
                  cacheOK = TRUE, extra = getOption("download.file.extra"))
    
    
    agenda <- readtext(file.name)
    agenda <- paste(agenda$text,collapse="")
    ref <- str_squish(agenda)
    }

  }
    
    
    
    
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
    
    
  
}}
}



