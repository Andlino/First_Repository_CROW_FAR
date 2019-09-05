## kalundborg.R
## Massimo G Losinno
## August 2019
## Scrape Kalundborg city council referater

# rm(list = ls())
# setwd("C:/Users/musse/Aarhus universitet/Tim Dennis Runck - CROW_FAR/First_Repository_CROW_FAR/data_collection_code")

library(RCurl)
library(stringr)
library(XML)
library(rvest)
library(data.table)
library(curl)



# Create directories to save files if don't exist
if(!dir.exists("../data_archive")) dir.create("../data_archive")
if(!dir.exists("../data_archive/kalundborg")) dir.create("../data_archive/kalundborg_archive")


#Getting the main url link in
master_url <- "https://polweb.nethotel.dk/Produkt/PolWeb/default.asp?p=kalundborg07"
master_url <- getURL(master_url) 
master_url <- read_html(master_url)

years <- as.character(10:17)
years <- c("07", "08", "09", years)

dates <- "Not a date" #creating a date count to distenguish the first scrape

dfs <- list() #list for dataframes for each meeting

#Creating a list of links to all committees
master_links <- master_url %>% html_nodes('.teaserLink') %>% html_attr("href")
master_links <- master_links[!grepl("top$", master_links)]

com_names <- master_url %>% html_nodes('.teaserLink') %>% html_text(trim = T)


x <- 1
#Starting main loop over the different committees
for (x in 1:length(master_links)) {
  
  print(paste("Working on committee", x, "out of", length(master_links)))
  
  
  for(yc in 1:length(years)){
    
    print(paste("Working on year", yc))
    
    
    #Getting dates and links for meetings in 2018
    url <- paste0(master_links[x], paste0("&year=20", years[yc]))
    url <- getURL(url) 
    url <- read_html(url)
    
    links <- url %>% html_nodes('.pHead .pHeadBackground') %>% html_attr("href")
    meet_dates <- url %>% html_nodes('.pHead .pHeadBackground') %>% html_text(trim = T)
    meet_dates <- str_extract(meet_dates,"[[:digit:]]*[[:punct:]][[:digit:]]*[[:punct:]][[:digit:]]*")
    
    if(length(links) >= 1){
      for (mc in 1:length(links)) {
        
        dates <- str_c(c(dates, meet_dates[mc]))
        date <- make.unique(dates, "-")[length(dates)]
        ref_date <- dates[length(dates)]
        
        
        file.name <- paste0("../data_archive/kalundborg_archive/", date, ".RData")
        
        
        junior_url <- links[mc]
        
        ok <- FALSE
        problem <- FALSE
        count <- 1
        
        #Reading the html for the first meeting
        while (ok == FALSE) {
          item_url <- tryCatch({                  
            getURL(junior_url)
          },
          error = function(e) {problm <- TRUE
          Sys.sleep(2)
          e
          }
          )
          if ("error" %in% class(item_url)) {
            print(paste("Problem with link", mc))
            count <- count + 1
            
          } else {
            if(problem == TRUE ){print(paste("Problem with", lf, "fixed"))}
            ok <- TRUE
          }
          if(count == 10){
            break
          }
          
        }
        
        
        #Check if the file already exists for the meeting
        if (file.exists(file.name)){
          #if archived file exists, load it instead of downloading again
          load(file.name)
        } else {
          
          junior_url2 <- paste0("HTTP://polweb.nethotel.dk/Produkt/PolWeb/Sog/ShowAgenda.asp?p=kalundborg07&ID=",
                                str_extract(junior_url, "[[:digit:]]*$"))   
          
          #Download the full html with the transcription in it     
          
          ok <- FALSE
          problem <- FALSE
          count <- 1
          
          #Reading the html for the first meeting
          while (ok == FALSE) {
            meeting <- tryCatch({                  
              getURL(junior_url2)
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
        
        
        # Extract and clean up transcriptions
        ref1 <- meet_url %>% html_nodes('.Section1') %>% html_text(trim = T)
        ref2 <- meet_url %>% html_nodes('.Section2') %>% html_text(trim = T)
        if(sum(nchar(unlist(ref1))) >= sum(nchar(unlist(ref2)))){ref <- ref1}else{
          if(sum(nchar(unlist(ref2))) > sum(nchar(unlist(ref1)))){ref <- ref2}
        }
        
        if(length(ref) == 0){
          ref <- meet_url %>% html_nodes('.WordSection1') %>% html_text(trim = T)}
        
        
        ref <- ref[grep("^[[:digit:]]*[[:punct:]]", ref)]
        
        ref <- str_squish(ref)
        
        
        if(length(ref) >= 1){
          
          item_numbers <- str_extract(ref, "^[[:digit:]]*")
          
          #Creating list of number for items
          nos <- item_numbers
          
          #Getting items
          item_url <- read_html(item_url)
          items <- item_url %>% html_nodes('.p') %>% html_text(trim = T)
          items <- items[grep("^[[:alpha:]]", items)]
          items <- items[as.numeric(item_numbers)]
          
          if(length(ref) != length(items)){
            items <- str_extract(ref, "^[[:digit:]].*Sagsnr")
            items <- sub(" Åbent Sagsnr$", "", items)
          }
          
          #assemble dataframe
          df <- data.frame(
            city = "Kalundborg",
            date = ref_date,
            agenda_no = nos,
            title = items,
            referat = ref,
            stringsAsFactors = F
          )
          
          #put that dataframe into our list of dataframes
          dfs[[length(dfs) + 1]] <- df}
      }
    }#End of links loop
  }#End of year loop
}#End of committee loop


#put together all dataframes into one big one
out <- as.data.frame(rbindlist(dfs))

#save final data for later
save(out, file = '../data_archive/kalundborg_07-17.RData')




