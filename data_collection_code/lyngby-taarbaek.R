## lyngby-taarbaek.R
## Massimo G Losinno
## August 2019
## Scrape Lyngby-Taarbaek city council referater

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
if(!dir.exists("../data_archive/lyngby-taarbaek_archive")) dir.create("../data_archive/lyngby-taarbaek_archive")

#set base url for looping -- this is more of a 'start' page for this task
start_url <- "https://www.ltk.dk/referater-2010-2017"

dfs <- list() #list for dataframes for each meeting
dates <- "Not a date" #creating a date count to distenguish the first scrape

months <- c("januar", "februar", "marts", "april", "maj", "juni", "juli", "august", "september", "oktober", "november", "december")

start_url <- read_html(start_url)

meet_links <- start_url %>% html_nodes('.accordion-content a') %>% html_attr("href")
meet_dates <- start_url %>% html_nodes('.accordion-content a') %>% html_text(trim = T) 

meet_links <- paste0("https://www.ltk.dk", meet_links)

mc_seq <- seq(from = 0, to = length(meet_links), by = 5)

for (mc in 1:length(meet_links)) {
  
  print_check <- grep(paste0("^", mc, "$"), mc_seq)
  if(length(print_check) == 1){  print(paste0("Working on meeting ", mc, " out of ", length(meet_links)))}
  
  
  date  <- meet_dates[mc]
  date <- str_extract(date,"[[:digit:]]*-[[:digit:]]*-[[:digit:]]*")

  dates <- str_c(c(dates, date))
  date <- make.unique(dates, "-")[length(dates)]
  ref_date <- dates[length(dates)]
  
  file.name <- paste0("../data_archive/lyngby-taarbaek_archive/", date, ".PDF")
  
  #Check to see if the pdf is already downloaded
  if (!file.exists(file.name)){
    
    
    ok <- FALSE
    problem <- FALSE
    count <- 1
    
    #Reading the html for the meeting
    while (ok == FALSE) {
      download <- tryCatch({                  
        download.file(meet_links[mc], 
                      destfile = file.name, 
                      method = "auto", quiet = T, mode = "wb",
                      cacheOK = TRUE, extra = getOption("download.file.extra"))
      },
      error = function(e) {problm <- TRUE
      Sys.sleep(2)
      e
      }
      )
      if ("error" %in% class(download)) {
        print(paste("Problem with link", mc))
        count <- count + 1
        
      } else {
        if(problem == TRUE ){print(paste("Problem with", mc, "fixed"))}
        ok <- TRUE
      }
      if(count == 10){
        break
      }}
    
    if(count == 10){
      next
    }
    
      }  
  
  ref <- pdf_text(file.name)
  ref <- paste(ref,collapse=" ")
  ref <- str_squish(ref)
  
  
  check <- grep("[[:alnum:]]", ref)
  if(length(check) == 0){next}
  
  #assemble dataframe
  df <- data.frame(
    city = "Lyngby-Taarbæk",
    date = ref_date,
    referat = ref,
    stringsAsFactors = F
  )
  
  #put that dataframe into our list of dataframes
  dfs[[length(dfs) + 1]] <- df 
  
  
}#End of meeting loop


  
  
#put together all dataframes into one big one
out <- as.data.frame(rbindlist(dfs))

#save final data for later
save(out, file = '../data_archive/lyngby-taarbaek_10-17.RData')
  
  
  
  
