
## naestved.R
## Massimo G Losinno
## August 2019
## Scrape Næstved city council referater

# rm(list = ls())
# setwd("C:/Users/musse/Aarhus universitet/Tim Dennis Runck - CROW_FAR/First_Repository_CROW_FAR/data_collection_code")

library(rvest)
library(RCurl)
library(stringr)
library(data.table)
library(XML)


# Create directories to save files if don't exist
if(!dir.exists("../data_archive")) dir.create("../data_archive")
if(!dir.exists("../data_archive/naestved_archive")) dir.create("../data_archive/naestved_archive")

#set base url for looping -- this is more of a 'start' page for this task
first_url <- "https://www.naestved.dk/ByraadPolitik/Dagsordenerogreferater/Se%20dagsordener%20og%20referater%20her/Historiske%20udvalg/15.aspx"
second_url <- "https://www.naestved.dk/ByraadPolitik/Dagsordenerogreferater/DagsordenReferatGammel.aspx"

  
count <- 1
dfs <- list() #list for dataframes for each meeting
dates <- "Not a date" #creating a date count to distenguish the first scrape

year <- c("17", "16", "15", "14")


#Creating the url to get links for each of the meetings
first_url <- read_html(first_url)


#Getting the links and dates for the meetings from the city council 
com_links <- first_url %>% html_nodes('#sublayoutcontent_0_leftcolumn .fontsize14 a') %>% html_attr("href")
com_links <- com_links[-4]
com_links <- com_links[-16]
com_links <- gsub(" ", "%20", com_links)

com_links <- paste0("https://www.naestved.dk", com_links, "?y=20")

for(cc in 1:length(com_links)){
  
  print(paste0("Working on committee ", cc, " out of ", length(com_links)))
  
  for(yc in 1:4){
  
  
    com_url <- tryCatch({                  
      read_html(paste0(com_links[cc], year[yc]))
    }, 
    error = function(e) 
      e)
    
    if("error" %in% class(com_url)){
      print(paste("No meetings in year", yc, "for committee", cc))
  }else{
    print(paste("Working on year", yc))
    
    meet_links <- com_url %>% html_nodes('#indhold a') %>% html_attr("href")
    
  meet_links <- meet_links[grep("^/ByraadPolitik.*", meet_links)]
  meet_links <- paste0("https://www.naestved.dk", meet_links)
  meet_links <- gsub(" ", "%20", meet_links)
  
  meet_dates <- com_url %>% html_nodes('#indhold a') %>% html_text(trim = T)
  meet_dates <- meet_dates[grep("[[:alnum:]]", meet_dates)]
  meet_dates <- str_extract(meet_dates,"[[:digit:]]*[[:punct:]][[:digit:]]*[[:punct:]][[:digit:]]*")
  meet_dates <- gsub("[[:punct:]]", "-", meet_dates)
  
  for (mc in 1:length(meet_links)) {
  
    print(paste("Working on meeting", mc, "out of", length(meet_links)))
    
    dates <- str_c(c(dates, meet_dates[mc]))
    date <- make.unique(dates, "-")[length(dates)]
    ref_date <- dates[length(dates)]
    
    file.name <- paste0("../data_archive/naestved_archive/", date, ".RData")
    
    if (file.exists(file.name)){
      #if archived file exists, load it instead of downloading again
      load(file.name)
    } else {
      
      
      ok <- FALSE
      problem <- FALSE
      count <- 1
      
      #Reading the html for the first meeting
      while (ok == FALSE) {
        html_link <- tryCatch({                  
          getURL(meet_links[mc])
        },
        error = function(e) {problm <- TRUE
        Sys.sleep(2)
        e
        }
        )
        if ("error" %in% class(html_link)) {
          print(paste("Problem with link", lc))
          count <- count + 1
          
        } else {
          if(problem == TRUE ){print(paste("Problem with", lc, "fixed"))}
          ok <- TRUE
        }
        if(count == 10){
          break
        }
        
      }
      
    meeting <- html_link
    save(meeting, file = file.name)
    }
    
    meet_url <- read_html(meeting)
     
    #Get agenda items
    items <- meet_url %>% html_nodes('.agendapointlink') %>% html_text(trim = T)
    
    if(length(items) >= 1){
    #Creating list of number for items
    nos <- 1:length(items)
    
    #Get transcriptions
    ref <- meet_url %>% html_nodes('br+ .esdhmeetingsection') %>% html_text(trim = T)
    ref <- str_squish(ref)
      
    #assemble dataframe
    df <- data.frame(
      city = "Næstved",
      date = ref_date,
      agenda_no = nos,
      title = items,
      referat = ref,
      stringsAsFactors = F
    )
    
    #put that dataframe into our list of dataframes
    dfs[[length(dfs) + 1]] <- df}
    
    
  }
  
  }
    
  }}



#put together all dataframes into one big one
out <- as.data.frame(rbindlist(dfs))

#save final data for later
save(out, file = '../data_archive/naestved_14-17.RData')
