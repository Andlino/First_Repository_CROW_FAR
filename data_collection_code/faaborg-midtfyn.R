
## faaborg-midtfyn.R
## Massimo G Losinno
## July 2019
## Scrape Faaborg-Midtfyn city council referater

# rm(list = ls())
# setwd("C:/Users/musse/Aarhus universitet/Tim Dennis Runck - CROW_FAR/First_Repository_CROW_FAR/data_collection_code")

library(rvest)
library(RCurl)
library(stringr)
library(data.table)
library(XML)


# Create directories to save files if don't exist
if(!dir.exists("../data_archive")) dir.create("../data_archive")
if(!dir.exists("../data_archive/faaborg-midtfyn_archive")) dir.create("../data_archive/faaborg-midtfyn_archive")

#set base url for looping -- this is more of a 'start' page for this task
com_links <- c("https://www.fmk.dk/politik/kommunalshybestyrelsen/tidligere-referater-fra-kommunalbestyrelsesmoeder/", "https://www.fmk.dk/index.php?id=15365", "https://www.fmk.dk/index.php?id=15670",
          "https://www.fmk.dk/index.php?id=15669", "https://www.fmk.dk/index.php?id=15672", "https://www.fmk.dk/index.php?id=15673", "https://www.fmk.dk/index.php?id=15674")
  
#list of months to convert dates later
months <- c("januar", "februar", "marts", "april", "maj", "juni", "juli", "august", "september", "oktober", "november", "december")


count <- 1
dfs <- list() #list for dataframes for each meeting
dates <- "Not a date" #creating a date count to distenguish the first scrape


for(cc in 1:length(com_links)){
  
  print(paste0("Working on committee ", cc, " out of ", length(com_links)))
  com_url <- read_html(com_links[cc])
  year_link <- paste0("https://www.fmk.dk/", com_url %>% html_nodes('#contents a') %>% html_attr("href")) 
  
  for(yc in 1:length(year_link)){  
    
    the_link  <- year_link[yc]
    
    extra <- T
    
    meet_links <- list()
    meet_dates <- list()
    ec <- 1
    
    while(extra == T){
    print(paste0("Working on year ", yc, " out of ", length(year_link)))
    

    ok <- FALSE
    problem <- FALSE
    count <- 1
    
    #Getting a list of years for the chosen committee
    while (ok == FALSE) {
      year_url <- tryCatch({                  
        read_html(the_link[ec])
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
    
    
    meet_links3 <- year_url %>% html_nodes('#lfagendassummaries_dateMenu a') %>% html_attr("href")
    if(length(meet_links3) == 0){next}
    
    meet_links[[ec]] <- paste0("https://www.fmk.dk", meet_links3)
    
    meet_dates_temp <- year_url %>% html_nodes('#lfagendassummaries_dateMenu a') %>% html_text(trim = T) 
    year <- year_url %>% html_nodes('.itemdate') %>% html_text(trim = T) 
    year <- str_extract(year[1], "[[:digit:]]*$")
    
    meet_dates_temp <- gsub("[.]", "", str_extract(meet_dates_temp, "[[:alnum:]]*[.][[:space:]][[:alnum:]]*[.][[:space:]][[:alpha:]]*"))
    meet_dates_temp <- sub("^d[[:space:]]", "", meet_dates_temp)
    meet_dates_temp <- paste(meet_dates_temp, year)
    meet_dates[[ec]] <- meet_dates_temp
    
    
    if(yc == 1){extra <- F}
    
    if(ec == 1){
      the_link <- year_url %>% html_nodes('#lfagendassummaries_yearMenu a') %>% html_attr("href")
      the_link <- paste0("https://www.fmk.dk", the_link)}
    
    if(ec == length(the_link)){extra <- F}
    
    ec <- ec +1
    }
    
    meet_dates <- unlist(meet_dates)
    meet_links <- unlist(meet_links)
    
    for(mc in 1:length(meet_links)){
      
      print(paste("Working on meeting", mc, "out of", length(meet_links)))
      
      
      date <- meet_dates[mc] 
      date <- sub("[[:space:]]", " ", date)
      
      month <- str_extract(date,"[[:digit:]]* [[:alpha:]]*")
      month <- sub("[[:digit:]]* ", "", month)
      month <- grep(month, months)
      if(month < 10){month <- paste0("0", month)}
      if(nchar(str_extract(date, "^[[:digit:]]*")) == 1){date <- paste0("0", date)}
      date <- sub(" [[:alpha:]]* " , paste0("-", month, "-"), date)
      
      dates <- str_c(c(dates, date))
      date <- make.unique(dates, "-")[length(dates)]
      ref_date <- dates[length(dates)]
      
      file.name <- paste0("../data_archive/faaborg-midtfyn_archive/", date, ".RData")
      
     
      if (file.exists(file.name)){
          #if archived file exists, load it instead of downloading again
          load(file.name)
          
      } else {
          
          
          the_meet <- meet_links[mc]
          if(str_extract(the_meet, "[[:alpha:]]*$") != "html"){
              the_meet <- paste0(the_meet, "/")}
          
          
          ok <- FALSE
          problem <- FALSE
          count <- 1
          
          #Reading the html for the meeting
          while (ok == FALSE) {
              meeting <- tryCatch({                  
                  getURL(the_meet)
              },
              error = function(e) {problm <- TRUE
              Sys.sleep(2)
              e
              }
              )
              if ("error" %in% class(meeting)) {
                  print(paste("Problem with link", mc))
                  count <- count + 1
                  
              } else {
                  if(problem == TRUE ){print(paste("Problem with", mc, "fixed"))}
                  ok <- TRUE
              }
              if(count == 5){
                  break
              }}
          
          if(count == 5){
              print(paste("Skipping meeting", mc, "in committee", cc))
              next
          }
          
          
          save(meeting, file = file.name)
      }
      
      meeting_url <- tryCatch({                  
          read_html(meeting) 
      },
      error = function(e) {problm <- TRUE
      Sys.sleep(2)
      e
      }
      )
      if ("error" %in% class(meeting_url)) {
          meeting_url <- read_html(meeting, options = "HUGE")} 
      
      
      
      
      items <- meeting_url %>% html_nodes('#indholdsfortegnelse a') %>% html_text(trim = T) 
      
      ref <- meeting_url %>% html_nodes('.dagsordenpunkt') %>% html_text(trim = T)
      
      if(length(items) == 0){
          print(paste("Skipping meeting", mc, "in committee", cc))
          next
      }
      
      if(length(ref) == 0){
          print(paste("Skipping meeting", mc, "in committee", cc))
          next
      }
      
      ref <- str_squish(ref)
      
          df <- data.frame(
          city = "Faaborg-Midtfyn",
          date = ref_date,
          agenda_no = 1:length(items),
          title = items,
          referat = ref,
          stringsAsFactors = F
      )
      
      #put that dataframe into our list of dataframes
      dfs[[length(dfs) + 1]] <- df 
      
      
    }#End of month loop
  }#End of year loop
}#End of committee loop
      
      

#put together all dataframes into one big one
out <- as.data.frame(rbindlist(dfs))

#save final data for later
save(out, file = '../data_archive/faaborg-midtfyn_10-08.RData')

      