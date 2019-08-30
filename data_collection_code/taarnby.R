## taarnby.R
## Massimo G Losinno
## August 2019
## Scrape Tårnby city council referater

# rm(list = ls())
# setwd("C:/Users/musse/Aarhus universitet/Tim Dennis Runck - CROW_FAR/First_Repository_CROW_FAR/data_collection_code")

library(rvest)
library(RCurl)
library(stringr)
library(data.table)
library(pdftools)


# Create directories to save files if don't exist
if(!dir.exists("../data_archive")) dir.create("../data_archive")
if(!dir.exists("../data_archive/taarnby_archive")) dir.create("../data_archive/taarnby_archive")

#set base url for looping -- this is more of a 'start' page for this task
start_url <- "https://www.taarnby.dk/politik/politiske-udvalg"

#list of months to convert dates later
months <- c("januar", "februar", "marts", "april", "maj", "juni", "juli", "august", "september", "oktober", "november", "december")


years <- as.character(2014:2017)

dfs <- list() #list for dataframes for each meeting
dates <- "Not a date" #creating a date count to distenguish the first scrape

start_url <- read_html(start_url)

com_links <- start_url %>% html_nodes('.navNiveau03 li a') %>% html_attr("href")
com_links <- paste0("https://www.taarnby.dk", com_links)
com_links <- c("https://www.taarnby.dk/politik/kommunalbestyrelsen/kommunalbestyrelsens-moeder,-dagsorden-og-referat", com_links)

for(cc in 1:length(com_links)){
  
  print(paste0("Working on committee ", cc, " out of ", length(com_links)))
  
  for(yc in 1:length(years)){  
  
      print(paste0("Working on year ", yc))
      
    com_url1 <- paste0(com_links[cc], "?year=", years[yc] )
    
    
    ok <- FALSE
    problem <- FALSE
    count <- 1
    
    #Reading the html for the first meeting
    while (ok == FALSE) {
      com_url <- tryCatch({                  
        read_html(com_url1)
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
    
    meet_links <- com_url %>% html_nodes('br+ a') %>% html_attr("href")
    meet_links_check <-grep(".pdf$", meet_links) 
    if(length(meet_links_check) >= 1){meet_links <- meet_links[meet_links_check]}else{next}
    meet_links <- paste0("https://www.taarnby.dk", meet_links)
    
    meet_dates <- com_url %>% html_nodes('.page-text h4 , label') %>% html_text(trim = T) 
    if(length(meet_dates) != length(meet_links)){meet_dates <- meet_dates[-1]}
    
    
    
    
    for (mc in 1:length(meet_dates)) {
      
      date <- meet_dates[mc] 
      
      date <- sub("^[[:alpha:]]* d[.] ", "", date)
      date <- sub("[.]", "", date)
      
      month <- str_extract(date,"^[[:digit:]]* [[:alpha:]]*")
      month <- sub("[[:digit:]]* ", "", month)
      month <- grep(month, months)
      if(month < 10){month <- paste0("0", month)}
      if(nchar(str_extract(date, "[[:digit:]]*")) == 1){date <- paste0("0", date)}
      date <- sub(" [[:alpha:]]* " , paste0("-", month, "-"), date)
      
      dates <- str_c(c(dates, date))
      date <- make.unique(dates, "-")[length(dates)]
      ref_date <- dates[length(dates)]
      
      
      file.name <- paste0("../data_archive/taarnby_archive/", date, ".PDF")
      
      #Check to see if the pdf is already downloaded
      if (!file.exists(file.name)){
        
        download.file(meet_links[mc], 
                      destfile = file.name, 
                      method = "auto", quiet = T, mode = "wb",
                      cacheOK = TRUE, extra = getOption("download.file.extra"))
      }
      
      agenda <- pdf_text(file.name)
      
      items <- str_extract_all(agenda, "[[:digit:]]*[.] .*[.][.][.][.]")
      items <- unlist(items)
      items <- gsub("[.]*[.]$", "", items)
      items <- str_squish(items)
      

      item_check <- grep("LUKKET", items)
      if(length(item_check) >= 1){items <- items[-item_check]}
      
      if(length(items) > 0){
        ref <- str_squish(agenda)
       
        length_check <- tryCatch({                  
            grep(items[length(items)], ref)[1]},
        error = function(e){e}
            )
        if(is.na(length_check)){items <- items[-length(items)]}
        if ("error" %in% class(length_check)) {items <- items[-length(items)]}
        
        ref <- ref[-1:-grep(items[length(items)], ref)[1]]
        ref <- paste(ref,collapse=" ")
        ref <- paste0(" ", ref)
        
        items2 <- str_extract(items, "^[[:digit:]]*[.] [[:alpha:]]*")
        
        refs <- list()
        for(rc in 1:length(items)){
          if(rc < length(items)){
            
            if(grepl("[(]", items2[rc]) == T & grepl("[)]", items2[rc]) == F){items2[rc] <- sub("[(].*", "", items2[rc])}
            if(grepl("[(]", items2[rc + 1]) == T & grepl("[)]", items2[rc + 1]) == F){items2[rc + 1] <- sub("[(].*", "", items2[rc + 1])}
            
            temp_ref    <- str_extract(ref, paste("(?=", items2[rc],"?).*", items2[rc + 1]))
            temp_ref <- sub(items2[rc + 1], "", temp_ref)
            refs[[rc]] <- temp_ref
          }else{
            refs[[rc]] <- str_extract(ref, paste("(?=", items2[rc],"?).*"))}
        }
        refs <- unlist(refs)  
        
      }else{if(length(agenda) > 0 & length(items) == 0){
        items <- NA
        ref <- paste(ref,collapse=" ")
        ref <- str_squish(ref)      
        refs < ref 
      }else{
        if(length(agenda) == 0 & length(items) == 0){next}}
      }
      
      
      #assemble dataframe
      df <- data.frame(
        city = "Taarnby",
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



##SECOND loop for the old meetings


url1 <- "https://www.taarnby.dk/politik/kommunalbestyrelsen/kommunalbestyrelsen-referater-foer-juni-2014"
url1 <- read_html(url1)
com_links1 <- url1 %>% html_nodes('.navNiveau04 .alone a') %>% html_attr("href")
com_dates1 <- url1 %>% html_nodes('.navNiveau04 .alone a') %>% html_text(trim = T) 

url2<- "https://www.taarnby.dk/politik/kommunalbestyrelsen/kommunalbestyrelsen-referater-foer-juni-2014/referater-2013"
url2 <- read_html(url2)
com_links2 <- url2 %>% html_nodes('.navNiveau05 a') %>% html_attr("href")
com_dates2 <- url2 %>% html_nodes('.navNiveau05 a') %>% html_text(trim = T) 

extra_dates <- gsub("[.]", "-", str_extract(c(com_dates1, com_dates2), "[[:digit:]]*[.][[:digit:]]*[.][[:digit:]]*"))
extra_links <- paste0("https://www.taarnby.dk", c(com_links1, com_links2))


start_url <- "https://www.taarnby.dk/politik/politiske-udvalg/aeldre-referater"
start_url <- read_html(start_url)
com_links <- start_url %>% html_nodes('.navNiveau04 a') %>% html_attr("href")
com_links <- paste0("https://www.taarnby.dk", com_links)


for(cc in 1:length(com_links)){
    
    print(paste0("Working on committee ", cc, " out of ", length(com_links)))
    
com_url <- read_html(com_links[cc])

year_links <- com_url %>% html_nodes('.navNiveau05 a') %>% html_attr("href")
year_links <- paste0("https://www.taarnby.dk", year_links)

for(yc in 1:length(year_links)){  
    
    print(paste0("Working on year ", yc, " out of ", length(year_links)))
    
    meet_url <- read_html(year_links[yc])
    if(cc != 1){
        meet_links <- meet_url %>% html_nodes('.navNiveau05 a') %>% html_attr("href")
        meet_links <- meet_links[grep("referat-", meet_links)]
        
        meet_dates <- meet_url %>% html_nodes('.navNiveau05 a') %>% html_text(trim = T) 
        meet_dates <- meet_dates[grep("referat-", meet_links)]}else{
            
            meet_links <- meet_url %>% html_nodes('.navNiveau06 a') %>% html_attr("href")
            meet_dates <- meet_url %>% html_nodes('.navNiveau06 a') %>% html_text(trim = T) 
        }


meet_dates <- gsub("[.]", "-", str_extract(meet_dates, "[[:digit:]]*[.][[:digit:]]*[.][[:digit:]]*"))
meet_links <- paste0("https://www.taarnby.dk", meet_links)

if(yc == 1 & cc == 1){
    meet_links <- c(meet_links, extra_links)
    meet_dates <- c(meet_dates, extra_dates)
}

if(yc != 1 & cc > 1){next}
for (mc in 1:length(meet_dates)) {
    
    
    date <- meet_dates[mc] 
    
    if(nchar(str_extract(date, "[[:digit:]]*$")) < 4){date <- sub("[[:digit:]]*$", paste0("20", str_extract(date, "[[:digit:]]*$")), date)}
    
    dates <- str_c(c(dates, date))
    date <- make.unique(dates, "-")[length(dates)]
    ref_date <- dates[length(dates)]
    
    file.name <- paste0("../data_archive/taarnby_archive/", date, ".PDF")
    
    #Check to see if the pdf is already downloaded
    if (!file.exists(file.name)){
        
        pdf_url <- read_html(meet_links[mc])
        pdf_link <- paste0("https://www.taarnby.dk", pdf_url %>% html_nodes('.page-text a') %>% html_attr("href"))
        
        d_check  <- tryCatch({ download.file(pdf_link[1], 
                                             destfile = file.name, 
                                             method = "auto", quiet = T, mode = "wb",
                                             cacheOK = TRUE, extra = getOption("download.file.extra"))},error = function(e){e}
        )
        print("sweet")
        if ("error" %in% class(d_check)) {
            next
        }
        
    }
    
    agenda <- pdf_text(file.name)
    
    items <- str_extract_all(agenda, "[[:digit:]]*[.] .*[.][.][.][.]")
    items <- unlist(items)
    items <- gsub("[.]*[.]$", "", items)
    items <- str_squish(items)
    
    
    item_check <- grep("LUKKET", items)
    if(length(item_check) >= 1){items <- items[-item_check]}
    
    if(length(items) > 0){
        ref <- str_squish(agenda)
        
        length_check <- tryCatch({                  
            grep(items[length(items)], ref)[1]},
            error = function(e){e}
        )
        if(is.na(length_check)){items <- items[-length(items)]}
        if ("error" %in% class(length_check)) {items <- items[-length(items)]}
        
        ref <- ref[-1:-grep(items[length(items)], ref)[1]]
        ref <- paste(ref,collapse=" ")
        ref <- paste0(" ", ref)
        
        items2 <- str_extract(items, "^[[:digit:]]*[.] [[:alpha:]]*")
        
        refs <- list()
        for(rc in 1:length(items)){
            if(rc < length(items)){
                
                if(grepl("[(]", items2[rc]) == T & grepl("[)]", items2[rc]) == F){items2[rc] <- sub("[(].*", "", items2[rc])}
                if(grepl("[(]", items2[rc + 1]) == T & grepl("[)]", items2[rc + 1]) == F){items2[rc + 1] <- sub("[(].*", "", items2[rc + 1])}
                
                temp_ref    <- str_extract(ref, paste("(?=", items2[rc],"?).*", items2[rc + 1]))
                temp_ref <- sub(items2[rc + 1], "", temp_ref)
                refs[[rc]] <- temp_ref
            }else{
                refs[[rc]] <- str_extract(ref, paste("(?=", items2[rc],"?).*"))}
        }
        refs <- unlist(refs)  
        
    }else{if(length(agenda) > 0 & length(items) == 0){
        items <- NA
        ref <- paste(ref,collapse=" ")
        ref <- str_squish(ref)      
        refs < ref 
    }else{
        if(length(agenda) == 0 & length(items) == 0){next}}
    }
    
    
    #assemble dataframe
    df <- data.frame(
        city = "Tårnby",
        date = ref_date,
        agenda_no = 1:length(items),
        title = items,
        referat = refs,
        stringsAsFactors = F
    )
    
    #put that dataframe into our list of dataframes
    dfs[[length(dfs) + 1]] <- df 
    
    
    }#End of second meeting loop
  }#End of second year loop
}#End of second committee loop
    
    
    
#put together all dataframes into one big one
out <- as.data.frame(rbindlist(dfs))

#save final data for later
save(out, file = '../data_archive/taarnby_13-17.RData')
