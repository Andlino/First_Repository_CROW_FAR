## vordingborg_14-17.R
## Massimo G Losinno
## August 2019
## Scrape Vordingborg city council referater

# rm(list = ls())
# setwd("C:/Users/musse/Aarhus universitet/Tim Dennis Runck - CROW_FAR/First_Repository_CROW_FAR/data_collection_code")

library(rvest)
library(RCurl)
library(stringr)
library(data.table)
library(pdftools)


# Create directories to save files if don't exist
if(!dir.exists("../data_archive")) dir.create("../data_archive")
if(!dir.exists("../data_archive/vordingborg_14-17_archive")) dir.create("../data_archive/vordingborg_14-17_archive")

#set base url for looping -- this is more of a 'start' page for this task
start_url <- "http://polweb.vordingborg.dk/open/Forms/AllItems.aspx"


dfs <- list() #list for dataframes for each meeting
dates <- "Not a date" #creating a date count to distenguish the first scrape

start_url <- read_html(start_url)

com_links <- start_url %>% html_nodes('.itx a') %>% html_attr("href")

com_check <- start_url %>% html_nodes('.itx a') %>% html_text(trim = T)
com_links <- com_links[-grep("konstituerende", com_check)]

com_links <- paste0("http://polweb.vordingborg.dk", com_links)
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
  
  year_check <- year_url %>% html_nodes('.itx a') %>% html_text(trim = T)
  years_links <- year_url %>% html_nodes('.itx a') %>% html_attr("href")
  
  checker <- grep("2018|2019", year_check)
  if(length(checker) >= 1){
    years_links <- years_links[-grep("2018|2019", year_check)]
    
    if(length(years_links) == 0){
      print(paste0("Nothing before 2018 in com ", cc))
      next}}
  
  years_links <- paste0("http://polweb.vordingborg.dk", years_links)

  
  years_links <- gsub("ø", "%C3%B8", years_links)
  years_links <- gsub("æ", "%C3%A6", years_links)
  years_links <- gsub("å", "%C3%A5", years_links)
  years_links <- gsub("Å", "%C3%85", years_links)
  years_links <- gsub("Ø", "%C3%98", years_links)
  years_links <- gsub("Æ", "%C3%86", years_links)
  years_links <- gsub("§", "%C2%A7", years_links)
  

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
    
    meet_links <- com_url %>% html_nodes('.itx a') %>% html_attr("href")
    meet_links <- paste0("http://polweb.vordingborg.dk", meet_links)
    
    meet_dates <- com_url %>% html_nodes('.itx a') %>% html_text(trim = T) 
    

    
    for (mc in 1:length(meet_links)) {
      
      
      date  <- meet_dates[mc]
      
      dates <- str_c(c(dates, date))
      date <- make.unique(dates, "-")[length(dates)]
      ref_date <- dates[length(dates)]
      
      file.name <- paste0("../data_archive/vordingborg_14-17_archive/", date, ".PDF")
      
      if (!file.exists(file.name)){
        
        ok <- FALSE
        problem <- FALSE
        count <- 1
        
        #Reading the html for the meeting, leads to a link to the meeting
        while (ok == FALSE) {
          meeting_url1 <- tryCatch({                  
            read_html(meet_links[mc])
          },
          error = function(e) {problm <- TRUE
          Sys.sleep(2)
          e
          }
          )
          if ("error" %in% class(meeting_url1)) {
            print(paste("Problem with link", mc))
            count <- count + 1
            
          }else {
            if(problem == TRUE ){print(paste("Problem with", yc, "fixed"))}
            ok <- TRUE
          }
          if(count == 10){
            break}}
        
        meeting_url <- meeting_url1 %>% html_nodes('.itx a') %>% html_attr("href")
        
        #If there this links to both the transcription and the list of agendas, then only the link to the transcription is kept
        if(length(meeting_url) > 1){
          met_check <- meeting_url1 %>% html_nodes('.itx a') %>% html_text(trim = T)
          meeting_url <- meeting_url[grep("Referat", met_check)[1]]
        }else{
          if(length(meeting_url) == 0){
            print(paste("No transcript for meeting", date, "in com", cc))
            next}}
        
        ok <- FALSE
        problem <- FALSE
        count <- 1
        
        #Following the link to the meeting
        while (ok == FALSE) {
          meeting_url2 <- tryCatch({                  
            read_html(paste0("http://polweb.vordingborg.dk", meeting_url))
          },
          error = function(e) {problm <- TRUE
          Sys.sleep(2)
          e
          }
          )
          if ("error" %in% class(meeting_url2)) {
            print(paste("Problem with link", mc))
            count <- count + 1
            
          }else {
            if(problem == TRUE ){print(paste("Problem with", yc, "fixed"))}
            ok <- TRUE
          }
          if(count == 10){
            break}}
    

        pdf_link <- meeting_url2 %>% html_nodes('.itx a') %>% html_attr("href")
        pdf_link <- gsub(" ", "%20", pdf_link[grep("uden", pdf_link)[1]])
        #Changing special danish characters to a format that can be read as a url
        pdf_link <- gsub("ø", "%C3%B8", pdf_link)
        pdf_link <- gsub("æ", "%C3%A6", pdf_link)
        pdf_link <- gsub("å", "%C3%A5", pdf_link)
        pdf_link <- gsub("Å", "%C3%85", pdf_link)
        pdf_link <- gsub("Ø", "%C3%98", pdf_link)
        pdf_link <- gsub("Æ", "%C3%86", pdf_link)
        pdf_link <- gsub("§", "%C2%A7", pdf_link)
        
        tester <- grep("[[:alnum:]]", pdf_link)
        if(length(tester) == 0){
          print(paste("No transcript for meeting", date, "in com", cc))
          next}
        
        pdf_link <- paste0("http://polweb.vordingborg.dk", pdf_link)
        
        download.file(pdf_link, 
                      destfile = file.name, 
                      method = "auto", quiet = T, mode = "wb",
                      cacheOK = TRUE, extra = getOption("download.file.extra"))
        
        
      }
      
      agenda <- pdf_text(file.name)
      
      
      items <- str_extract_all(agenda, "[[:digit:]]*[.] .*[.][.][.][.]")
      items <- unlist(items)
      items <- gsub("[.]*[.]$", "", items)
      items <- str_squish(items)
      luk_check <- grep("Lukket", items)
      if(length(luk_check) != 0){
        items <- items[-grep("LUKKET", items)]}
      
      if(length(agenda) > 0 & length(items) > 0){
        ref <- str_squish(agenda)
        
        page_check <- grep(items[length(items)], ref)
        if(length(page_check) != 0){
          ref <- ref[-1:-page_check[1]]}else{
            ref <- ref[-1:-2]
          }
        
        
        ref <- paste(ref,collapse=" ")
        
        
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
      }else{
        if(length(agenda) > 0 & length(items) == 0){
          items <- NA
          ref <- paste(ref,collapse=" ")
          ref <- str_squish(ref)      
          refs < ref}
      }
      
      
      if(length(agenda) == 0 & length(items) == 0){
        
        print(paste("Skipping meeting", mc, date))
        next}
      
      
      #assemble dataframe
      df <- data.frame(
        city = "Vordingborg",
        date = ref_date,
        agenda_no = 1:length(items),
        title = items,
        referat = refs,
        stringsAsFactors = F
      )
      
      #put that dataframe into our list of dataframes
      dfs[[length(dfs) + 1]] <- df 
      
      
      
      
      
    }#end of meeting loop
  }#End of year loop
}#End of committee loop



#put together all dataframes into one big one
out <- as.data.frame(rbindlist(dfs))

#save final data for later
save(out, file = '../data_archive/vordingborg_14-17.RData')


