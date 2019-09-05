    
    ## kolding.R
    ## Massimo G Losinno
    ## July 2019
    ## Scrape Kolding city council referater
    
    # rm(list = ls())
    # setwd("C:/Users/musse/Aarhus universitet/Tim Dennis Runck - CROW_FAR/First_Repository_CROW_FAR/data_collection_code")
    
    library(rvest)
    library(RCurl)
    library(stringr)
    library(data.table)
    library(XML)
    
    
    # Create directories to save files if don't exist
    if(!dir.exists("../data_archive")) dir.create("../data_archive")
    if(!dir.exists("../data_archive/kolding_archive")) dir.create("../data_archive/kolding_archive")
    
    #set base url for looping -- this is more of a 'start' page for this task
    first_url <- "https://www.kolding.dk/politik/byradet/dagsordener-og-referater-byradet/referater-fra-tidligere-ar-byradet"
    second_url <- "https://www.kolding.dk/politik/udvalg/referater-fra-foregaende-byradsperiode"
    
    #list of months to convert dates later
    months <- c("januar", "februar", "marts", "april", "maj", "juni", "juli", "august", "september", "oktober", "november", "december")
    
    
    count <- 1
    dfs <- list() #list for dataframes for each meeting
    dates <- "Not a date" #creating a date count to distenguish the first scrape
    
    
    #Creating the url to get links for each of the meetings
    first_url <- read_html(first_url)
    
    #Getting the links and dates for the meetings from the city council 
    links <- first_url %>% html_nodes('#reditemsItems a') %>% html_attr("href")
    link_dates <- first_url %>% html_nodes("#reditemsItems a") %>% html_text(trim = T)
    links <- links[-grep("2019|2018", link_dates)]
    link_dates <- link_dates[-grep("2019|2018", link_dates)]
    links <- paste0("https://www.kolding.dk", links)
    
    link_dates <- str_extract(link_dates,"[[:digit:]]*[[:punct:]] [[:alnum:]]* .*$")
    link_dates <- sub("[[:punct:]]", "", link_dates)
    
    for(lc in 1:length(links)){
        
        print(paste0("Working on link nr: ", lc, " / ", length(links)))
        
        #get date for the specific meeting
        date <- link_dates[lc]
        month <- str_extract(date,"[[:digit:]]* [[:alpha:]]*")
        month <- sub("[[:digit:]]* ", "", month)
        month <- grep(month, months)
        if(month < 10){month <- paste0("0", month)}
        date <- sub(" [[:alpha:]]* " , paste0("-", month, "-"), date)
        dates <- str_c(c(dates, date))
        date <- make.unique(dates, "-")[length(dates)]
        ref_date <- dates[length(dates)]
        
        #Creating the url to get links for each of the meetings
        meeting_url <- read_html(links[lc])
        
        items <- meeting_url %>% html_nodes(".agendaitem-name a") %>% html_text(trim = T)
        nos <- 1:length(items)
        
    
        file.name <- paste0("../data_archive/kolding_archive/", date, ".RData")
        
        
        
        if (file.exists(file.name)){
            #if archived file exists, load it instead of downloading again
            load(file.name)
        } else {
            
            html_link <- read_html(links[lc])
            meeting <- htmlParse(html_link)
            #Saving the xml for the meeting
            meeting <- saveXML(meeting)
            save(meeting, file = file.name)
        }
        
        meeting <- htmlParse(meeting)
        
        ref <- xpathSApply(doc = meeting, 
                           path = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'agendabullet', ' ' ))]", 
                           xmlValue)
        ref <- str_squish(ref)
        
        #assemble dataframe
        df <- data.frame(
            city = "Kolding",
            date = ref_date,
            agenda_no = nos,
            title = items,
            referat = ref,
            stringsAsFactors = F
        )
        
        #put that dataframe into our list of dataframes
        dfs[[length(dfs) + 1]] <- df    
        }    
    
    
    
    #Start on the second main url
    print("Starting on the second loop")
    
    second_url <- read_html(second_url)
    
    links <- second_url %>% html_nodes('#current a') %>% html_attr("href")
    links <- links[-1]
    links <- paste0("https://www.kolding.dk", links)
    
    
    for(muc in 1:length(links)){
        
        print(paste0("Working on committee ", muc, " / ", length(links)))
        
        com_url <- read_html(links[muc])
        
        com_links <- com_url %>% html_nodes('#reditemsItems a') %>% html_attr("href")
        com_dates <- com_url %>% html_nodes("#reditemsItems a") %>% html_text(trim = T)
        com_links <- paste0("https://www.kolding.dk", com_links)
        
        com_dates <- str_extract(com_dates,"[[:digit:]]*[[:punct:]] [[:alnum:]]* .*$")
        com_dates <- sub("[[:punct:]]", "", com_dates)
        
        for(lc in 1:length(com_links)){
            
            print(paste0("Working on link nr: ", lc, " / ", length(com_links)))
            
           
            #Creating the url to get links for each of the meetings
            meeting_url <- read_html(com_links[lc])
            
            items <- meeting_url %>% html_nodes(".agendaitem-name a") %>% html_text(trim = T)
            
            if(length(items) >= 1){
                nos <- 1:length(items)
            
            
             #get date for the specific meeting
            date <- com_dates[lc]
            month <- str_extract(date,"[[:digit:]]* [[:alpha:]]*")
            month <- sub("[[:digit:]]* ", "", month)
            month <- grep(month, months)
            if(month < 10){month <- paste0("0", month)}
            date <- sub(" [[:alpha:]]* " , paste0("-", month, "-"), date)
            
            dates <- str_c(c(dates, date))
            date <- make.unique(dates, "-")[length(dates)]
            ref_date <- dates[length(dates)]
            
            
            
            
            
            file.name <- paste0("../data_archive/kolding_archive/", date, ".RData")
            
            
            
            if (file.exists(file.name)){
                #if archived file exists, load it instead of downloading again
                load(file.name)
            } else {
                
                html_link <- read_html(com_links[lc])
                meeting <- htmlParse(html_link)
                #Saving the xml for the meeting
                meeting <- saveXML(meeting)
                save(meeting, file = file.name)
            }
            
            meeting <- htmlParse(meeting)
            
            ref <- xpathSApply(doc = meeting, 
                               path = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'agendabullet', ' ' ))]", 
                               xmlValue)
            ref <- str_squish(ref)
            
            #assemble dataframe
            df <- data.frame(
                city = "Kolding",
                date = ref_date,
                agenda_no = nos,
                title = items,
                referat = ref,
                stringsAsFactors = F
            )
            
            #put that dataframe into our list of dataframes
            dfs[[length(dfs) + 1]] <- df    
        }} 
        
        
    }
    
    
    
    
    #put together all dataframes into one big one
    out <- as.data.frame(rbindlist(dfs))
    
    #save final data for later
    save(out, file = '../data_archive/kolding_14-17.RData')
    
    
    
    
