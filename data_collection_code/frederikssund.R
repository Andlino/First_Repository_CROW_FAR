## frederikssund.R
## Massimo G Losinno
## August 2019
## Scrape Frederikssund city council referater

# rm(list = ls())
# setwd("C:/Users/musse/Aarhus universitet/Tim Dennis Runck - CROW_FAR/First_Repository_CROW_FAR/data_collection_code")

library(rvest)
library(RCurl)
library(stringr)
library(data.table)
library(pdftools)


# Create directories to save files if don't exist
if(!dir.exists("../data_archive")) dir.create("../data_archive")
if(!dir.exists("../data_archive/frederikssund")) dir.create("../data_archive/frederikssund_archive")


#Getting the main url link in
start_url <- "https://stadsarkivet.frederikssund.dk/poludvraadnaevn/fr07-/frurn07-"

dates <- "Not a date" #creating a date count to distenguish the first scrape

dfs <- list() #list for dataframes for each meeting

#list of months to convert dates later
months <- c("januar", "februar", "marts", "april", "maj", "juni", "juli", "august", "september", "oktober", "november", "december")


start_url <- read_html(start_url)

com_links <- start_url %>% html_nodes('a') %>% html_attr("href")
com_links <- com_links[24:52]
com_links <- paste0("https://stadsarkivet.frederikssund.dk", com_links)
com_links <- c("https://stadsarkivet.frederikssund.dk/poludvraadnaevn/fr07-/frbr07-/frbrref07-", com_links)


for(cc in 1:length(com_links)){
    
    print(paste0("Working on committee ", cc, " out of ", length(com_links)))
    com_url <- read_html(com_links[cc])
    year_link <- paste0("https://stadsarkivet.frederikssund.dk", com_url %>% html_nodes('.table-styled a') %>% html_attr("href")) 
    
    year_check <- grep("2018|2019|2006", com_url %>% html_nodes('.table-styled a') %>% html_text(trim = T) )
    if(length(year_check) >= 1){year_link <- year_link[-year_check]} 

    if(length(year_link) == 0){next}

    for(yc in 1:length(year_link)){  
        
        if(cc == 3){
            
            if(yc > 1){next}
            
            meet_links <- paste0("https://stadsarkivet.frederikssund.dk", com_url %>% html_nodes('.table-styled a') %>% html_attr("href")) 
            
            meet_dates <- sub("[.]", " ", com_url %>% html_nodes('.table-styled a') %>% html_text(trim = T))
            meet_dates <- sub("  ", " ", meet_dates)
            date_check <- grep("^[[:digit:]]", meet_dates)
            
            meet_links <- meet_links[date_check]
            meet_dates <- meet_dates[date_check]
            
        }else{
        
        
        print(paste("Working on year", yc, "out of", length(year_link)))
        
        ok <- FALSE
        problem <- FALSE
        count <- 1
        
        #Getting a list of years for the chosen committee
        while (ok == FALSE) {
            year_url <- tryCatch({                  
                read_html(year_link[yc])
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
        
        
        meet_links <- paste0("https://stadsarkivet.frederikssund.dk", year_url %>% html_nodes('.table-styled a') %>% html_attr("href")) 
        
        meet_dates <- sub("[.]", " ", year_url %>% html_nodes('.table-styled a') %>% html_text(trim = T))
        meet_dates <- sub("  ", " ", meet_dates)
        
        date_check <- grep("^[[:digit:]]", meet_dates)
        
        meet_links <- meet_links[date_check]
        meet_dates <- meet_dates[date_check]        
        
        pdf_check <- grep(".pdf", meet_links)
        meet_links <- meet_links[pdf_check]
        meet_dates <- meet_dates[pdf_check]}        
        
        
        if(length(meet_links) == 0){next}
        
        for(mc in 1:length(meet_links)){
            
        date <- meet_dates[mc] 
        if(cc == 1 & mc == 6 & yc == 2){date <- "22 juni 2011"}
        
        month <- str_extract(date,"[[:digit:]]* [[:alpha:]]*")
        month <- sub("[[:digit:]]* ", "", month)
        month <- grep(month, months)
        if(month < 10){month <- paste0("0", month)}
        if(nchar(str_extract(date, "^[[:digit:]]*")) == 1){date <- paste0("0", date)}
        date <- sub(" [[:alpha:]]* " , paste0("-", month, "-"), date)
        
        dates <- str_c(c(dates, date))
        date <- make.unique(dates, "-")[length(dates)]
        ref_date <- dates[length(dates)]
        
        file.name <- paste0("../data_archive/frederikssund_archive/", date, ".PDF")
        
        if (!file.exists(file.name)){
           
             download.file(meet_links[mc], 
                          destfile = file.name, 
                          method = "auto", quiet = T, mode = "wb",
                          cacheOK = TRUE, extra = getOption("download.file.extra"))
            }
            
        
        agenda <- pdf_text(file.name)
        
        item_agenda <- grep("Journal nr.:", agenda)
        if(length(item_agenda) >= 1){
        items <- agenda[-item_agenda[1]:-length(agenda)]
        
        items <- str_extract_all(items, "Sag nr[.] [[:digit:]]* .*")
        items <- unlist(items)
        items <- gsub("  *", " ", items)
       
        if(length(items) == 0){
            items <- NA
            ref <- str_squish(agenda)
            ref <- paste(ref,collapse=" ")
            refs <- paste0(" ", ref)
        }else{
        
         
        ref <- agenda[item_agenda[1]:length(agenda)]
        ref <- str_squish(ref)
        ref <- paste(ref,collapse=" ")
        ref <- paste0(" ", ref)
        
        items2 <- str_extract(items, "Sag nr[.] [[:digit:]]*")
    
        refs <- list()
        for(rc in 1:length(items)){
            if(rc < length(items)){
                
                if(grepl("[(]", items2[rc]) == T & grepl("[)]", items2[rc]) == F){items2[rc] <- sub("[(].*", "", items2[rc])}
                if(grepl("[(]", items2[rc + 1]) == T & grepl("[)]", items2[rc + 1]) == F){items2[rc + 1] <- sub("[(].*", "", items2[rc + 1])}
                
                temp_ref    <- str_extract(ref, paste("(?=", items2[rc],"?).*", items2[rc + 1]))
                temp_ref <- sub(items2[rc + 1], "", temp_ref)
                temp_ref <- sub("^ ", "", temp_ref)
                refs[[rc]] <- temp_ref
            }else{
                temp_ref <- str_extract(ref, paste("(?=", items2[rc],"?).*"))}
                temp_ref <- sub("^ ", "", temp_ref)
                refs[[rc]] <- temp_ref
        }
        refs <- unlist(refs) } 
        }else{
            
            items <- NA
            ref <- str_squish(agenda)
            ref <- paste(ref,collapse=" ")
            refs <- paste0(" ", ref)
            
        }
            
        na_check <- na.omit(refs)
        
        
        
        if(length(agenda) == 0){next}
        
        
        #assemble dataframe
        df <- data.frame(
            city = "Frederikssund",
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


#put together all dataframes into one big one
out <- as.data.frame(rbindlist(dfs))

#save final data for later
save(out, file = '../data_archive/frederikssund_07-17.RData')
        
        
        