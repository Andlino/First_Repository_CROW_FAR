## copenhagen.R
## Massimo G Losinno
## July 2019
## Scrape Copengange city council referater

# setwd("C:/Users/musse/Aarhus universitet/Tim Dennis Runck - CROW_FAR/First_Repository_CROW_FAR/data_collection_code")

library(rvest)
library(RCurl)
library(stringr)
library(XML)
library(data.table)
library(curl)


# Create directories to save files if don't exist
if(!dir.exists("../data_archive")) dir.create("../data_archive")
if(!dir.exists("../data_archive/copenhagen_archive")) dir.create("../data_archive/copenhagen_archive")

#set base url for looping -- this is more of a 'start' page for this task
start_url <- "https://www.kk.dk/dagsordener-og-referater?field_committee_type_tid%5B%5D=13957&field_committee_type_tid%5B%5D=13960&field_committee_type_tid%5B%5D=13959&field_committee_type_tid%5B%5D=13958&title=&field_date_single_value%5Bvalue%5D%5Bdate%5D=01.01.2007&field_date_single_value_1%5Bvalue%5D%5Bdate%5D=01.01.2018&page="

dfs <- list()

x <- 0
#Starting main loop over the different pages
while (x <= 320) { #320 = magic number
   
    print(paste0("Working on page ", x + 1, "/", 321))
    
     y <- 1
    
    url <- paste0(start_url, x) 
    url <- read_html(url)

    #Getting the dates and links for the meetings of the page
    links <- url %>% html_nodes('#main-content .first a') %>% html_attr("href")
    links <- links[1:10]
    links <- paste0("https://www.kk.dk", links)
    
    #getting dates from the page before, to make sure that any meetings on the same day are taken into account
    if(x > 1){
    url2 <- paste0(start_url, x - 1) 
    url2 <- read_html(url2)
    url3 <- paste0(start_url, x - 2) 
    url3 <- read_html(url3)
    dates3 <- url %>% html_nodes('.date-display-single') %>% html_text(trim = T)
    dates2 <- url2 %>% html_nodes('.date-display-single') %>% html_text(trim = T)
    dates1 <- url3 %>% html_nodes('.date-display-single') %>% html_text(trim = T)
    dates <- str_c(c(dates1[1:10], dates2[1:10], dates3[1:10]))
    dates <- gsub("[[:punct:]][[:space:]]", "-", dates)
    dates <- make.unique(dates, "-")
    dates <- dates[21:30]
    } else{
        if(x == 1){
            url2 <- paste0(start_url, x - 1) 
            url2 <- read_html(url2)
            dates2 <- url %>% html_nodes('.date-display-single') %>% html_text(trim = T)
            dates1 <- url2 %>% html_nodes('.date-display-single') %>% html_text(trim = T)
            dates <- str_c(c(dates1, dates2))
            dates <- gsub("[[:punct:]][[:space:]]", "-", dates)
            dates <- make.unique(dates, "-")
            dates <- dates[11-20]}
        if(x == 0){
        dates <- url %>% html_nodes('.date-display-single') %>% html_text(trim = T)
        dates <- gsub("[[:punct:]][[:space:]]", "-", dates)
        dates <- make.unique(dates, "-")}
    }
    
    #Starting secondary loop over the different meetings
    while (y <= length(links)) {
      
        print(paste0("Working on link ", y, "/", length(links)))
        
     
      
        
        ref_url <- read_html(links[y])
             

        #pull out the agenda items from meeting b
        items <- ref_url %>% html_nodes("td > a") %>% html_text(trim = T)
        
        #pull out agenda item numbers for meeting b
        nos <- ref_url %>% html_nodes(".agenda-item-number") %>% html_text(trim = T)
        
        #pull out links to each referat item
        ref_links <- ref_url %>% html_nodes("td > a") %>% html_attr("href")
        
        #Pull out each referat item and put it into a list, to then unlist the list
        refs <- list()
        for(lf in 1:length(nos)){
            
            file.name <- paste0("../data_archive/copenhagen_archive/", dates[y], " item ", nos[lf], ".RData")
            
            if (file.exists(file.name)){
                #if archived file exists, load it instead of downloading again
                load(file.name)
            } else {
            
            agenda_link <- read_html(ref_links[lf])
            agenda_link <- htmlParse(agenda_link)
            agenda <- saveXML(agenda_link)
            save(agenda, file = file.name)
            }
            
            agenda <- htmlParse(agenda)
            
            ref <- xpathSApply(doc = agenda, 
                                       path = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'sec-inner', ' ' ))]", 
                               xmlValue)
            
            ref <- str_squish(ref[5])
            ref <- sub("Del Tilgængelighed Udskriv Del på Facebook Linkedin share Del på Twitter E-mail ", "", ref)
            refs[[lf]] <- ref
            }
          refs <- unlist(refs)  
        
         #pull out date of meeting b
        date <- ref_url %>% html_nodes(".agenda-overview td") %>% html_text(trim = T)
        date <- str_extract(date, "[[:alnum:]]*[[:punct:]][[:alnum:]]*[[:punct:]][[:alnum:]]*")
        date <- as.Date(date, "%d-%m-%Y")
        
        #assemble dataframe
        df <- data.frame(
            city = "Copenhagen",
            date = date,
            agenda_no = nos,
            title = items,
            referat = refs,
            stringsAsFactors = F
        )

        #put that dataframe into our list of dataframes
        dfs[[length(dfs) + 1]] <- df
        
    y <- y +1
    }
    ###################################################################
    ## END SECONDARY LOOP ###################################################
    ###################################################################
  x <- x +1  
}
###################################################################
## END SEARCh PAGE LOOP ##################################################
###################################################################


#put together all dataframes into one big one
out <- as.data.frame(rbindlist(dfs))
save(out, file = '../data_archive/copenhagen_07-17.RData')