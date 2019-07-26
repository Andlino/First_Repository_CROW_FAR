## copenhagen.R
## Massimo G Losinno
## July 2019
## Scrape Copengange city council referater

# rm(list = ls())
# setwd("C:/Users/au534428/Aarhus universitet/Tim Dennis Runck - CROW_FAR/First_Repository_CROW_FAR/data_collection_code")

library(rvest)
library(RCurl)
library(stringr)
library(XML)
library(data.table)
library(curl)
library(fun)
library(pdftools)
library(readtext)


# Create directories to save files if don't exist
if(!dir.exists("../data_archive")) dir.create("../data_archive")
if(!dir.exists("../data_archive/copenhagen_archive")) dir.create("../data_archive/copenhagen_archive")

#set base url for looping -- this is more of a 'start' page for this task
start_url <- "https://www.kk.dk/dagsordener-og-referater?field_committee_type_tid%5B%5D=13957&field_committee_type_tid%5B%5D=13960&field_committee_type_tid%5B%5D=13959&field_committee_type_tid%5B%5D=13958&title=&field_date_single_value%5Bvalue%5D%5Bdate%5D=01.01.2007&field_date_single_value_1%5Bvalue%5D%5Bdate%5D=01.01.2018&page="

count <- 1
dfs <- list() #list for dataframes for each meeting
dates <- "Not a date" #creating a date count to distenguish the first scrape

x <- 0
#Starting main loop over the different pages
while (x <= 320) { #320 = magic number
    
    print(paste0("Working on page ", x + 1, "/", 321))
    
    y <- 1

    #Creating the url to get li nks for each of the meetings
    url <- paste0(start_url, x) 
    url <- read_html(url)
    
    #Getting the links for the meetings of the page
    links <- url %>% html_nodes('#main-content .first a') %>% html_attr("href")
    links2 <- grep("^/indhold", links)
    links <- links[links2] #Making sure that only links to meetings get into the list of links
    links <- paste0("https://www.kk.dk", links)
    
    
    #Starting secondary loop over the different meetings
    while (y <= length(links)) {
        
        print(paste0("Working on link ", y, "/", length(links)))
        
        
        ok <- FALSE
        problem <- FALSE
        count <- 1
        
        #Reading the html for the first meeting
        while (ok == FALSE) {
            ref_url <- tryCatch({                  
                read_html(links[y])
            },
            error = function(e) {problm <- TRUE
            Sys.sleep(2)
            e
            }
            )
            if ("error" %in% class(ref_url)) {
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
        

        #pull out the agenda items from meeting 
        items <- ref_url %>% html_nodes("td > a") %>% html_text(trim = T)
        
        #pull out links to each referat item
        ref_links <- ref_url %>% html_nodes("td > a") %>% html_attr("href")
        
        #Making item numbers
        nos <- 1:length(ref_links)
        
        #Pull the date for the meeting, this is also the name for the archive files, which is why their is a loop to make sure meetings on the same day gets different names
        date <- ref_url %>% html_nodes(".agenda-overview td") %>% html_text(trim = T)
        date <- str_extract(date, "[[:alnum:]]*[[:punct:]][[:alnum:]]*[[:punct:]][[:alnum:]]*")
        dates <- str_c(c(dates, date))
        date <- make.unique(dates, "-")[length(dates)]
        
        
        #Pull out each referat item and put it into a list, to then unlist the list
        refs <- list()
        
        if (length(ref_links) == 0){
            
            y <- y +1
            next}
        for(lf in 1:length(ref_links)){
            
            pdf <- grepl("[[:punct:]]PDF$|[[:punct:]]DOC$", ref_links[lf])
            if(pdf != TRUE){

                file.name <- paste0("../data_archive/copenhagen_archive/", date, " item ", nos[lf], ".RData")
                pdf <- 1
                
            }else{
            
            pdf <- grepl("[[:punct:]]PDF$", ref_links[lf])
            if(pdf == TRUE){
            print(paste(y, "is a PDF"))
            
                file.name <- paste0("../data_archive/copenhagen_archive/", date, " item ", nos[lf], ".PDF")
                pdf <- 2
                
            }else{
                pdf <- grepl("[[:punct:]]DOC$", ref_links[lf])
                if(pdf == TRUE){
                print(paste(y, "is a DOC"))
                    
                    file.name <- paste0("../data_archive/copenhagen_archive/", date, " item ", nos[lf], ".DOC")
                    pdf <- 3
                    }}}
            
            
            

            if (file.exists(file.name)){
                #if archived file exists, load it instead of downloading again
                if(pdf == 1){load(file.name)}
            } else {
               
                
                if(pdf == 1){
                #Reading and downloading the individual items fromn their individual webpages
                ok <- FALSE
                problem <- FALSE
                count <- 1
                
                while (ok == FALSE) {
                    agenda_link <- tryCatch({                  
                        read_html(ref_links[lf])
                    },
                    error = function(e) {problm <- TRUE
                    Sys.sleep(2)
                    e
                    }
                    )
                    if ("error" %in% class(agenda_link)) {
                        print(paste("Problem with", lf))
                        count <- count + 1
                        
                    } else {
                        if(problem == TRUE ){print(paste("Problem with", lf, "fixed"))}
                        ok <- TRUE
                    }
                    if(count == 10){
                        break
                    }
                    
                    }
                
                agenda_link <- tryCatch({
                    htmlParse(agenda_link)
                },
                error = function(e) {problm <- TRUE
                Sys.sleep(2)
                e
                })
                
                if("error" %in% class(agenda_link)) {print(paste("Not downloading xml", y, lf))
                    count <- 10
                    break
                }else{
                    
                    #Saving the xml files
                    #Saving each individual item
                    agenda <- saveXML(agenda_link)
                    save(agenda, file = file.name)
                }
                
                
               
                }else{
                    
                if(pdf == 2){
                    
                    error_link <- tryCatch({
                        read_html(ref_links[lf])
                        },
                        error = function(e) {problm <- TRUE
                        Sys.sleep(2)
                        e
                        })
                    
                    if("error" %in% class(error_link)) {print(paste("Not downloading pdf", y, lf))
                        count <- 10
                        break
                        }else{
                        
                        #Saving the pdf files
                        download.file(ref_links[lf], 
                                      destfile = file.name, 
                                      method = "auto", quiet = T, mode = "wb",
                                      cacheOK = TRUE, extra = getOption("download.file.extra"))
                        }
                    
                    
                    agenda <- pdf_text(file.name)
                    agenda <- paste(agenda,collapse="")
                    ref <- str_squish(agenda)
                }else{
                if(pdf == 3){
                    
                    error_link <- tryCatch({
                        read_html(ref_links[lf])
                    },
                    error = function(e) {problm <- TRUE
                    Sys.sleep(2)
                    e
                    })
                    
                    if("error" %in% class(error_link)) {print(paste("Not downloading DOC", y, lf))
                        count <- 10
                        break
                    }else{
                    
                    download.file(ref_links[lf], 
                                  destfile = file.name, 
                                  method = "auto", quiet = FALSE, mode = "wb",
                                  cacheOK = TRUE, extra = getOption("download.file.extra"))
                    }
                    
                    
                     agenda <- readtext(file.name)
                    agenda <- paste(agenda$text,collapse="")
                    ref <- str_squish(agenda)
                    }    
                    
                    
                }}}
            if(pdf == 1){
            agenda <- htmlParse(agenda)
            
            ref <- xpathSApply(doc = agenda, 
                               path = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'sec-inner', ' ' ))]", 
                               xmlValue)
    
            if(length(ref) == 0){
                ref <- xpathSApply(doc = agenda, 
                                   path = "//div/p[.]", 
                                   xmlValue)
                
                ref <- paste(ref, collapse = "")
                ref <- str_squish(ref)
            }else{
                ref <- str_squish(ref[5])
                ref <- sub("Del Tilgængelighed Udskriv Del på Facebook Linkedin share Del på Twitter E-mail ", "", ref)    
            }}
            
            refs[[lf]] <- ref
        }
        if(count == 10) {count <- 1
        y <- y +1
        next}
        
        refs <- unlist(refs)  
        
        
        
        
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
if(x == 320){shutdown(wait = 0)
    }
