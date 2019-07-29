## frederiksberg.R
## Matt W. Loftis
## July 2019
## Scrape Frederiksberg city council referater

# rm(list = ls())
# setwd("C:/Users/musse/Aarhus universitet/Tim Dennis Runck - CROW_FAR/First_Repository_CROW_FAR/data_collection_code")


library(rvest)
library(RCurl)
library(stringr)
library(data.table)
library(XML)

# Create directories to save files if don't exist
if(!dir.exists("../data_archive")) dir.create("../data_archive")
if(!dir.exists("../data_archive/frederiksberg_archive")) dir.create("../data_archive/frederiksberg_archive")

#set base url for looping -- this is more of a 'start' page for this task
start_url <- "https://www.frederiksberg.dk/politik/dagsordener-referater-og-modeplan/find-dagsordener-og-referater"

#Launch 'session' at start_url
main.session <- html_session(start_url)

#Extract main form (two drop-down menus) from the start page
main.form <- html_form(main.session)

months <- c("januar", "februar", "marts", "april", "maj", "juni", "juli", "august", "september", "oktober", "november", "december")


#Pull out possible years -- options in the form on the start page
years.list <- main.form[[2]]$fields$year$options
years.list <- years.list[-1] #drop first option since it's garbage
years.list <- years.list[years.list < 2018]

dates <- "Not a date" #creating a date count to distenguish the first scrape

#empty list for storing data frames
dfs <- list()

###################################################################
## START YEAR LOOP ################################################
###################################################################

for(j in 1:length(years.list)) { #LOOP OVER YEARS
  print(paste("Working on year", years.list[j])) #report progress in console
  

  #set main form to year j
  form.set.year <- set_values(main.form[[2]], year = years.list[j])
  
  #fix 'type' of object for the button
  #NB: this enables rvest to realize it should
  #click the button attached to this form
  form.set.year$fields[[4]]$type <- "submit"
  
  #submit form -- i.e. retrieve page with new form for year j
  year.page <- submit_form(main.session, form.set.year)
  
  #extract form data for year j 
  year.form <- html_form(year.page)
  
  #extract months
  months_count <- year.form[[2]]$fields$month$options
  months_count <- months_count[-1]
  
  com_count <- year.form[[2]]$fields$committee$options
  com_count <- com_count[-1]
  
    for(mc in 1:length(months_count)){

  month <- months_count[mc]  

  
      for(cc in 1:length(com_count)){
        
          print(paste0("Working on year ", years.list[j], " - month ", month, " - committee ", cc, "/", length(com_count))) #report progress in console
          
        form.set.mtg <- set_values(year.form[[2]], committee = com_count[cc], month = month)
        
        #again, fix the 'type' associated with the form submission button
          form.set.mtg$fields[[4]]$type <- "submit"
  
        #submit the form to retrieve meeting i from year j
          mtg.page <- submit_form(year.page, form.set.mtg)
          month_url <- mtg.page$response$request$url
          month_url <- read_html(month_url)
          
          links <- month_url %>% html_nodes('.views-field-field-mw-meeting-committee a') %>% html_attr("href")
          if(length(links) >= 1){
            
            links <- paste0("https://www.frederiksberg.dk", links)
            
            for(lc in 1:length(links)){
              
                
                
                ok <- FALSE
                problem <- FALSE
                count <- 1
                
                #Reading the html for the first meeting
                while (ok == FALSE) {
                    html_link <- tryCatch({                  
                        read_html(links[lc])
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
                if(count == 10) {count <- 1
                lc <- lc +1
                next}
                
                
                
              #pull out the agenda items from meeting i in year j
              items <- html_link %>% html_nodes(".open-meeting") %>% html_text(trim = T)
              if(length(items) >= 1){
              
              header_date_1 <- html_link %>% html_nodes(".field--type-string") %>% html_text(trim = T)
              header_date <- str_extract(header_date_1, paste("den [[:digit:]]*[[:punct:]] [[:alnum:]]*", years.list[j]))
              header_date <- sub("den ", "", header_date)
              header_date <- sub("[[:punct:]]", "", header_date)
              header_date <- sub(months[as.numeric(month)], month, header_date)
              if(as.numeric(month) < 10){header_date <- sub(" [[:digit:]] ", paste0(" 0", month, " "), header_date)}
              if((as.numeric(str_extract(header_date, "^[[:digit:]]*"))) < 10 & str_extract(header_date, "^[[:digit:]]") != 0){header_date <- paste0("0", header_date)}
              header_date <- gsub(" ", "-", header_date)
            
              dates <- str_c(c(dates, header_date))
              date <- make.unique(dates, "-")[length(dates)]
              ref_date <- dates[length(dates)]
              
              file.name <- paste0("../data_archive/frederiksberg_archive/", date, ".RData")
              
              
            
            if (file.exists(file.name)){
                #if archived file exists, load it instead of downloading again
                load(file.name)
            } else {
            
                
                meeting <- htmlParse(html_link)
                #Saving each individual item
                meeting <- saveXML(meeting)
                save(meeting, file = file.name)
            }
             
                
                #pull out agenda item numbers for meeting i in year j
                nos <- 1:length(items)
                
                
                meeting <- htmlParse(meeting)
                
                ref <- xpathSApply(doc = meeting, 
                                   path = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'container', ' ' ))] | //*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'active', ' ' ))]", 
                                   xmlValue)
                
                ref <- str_squish(ref[3])
                ref <- sub(paste(header_date_1, " Fold alle ud "), "", ref)
                ref <- sub("Del Facebook Linkedin Twitter Udskriv Abonnér", "", ref)
                
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
                    
                     
                
                #assemble dataframe
                df <- data.frame(
                    city = "Frederiksberg",
                    date = ref_date,
                    agenda_no = nos,
                    title = items,
                    referat = refs,
                    stringsAsFactors = F
                )
                
                #put that dataframe into our list of dataframes
                dfs[[length(dfs) + 1]] <- df    
                
                
          }}
          
      }}}}

#put together all dataframes into one big one
out <- as.data.frame(rbindlist(dfs))

#save final data for later
save(out, file = '../data_archive/frederiksberg_12-17.RData')
