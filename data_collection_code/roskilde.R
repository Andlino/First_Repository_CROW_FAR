
## roskilde.R
## Massimo G Losinno
## July 2019
## Scrape Roskilde city council referater

# rm(list = ls())
# setwd("C:/Users/musse/Aarhus universitet/Tim Dennis Runck - CROW_FAR/First_Repository_CROW_FAR/data_collection_code")


library(rvest)
library(RCurl)
library(stringr)
library(data.table)
library(XML)
library(fun)

# Create directories to save files if don't exist
if(!dir.exists("../data_archive")) dir.create("../data_archive")
if(!dir.exists("../data_archive/roskilde_archive")) dir.create("../data_archive/roskilde_archive")

#set base url for looping -- this is more of a 'start' page for this task
start_url <- "https://roskilde.dk/kommunen/byraad-og-udvalg/udvalg-og-referater"

#Launch 'session' at start_url
main.session <- html_session(start_url)

#Extract main form (two drop-down menus) from the start page
main.form <- html_form(main.session)


#Make list of years to loop over
years.list <- main.form[[2]]$fields$`meetings_date_from[value][year]`$options
years.list <- years.list[-1] #drop first option since it's garbage
years.list <- years.list[years.list < 2018]    


#Make list of months to loop over
months.list <- main.form[[2]]$fields$`meetings_date_from[value][month]`$options
months.list <- months.list[-1] #drop first option since it's garbage


#Make list of committees to loop over
com.list <- main.form[[2]]$fields$field_os2web_meetings_committee_tid$options
com.list <- com.list[-1] #drop first option since it's garbage



#empty list for storing data frames
dfs <- list()

###################################################################
## START YEAR LOOP ################################################
###################################################################



for(j in 1:length(years.list)) { #LOOP OVER YEARS
  print(paste("Working on year", years.list[j])) #report progress in console
  
  #set main form to year j
  form.set.year <- set_values(main.form[[2]], `meetings_date_from[value][year]` = years.list[j])
  
  
  #fix 'type' of object for the button
  #NB: this enables rvest to realize it should
  #click the button attached to this form
  form.set.year$fields[[4]]$type <- "submit"
  
  #submit form -- i.e. retrieve page with new form for year j
  year.page <- submit_form(main.session, form.set.year)
  
  #extract form data for year j 
  year.form <- html_form(year.page)
  
  ### Starting loop over months ###
  for(mc in 1:length(months.list)){
    
    dates <- "Not a date" #creating a date count to distenguish the first scrape in each month
    
    month <- months.list[mc]  
    
    ### Starting loop over committees ###    
    for(cc in 1:length(com.list)){
      
      print(paste0("Working on year ", years.list[j], " - month ", month, " - committee ", cc, "/", length(com.list))) #report progress in console
      
      
      
      form.set.mtg <- set_values(year.form[[2]], 'field_os2web_meetings_committee_tid' = com.list[cc], 'meetings_date_from[value][month]' = month)
      
      #again, fix the 'type' associated with the form submission button
      form.set.mtg$fields[[4]]$type <- "submit"
      
      mtg.page <- submit_form(year.page, form.set.mtg)    
      
      #submit the form to retrieve meetings  from year j month mc and meeting cc
      month_url <- mtg.page$response$request$url
      month_url <- read_html(month_url)      
     
      
      
       links <- month_url %>% html_nodes('a') %>% html_attr("href")      
       links <- links[-1:-32]
       link_numbers <- grep("^/dagsorden-og-referat/moeder/.*", links)
      
       if(length(link_numbers) >= 1){
         
         links <- links[link_numbers]
         links <- unique(links)
         links <- paste0("https://roskilde.dk", links)
         
         #Get dates for each meeting
         met_dates <- str_extract(links, "-[[:digit:]]*-[[:digit:]]*-[[:digit:]]*")
         met_dates <- sub("^-", "", met_dates)
         
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
         items <- html_link %>% html_nodes(".meetnig-title") %>% html_text(trim = T)
         
         #pull out agenda item numbers for meeting i in year j
         nos <- 1:length(items)
         
         #Checking if any agenda items are closed, in which case they are to be left out
         item_check <- grep("(Lukket)", items)
         
         if(length(item_check) >= 1){
         items <- items[-item_check]
         nos <- nos[-item_check]
         }
         
         if(length(items) >= 1){
         
           date <- met_dates[lc]
           
           dates <- str_c(c(dates, date))
           date <- make.unique(dates, "-")[length(dates)]
           ref_date <- dates[length(dates)]
           
           file.name <- paste0("../data_archive/roskilde_archive/", date, ".RData")
           
           
           
           
           if (file.exists(file.name)){
             #if archived file exists, load it instead of downloading again
             load(file.name)
           } else {
             
             meeting <- htmlParse(html_link)
             #Saving each individual item
             meeting <- saveXML(meeting)
             save(meeting, file = file.name)
           }
           
           
           
           
           meeting <- htmlParse(meeting)
           
           ref <- xpathSApply(doc = meeting, 
                              path = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'bullet-body', ' ' ))]", 
                              xmlValue)
           
           ref <- str_squish(ref)
          
           
           #assemble dataframe
           df <- data.frame(
             city = "Roskilde",
             date = ref_date,
             agenda_no = nos,
             title = items,
             referat = ref,
             stringsAsFactors = F
           )
           
           #put that dataframe into our list of dataframes
           dfs[[length(dfs) + 1]] <- df  
         }
           
            
         }}
    }}
}



#put together all dataframes into one big one
out <- as.data.frame(rbindlist(dfs))

#save final data for later
save(out, file = '../data_archive/roskilde_14-17.RData')

#Shut down the computer
shutdown(wait = 0)