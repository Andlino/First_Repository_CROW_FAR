## frederiksberg.R
## Matt W. Loftis
## July 2019
## Scrape Aarhus city council referater

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

dates <- "19" #creating a date count to distenguish the first scrape

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
              
              html_link <- read_html(links[lc])
                
              header_date <- html_link %>% html_nodes(".field--type-string") %>% html_text(trim = T)
              header_date <- str_extract(header_date, paste("den [[:digit:]]*[[:punct:]] [[:alnum:]]*", years.list[j]))
              header_date <- sub("den ", "", header_date)
              header_date <- sub("[[:punct:]]", "", header_date)
              header_date <- sub(months[as.numeric(month)], month, header_date)
              if(as.numeric(month) < 10){header_date <- sub(" [[:digit:]] ", paste0(" 0", month, " "), header_date)}
              header_date <- gsub(" ", "-", header_date)
            
              dates <- str_c(c(dates, header_date))
              date <- make.unique(dates, "-")[length(dates)]
              
              file.name <- paste0("../data_archive/copenhagen_frederiksberg/", date, ".RData")
              
              }
            
            if (file.exists(file.name)){
                #if archived file exists, load it instead of downloading again
                load(file.name)
            } else {
            
                
                html_link <- htmlParse(html_link)
                #Saving each individual item
                meeting <- saveXML(html_link)
                save(agenda, file = file.name)
            }
             
                #pull out the agenda items from meeting i in year j
                items <- html_link %>% html_nodes(".open-meeting") %>% html_text(trim = T)
                
                #pull out agenda item numbers for meeting i in year j
                nos <- 1:length(items)
                
                
                meeting <- htmlParse(meeting)
                
                ref <- xpathSApply(doc = meeting, 
                                   path = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'container', ' ' ))] | //*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'active', ' ' ))]", 
                                   xmlValue)
                
                
                #assemble dataframe
                df <- data.frame(
                    city = "Aarhus",
                    date = date,
                    agenda_no = nos,
                    title = items,
                    referat = ref,
                    stringsAsFactors = F
                )
                
                #put that dataframe into our list of dataframes
                dfs[[length(dfs) + 1]] <- df    
                
                
          }
          
          }}