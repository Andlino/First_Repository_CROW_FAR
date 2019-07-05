## aarhus.R
## Matt W. Loftis
## July 2019
## Scrape Aarhus city council referater

library(rvest)
library(RCurl)
library(stringr)
library(data.table)

# Create directories to save files if don't exist
if(!dir.exists("../data_archive")) dir.create("../data_archive")
if(!dir.exists("../data_archive/aarhus_archive")) dir.create("../data_archive/aarhus_archive")

#set base url for looping -- this is more of a 'start' page for this task
start_url <- "https://aarhus.dk/demokrati/politik/dagsordner-og-referater/v/16009/Aarhus-Byrad/Referat/2018-06-20/?agendaId=344520"

#Launch 'session' at start_url
main.session <- html_session(start_url)

#Extract main form (two drop-down menus) from the start page
main.form <- html_form(main.session)

#Pull out possible years -- options in the form on the start page
years.list <- main.form[[1]]$fields$year$options
years.list <- years.list[-1] #drop first option since it's garbage
years.list <- years.list[years.list < 2018]

#empty list for storing data frames
dfs <- list()

###################################################################
## START YEAR LOOP ################################################
###################################################################

for(j in 1:length(years.list)) { #LOOP OVER YEARS
    print(paste("Working on year", years.list[j])) #report progress in console
    
    #set main form to year j
    form.set.year <- set_values(main.form[[1]], year = years.list[j])
    
    #fix 'type' of object for the button
    #NB: this enables rvest to realize it should
    #click the button attached to this form
    form.set.year$fields[[3]]$type <- "submit"
    
    #submit form -- i.e. retrieve page with new form for year j
    year.page <- submit_form(main.session, form.set.year)
    
    #extract form data for year j -- this gives unique codes for each mtg in year j
    year.form <- html_form(year.page)
    
    #extract unique codes for each meeting in year j
    year.mtgs <- year.form[[1]]$fields$agendaId$options
    
    #extract dates of meetings for file names
    short.date <- attributes(year.mtgs)$names
    short.date <- gsub(".", "", short.date, fixed = T)
    short.date <- gsub(" ", "-", short.date, fixed = T)
    
    
    ###################################################################
    ## START MEETING LOOP #############################################
    ###################################################################
    
    for(i in 1:length(year.mtgs)) { #LOOP OVER MEETINGS
        print(paste("------- Meeting", i, "of", length(year.mtgs), "in year", years.list[j])) #print progress to console
        
        #define file name for archived copy of page
        file.name <- paste0("../data_archive/aarhus_archive/", years.list[j], "--", short.date[i], ".RData")
        
        #CHECK IF FILE EXISTS BEFORE RE-DOWNLOADING
        if (file.exists(file.name)){
            #if archived file exists, load it instead of downloading again
            load(file.name)
            mtg.page <- read_html(to.save)
        } else {
            #set values in main form equal to year j and meeting i
            form.set.mtg <- set_values(year.form[[1]], agendaId = year.mtgs[i])
            
            #again, fix the 'type' associated with the form submission button
            form.set.mtg$fields[[3]]$type <- "submit"
            
            #submit the form to retrieve meeting i from year j
            mtg.page <- submit_form(year.page, form.set.mtg)
            
            #download and archive page for later
            to.save <- getURL(mtg.page$response$request$url)
            save(to.save, file = file.name)
        }
        
        #pull out the agenda items from meeting i in year j
        items <- mtg.page %>% html_nodes(".esdh__headline") %>% html_text(trim = T)
        
        #pull out agenda item numbers for meeting i in year j
        nos <- mtg.page %>% html_nodes(".numeric-badge") %>% html_text(trim = T)
        
        #pull out referat item
        ref <- mtg.page %>% html_nodes(".accordion__content") %>% html_text(trim = T)
        
        #pull out date of meeting i in year j
        date <- mtg.page %>% html_nodes("#content-headline") %>% html_text(trim = T)
        date <- str_extract(date, "[[:digit:]]+-[[:digit:]]+-[[:digit:]]+")
        
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
    ###################################################################
    ## END MTG LOOP ###################################################
    ###################################################################
    
}
###################################################################
## END YEAR LOOP ##################################################
###################################################################


#put together all dataframes into one big one
out <- as.data.frame(rbindlist(dfs))

#save final data for later
save(out, file = '../data_archive/aarhus_13-17.RData')


