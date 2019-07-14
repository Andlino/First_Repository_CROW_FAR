## Aalborg.R
## Tim Runck
## July 2019
## Scrape Aalborg city council referater

library(rvest)
library(RCurl)
library(stringr)
library(XML)
library(data.table)
library(curl)
library(RSelenium)
library(plyr)

# Create directories to save files if don't exist
if(!dir.exists("../data_archive")) dir.create("../data_archive")
if(!dir.exists("../data_archive/aalborg_archive")) dir.create("../data_archive/aalborg_archive")

nytnyt <- function (periods =c(1,15)){
    tictoc <- runif(1, periods[1], periods[2])
    cat(paste0(Sys.time()), "- Sleeping for", round(tictoc, 2), "seconds\n")
    Sys.sleep(tictoc)
}

#set base url for looping -- this is more of a 'start' page for this task
start_url <- "https://www.aalborg.dk/politik/dagsordener-og-referater/byraadet/arkiv-byraadet"

p <- read_html(start_url)

#Getting the dates and links for the meetings of the page
links <- p %>% html_nodes('td > a') %>% html_attr("href")

new_url1 <- "https://www.aalborg.dk"

new_url2 <- paste0(new_url1, links[1:114])


############################################################
##################################################################
## NB: START-UP VARIES ACROSS PLATFORMS ###########################
###################################################################
#docker run -d -p 4445:4444 selenium/standalone-firefox:2.53.0
#docker ps
#docker-machine ip
#john@gaming-J MINGW64 ~
# $ docker pull selenium/standalone-firefox-debug:2.53.0
#2.53.0: Pulling from selenium/standalone-firefox-debug


#john@gaming-J MINGW64 ~
#  $ docker run -d -p 4445:4444 -p 5901:5900 selenium/standalone-firefox-debug:2.53.0
#ee0c0bb8b711723e653ccc26219e314a37c28d2027f939046adcfc90a4beb645

#john@gaming-J MINGW64 ~
#  $ docker-machine ip
#192.168.99.100

#john@gaming-J MINGW64 ~
#  $ docker ps --format 'table {{.Names}}\t{{.Image}}\t{{.ID}}'
#NAMES               IMAGE                                      CONTAINER ID
#focused_tesla       selenium/standalone-firefox-debug:2.53.0   ee0c0bb8b711

#john@gaming-J MINGW64 ~
#  $


remDr <- remoteDriver(remoteServerAddr = "192.168.99.100", port = 4445L)
remDr$open()

dfs <- list()

for (i in new_url2[1:114]) {
    
    remDr$navigate(paste0(i))
    remDr$setTimeout(type = "page load", milliseconds = 30000)
    
    {
        
        webElem <- remDr$findElements(using = "xpath", "//*[@id='ContentPlaceHolderDefault_ContentPlaceHolderDefault_ctl06_Referat_25_Header']/tbody/tr[1]/td[3]/span[1]/a")
        links <- unlist(lapply(webElem, function(x){x$getElementAttribute('href')}))
        
        
        df <- data.frame(
            links = links,
            stringsAsFactors = FALSE
        )
        
        dfs[[length(dfs) + 1]] <- df
    }
    
    print(i)
    nytnyt()
}

df <- ldply(dfs, data.frame)


remDr$screenshot(display = TRUE)

webElem2 <- remDr$findElements(using = "xpath", "/html/body/div[1]/div/div[1]/div[1]/button")
webElem2$clickElement()

cookies <- remDr$getAllCookies	
