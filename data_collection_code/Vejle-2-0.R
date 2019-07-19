########################## Vejle 2.0 ##############################

##############
## Vejle.R
## Tim Runck
## July 2019
## Scrape Odense city council referater

library(rvest)
library(RCurl)
library(stringr)
library(XML)
library(data.table)
library(curl)
library(magrittr)
library(RSelenium)

# Create directories to save files if don't exist
if(!dir.exists("../data_archive")) dir.create("../data_archive")
if(!dir.exists("../data_archive/vejle_archive")) dir.create("../data_archive/vejle_archive")


remDr <- remoteDriver(remoteServerAddr = "192.168.99.100", port = 4445L)
remDr$open()

remDr$navigate("https://dagsordener.vejle.dk")

remDr$screenshot(display = TRUE)


webElem <- remDr$findElement(using = "xpath", "//*[@id='udvalg-title']")
webElem$clickElement()
webElem2 <- remDr$findElement(using = "xpath", "//*[@id='udvalg-dropdown-ul']/li[3]/a")
webElem2$clickElement()
webElem3 <- remDr$findElement(using = "xpath", "//*[@id='udvalg-dropdown-ul']/li[3]/ul/li[2]/a")
webElem3$clickElement()
webElem4 <- remDr$findElement(using = "xpath", "//*[@id='searchButton']")
webElem4$clickElement()

element <- remDr$findElement("css", "body")
element$sendKeysToElement(list(key = "page_down"))

elements <- remDr$findElements(using = "css","#resultaterplaceholder > a")
links <- unlist(lapply(elements, function(x){x$getElementAttribute('href')}))


dfs <- list()
for(i in 1:length(links)) { #LOOP OVER MEETINGS
    
    
    remDr$navigate(links[[i]])
    remDr$setTimeout(type = "page load", milliseconds = 30000)
    
    {
        source <- remDr$getPageSource()[[1]]
        p <- read_html(source)
        
        
        text <- p %>% html_nodes('.punkt') %>% html_text(trim = T)
        date <- p %>% html_nodes('.dagsordeninfo .dato') %>% html_text(trim=T)
        label <- p %>% html_nodes('.text-center.punkt-tabel') %>% html_text(trim=T)
        title <- p %>% html_nodes('.overskrift') %>% html_text(trim=T)
        
        df <- data.frame(
            city = "Vejle",
            date = date, 
            label = label,
            title = title, 
            text = text, 
            stringsAsFactors = FALSE
        )
        
        dfs[[length(dfs) + 1]] <- df
    }
    
    print(i)
    
}
