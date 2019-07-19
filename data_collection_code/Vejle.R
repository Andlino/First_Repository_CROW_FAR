###############
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


remDr <- remoteDriver(remoteServerAddr = "192.168.99.100", port = 4445L)
remDr$open()

remDr$navigate("https://dagsordener.vejle.dk/vis/#f0099692-ebea-4e99-823a-2f3c07831a8e")

remDr$screenshot(display = TRUE)


webElem <- remDr$findElement(using = "xpath", "//*[@id='udvalg-title']")
webElem$clickElement()
webElem2 <- remDr$findElement(using = "xpath", "//*[@id='udvalg-dropdown-ul']/li[3]/a")
webElem2$clickElement()
webElem3 <- remDr$findElement(using = "xpath", "//*[@id='udvalg-dropdown-ul']/li[3]/ul/li[2]/a")
webElem3$clickElement()
webElem4 <- remDr$findElement(using = "xpath", "//*[@id='searchButton']")
webElem4$clickElement()

source <- remDr$getPageSource()[[1]]
p <- read_html(source)

text <- p %>% html_nodes('.punkt') %>% html_text(trim = T)

fulltext <- str_c(c(text),collapse=' ')    





element <- remDr$findElement("css", "body")
element$sendKeysToElement(list(key = "page_down"))

elements <- remDr$findElements(using = "css","#resultaterplaceholder > a")
links <- unlist(lapply(elements, function(x){x$getElementAttribute('href')}))




dfs <- list()
for(i in 1:length(links)) { #LOOP OVER MEETINGS
    
    
    remDr$navigate(i)
    remDr$setTimeout(type = "page load", milliseconds = 30000)
    
    {
        
        
        source <- getURL(links[[i]])
        p <- read_html(source)
        city <- "Vejle"
        date <- remDr$findElements(using = "css", ".dagsordeninfo .dato")
        date <- lapply(date,function(x) x$getElementText()[[1]])
        date <- unlist(date)
        
        label <-  remDr$findElements(using = "css",".text-center.punkt-tabel")
        label <- lapply(label,function(x) x$getElementText()[[1]])
        
        title <- remDr$findElements(using = "css", ".overskrift")
        title <- lapply(title,function(x) x$getElementText()[[1]])
        
        text <- remDr$findElements(using = "css",".punkt")
        text <- lapply(text,function(x) x$getElementText()[[1]])
        
        
        test <- remDr$findElements(using = "css","span")
        texts_lapply <- lapply(test,function(x) x$getElementText()[[1]])
        testlist <- texts_lapply[lapply(texts_lapply,length)>0 & texts_lapply != "" & texts_lapply != " "]
        fulltext <- str_c(c(testlist),collapse=' ')    
        
        
        
        df <- data.frame(
            city = city,
            date = date, 
            string = string,
            stringsAsFactors = FALSE
        )
        
        dfs[[length(dfs) + 1]] <- df
    }
    
    print(i)
    
}


############## CLICK ALL SUBSETS ############################
element <- remDr$findElement(using = "css",".overskrift")
remDr$mouseMoveToLocation(webElement = element)
elements <- remDr$findElements(using = "css",".overskrift")
for(element in elements){
    element$clickElement()
}

test <- remDr$findElements(using = "css","span")
# get element text
text <- test$getElementText()[[1]]
#get element attribute value(get the value for class)
element$getElementAttribute("class")[[1]]

texts_lapply <- lapply(test,function(x) x$getElementText()[[1]])
testlist <- texts_lapply[lapply(texts_lapply,length)>0 & texts_lapply != "" & texts_lapply != " "]

string <- str_c(c(testlist),collapse=' ')    


texts <- c()
for(ele in elements){
    texts <- append(texts,ele$getElementText()[[1]])
}

remDr$refresh()
remDr$screenshot(display = TRUE)


#################################################################################################
##########################################NEW APPROACH###########################################
#################################################################################################

start_url <- links[[i]]

#Launch 'session' at start_url
main.session <- html_session(start_url)

#Extract main form (two drop-down menus) from the start page
main.form <- html_form(main.session)

source <- getURL(links[[i]])
p <- read_html(source)

withJS <- xml2::read_html(links[[i]]) %>%
    rvest::html_nodes(body) %>%
    rvest::html_text()
