########################## Randers ##############################

##############
## Randers.R
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

remDr$navigate("https://dagsordener.randers.dk")
remDr$screenshot(display = TRUE)

webElem <- remDr$findElement(using = "xpath", "//*[@id='udvalg-title']")
webElem$clickElement()
webElem2 <- remDr$findElement(using = "xpath", "//*[@id='udvalg-dropdown-ul']/li[5]/a")
webElem2$clickElement()
webElem3 <- remDr$findElement(using = "xpath", "//*[@id='soeg']/div[6]")
webElem3$clickElement()

element <- remDr$findElement("css", "body")
############ PRESS SCROLL DOWN MULTIPLE TIMES!!!##############
element$sendKeysToElement(list(key = "page_down"))
elements <- remDr$findElements(using = "xpath","//*[@id='resultaterplaceholder']/a")
links <- unlist(lapply(elements, function(x){x$getElementAttribute('href')}))

if(!dir.exists("./data_archive")) dir.create("./data_archive")
if(!dir.exists("./data_archive/randers_archive")) dir.create("./data_archive/randers_archive")

dfs <- list()
x <- 35
# create progress bar
pb <- txtProgressBar(min = 1, max = 330, initial = 1, char = "-", width = 60, style = 3)

for(i in links[35:123]) { #LOOP OVER MEETINGS
    
        setTxtProgressBar(pb, i)
        
        
        remDr$navigate(i)
        remDr$refresh()
        Sys.sleep(10)
       
    
    source <- remDr$getPageSource()[[1]]
    p <- read_html(source)
    
    titlepage <- p %>% html_nodes('.title') %>% html_text(trim=T)
    titlepage <- gsub("\n", "", titlepage)
    titlepage <- str_replace_all(titlepage, "  " , "")
    titlepage <- paste0(titlepage, sep = "", collapse = "")
    
    #define file name for archived copy of page
    file.name <- paste0("./data_archive/randers_archive/", x, "---", titlepage, ".RData")
    
    #CHECK IF FILE EXISTS BEFORE RE-DOWNLOADING
    if (file.exists(file.name)){
        #if archived file exists, load it instead of downloading again
        load(file.name)
    } else {
        source <- remDr$getPageSource()[[1]]
        save(source, file = file.name)
        x <- x+1
    }
    
    {
        
        p <- read_html(source)
        text <- p %>% html_nodes('.punkt') %>% html_text(trim = T)
        date <- p %>% html_nodes('.dagsordeninfo .dato') %>% html_text(trim=T)
        label <- p %>% html_nodes('.text-center.punkt-tabel') %>% html_text(trim=T)
        title <- p %>% html_nodes('.overskrift') %>% html_text(trim=T)
        
        df <- data.frame(
            city = "Randers",
            date = date, 
            label = label,
            title = title, 
            text = text, 
            stringsAsFactors = FALSE
        )
        
        dfs[[length(dfs) + 1]] <- df
    }
    

}


df <- as.data.frame(rbindlist(dfs))
save(df, file = './data_archive/Randers.RData')
