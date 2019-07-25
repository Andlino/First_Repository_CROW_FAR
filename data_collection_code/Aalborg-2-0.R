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
if(!dir.exists("./data_archive")) dir.create("./data_archive")
if(!dir.exists("./data_archive/aalborg_archive")) dir.create("./data_archive/aalborg_archive")


remDr <- remoteDriver(remoteServerAddr = "192.168.99.100", port = 4445L)
remDr$open()


remDr$navigate("https://www.aalborg.dk/politik/dagsordener-og-referater/byraadet/arkiv-byraadet")
remDr$screenshot(display = TRUE)

#accept popup

webElem <- remDr$findElement(using = "xpath", "/html/body/div[2]/div/div[1]/div[1]/button")
webElem$clickElement()

# Extract links

elements <- remDr$findElements(using = "css","#ContentPlaceHolderDefault_ContentPlaceHolderDefault_ctl06_Arkiv_25_gvResultsDagsorden a")
links <- unlist(lapply(elements, function(x){x$getElementAttribute('href')}))

dfs <- list()

x <- 1
# create progress bar
pb <- txtProgressBar(min = 1, max = 114, initial = 1, char = "-", width = 60, style = 3)

for(i in 1:length(links)) { #LOOP OVER MEETINGS
    
    setTxtProgressBar(pb, i)
    
    {
        remDr$navigate(links[[i]])
        remDr$setTimeout(type = "page load", milliseconds = 30000)
        Sys.sleep(5)
        
        elements2 <- remDr$findElements(using = "css","#ContentPlaceHolderDefault_ContentPlaceHolderDefault_ctl06_Referat_25_dvReferat strong")
        titlepage <- lapply(elements2,function(x) x$getElementText()[[1]])
        titlepage <- unlist(titlepage)
        
        file.name <- paste0("./data_archive/aalborg_archive/", x, "---", titlepage, ".RData")
        
        source <- remDr$getPageSource()[[1]]
        save(source, file = file.name)
        x <- x+1
    }
    elements3 <- remDr$findElements(using = "xpath","//*[@id='ContentPlaceHolderDefault_ContentPlaceHolderDefault_ctl06_Referat_25_Header']/tbody/tr[1]/td[3]/span[1]/a")
    linkspdf <- unlist(lapply(elements3, function(x){x$getElementAttribute('href')}))

    df <- data.frame(
        city = "Aalborg",
        title = titlepage, 
        linkspdf = linkspdf, 
        stringsAsFactors = FALSE
    )
    
    dfs[[length(dfs) + 1]] <- df
    
}


df <- as.data.frame(rbindlist(dfs))
save(df, file = './data_archive/AalborgPdfLinks.RData')

#################################################################################################
#################################################################################################


dfs <- list()

x <- 1

for(i in 1:length(links)) {
    
    url <- links[[i]]
    to.save <- getURL(url) # download html source code
    p <- read_html(to.save)
    titlepage <- p %>% html_nodes("#ContentPlaceHolderDefault_ContentPlaceHolderDefault_ctl06_Referat_25_dvReferat strong") %>% html_text(trim = TRUE)
    file.name <- paste0("./data_archive/aalborg_archive/", x, "---", titlepage, ".RData")
    save(to.save, file = file.name) # save downloaded source code
    
    linkspdf1 <- p %>% html_nodes("td > span > a") %>% html_attr('href')
    linkspdf2 <- "https://www.aalborg.dk/"
    linkspdf <- paste0(linkspdf2, linkspdf1)
    
    df <- data.frame(
        city = "Aalborg",
        title = titlepage, 
        linkspdf = linkspdf, 
        stringsAsFactors = FALSE
    )
    
    dfs[[length(dfs) + 1]] <- df
    x <- x+1
}

df <- as.data.frame(rbindlist(dfs))
save(df, file = './data_archive/AalborgPdfLinks.RData')

pdfurl <- "https://www.aalborg.dk/usercontrols/AalborgKommune/Referater/Pdf.aspx?pdfnavn=2017-11-13%2016.00.pdf&type=moede&pdfid=14322"
download.file(pdfurl, 'introductionToR.pdf', mode="wb")

for (i in 1:length(df$linkspdf)) {
    pdfurl <- df$linkspdf[[i]]
    title <- df$title[[i]]
    title <- paste0(title, ".pdf")
    
    download.file(pdfurl, title, mode="wb")

}
