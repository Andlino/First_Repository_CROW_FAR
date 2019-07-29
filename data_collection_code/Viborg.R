########################## Viborg ##############################

##############
## Viborg.R
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


##############################################################################################################################
############################################################ ARCHIVE 2013 ####################################################
##############################################################################################################################

# Create directories to save files if don't exist
if(!dir.exists("./data_archive")) dir.create("./data_archive")
if(!dir.exists("./data_archive/viborg_archive_2013")) dir.create("./data_archive/viborg_archive_2013")


url <- "https://kommune.viborg.dk/Politik/Dagsordener-og-referater/Dagsordner-og-referater-fra-byraad-udvalg-raad-naevn/Byraadet?y=2013"


remDr <- remoteDriver(remoteServerAddr = "192.168.99.100", port = 4445L)
remDr$open()

remDr$navigate("https://kommune.viborg.dk/Politik/Dagsordener-og-referater/Dagsordner-og-referater-fra-byraad-udvalg-raad-naevn/Byraadet?y=2013")
remDr$screenshot(display = TRUE)

source <- remDr$getPageSource()[[1]]
p <- read_html(source)

link <- p %>% html_nodes("#main > div.place-right > div > div.esdh > ul > li > a") %>% html_attr("href")

url2 <- "https://kommune.viborg.dk"
urlnew <- paste0(url2, link)

remDr$navigate(urlnew)
remDr$screenshot(display = T)

source2 <- remDr$getPageSource()[[1]]
p2 <- read_html(source2)

dfs <- list()
x <- 1

for(i in 1:length(urlnew)) { #LOOP OVER MEETINGS
    
    
    remDr$navigate(urlnew[[i]])
    remDr$setTimeout(type = "page load", milliseconds = 30000)
    
    source2 <- remDr$getPageSource()[[1]]
    p2 <- read_html(source2)
    label <- p2 %>% html_nodes("a > span.esdh-title") %>% html_text(trim = T)
    
    textlist <- list()
    for (i in 1:length(label)){
        test1 <- "#"
        test2 <- i 
        test3 <- " li"
        test4 <- paste0(test1, test2, test3)
        
        text <- p2 %>% html_nodes(test4) %>% html_text(trim = T)
        text <- paste0(text, sep = " ", collapse = "")
        
        textframe <- data.frame(text = text, 
                                stringsAsFactors = F)
    
        textlist[[length(textlist) + 1]] <- textframe
        
        
        }
    

    text <- unlist(textlist)
    #text <- p2 %>% html_nodes("#93 li") %>% html_text(trim = T)
    date <- p2 %>% html_nodes('#main > div.place-right > div > h1') %>% html_text(trim=T)
    date <- unlist(str_extract_all(date, "\\d.+"))
    label <- p2 %>% html_nodes("a > span.esdh-title") %>% html_text(trim = T)
    title <- p2 %>% html_nodes("#main > div.place-right > div > h1") %>% html_text(trim = T)    
    
    
    file.name <- paste0("./data_archive/viborg_archive_2013/", x, "---", title, ".RData")
    
    df <- data.frame(
        city = "Viborg",
        date = date, 
        title = title, 
        label = label,
        text = text, 
        stringsAsFactors = FALSE
    )
    
    save(source, file = file.name)
    
    x <- x + 1
    
    dfs[[length(dfs) + 1]] <- df
}

save(df, file = './data_archive/viborg2013.RData')

##############################################################################################################################
############################################################ ARCHIVE 2014 ####################################################
##############################################################################################################################


# Create directories to save files if don't exist
if(!dir.exists("./data_archive")) dir.create("./data_archive")
if(!dir.exists("./data_archive/viborg_archive_2014")) dir.create("./data_archive/viborg_archive_2014")


remDr$navigate("https://kommune.viborg.dk/Politik/Dagsordener-og-referater/Dagsordner-og-referater-fra-byraad-udvalg-raad-naevn/Byraadet?y=2014")
remDr$screenshot(display = TRUE)

source <- remDr$getPageSource()[[1]]
p <- read_html(source)

link <- p %>% html_nodes("#main > div.place-right > div > div.esdh > ul > li > a") %>% html_attr("href")

url2 <- "https://kommune.viborg.dk"
urlnew <- paste0(url2, link)

dfs <- list()
x <- 1

for(i in 1:length(urlnew)) { #LOOP OVER MEETINGS
    
    
    remDr$navigate(urlnew[[i]])
    remDr$setTimeout(type = "page load", milliseconds = 30000)
    
    source2 <- remDr$getPageSource()[[1]]
    p2 <- read_html(source2)
    
    label <- p2 %>% html_nodes("a > span.esdh-title") %>% html_text(trim = T)
    
    textlist <- list()
    for (i in 1:length(label)){
        test1 <- "#"
        test2 <- i 
        test3 <- " li"
        test4 <- paste0(test1, test2, test3)
        
        text <- p2 %>% html_nodes(test4) %>% html_text(trim = T)
        text <- paste0(text, sep = " ", collapse = "")
        
        textframe <- data.frame(text = text, 
                                stringsAsFactors = F)
        
        textlist[[length(textlist) + 1]] <- textframe
        
        
    }
    
    
    text <- unlist(textlist)
    #text <- p2 %>% html_nodes("#93 li") %>% html_text(trim = T)
    date <- p2 %>% html_nodes('#main > div.place-right > div > h1') %>% html_text(trim=T)
    date <- unlist(str_extract_all(date, "\\d.+"))
    label <- p2 %>% html_nodes("a > span.esdh-title") %>% html_text(trim = T)
    title <- p2 %>% html_nodes("#main > div.place-right > div > h1") %>% html_text(trim = T)    
    
    
    file.name <- paste0("./data_archive/viborg_archive_2014/", x, "---", title, ".RData")
    
    df <- data.frame(
        city = "Viborg",
        date = date, 
        title = title, 
        label = label,
        text = text, 
        stringsAsFactors = FALSE
    )
    
    save(source, file = file.name)
    
    x <- x + 1
    
    dfs[[length(dfs) + 1]] <- df
}

df <- as.data.frame(rbindlist(dfs))
save(df, file = './data_archive/viborg2014.RData')


##############################################################################################################################
############################################################ ARCHIVE 2015 ####################################################
##############################################################################################################################


# Create directories to save files if don't exist
if(!dir.exists("./data_archive")) dir.create("./data_archive")
if(!dir.exists("./data_archive/viborg_archive_2015")) dir.create("./data_archive/viborg_archive_2015")


remDr$navigate("https://kommune.viborg.dk/Politik/Dagsordener-og-referater/Dagsordner-og-referater-fra-byraad-udvalg-raad-naevn/Byraadet?y=2015")
remDr$screenshot(display = TRUE)

source <- remDr$getPageSource()[[1]]
p <- read_html(source)

link <- p %>% html_nodes("#main > div.place-right > div > div.esdh > ul > li > a") %>% html_attr("href")

url2 <- "https://kommune.viborg.dk"
urlnew <- paste0(url2, link)

dfs <- list()
x <- 1

for(i in 1:length(urlnew)) { #LOOP OVER MEETINGS
    
    
    remDr$navigate(urlnew[[i]])
    remDr$setTimeout(type = "page load", milliseconds = 30000)
    
    source2 <- remDr$getPageSource()[[1]]
    p2 <- read_html(source2)
    
    label <- p2 %>% html_nodes("a > span.esdh-title") %>% html_text(trim = T)
    
    textlist <- list()
    for (i in 1:length(label)){
        test1 <- "#"
        test2 <- i 
        test3 <- " li"
        test4 <- paste0(test1, test2, test3)
        
        text <- p2 %>% html_nodes(test4) %>% html_text(trim = T)
        text <- paste0(text, sep = " ", collapse = "")
        
        textframe <- data.frame(text = text, 
                                stringsAsFactors = F)
        
        textlist[[length(textlist) + 1]] <- textframe
        
        
    }
    
    
    text <- unlist(textlist)
    #text <- p2 %>% html_nodes("#93 li") %>% html_text(trim = T)
    date <- p2 %>% html_nodes('#main > div.place-right > div > h1') %>% html_text(trim=T)
    date <- unlist(str_extract_all(date, "\\d.+"))
    label <- p2 %>% html_nodes("a > span.esdh-title") %>% html_text(trim = T)
    title <- p2 %>% html_nodes("#main > div.place-right > div > h1") %>% html_text(trim = T)    
    
    
    file.name <- paste0("./data_archive/viborg_archive_2015/", x, "---", title, ".RData")
    
    df <- data.frame(
        city = "Viborg",
        date = date, 
        title = title, 
        label = label,
        text = text, 
        stringsAsFactors = FALSE
    )
    
    save(source, file = file.name)
    
    x <- x + 1
    
    dfs[[length(dfs) + 1]] <- df
}

df <- as.data.frame(rbindlist(dfs))
save(df, file = './data_archive/viborg2015.RData')


##############################################################################################################################
############################################################ ARCHIVE 2016 ####################################################
##############################################################################################################################


# Create directories to save files if don't exist
if(!dir.exists("./data_archive")) dir.create("./data_archive")
if(!dir.exists("./data_archive/viborg_archive_2016")) dir.create("./data_archive/viborg_archive_2016")


remDr$navigate("https://kommune.viborg.dk/Politik/Dagsordener-og-referater/Dagsordner-og-referater-fra-byraad-udvalg-raad-naevn/Byraadet?y=2016")
remDr$screenshot(display = TRUE)

source <- remDr$getPageSource()[[1]]
p <- read_html(source)

link <- p %>% html_nodes("#main > div.place-right > div > div.esdh > ul > li > a") %>% html_attr("href")

url2 <- "https://kommune.viborg.dk"
urlnew <- paste0(url2, link)

dfs <- list()
x <- 1

for(i in 1:length(urlnew)) { #LOOP OVER MEETINGS
    
    
    remDr$navigate(urlnew[[i]])
    remDr$setTimeout(type = "page load", milliseconds = 30000)
    
    source2 <- remDr$getPageSource()[[1]]
    p2 <- read_html(source2)
    
    label <- p2 %>% html_nodes("a > span.esdh-title") %>% html_text(trim = T)
    
    textlist <- list()
    for (i in 1:length(label)){
        test1 <- "#"
        test2 <- i 
        test3 <- " li"
        test4 <- paste0(test1, test2, test3)
        
        text <- p2 %>% html_nodes(test4) %>% html_text(trim = T)
        text <- paste0(text, sep = " ", collapse = "")
        
        textframe <- data.frame(text = text, 
                                stringsAsFactors = F)
        
        textlist[[length(textlist) + 1]] <- textframe
        
        
    }
    
    
    text <- unlist(textlist)
    #text <- p2 %>% html_nodes("#93 li") %>% html_text(trim = T)
    date <- p2 %>% html_nodes('#main > div.place-right > div > h1') %>% html_text(trim=T)
    date <- unlist(str_extract_all(date, "\\d.+"))
    label <- p2 %>% html_nodes("a > span.esdh-title") %>% html_text(trim = T)
    title <- p2 %>% html_nodes("#main > div.place-right > div > h1") %>% html_text(trim = T)    
    
    
    file.name <- paste0("./data_archive/viborg_archive_2016/", x, "---", title, ".RData")
    
    df <- data.frame(
        city = "Viborg",
        date = date, 
        title = title, 
        label = label,
        text = text, 
        stringsAsFactors = FALSE
    )
    
    save(source, file = file.name)
    
    x <- x + 1
    
    dfs[[length(dfs) + 1]] <- df
}

df <- as.data.frame(rbindlist(dfs))
save(df, file = './data_archive/viborg2016.RData')

##############################################################################################################################
############################################################ ARCHIVE 2017 ####################################################
##############################################################################################################################


# Create directories to save files if don't exist
if(!dir.exists("./data_archive")) dir.create("./data_archive")
if(!dir.exists("./data_archive/viborg_archive_2017")) dir.create("./data_archive/viborg_archive_2017")


remDr$navigate("https://kommune.viborg.dk/Politik/Dagsordener-og-referater/Dagsordner-og-referater-fra-byraad-udvalg-raad-naevn/Byraadet?y=2017")
remDr$screenshot(display = TRUE)

source <- remDr$getPageSource()[[1]]
p <- read_html(source)

link <- p %>% html_nodes("#main > div.place-right > div > div.esdh > ul > li > a") %>% html_attr("href")

url2 <- "https://kommune.viborg.dk"
urlnew <- paste0(url2, link)

dfs <- list()
x <- 1

for(i in 1:length(urlnew)) { #LOOP OVER MEETINGS
    
    
    remDr$navigate(urlnew[[i]])
    remDr$setTimeout(type = "page load", milliseconds = 30000)
    
    source2 <- remDr$getPageSource()[[1]]
    p2 <- read_html(source2)
    
    label <- p2 %>% html_nodes("a > span.esdh-title") %>% html_text(trim = T)
    
    textlist <- list()
    for (i in 1:length(label)){
        test1 <- "#"
        test2 <- i 
        test3 <- " li"
        test4 <- paste0(test1, test2, test3)
        
        text <- p2 %>% html_nodes(test4) %>% html_text(trim = T)
        text <- paste0(text, sep = " ", collapse = "")
        
        textframe <- data.frame(text = text, 
                                stringsAsFactors = F)
        
        textlist[[length(textlist) + 1]] <- textframe
        
        
    }
    
    
    text <- unlist(textlist)
    #text <- p2 %>% html_nodes("#93 li") %>% html_text(trim = T)
    date <- p2 %>% html_nodes('#main > div.place-right > div > h1') %>% html_text(trim=T)
    date <- unlist(str_extract_all(date, "\\d.+"))
    label <- p2 %>% html_nodes("a > span.esdh-title") %>% html_text(trim = T)
    title <- p2 %>% html_nodes("#main > div.place-right > div > h1") %>% html_text(trim = T)    
    
    
    file.name <- paste0("./data_archive/viborg_archive_2017/", x, "---", title, ".RData")
    
    df <- data.frame(
        city = "Viborg",
        date = date, 
        title = title, 
        label = label,
        text = text, 
        stringsAsFactors = FALSE
    )
    
    save(source, file = file.name)
    
    x <- x + 1
    
    dfs[[length(dfs) + 1]] <- df
}

df <- as.data.frame(rbindlist(dfs))
save(df, file = './data_archive/viborg2017.RData')


