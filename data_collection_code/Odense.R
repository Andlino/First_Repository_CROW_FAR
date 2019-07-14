## Odense.R
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


url2007 <- "https://www.odense.dk/politik/dagsordner-og-referater/odense-byrad/dagsordner-og-referater-foer-111-2015/dagsordner-og-referater-2007-2009/dagsordener-og-referater-2007"
url2008 <- "https://www.odense.dk/politik/dagsordner-og-referater/odense-byrad/dagsordner-og-referater-foer-111-2015/dagsordner-og-referater-2007-2009/dagsordener-og-referater-2008"
url2009 <- "https://www.odense.dk/politik/dagsordner-og-referater/odense-byrad/dagsordner-og-referater-foer-111-2015/dagsordner-og-referater-2007-2009/dagsordener-og-referater-2009"
url2010 <- "https://www.odense.dk/politik/dagsordner-og-referater/odense-byrad/dagsordner-og-referater-foer-111-2015/dagsordner-og-referater-2010-2013/dagsordner-og-referater-2010"
url2011 <- "https://www.odense.dk/politik/dagsordner-og-referater/odense-byrad/dagsordner-og-referater-foer-111-2015/dagsordner-og-referater-2010-2013/dagsordner-og-referater-2011"
url2012 <- "https://www.odense.dk/politik/dagsordner-og-referater/odense-byrad/dagsordner-og-referater-foer-111-2015/dagsordner-og-referater-2010-2013/dagsordner-og-referater-2012"
url2013 <- "https://www.odense.dk/politik/dagsordner-og-referater/odense-byrad/dagsordner-og-referater-foer-111-2015/dagsordner-og-referater-2010-2013/dagsordner-og-referater-2013"
url2014 <- "https://www.odense.dk/politik/dagsordner-og-referater/odense-byrad/dagsordner-og-referater-foer-111-2015/dagsordner-og-referater-2014"
url2015 <- "https://www.odense.dk/politik/dagsordner-og-referater/odense-byrad/dagsordner-og-referater-foer-111-2015/dagsordner-og-referater-2015"
url20142017 <- "https://www.odense.dk/politik/dagsordner-og-referater/odense-byrad/odense-byraad-2014-til-2017"


p <- read_html(url2007)
links2007 <- p %>% html_nodes("#canvas > section > div > div > div.column-9 > article > div > ul > li > a") %>% html_attr("href")
p <- read_html(url2008)
links2008 <- p %>% html_nodes("#canvas > section > div > div > div.column-9 > article > div > ul > li > a") %>% html_attr("href")
p <- read_html(url2009)
links2009 <- p %>% html_nodes("#canvas > section > div > div > div.column-9 > article > div > ul > li > a") %>% html_attr("href")
p <- read_html(url20142017)
links20142017 <- p %>% html_nodes("#agendaLinks a") %>% html_attr("href")

# Load the Library
library(RSelenium)

# start the server and browser(you can use other browsers here)
remDr <- remoteDriver(remoteServerAddr = "192.168.99.100", port = 4445L)
remDr$open()

remDr$navigate("https://www.odense.dk/politik/dagsordner-og-referater/odense-byrad/dagsordner-og-referater-foer-111-2015/dagsordner-og-referater-2010-2013/dagsordner-og-referater-2010")
remDr$screenshot(display = TRUE)

element$clickElement()
element <- remDr$findElement(using = "css selector", ".cookie-button")

# select iframe element
element2 <- remDr$findElement(using = "css selector","#danlaes > div > ul > li:nth-child(2)")
#switch to the iframe
element2$clickElement()

element <- remDr$findElement(using = "css","#AgendasAndMinutes")
#switch to the iframe
remDr$switchToFrame(element)

elements <- remDr$findElements(using = "css","#danlaes > div > ul > li:nth-child(2) > a")
elements[[1]]$clickElement()

#switch back to the default page
driver$switchToFrame()
