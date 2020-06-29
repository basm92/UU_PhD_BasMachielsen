#Districts and municipalities
## Attempt with scraping

#Need XPath for extra municipalities
setwd("/home/bas/Documents/git/UU_PhD_BasMachielsen/Paper1")
library(tidyverse)
library(rvest)
library(stringr)
library(XML)
library(devtools)
library(RSelenium)

remDr <- RSelenium::remoteDriver(remoteServerAddr = "localhost",
                                 port = 4445L,
                                 browserName = "chrome")


# Now, repeat the task for municipalities per districts
urls <- paste("http://resources.huygens.knaw.nl/verkiezingentweedekamer/databank/zoek_district/gemeenten_per_district?District_ID=", seq(10,150), sep = "")


remDr$open()

#New version with indicators when a municipality belonged to a certain district
out <- NULL

#Data cleaning auxiliary function
makethetable <- function(nodes){
  nodes <- gsub(nodes,pattern = "<td class=\"box-mark\">", replacement = "<td class=\"box-mark\"> 1")
  
  table <- nodes %>%
    XML::readHTMLTable() %>%
    as.data.frame()
  
  table <- table[-1,]
  table <- table %>%
    janitor::row_to_names(1)
  
  districtname <- read_html(urls[i]) %>%
    html_nodes("#content-text > h1:nth-child(2)") %>%
    html_text()
  
  data <- data.frame(table, districtname)
  data
}

#Loop over all pages
for (i in 1:length(urls)) {
  
  remDr$navigate(urls[i])
  
  nodes <- remDr$getPageSource() %>%
    magrittr::extract2(1) %>%
    read_html(urls[i]) %>%
    html_nodes(".vertical")
  
  if (length(nodes) == 0) {
    next
  } 
  
  data <- makethetable(nodes)
  
  out <- rbind(out, data)
  
  Sys.sleep(0.5)
  
  #Get the other pages
  try(remDr$findElement("xpath", "//a[contains(text(), 'Volgende')]")) -> test
  
  while(class(test) != "try-error"){
  
    test$clickElement()
    
    nodes <- remDr$getPageSource() %>%
      magrittr::extract2(1) %>%
      read_html(urls[i]) %>%
      html_nodes(".vertical")
    
    data <- makethetable(nodes)
    
    out <- rbind(out, data)

    try(remDr$findElement("xpath", "//a[contains(text(), 'Volgende')]")) -> test
  }
  
  Sys.sleep(0.5)
  
}

final <- out %>%
  mutate_all(str_replace, "Â", "") %>%
  mutate_at(c(2,3,4,5), str_trim) %>%
  mutate_at(c(2,3,4,5), as.numeric) 

colnames(final) <- c("gemeente", "1848", "1850", "1888", "1897", "district")

write.csv(final, "./Data/Municipalities_and_districts.csv")
