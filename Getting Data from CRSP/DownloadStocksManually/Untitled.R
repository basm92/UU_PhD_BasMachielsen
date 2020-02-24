library(XML)
library(rvest)
library(tidyverse)
library(lubridate)

#This is a test
source <- read_html("https://finance.yahoo.com/quote/AAPL/history?p=AAPL")
html <- htmlTreeParse(source, useInternalNodes = T, asText = T)
tableNodes <- getNodeSet(html, "//table")
a <- readHTMLTable(tableNodes[[1]])

a %>%
    mutate(Date = mdy(Date), `Close*` = as.numeric(as.character(`Close*`))) %>%
    ggplot(aes(x = Date, y = `Close*`)) + geom_line()

#This is a script to get all the tickers
i <- NULL
b <- NULL
for (i in letters) {
    tickers <- read_html(paste("http://eoddata.com/stocklist/NYSE/",i,".htm",sep = ""))
    a <- tickers %>%
        html_nodes("#ctl00_cph1_divSymbols > table:nth-child(1)") %>%
        html_table(fill = TRUE) %>%
        as.data.frame() %>%
        select(1)
    b <- rbind(b,a)
    
}

# Now, generalize to get all pages (run in batches)
e <- NULL 
f <- NULL
for (i in 1:100) { #Change 1:100 according to the batch, or to 'b[,1]'
    source <- read_html(paste("https://finance.yahoo.com/quote/",b[i,1],"/history?p=",b[i,1], sep = ""))
    html <- htmlTreeParse(source, useInternalNodes = T, asText = T)
    tableNodes <- getNodeSet(html, "//table")
    d <- readHTMLTable(tableNodes[[1]])
    e <- cbind(d, rep(b[i,1], nrow(d)))
    try(f <- rbind(f, e))
    print(b[i,1])
}


#Now, a version where I download all the files
e <- NULL
f <- NULL
for (i in 773:3105) { #Change according to the batch
    download.file(paste("https://finance.yahoo.com/quote/",b[i,1],"/history?p=",b[i,1], sep = ""), 
                  destfile = "hello.html")
    source <- read_html("hello.html")
    html <- htmlTreeParse(source, useInternalNodes = T, asText = T)
    tableNodes <- getNodeSet(html, "//table")
    d <- try(readHTMLTable(tableNodes[[1]]))
    try(e <- cbind(d, rep(b[i,1], nrow(d))))
    try(f <- rbind(f, e))
    print(b[i,1])
}

write.csv(f, "stocks.csv")


momentum <- function(date_begin, date_end = date_begin, stocks) {
    date_begin <- as.Date(date_begin)
    date_end <- as.Date(date_end)
    stocks %>%
        mutate(returns = )
}
           