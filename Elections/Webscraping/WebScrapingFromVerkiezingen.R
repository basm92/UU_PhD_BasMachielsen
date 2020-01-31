# Try to webscrape from verkiezingsuitslagen

## Load libraries
library(rvest)
library(stringr)
library(dplyr)
library(curl)

## Common part of URL
source <- "http://resources.huygens.knaw.nl/verkiezingentweedekamer/databank/uitslag_per_persoon?persoon_ID="

## Part for persoon_ID - Fill in the required persoon IDs (20 to 1900)
numbers <- as.character(seq(16,17))

## Collect the URLS in a variable
urls <- NULL

for(i in numbers){
  urls[i] <- paste("http://resources.huygens.knaw.nl/verkiezingentweedekamer/databank/uitslag_per_persoon?persoon_ID=",i, sep = "")
}

## Make a table with name and data for every of the urls
a <- NULL
b <- NULL

for(i in urls) {
  a[i] <- read_html(i) %>%
    html_nodes(".vertical") %>%
    html_table(fill = TRUE)
  b[i] <- read_html(i) %>%
    html_nodes("#content-text > table:nth-child(3)") %>%
    html_text(trim = TRUE) %>%
    str_extract("\\'(.+)\\'")
  a[[i]][5] <- b[i]
}

## Make 1 big data frame
test <- a[[1]]
for (i in 2:length(a)) {
  test <- rbind(test,a[[i]])
}

## This is the entire algorithm. 

# Now try an error-robust version
numbers <- as.character(seq(1601,1700))

## Collect the URLS in a variable
urls <- NULL

for(i in numbers){
  urls[i] <- paste("http://resources.huygens.knaw.nl/verkiezingentweedekamer/databank/uitslag_per_persoon?persoon_ID=",i, sep = "")
}

## Define the error-robust readurl function

readurl <- function(url) {
    out <- tryCatch(
      {
      inter <- read_html(url) %>%
          html_nodes(".vertical") %>%
          html_table(fill = TRUE);
      inter[[1]][5] <- read_html(url) %>%
          html_nodes("#content-text > table:nth-child(3)") %>%
          html_text(trim = TRUE) %>%
          str_extract("\\'(.+)\\'");
      inter <- as.data.frame(inter); 
      return(inter)
      },
      error = function(i) {
        message(paste("URL doesnt work:", i))
        list("District" = NA, "Aanbevolen door" = NA, "Aantal stemmen" = NA, "Percentage" = NA, "V5" = NA)
      }
    )
    return(out)
}

test <- lapply(urls, readurl)

merge.to.data.frame <- function(x) {
  a <- x[[1]]
  for(i in 2:length(x)) {
    names(x[[i]]) <- names(a)
  }
  for(i in 2:length(x)) {
    a <- rbind(a, x[[i]] )
  }
  return(a)
}

test2 <- merge.to.data.frame(test)
test2 <- mapply(as.character, test2[,1:5])


write.csv(test2, file = "allcandidates1601_1700.csv")

