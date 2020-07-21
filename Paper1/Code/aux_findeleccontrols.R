#aux_function for find_eleccontrols.R

library(tidyverse)
library(stringr)
library(lubridate)
library(readxl)
library(stringdist)

## Read in an elections dataset
if(Sys.info()[[8]] == "bas"){
    setwd("/home/bas/Documents/git/UU_PhD_BasMachielsen/Paper1")
}else{
    setwd("/Users/basmachielsen/Documents/git/UU_PhD_BasMachielsen/Paper1")
}

files <- dir("./Data")
allelections <- files[grepl("Uitslag_TK", files)]

b <- NULL
for(i in allelections){
    a <- read.csv(paste("./Data/",i, sep = ""), sep = ";", encoding = "utf-8")
    
    title <- readr::parse_factor(i)
    
    a <- cbind(a, title)
    b <- rbind(b,a)
}

### Clean the dataset
allelections <- b %>%
    separate(title, into = c("a", "main", "side"), sep = "_") %>%
    select(-a) %>%
    mutate(main = str_remove(main, "TK"), 
           side = str_remove(side, "\\.csv"), 
           side = str_remove(side, "\\(1\\)")) %>%
    separate(main, sep = c(4,6), into = c("year","month", "day")) %>%
    unite("main", c("year","month", "day"), sep = "-") %>%
    mutate(main = ymd(main), side = dmy(side))

## Read in the electoral results with parties and names dataset
files <- dir("./Data")
allcandidates <- files[grepl("allcandidates", files)]

b <- NULL

for(i in allcandidates) {
    a <- read_csv(paste("./Data/", i, sep = ""),locale = readr::locale(encoding = "latin1"))
    colnames(a) <- c("X1", "District", "Aanbevolen.door", "Aantal.stemmen", "Percentage", "V5")
    b <- rbind(b, a)
}

allcandidates <- b %>%
    select(-1) %>%
    na.omit() %>%
    mutate(name = as.character(V5)) %>%
    mutate(name = gsub("'", "", name)) %>%
    select(-V5) %>%
    as_tibble() %>%
    separate("District", into = c("District", "Date"), sep = "\\(") %>%
    mutate(Date = str_replace(Date, "\\)", ""), 
           Aanbevolen.door = as.character(Aanbevolen.door),
           Percentage = as.numeric(str_replace(Percentage, "%", "")),
           Aantal.stemmen = as.numeric(Aantal.stemmen),
           Date = dmy(Date))

write.csv(allcandidates, "./Data/functiondata_allcandidates.csv", row.names = F)
write.csv(allelections, "./Data/functiondata_allelections.csv", row.names = F)
