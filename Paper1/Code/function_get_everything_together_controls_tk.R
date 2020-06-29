#Alles voor TK
setwd("/home/bas/Documents/git/UU_PhD_BasMachielsen/Paper1")

library(readxl)
library(tidyverse)
library(lubridate)
library(stringr)

#From the politician number, find the district
politicians <- read_excel("./Data/tk_1815tot1950uu.xlsx", sheet = 1)
career <- read_excel("./Data/tk_1815tot1950uu.xlsx", sheet = 2)

career <- career %>%
  separate(datum, sep = "/", into = c("from", "to")) %>%
  mutate(from = dmy(from), to = dmy(to)) %>%
  filter(grepl("Tweede Kamer", waarde), 
         from > "1860-01-01", to < "1930-01-01") %>%
  mutate(toelichting = str_replace(toelichting, "voor het kiesdistrict ", ""))


find_district <- function(polid, date){
  
  career %>%
    filter(`b1-nummer` %in% polid) %>%
    filter(from < ymd(date), to > ymd(date)) %>%
    select(`b1-nummer`, toelichting) %>%
    filter(!is.na(toelichting))
  
}

## Example
find_district(c("00175", "00557"), "1914-09-17")
find_district(politicians$`b1-nummer`, "1910-01-01")

# From the district and time, find the electoral variables
## Nearest socialists, socialist dummy, show-up rate, 
## nearest competitor, amount of votes, percentage of votes


#From the district and time, find the strikes

## Read in the strikes (municipality level) and clean it
strikes <- read.csv("./Data/howmuch.csv") %>%
  filter(JAAR > 1870, JAAR < 1919, GEMEENTE != "", GEMEENTE != "nvt") %>%
  group_by(JAAR, GEMEENTE, PROVINCIE) %>%
  summarise(howmuch = sum(howmuch))

## Read in the municipality to district
municipalities_to_districts <- read.csv("./Data/Municipalities_and_districts.csv") %>%
  select(-1)

names(municipalities_to_districts) %>%
  str_replace("X", "") -> names(municipalities_to_districts)

municipalities_to_districts <- municipalities_to_districts %>%
  pivot_longer(2:5, "year") %>%
  filter(value == 1) %>%
  select(-value) %>%
  mutate(year = as.numeric(year))

## Find the appropriate district for each municipality-year in strikes

find_district <- function(strikesdf) {
  
  # where to look?
  strikesdf$temp <- ifelse(strikesdf$JAAR < 1848, 
         1848, 
         ifelse(
           between(strikesdf$JAAR, 1848, 1850),
           1848,
           ifelse(between(strikesdf$JAAR, 1850, 1888),
                  1850,
                  ifelse(between(strikesdf$JAAR, 1888, 1897),
                         1888, 
                         1897)
           )
         )
  )
  
  df <- left_join(strikesdf, municipalities_to_districts, by = c("GEMEENTE" = "gemeente", "temp" = "year"))
  df
  
}

## Manually edit most important NA's:
## Amsterdam Rotterdam, Utrecht, Den Haag, Tilburg and Almelo, Vlissingen
find_district(strikes) -> test



#From the district and time, find the economic control variables

#From the polid, find the demographic control variables
## Tenure, age of death, age of entrance, electoral horizon


#From the polid, find the wealth


#From the wealth and vote date, compute the wealth back in time







              