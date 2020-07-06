#Alles voor TK
setwd("/home/bas/Documents/git/UU_PhD_BasMachielsen/Paper1")

library(readxl)
library(tidyverse)
library(lubridate)
library(stringr)

# Functions that we need to match a dataframe with votes, politician's names and their id's to all independent variables:
# find politician_id(name, date) <- nog maken
# find_district(polid, date)
# find_strikes(district, year)
# find_religion(district, year)
# find_demographics(dataframe with $vote, $b1-nummer and $toelichting = district)
# find_eleccontrols

# find_econcontrols <- nog maken 
#From the district and time, find the economic and district-level control variables
## Some economic variables, education, demographics, professional distribution,

#find_politician_id

find_politician_id <- function(names, date) {
  politicians <- readxl::read_excel("./Data/tk_1815tot1950uu.xlsx", sheet = 1)
  
  temp <- politicians %>%
    janitor::clean_names() %>%
    mutate(begin_periode = ymd(begin_periode), 
          einde_periode = ymd(einde_periode)) %>%
    filter(date > begin_periode, date < einde_periode)
  
  matches <- stringdist::amatch(names, temp$achternaam, method = "jw", maxDist = 10)
  
  data.frame(names = names, 
             polnames = temp$achternaam[matches],
             polid = temp$b1_nummer[matches]
             )
}

##Example
date <- ymd("1880-01-01")
names <- c("Godefroi","Idzerda", "Bieberstein")

find_politician_id(names, date)

#find_district
#From the politician number, find the district

find_district <- function(polid, date){
  
  politicians <- read_excel("./Data/tk_1815tot1950uu.xlsx", sheet = 1)
  career <- read_excel("./Data/tk_1815tot1950uu.xlsx", sheet = 2)
  
  career <- career %>%
    separate(datum, sep = "/", into = c("from", "to")) %>%
    mutate(from = dmy(from), to = dmy(to)) %>%
    filter(grepl("Tweede Kamer", waarde), 
           from > "1860-01-01", to < "1930-01-01") %>%
    mutate(toelichting = str_replace(toelichting, "voor het kiesdistrict ", ""))
  
  career %>%
    filter(`b1-nummer` %in% polid) %>%
    filter(from < ymd(date), to > ymd(date)) %>%
    select(`b1-nummer`, toelichting) %>%
    filter(!is.na(toelichting))
  
}

## Example
find_district(c("00175", "00557"), "1914-09-17")
find_district(politicians$`b1-nummer`, "1900-01-01")

#find_strikes

## Aggregate per district-year
find_strikes <- function(districts = NULL, year) {
  strikespd <- read.csv("./Data/strikesperdistrict.csv") %>%
    select(-1) %>%
    na.omit()
  
  if(!is.null(districts)){
  strikespd %>%
    group_by(district, JAAR) %>%
    summarise(amount = sum(howmuch)) %>%
      filter(district == districts, JAAR == year)
  }
  else {
    strikespd %>%
      group_by(district, JAAR) %>%
      summarise(amount = sum(howmuch)) %>%
      filter(JAAR == year)
  }
  
}

# find_religion
#Religion on a district level
## Dataset religion and inhabitants per district

find_religion <- function(district, year, append = TRUE) {
  
  religion <- read.csv("./Data/Religion_inhabitants_per_district.csv") %>%
    select(-1)
  
  distance <- min(abs(unique(religion$jaar) - year))# %>%
  check <- (year + distance) %in% unique(religion$jaar)
  
  if(check == TRUE){
    
  target <- year + distance
  
  } else {
  
  target <- year - distance
  
  }
  
  data <- religion %>%
    filter(districtname %in% district & jaar == target)
  
  if(append == TRUE) {
    #Get the second year you want
    temp <- abs(unique(year - religion$jaar)) %>%
      sort() %>%
      magrittr::extract(2)
    
    check <- (year + temp) %in% unique(religion$jaar)
    
    if(check == TRUE) {
      
      target2 <- year + temp
        
    } else {
        
      target2 <- year - temp
      
    }
    
    #Add the missing districts
    district2 <- setdiff(district, data$districtname)
    
    data <- religion %>%
      filter((districtname %in% district & jaar == target) | 
               (districtname %in% district2 & jaar == target2)
             )
    
    
    #Third iteration, if there are still missing data:
    temp <- abs(unique(year - religion$jaar)) %>%
      sort() %>%
      magrittr::extract(3)
    
    check <- (year + temp) %in% unique(religion$jaar)
    
    if(check == TRUE) {
      
      target3 <- year + temp
      
    } else {
      
      target3 <- year - temp
      
    }
    
    district3 <- setdiff(district2, data$districtname)
    
    data <- religion %>%
      filter((districtname %in% district & jaar == target) | 
               (districtname %in% district2 & jaar == target2) | 
               (districtname %in% district3 & jaar == target3)
             )
  }
  
  data
}

## Example
find_religion("Gulpen", 1895)
find_religion(c("Amsterdam", "Gulpen"), 1900)


#find_demographics
#From the polid, find the demographic control variables
## Tenure, age of death, age of entrance, electoral horizon (next election and until pension), party affiliation
politicians2 <- read_excel("./Data/tk_1815tot1950uu.xlsx", sheet = 1)
career2 <- read_excel("./Data/tk_1815tot1950uu.xlsx", sheet = 2)

career2 <- career2 %>%
  filter(rubriek == "3010" | rubriek == "3020") %>%
  dplyr::select(c(1:2, 4)) %>%
  dplyr::group_split(rubriek)
  
together <- merge(politicians2, career2[[1]]) %>%
  merge(career2[[2]], by = "b1-nummer")

colnames(together)[c(12,14)] <- c("dateofbirth", "dateofdeath")

together <- together %>%
  janitor::clean_names() %>%
  as_tibble() %>%
  mutate(across(contains("periode"), ymd), across(contains("date"), dmy))


## Get the dataset for when the next election was enacted (measuring short-term election cycle)

### This is an auxiliary Function: Load elections, and filter to the following information: 
### number, district, days to next election in that district
### To be used within find_demographcis
source("./Code/aux_which_elections.R")

#Before running this, make sure you have run the two lines with distrpoliddate
#and cleaned its output
#Then, get the demographic variables
find_demographics <- function(distrpoliddate) {
  
  date <- ymd(distrpoliddate$date)[1]
  
  data <- together %>%
    filter(b1_nummer %in% distrpoliddate$`b1-nummer`) %>%
    mutate(tenure = date - begin_periode,
           age_of_death = dateofdeath-dateofbirth,
           age_of_entrance = begin_periode - dateofbirth,
           age_of_vote = date - dateofbirth,
           long_elec_horiz = einde_periode - date
    )
  
  #Merge the dataset with the election with the data set here ^^
  temp <- which_elections(distrpoliddate)
  data <- merge(data, temp, 
        by.x = "b1_nummer", 
        by.y = "b1-nummer")
  
  ## A conversion table for party affiliation (so i can create that variable)
  temp <- read.csv("./Data/key_politicalparty_category.csv") %>%
    select(-1) %>%
    rename(polparty=class)
  
  ## Merge this variable in the results, and finalize the output
  merge(data, temp,
        by.x = "partij_en_fractie_s",
        by.y = "partys") %>%
    as_tibble()
}


## Example
## You have to call the arguments date, b1-nummer and toelichting
date <- ymd("1910-01-01")
polid <- c("00001", "00034")

find_district(polid, date) -> example
example <- cbind(example, date)
example[2,2] <- "Tilburg"

find_demographics(example)


#find_wealth
#From the polid, find the wealth
##From the wealth and vote date, compute the wealth back in time


              