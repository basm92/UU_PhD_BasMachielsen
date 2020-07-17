## Examples for all functions

# Functions that we need to match a dataframe with votes, 
# politician's names and their id's to all independent variables:
#
# find politician_id(name, date) <- nog maken
# find_district(polid, date)
# find_strikes(district, year)
# find_religion(district, year)
# find_demographics(dataframe with $vote, $b1-nummer and $toelichting = district)
# find_eleccontrols(district, year)
# find_econcontrols <- (district, year)
# From the district and time, find the economic and district-level control variables
# Some economic variables, education, demographics, professional distribution,
# find_wealth <- nog maken

if(Sys.info()[[8]] == "bas"){
    setwd("/home/bas/Documents/git/UU_PhD_BasMachielsen/Paper1")
}else{
    setwd("/Users/basmachielsen/Documents/git/UU_PhD_BasMachielsen/Paper1")
    }

library(readxl)
library(tidyverse)
library(lubridate)
library(stringr)


#find_politician_id(names, date)
source("./Code/find_politician_id.R")
date <- ymd("1880-01-01")
names <- c("Godefroi","Idzerda", "Bieberstein")

find_politician_id(names, date)

#find_district(polid, date)
source("./Code/find_district.R")

politicians <- read_excel("./Data/tk_1815tot1950uu.xlsx", sheet = 1)
find_district(c("00175", "00557"), "1914-09-17")
find_district(politicians$`b1-nummer`, "1880-01-01")
find_district(politicians$`b1-nummer`, "1905-06-23")


#find_strikes(districts, year)
source("./Code/find_strikes.R")

find_strikes(c("Hontenisse", "Rotterdam", "Almelo", "Amsterdam"), 1900)

#find_religion
source("./Code/find_religion.R")

find_religion("Gulpen", 1895)
find_religion(c("Amsterdam", "Gulpen"), 1900)

#find_demographics(distrpoliddate)
source("./Code/find_demographics.R")

## You have to give a function call to find_demographics using a data frame
## The data frame should have the variables b1-nummer, toelichting and date
mijnjaar <- ymd("1888-01-01")
polid <- politicians$`b1-nummer`

find_district(polid, mijnjaar) -> example
example <- cbind(example, date = mijnjaar)
#example[3,2] <- "Tilburg"
#Before running this, make sure you have run the two lines with distrpoliddate
#and cleaned its output
find_demographics(example)

#find_eleccontrols(district, date, mindist)
## mindist is a kind of strictness parameter
source("./Code/find_eleccontrols.R")

find_district(politicians3$`b1-nummer`, "1905-02-04") -> districtstest
### Correct this vector manually
districtstest$toelichting <- district
date <- ymd("1905-02-04")

find_eleccontrols(districtstest$toelichting, date)

#find_econcontrols(district, year)
source("./Code/find_econcontrols.R")

thedis <- find_district(politicians$`b1-nummer`, "1872-05-01")

find_econcontrols(thedis$toelichting, 1872)
#find_wealth


