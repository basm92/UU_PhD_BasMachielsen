## Examples for all functions

# Functions that we need to match a dataframe with votes, 
# politician's names and their id's to all independent variables:
#
# find politician_id(name, date) <- nog maken
# find_district(polid, date)
# find_strikes(district, year)
# find_religion(district, year)
# find_demographics(dataframe with $vote, $b1-nummer and $toelichting = district)
# find_eleccontrols

# find_econcontrols <- nog maken 
# From the district and time, find the economic and district-level control variables
# Some economic variables, education, demographics, professional distribution,

setwd("/home/bas/Documents/git/UU_PhD_BasMachielsen/Paper1")

library(readxl)
library(tidyverse)
library(lubridate)
library(stringr)


#find_politician_id(names, date)
##source()
date <- ymd("1880-01-01")
names <- c("Godefroi","Idzerda", "Bieberstein")

find_politician_id(names, date)

#find_district(polid, date)
source("./Code/find_district.R")

politicians <- read_excel("./Data/tk_1815tot1950uu.xlsx", sheet = 1)
find_district(c("00175", "00557"), "1914-09-17")
find_district(politicians$`b1-nummer`, "1900-01-01")
find_district(politicians$`b1-nummer`, "1880-01-01")


#find_strikes(districts, year)
find_strikes(c("Hontenisse", "Rotterdam", "Almelo", "Amsterdam"), 1900)

#find_religion
find_religion("Gulpen", 1895)
find_religion(c("Amsterdam", "Gulpen"), 1900)

#find_demographics
## You have to call the arguments date, b1-nummer and toelichting
date <- ymd("1910-01-01")
polid <- politicians$`b1-nummer`

find_district(polid, date)
example <- cbind(example, date)
example[2,2] <- "Tilburg"
#Before running this, make sure you have run the two lines with distrpoliddate
#and cleaned its output
find_demographics(example)

#find_eleccontrols


#find_econcontrols

