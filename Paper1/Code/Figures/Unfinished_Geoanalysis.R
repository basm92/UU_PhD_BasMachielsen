# Here, I want to make an overview of the religious composition of the population, and I want to do it graphically

##Load libraries
library(readxl)
library(tidyverse)
library(rgdal)
library(stringr)
library(lubridate)
library(stringdist)
library(fuzzyjoin)

x <- c("ggmap", "rgdal", "rgeos", "maptools", "tmap")
lapply(x, library, character.only = TRUE)

#Read the maps
nl1880 <- readOGR(dsn = "../Data", layer = "nl_1880")
nl1900 <- readOGR(dsn = "../Data", layer = "nl_1900")
nl1920 <- readOGR(dsn = "../Data", layer = "nl_1920")

#Read the data
Religion <- read.csv("../Data/Religion_inhabitants_per_district.csv") %>%
  select(-1)

Municipalities <- read.csv("../Data/Municipalities_and_districts.csv") %>%
  select(-1)

