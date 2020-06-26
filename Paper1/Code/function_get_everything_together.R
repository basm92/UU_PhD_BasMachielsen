setwd("/home/bas/Documents/git/UU_PhD_BasMachielsen/Paper1")

library(readxl)
library(janitor)
library(tidyverse)

#Return
data <- read_xlsx('./Data/rateofreturnoneveryting_data.xlsx', sheet = 2)
metadata <- read_xlsx('./Data/rateofreturnoneveryting_data.xlsx', sheet = 3)

analysis <- read_xlsx("./Data/AnalysisFile.xlsx", sheet = "Analysis") %>%
  clean_names()


#If you give me the wealth at time of dea   th, I give you back the wealth at time timeoflaw
backreturn <- function(dataframe, timeoflaw){
  
  #First step: How many years?
  years <- analysis$yod - 1918
  
  
  #Step 2: Interest rates for each asset class in each year 
  
  re <- re * -(1/interestrate)
  
  
}
