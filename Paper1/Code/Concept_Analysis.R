# Voting analyses (Kiesrecht)

## Set-up
## Load packages and source all functions
if(Sys.info()[[8]] == "bas"){
    setwd("/home/bas/Documents/git/UU_PhD_BasMachielsen/Paper1")
}else{
    setwd("/Users/basmachielsen/Documents/git/UU_PhD_BasMachielsen/Paper1")
}

library(readxl)
library(tidyverse)
library(lubridate)
library(stringr)

## Load a list of all the voting results
files <- dir("./Code/Voting Data") 
hoi <- list()
for(i in files) {
    hoi[i] <- paste0("./Code/Voting Data/", i)
}

kieswetten <- lapply(hoi, source)

#Manually find the ID's and Districts
source("./Code/find_politician_id.R")
source("./Code/find_district.R")

#The dataset should have the variable politician, 
#a variable for the vote yes/no, vote
#a variable describing the law, wet
datum <- "1918-10-06"
id <- find_politician_id(kieswet1918$politician, date = datum)
voting_outcomes <- merge(kieswet1918, id, by.x = "politician", by.y = "names")

district <- find_district(voting_outcomes$polid, datum)
voting_outcomes <- left_join(voting_outcomes, district, by = c("polid" = "b1-nummer"))

#Then automatically find all variables
source("./Code/get_all_variables.R")
kieswet1918 <- get_all_variables(voting_outcomes, "1918-10-06")


model1 <- glm(data = kieswet1892, 
              formula = as.numeric(vote) ~ log(1+wealth_timevote) +
                 industry_share + services_share + 
                  share_aandeel + total + 
                  amount_strikes + Percentage + 
                  diff + #socialistdum + 
                  socialistpercentage + 
                  Turnout + nearestcompetitormargin +
                 polparty + as.numeric(days_to_next_el) + as.numeric(long_elec_horiz) +#  +
                  as.numeric(age_of_vote) + as.numeric(age_of_entrance) +
    Gereformeerd_pct + Hervormd_pct + RK_pct, 
             family = binomial(link = "logit"))



summary(model1)