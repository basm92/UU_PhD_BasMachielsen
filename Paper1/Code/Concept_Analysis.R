# Voting analyses (Kiesrecht)

## Set-up
## Load packages and source all functions
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

hoi <- map(hoi, source)

wetten <- list()
for(i in 1:length(hoi)){
    wetten[i] <- hoi[[i]][1]
}

wetten <- wetten[-6]

#Manually find the ID's and Districts
source("./Code/find_politician_id.R")
source("./Code/find_district.R")

#The dataset should have the variable politician, 
#a variable for the vote yes/no, vote
#a variable describing the law, wet
#Example
#datum <- "1918-10-06"
#id <- find_politician_id(kieswet1918$politician, date = datum)
#voting_outcomes <- merge(kieswet1918, id, by.x = "politician", by.y = "names")

#district <- find_district(voting_outcomes$polid, datum)
#voting_outcomes <- left_join(voting_outcomes, district, by = c("polid" = "b1-nummer"))



#Then automatically find all variables
source("./Code/get_all_variables.R")
#kieswet1918 <- get_all_variables(voting_outcomes, "1918-10-06")

id <- find_politician_id(wetten[[10]]$politician, wetten[[10]]$date[1])
voting_outcomes <- merge(wetten[[10]], id, by.x = "politician", by.y = "names")

district <- find_district(voting_outcomes$polid, voting_outcomes$date[1])

#Correct the district
#district <- district %>%
#    mutate(toelichting = str_replace(toelichting, "(in )", "")) %>%
#    mutate(toelichting = str_replace_all(toelichting, "\\d{4}-\\d{4} ", ""))


voting_outcomes <- left_join(voting_outcomes, district, by = c("polid" = "b1-nummer"))

voting_outcomes <- get_all_variables(voting_outcomes, voting_outcomes$date[1])

#voting_outcomes <- voting_outcomes %>%
#    as_tibble() %>%
 #   distinct(wealth_timevote, .keep_all = T)

model1 <- glm(data = voting_outcomes, 
              formula = as.numeric(vote) ~ 
                  wealth_timevote +
               polparty,
       #          as.numeric(days_to_next_el) + 
      #          as.numeric(long_elec_horiz),
    #             services_share+
        #          socialistpercentage +
        #          industry_share ,
             #    share_aandeel + 
              #   total + 
        #        amount_strikes ,
          #         Percentage ,
        #          diff,
              #      socialistdum + 
  #                Turnout + nearestcompetitormargin +
  #               as.numeric(age_of_vote) 
  # + as.numeric(age_of_entrance) + RK_pct, 
             family = binomial(link = "logit"))



summary(model1)
broom::tidy(model1) -> test
dim(model1$model)[2]
