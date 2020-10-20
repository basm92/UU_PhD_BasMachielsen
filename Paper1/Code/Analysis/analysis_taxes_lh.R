taxes <- list.files("./Code/Voting Data/Lower House/Taxes") 
sapply(paste0("./Code/Voting Data/Lower House/Taxes/", taxes), source)

source("./Code/find_politician_id.R")
source("./Code/find_district.R")
source("./Code/get_all_variables.R")
source("./Code/clean_districts.R")

# load libs
library(readxl)
library(tidyverse)
library(lubridate)

#Successiewet 1878
id <- find_politician_id(successiewet1878$politician, successiewet1878$date[1])
voting_outcomes <- left_join(successiewet1878, id, by = c("politician" = "names"))
distr <- find_district(voting_outcomes$polid, voting_outcomes$date[1])
voting_outcomes <- left_join(voting_outcomes, distr, by = c("polid" = "b1-nummer"))

#districts
voting_outcomes$toelichting <- clean_districts(voting_outcomes$toelichting, voting_outcomes$date[1])
sw1878 <- get_all_variables(voting_outcomes, voting_outcomes$date[1])

modelsuccessiewet1878 <- sw1878 %>%
  glm(formula = as.numeric(vote) ~
        log(1+wealth_timevote) +
        agricul_share + 
        share_aandeel + 
        amount_strikes +
        Percentage +
        Turnout +
        days_to_next_el +
        age_of_vote +
        RK_pct +
        polparty, family=binomial(link = "logit")) 

a <- nobs(modelsuccessiewet1878) ; b <-  modelsuccessiewet1878$data$law[1]
n_voted <- length(voting_outcomes$vote)
bmodelsuccessiewet1878 <- broom::tidy(modelsuccessiewet1878) %>%
  cbind(law = b, n = a , n_voted = n_voted)

#successiewet1911
id <- find_politician_id(successiewet1911$politician, successiewet1911$date[1])
voting_outcomes <- left_join(successiewet1911, id, by = c("politician" = "names"))
distr <- find_district(voting_outcomes$polid, voting_outcomes$date[1])
voting_outcomes <- left_join(voting_outcomes, distr, by = c("polid" = "b1-nummer"))
#districts
voting_outcomes$toelichting <- clean_districts(voting_outcomes$toelichting, voting_outcomes$date[1])
sw1911 <- get_all_variables(voting_outcomes, voting_outcomes$date[1])

modelsuccessiewet1911 <- sw1911 %>%
  glm(formula = as.numeric(vote) ~
        log(1+wealth_timevote) +
        agricul_share + 
        #    share_aandeel + 
        #   amount_strikes +
        #    Percentage +
        Turnout +
        days_to_next_el +
        #     age_of_vote +
        RK_pct +
        socialistpercentage +
        polparty
      , family=binomial(link = "logit")) 

a <- nobs(modelsuccessiewet1911) ; b <-  modelsuccessiewet1911$data$law[1]
n_voted <- length(voting_outcomes$vote)
bmodelsuccessiewet1911 <- broom::tidy(modelsuccessiewet1911) %>%
  cbind(law = b, n = a , n_voted = n_voted)


#successiewet1916
id <- find_politician_id(successiewet1916$politician, successiewet1916$date[1])
voting_outcomes <- left_join(successiewet1916, id, by = c("politician" = "names"))
distr <- find_district(voting_outcomes$polid, voting_outcomes$date[1])
voting_outcomes <- left_join(voting_outcomes, distr, by = c("polid" = "b1-nummer"))
#districts
voting_outcomes$toelichting <- clean_districts(voting_outcomes$toelichting, voting_outcomes$date[1])
sw1916 <- get_all_variables(voting_outcomes, voting_outcomes$date[1])

modelsuccessiewet1916 <- sw1916 %>%
  glm(formula = as.numeric(vote) ~
        log(1+wealth_timevote) +
       # agricul_share + 
        #    share_aandeel + 
        #   amount_strikes +
     #       Percentage +
     #   Turnout +
    #    days_to_next_el +
        #     age_of_vote +
    #    RK_pct +
     #   socialistpercentage +
       polparty
      , family=binomial(link = "logit")) 

a <- nobs(modelsuccessiewet1916) ; b <-  modelsuccessiewet1916$data$law[1]
n_voted <- length(voting_outcomes$vote)
bmodelsuccessiewet1916 <- broom::tidy(modelsuccessiewet1916) %>%
  cbind(law = b, n = a , n_voted = n_voted)

#inkomstenbelasting1893
id <- find_politician_id(inkomstenbelasting1893$politician, inkomstenbelasting1893$date[1])
voting_outcomes <- left_join(inkomstenbelasting1893, id, by = c("politician" = "names"))
distr <- find_district(voting_outcomes$polid, voting_outcomes$date[1])
voting_outcomes <- left_join(voting_outcomes, distr, by = c("polid" = "b1-nummer"))
#districts
voting_outcomes$toelichting <- clean_districts(voting_outcomes$toelichting, voting_outcomes$date[1])
ib1893 <- get_all_variables(voting_outcomes, voting_outcomes$date[1])

modelinkomstenbelasting1893 <- ib1893 %>%
  glm(formula = as.numeric(vote) ~
        log(1+wealth_timevote) +
        agricul_share + 
        share_aandeel + 
        amount_strikes +
        Percentage +
        Turnout +
        days_to_next_el +
        age_of_vote +
        RK_pct +
        polparty, family=binomial(link = "logit")) 

a <- nobs(modelinkomstenbelasting1893) ; b <-  modelinkomstenbelasting1893$data$law[1]
n_voted <- length(voting_outcomes$vote)
bmodelinkomstenbelasting1893 <- broom::tidy(modelinkomstenbelasting1893) %>%
  cbind(law = b, n = a , n_voted = n_voted)

#inkomstenbelasting1914
id <- find_politician_id(inkomstenbelasting1914$politician, inkomstenbelasting1914$date[1])
voting_outcomes <- left_join(inkomstenbelasting1914, id, by = c("politician" = "names"))
distr <- find_district(voting_outcomes$polid, voting_outcomes$date[1])
voting_outcomes <- left_join(voting_outcomes, distr, by = c("polid" = "b1-nummer"))
#districts
voting_outcomes$toelichting <- clean_districts(voting_outcomes$toelichting, voting_outcomes$date[1])
ib1914 <- get_all_variables(voting_outcomes, voting_outcomes$date[1])

modelinkomstenbelasting1914 <- ib1914 %>%
  glm(formula = as.numeric(vote) ~
        log(1+wealth_timevote) +
        agricul_share + 
 #       share_aandeel + 
   #     amount_strikes +
  #      Percentage +
        Turnout +
      #  days_to_next_el +
   #     age_of_vote +
        RK_pct +
        polparty, family=binomial(link = "logit")) 

a <- nobs(modelinkomstenbelasting1914) ; b <-  modelinkomstenbelasting1914$data$law[1]
n_voted <- length(voting_outcomes$vote)
bmodelinkomstenbelasting1914 <- broom::tidy(modelinkomstenbelasting1914) %>%
  cbind(law = b, n = a , n_voted = n_voted)

#staatsschuldwet1914
id <- find_politician_id(staatsschuldwet1914$politician, staatsschuldwet1914$date[1])
voting_outcomes <- left_join(staatsschuldwet1914, id, by = c("politician" = "names"))
distr <- find_district(voting_outcomes$polid, voting_outcomes$date[1])
voting_outcomes <- left_join(voting_outcomes, distr, by = c("polid" = "b1-nummer"))
#districts
voting_outcomes$toelichting <- clean_districts(voting_outcomes$toelichting, voting_outcomes$date[1])
ss1914 <- get_all_variables(voting_outcomes, voting_outcomes$date[1])

modelstaatsschuldwet1914 <- ss1914 %>%
  glm(formula = as.numeric(vote) ~
        log(1+wealth_timevote) +
              agricul_share + 
         #      share_aandeel + 
             amount_strikes +
        #      Percentage +
        Turnout +
        #  days_to_next_el +
        #     age_of_vote +
             RK_pct +
        polparty, family=binomial(link = "logit")) 

a <- nobs(modelstaatsschuldwet1914) ; b <-  modelstaatsschuldwet1914$data$law[1]
n_voted <- length(voting_outcomes$vote)
bmodelstaatsschuldwet1914 <- broom::tidy(modelstaatsschuldwet1914) %>%
  cbind(law = b, n = a , n_voted = n_voted)

stargazer(modelinkomstenbelasting1893, modelinkomstenbelasting1914, modelsuccessiewet1878,
          modelsuccessiewet1911, modelsuccessiewet1916, modelstaatsschuldwet1914, 
          column.labels = c("IB 1893", "IB1914", "SW1878", "SW1911", "SW1916",
                            "GD 1914"), font.size = "scriptsize", 
          notes = "IB = Inkomstenbelasting, SW = Successiewet, GD = Staatsschuld",
          column.sep.width = "0pt", dep.var.labels.include = F, title = "Analysis of Fiscal Legislation")



### all tax models together

fiscal <- bind_rows(ib1893, ib1914, sw1878, sw1911, sw1916, ss1914)


# remove this
#fiscal %>%
#  select(polid, politician, date, dateofdeath, W_DEFLATED) %>%
#  group_by(polid) %>%
#  slice_min(date) %>%
#  distinct(politician, .keep_all = T) %>%
#  rename(firstvote = date, w_deflated = W_DEFLATED) %>%
#  tibble::add_column(dod_father = "", 
#                     dod_mother = "",
#                     source_father = "",
#                     source_mother = "",
 #                    wealth_father = "",
 #                    wealth_mother = "") %>%
 # write_csv2("./Data/instrumental_variable_wealth_par.csv")

#until this

all1 <- fiscal %>%
glm(formula = as.numeric(vote) ~
      log(1+wealth_timevote) +
 #     agricul_share + 
 #     share_aandeel + 
 #     amount_strikes +
 #     Percentage +
 #     Turnout +
 #     days_to_next_el +
 #     age_of_vote +
  #    RK_pct +
      polparty, family=binomial(link = "logit"))

all2 <- fiscal %>%
  glm(formula = as.numeric(vote) ~
        log(1+wealth_timevote) +
        #     agricul_share + 
        #     share_aandeel + 
        #     amount_strikes +
        #     Percentage +
        #     Turnout +
        #     days_to_next_el +
        #     age_of_vote +
            RK_pct +
        polparty, family=binomial(link = "logit"))

all3 <- fiscal %>%
  glm(formula = as.numeric(vote) ~
        log(1+wealth_timevote) +
             agricul_share + 
             share_aandeel + 
             amount_strikes +
        #     Percentage +
        #     Turnout +
             days_to_next_el +
             age_of_vote +
            RK_pct +
        polparty, family=binomial(link = "logit"))

all4 <- fiscal %>%
  glm(formula = as.numeric(vote) ~
        log(1+wealth_timevote) +
        #     agricul_share + 
        #     share_aandeel + 
        #     amount_strikes +
             Percentage +
             Turnout +
             days_to_next_el +
             age_of_vote +
            RK_pct +
        polparty, family=binomial(link = "logit"))


all5 <- fiscal %>%
  glm(formula = as.numeric(vote) ~
        log(1+wealth_timevote) +
        services_share + 
        nearestcompetitormargin +  
             amount_strikes +
             Percentage +
             Turnout +
             days_to_next_el +
             age_of_vote +
        RK_pct +
        polparty, family=binomial(link = "logit"))



all6 <- fiscal %>%
  glm(formula = as.numeric(vote) ~
        log(1+wealth_timevote):law +
   #     agricul_share + 
   #   share_aandeel + 
        amount_strikes +
        Percentage +
        Turnout +
        days_to_next_el +
        age_of_vote +
        RK_pct +
        polparty, family=binomial(link = "logit"))

all7 <- fiscal %>%
  glm(formula = as.numeric(vote) ~
        log(1+wealth_timevote):law +
             services_share + 
           nearestcompetitormargin + 
        amount_strikes +
        Percentage +
        Turnout +
        days_to_next_el +
        age_of_vote +
        RK_pct +
        polparty, family=binomial(link = "logit"))
  
stargazer(all1, all2, all3, all4, all5,
          font.size = "scriptsize", 
          column.sep.width = "0pt", 
          dep.var.labels.include = F, 
          title = "Analysis of Fiscal Legislation")

stargazer(all6, all7,
          font.size = "scriptsize", 
          column.sep.width = "0pt", 
          dep.var.labels.include = F, 
          title = "Splitting up the effects of Models (4) and (5)")

