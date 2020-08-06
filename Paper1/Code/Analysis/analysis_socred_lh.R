socred <- list.files("./Code/Voting Data/Lower House/Social Redistribution") 
sapply(paste0("./Code/Voting Data/Lower House/Social Redistribution/", socred), source)

source("./Code/find_politician_id.R")
source("./Code/find_district.R")
source("./Code/get_all_variables.R")
source("./Code/clean_districts.R")

#antistakingswet 1903
id <- find_politician_id(antistakingswet1903$politician, antistakingswet1903$date[1])
voting_outcomes <- left_join(antistakingswet1903, id, by = c("politician" = "names"))
distr <- find_district(voting_outcomes$polid, voting_outcomes$date[1])
voting_outcomes <- left_join(voting_outcomes, distr, by = c("polid" = "b1-nummer"))
#districts
voting_outcomes$toelichting <- clean_districts(voting_outcomes$toelichting, voting_outcomes$date[1])
asw1903 <- get_all_variables(voting_outcomes, voting_outcomes$date[1])

modelantistakingswet1903 <- asw1903 %>%
  glm(formula = as.numeric(vote) ~
        log(1+wealth_timevote) +
    #    agricul_share + 
     #   share_aandeel + 
    #    amount_strikes +
   #     Percentage +
    #    Turnout +
   #     days_to_next_el +
   #     age_of_vote +
  #      RK_pct +
        polparty, family=binomial(link = "logit")) 

a <- nobs(modelantistakingswet1903) ; b <-  modelantistakingswet1903$data$law[1]
n_voted <- length(voting_outcomes$vote)
bmodelantistakingswet1903 <- broom::tidy(modelantistakingswet1903) %>%
  cbind(law = b, n = a , n_voted = n_voted)
#arbeidscontractwet 1906
id <- find_politician_id(arbeidscontractwet1907$politician, arbeidscontractwet1907$date[1])
voting_outcomes <- left_join(arbeidscontractwet1907, id, by = c("politician" = "names"))
distr <- find_district(voting_outcomes$polid, voting_outcomes$date[1])
voting_outcomes <- left_join(voting_outcomes, distr, by = c("polid" = "b1-nummer"))

voting_outcomes$toelichting <- clean_districts(voting_outcomes$toelichting, voting_outcomes$date[1])
acw1907 <- get_all_variables(voting_outcomes, voting_outcomes$date[1])

modelarbeidscontractwet1907 <- acw1907 %>%
  glm(formula = as.numeric(vote) ~
        log(1+wealth_timevote) +
            services_share + 
     #     nearestcompetitormargin + 
            amount_strikes +
      #       Percentage +
            Turnout +
   #          days_to_next_el +
        #     age_of_vote +
           RK_pct# +
      #  polparty
   , family=binomial(link = "logit")) 

a <- nobs(modelarbeidscontractwet1907) ; b <-  modelarbeidscontractwet1907$data$law[1]
n_voted <- length(voting_outcomes$vote)
bmodelarbeidscontractwet1907 <- broom::tidy(modelarbeidscontractwet1907) %>%
  cbind(law = b, n = a , n_voted = n_voted)

#hogeronderwijswet 1904
id <- find_politician_id(hogeronderwijswet1904$politician, hogeronderwijswet1904$date[1])
voting_outcomes <- left_join(hogeronderwijswet1904, id, by = c("politician" = "names"))
distr <- find_district(voting_outcomes$polid, voting_outcomes$date[1])
voting_outcomes <- left_join(voting_outcomes, distr, by = c("polid" = "b1-nummer"))

voting_outcomes$toelichting <- clean_districts(voting_outcomes$toelichting, voting_outcomes$date[1])
how1904 <- get_all_variables(voting_outcomes, voting_outcomes$date[1])

modelhogeronderwijswet1904 <- how1904 %>%
  glm(formula = as.numeric(vote) ~
        log(1+wealth_timevote) +
   #     services_share + 
   #     nearestcompetitormargin + 
        amount_strikes +
  #      Percentage +
  #      Turnout +
        #          days_to_next_el +
        age_of_vote +
 #       RK_pct +
        polparty, family=binomial(link = "logit")) 

a <- nobs(modelhogeronderwijswet1904) ; b <-  modelhogeronderwijswet1904$data$law[1]
n_voted <- length(voting_outcomes$vote)
bmodelhogeronderwijswet1904 <- broom::tidy(modelhogeronderwijswet1904) %>%
  cbind(law = b, n = a , n_voted = n_voted)


#kinderwet 1874
id <- find_politician_id(kinderwet1874$politician, kinderwet1874$date[1])
voting_outcomes <- left_join(kinderwet1874, id, by = c("politician" = "names"))
distr <- find_district(voting_outcomes$polid, voting_outcomes$date[1])
voting_outcomes <- left_join(voting_outcomes, distr, by = c("polid" = "b1-nummer"))

voting_outcomes$toelichting <- clean_districts(voting_outcomes$toelichting, voting_outcomes$date[1])
kw1874 <- get_all_variables(voting_outcomes, voting_outcomes$date[1])

modelkinderwet1874 <- kw1874 %>%
  glm(formula = as.numeric(vote) ~
        log(1+wealth_timevote) +
             services_share + 
             nearestcompetitormargin + 
        amount_strikes +
        #      Percentage +
        #      Turnout +
        #          days_to_next_el +
        age_of_vote +
             RK_pct +
        polparty, family=binomial(link = "logit")) 

a <- nobs(modelkinderwet1874) ; b <-  modelkinderwet1874$data$law[1]
n_voted <- length(voting_outcomes$vote)
bmodelkinderwet1874 <- broom::tidy(modelkinderwet1874) %>%
  cbind(law = b, n = a , n_voted = n_voted)

#leerplichtwet 1901
id <- find_politician_id(leerplichtwet1901$politician, leerplichtwet1901$date[1])
voting_outcomes <- left_join(leerplichtwet1901, id, by = c("politician" = "names"))
distr <- find_district(voting_outcomes$polid, voting_outcomes$date[1])
voting_outcomes <- left_join(voting_outcomes, distr, by = c("polid" = "b1-nummer"))

voting_outcomes$toelichting <- clean_districts(voting_outcomes$toelichting, voting_outcomes$date[1])
lpw1901 <- get_all_variables(voting_outcomes, voting_outcomes$date[1])

modelleerplichtwet1901 <- lpw1901 %>%
  glm(formula = as.numeric(vote) ~
        log(1+wealth_timevote) +
      #  agricul_share + 
        nearestcompetitormargin + 
       # amount_strikes +
        #      Percentage +
              Turnout +
                  days_to_next_el +
        age_of_vote +
       # RK_pct +
        polparty, family=binomial(link = "logit")) 

a <- nobs(modelleerplichtwet1901) ; b <-  modelleerplichtwet1901$data$law[1]
n_voted <- length(voting_outcomes$vote)
bmodelleerplichtwet1901 <- broom::tidy(modelleerplichtwet1901) %>%
  cbind(law = b, n = a , n_voted = n_voted)


#ongevallenwet1901
id <- find_politician_id(ongevallenwet1901$politician, ongevallenwet1901$date[1])
voting_outcomes <- left_join(ongevallenwet1901, id, by = c("politician" = "names"))
distr <- find_district(voting_outcomes$polid, voting_outcomes$date[1])
voting_outcomes <- left_join(voting_outcomes, distr, by = c("polid" = "b1-nummer"))

voting_outcomes$toelichting <- clean_districts(voting_outcomes$toelichting, voting_outcomes$date[1])
ogw1901 <- get_all_variables(voting_outcomes, voting_outcomes$date[1])

modelongevallenwet1901 <- ogw1901 %>%
  glm(formula = as.numeric(vote) ~
        log(1+wealth_timevote) +
        #  agricul_share + 
        nearestcompetitormargin + 
        # amount_strikes +
        #      Percentage +
        Turnout +
        days_to_next_el +
        age_of_vote +
         RK_pct +
        polparty, family=binomial(link = "logit")) 

a <- nobs(modelongevallenwet1901) ; b <-  modelongevallenwet1901$data$law[1]
n_voted <- length(voting_outcomes$vote)
bmodelongevallenwet1901 <- broom::tidy(modelongevallenwet1901) %>%
  cbind(law = b, n = a , n_voted = n_voted)



#staatspensioen 1916
id <- find_politician_id(staatspensioen1916$politician, staatspensioen1916$date[1])
voting_outcomes <- left_join(staatspensioen1916, id, by = c("politician" = "names"))
distr <- find_district(voting_outcomes$polid, voting_outcomes$date[1])
voting_outcomes <- left_join(voting_outcomes, distr, by = c("polid" = "b1-nummer"))

voting_outcomes$toelichting <- clean_districts(voting_outcomes$toelichting, voting_outcomes$date[1])
sp1916 <- get_all_variables(voting_outcomes, voting_outcomes$date[1])

modelstaatspensioen1916 <- sp1916 %>%
  glm(formula = as.numeric(vote) ~
      log(1+wealth_timevote) +
       # services_share +
      #  nearestcompetitormargin +
      #  agricul_share + 
      #  nearestcompetitormargin + 
        amount_strikes +
        #      Percentage +
       # Turnout +
        days_to_next_el +
        age_of_vote +
        RK_pct 
   #     polparty
       , family=binomial(link = "logit")) 

a <- nobs(modelstaatspensioen1916) ; b <-  modelstaatspensioen1916$data$law[1]
n_voted <- length(voting_outcomes$vote)
bmodelstaatspensioen1916 <- broom::tidy(modelstaatspensioen1916) %>%
  cbind(law = b, n = a , n_voted = n_voted)


#wetouderdom1913
id <- find_politician_id(wetouderdominvaliditeit1913$politician, wetouderdominvaliditeit1913$date[1])
voting_outcomes <- left_join(wetouderdominvaliditeit1913, id, by = c("politician" = "names"))
distr <- find_district(voting_outcomes$polid, voting_outcomes$date[1])
voting_outcomes <- left_join(voting_outcomes, distr, by = c("polid" = "b1-nummer"))

voting_outcomes$toelichting <- clean_districts(voting_outcomes$toelichting, voting_outcomes$date[1])
woi1913 <- get_all_variables(voting_outcomes, voting_outcomes$date[1])

modelwetouderdominvaliditeit1913 <- woi1913 %>%
  glm(formula = as.numeric(vote) ~
        log(1+wealth_timevote) +
     #     agricul_share + 
       nearestcompetitormargin + 
         amount_strikes +
   #           Percentage +
   #     Turnout +
    #    days_to_next_el +
     #   age_of_vote +
     #   RK_pct +
        polparty, family=binomial(link = "logit")) 

a <- nobs(modelwetouderdominvaliditeit1913) ; b <-  modelwetouderdominvaliditeit1913$data$law[1]
n_voted <- length(voting_outcomes$vote)
bmodelwetouderdominvaliditeit1913 <- broom::tidy(modelwetouderdominvaliditeit1913) %>%
  cbind(law = b, n = a , n_voted = n_voted)

#woningwet1901
id <- find_politician_id(woningwet1901$politician, woningwet1901$date[1])
voting_outcomes <- left_join(woningwet1901, id, by = c("politician" = "names"))
distr <- find_district(voting_outcomes$polid, voting_outcomes$date[1])
voting_outcomes <- left_join(voting_outcomes, distr, by = c("polid" = "b1-nummer"))

voting_outcomes$toelichting <- clean_districts(voting_outcomes$toelichting, voting_outcomes$date[1])
ww1901 <- get_all_variables(voting_outcomes, voting_outcomes$date[1])

modelwoningwet1901 <- ww1901 %>%
  glm(formula = as.numeric(vote) ~
        log(1+wealth_timevote) +
         #    agricul_share + 
        nearestcompetitormargin + 
        amount_strikes +
        #           Percentage +
        #     Turnout +
        #    days_to_next_el +
           age_of_vote +
           RK_pct +
        polparty, family=binomial(link = "logit")) 

a <- nobs(modelwoningwet1901) ; b <-  modelwoningwet1901$data$law[1]
n_voted <- length(voting_outcomes$vote)
bmodelwoningwet1901 <- broom::tidy(modelwoningwet1901) %>%
  cbind(law = b, n = a , n_voted = n_voted)


#ziektewet1913
id <- find_politician_id(ziektewet1913$politician, ziektewet1913$date[1])
voting_outcomes <- left_join(ziektewet1913, id, by = c("politician" = "names"))
distr <- find_district(voting_outcomes$polid, voting_outcomes$date[1])
voting_outcomes <- left_join(voting_outcomes, distr, by = c("polid" = "b1-nummer"))

voting_outcomes$toelichting <- clean_districts(voting_outcomes$toelichting, voting_outcomes$date[1])
zw1913 <- get_all_variables(voting_outcomes, voting_outcomes$date[1])

modelziektewet1913 <- zw1913 %>%
  glm(formula = as.numeric(vote) ~
        log(1+wealth_timevote) +
      #  services_share +
        #    agricul_share + 
      #  nearestcompetitormargin + 
        amount_strikes +
       #        Percentage +
            Turnout +
            days_to_next_el +
   #     age_of_vote +
    #    RK_pct +
        polparty, family=binomial(link = "logit")) 

a <- nobs(modelziektewet1913) ; b <-  modelziektewet1913$data$law[1]
n_voted <- length(voting_outcomes$vote)
bmodelziektewet1913 <- broom::tidy(modelziektewet1913) %>%
  cbind(law = b, n = a , n_voted = n_voted)

### Analysis in 1 table
stargazer(modelantistakingswet1903, modelarbeidscontractwet1907,
          modelhogeronderwijswet1904, modelkinderwet1874,leerplichtwet1901,
          column.labels = c("AS1903", "ACW1907", "HOW1904", "KW1874",
                            "LPW1901"), 
          font.size = "scriptsize",
          column.sep.width = "0pt", 
          dep.var.labels.include = F,
          title = "Analysis of Social Redistribution")


stargazer(modelongevallenwet1901, modelstaatspensioen1916,
modelwetouderdominvaliditeit1913, modelwoningwet1901, 
modelziektewet1913, 
column.labels = c("OGW1901", "SP1916","WOI1913",
                  "WW1901", "ZW1913"), font.size = "scriptsize",
column.sep.width = "0pt", dep.var.labels.include = F, title = "Analysis of Social Redistribution")



#notes = "AS = Anti-Strike, ACW = Labor Contract, \\\\
#          HOW = Higher Education Laws, KW = Child Labor Regulation, \\\\
 #         LP = Leerplichtwet, OGW = Ongevallenwet, SP = State Pension, \\\\
 #         WOI = Ouderdom en Invaliditeit, WW = Woningwet, \\\\
#         ZW = Ziektewet"


## combine in 1 dataframe

socred <- bind_rows(asw1903, acw1907,
          how1904, kw1874,lpw1901,
          ogw1901, sp1916,
          woi1913, ww1901, zw1913)

all1 <- socred %>%
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

all2 <- socred %>%
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


all3 <- socred %>%
  glm(formula = as.numeric(vote) ~
        log(1+wealth_timevote) +
        #     agricul_share + 
        #     share_aandeel + 
        #     amount_strikes +
        #     Percentage +
        #     Turnout +
             days_to_next_el +
             age_of_vote +
            RK_pct +
        polparty, family=binomial(link = "logit"))


all4 <- socred %>%
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


all5 <- socred %>%
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


stargazer(all1, all2, all3, all4, all5,
          column.sep.width = "0pt", 
          dep.var.labels.include = F, 
          font.size = "scriptsize",
          title = "Analysis of Social Redistribution")


all6 <- socred %>%
  glm(formula = as.numeric(vote) ~
        log(1+wealth_timevote):law +
        #     agricul_share + 
        #   share_aandeel + 
        amount_strikes +
     #   Percentage +
     #   Turnout +
        days_to_next_el +
        age_of_vote +
        RK_pct +
        polparty, family=binomial(link = "logit"))

all7 <- socred %>%
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

stargazer(all6, all7,
          column.sep.width = "0pt", 
          dep.var.labels.include = F, 
          font.size = "scriptsize",
          title = "Analysis of Social Redistribution")

