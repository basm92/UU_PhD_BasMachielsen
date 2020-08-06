elec <- list.files("./Code/Voting Data/Lower House/Electoral Law") 
sapply(paste0("./Code/Voting Data/Lower House/Electoral Law/", elec), source)

source("./Code/find_politician_id.R")
source("./Code/find_district.R")
source("./Code/get_all_variables.R")
source("./Code/clean_districts.R")

#kieswet 1872
id <- find_politician_id(kieswet1872$politician, kieswet1872$date[1])
voting_outcomes <- left_join(kieswet1872, id, by = c("politician" = "names"))
distr <- find_district(voting_outcomes$polid, voting_outcomes$date[1])
voting_outcomes <- left_join(voting_outcomes, distr, by = c("polid" = "b1-nummer"))
#districts
voting_outcomes$toelichting <- clean_districts(voting_outcomes$toelichting, voting_outcomes$date[1])
kw1872 <- get_all_variables(voting_outcomes, voting_outcomes$date[1])

modelkieswet1872 <- kw1872 %>%
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

a <- nobs(modelkieswet1872) ; b <-  modelkieswet1872$data$law[1]
n_voted <- length(voting_outcomes$vote)
bmodelkieswet1872 <- broom::tidy(modelkieswet1872) %>%
  cbind(law = b, n = a , n_voted = n_voted)

#kieswet1887
id <- find_politician_id(kieswet1887$politician, kieswet1887$date[1])
voting_outcomes <- left_join(kieswet1887, id, by = c("politician" = "names"))
distr <- find_district(voting_outcomes$polid, voting_outcomes$date[1])
voting_outcomes <- left_join(voting_outcomes, distr, by = c("polid" = "b1-nummer"))
#districts
voting_outcomes$toelichting <- clean_districts(voting_outcomes$toelichting, voting_outcomes$date[1])
kw1887 <- get_all_variables(voting_outcomes, voting_outcomes$date[1])

modelkieswet1887 <- kw1887 %>%
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

a <- nobs(modelkieswet1887) ; b <-  modelkieswet1887$data$law[1]
n_voted <- length(voting_outcomes$vote)
bmodelkieswet1887 <- broom::tidy(modelkieswet1887) %>%
  cbind(law = b, n = a , n_voted = n_voted)

#kieswet1892
id <- find_politician_id(kieswet1892$politician, kieswet1892$date[1])
voting_outcomes <- left_join(kieswet1892, id, by = c("politician" = "names"))
distr <- find_district(voting_outcomes$polid, voting_outcomes$date[1])
voting_outcomes <- left_join(voting_outcomes, distr, by = c("polid" = "b1-nummer"))
#districts
voting_outcomes$toelichting <- clean_districts(voting_outcomes$toelichting, voting_outcomes$date[1])
kw1892 <- get_all_variables(voting_outcomes, voting_outcomes$date[1])

modelkieswet1892 <- kw1892 %>%
  glm(formula = as.numeric(vote) ~
        log(1+wealth_timevote) +
        agricul_share + 
        share_aandeel + 
   #     amount_strikes +
        Percentage +
        Turnout +
    #    days_to_next_el +
    #    age_of_vote +
        RK_pct +
        polparty, family=binomial(link = "logit")) 

a <- nobs(modelkieswet1892) ; b <-  modelkieswet1892$data$law[1]
n_voted <- length(voting_outcomes$vote)
bmodelkieswet1892 <- broom::tidy(modelkieswet1892) %>%
  cbind(law = b, n = a , n_voted = n_voted)


#kieswet1896
id <- find_politician_id(kieswet1896$politician, kieswet1896$date[1])
voting_outcomes <- left_join(kieswet1896, id, by = c("politician" = "names"))
distr <- find_district(voting_outcomes$polid, voting_outcomes$date[1])
voting_outcomes <- left_join(voting_outcomes, distr, by = c("polid" = "b1-nummer"))
#districts
voting_outcomes$toelichting <- clean_districts(voting_outcomes$toelichting, voting_outcomes$date[1])
kw1896 <- get_all_variables(voting_outcomes, voting_outcomes$date[1])

modelkieswet1896 <- kw1896 %>%
  glm(formula = as.numeric(vote) ~
        log(1+wealth_timevote) +
        agricul_share + 
        share_aandeel + 
        amount_strikes +
   #     Percentage +
   #     Turnout +
   #     days_to_next_el +
        age_of_vote +
        RK_pct +
        polparty, family=binomial(link = "logit")) 

a <- nobs(modelkieswet1896) ; b <-  modelkieswet1896$data$law[1]
n_voted <- length(voting_outcomes$vote)
bmodelkieswet1896 <- broom::tidy(modelkieswet1896) %>%
  cbind(law = b, n = a , n_voted = n_voted)

# separate analysis table
stargazer(modelkieswet1872, modelkieswet1887, modelkieswet1892, modelkieswet1896,
          column.labels = c("1872", "1887", "1892", "1896"), 
          font.size = "scriptsize",
          column.sep.width = "0pt", 
          dep.var.labels.include = F, title = "Analysis of Suffrage Extension")

# combine everything in 1 table
elec <- bind_rows(kw1872, kw1887, kw1892, kw1896)

all1 <- elec %>%
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

all2 <- elec %>%
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


all3 <- elec %>%
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


all4 <- elec %>%
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


all5 <- elec %>%
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

#make the table
stargazer(all1, all2, all3, all4, all5,
          column.sep.width = "0pt", 
          dep.var.labels.include = F, 
          font.size = "scriptsize",
          title = "Analysis of Suffrage Extensions")

#split two models
all6 <- elec %>%
  glm(formula = as.numeric(vote) ~
        log(1+wealth_timevote):law +
        #     agricul_share + 
        #     share_aandeel + 
        #     amount_strikes +
        Percentage +
        Turnout +
        days_to_next_el +
        age_of_vote +
        RK_pct +
        polparty, family=binomial(link = "logit"))


all7 <- elec %>%
  glm(formula = as.numeric(vote) ~
        log(1+wealth_timevote):law +
        agricul_share + 
        share_aandeel + 
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
          title = "Analysis of Suffrage Extension")



