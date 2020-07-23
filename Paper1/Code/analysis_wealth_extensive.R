# Do all analyses in order:
source("./Code/find_politician_id.R")
source("./Code/find_district.R")
source("./Code/get_all_variables.R")

#Voting law 1872
source("./Code/Voting Data/VotingRecordsElectoralLaw1872.R")

id <- find_politician_id(kieswet1872$politician, kieswet1872$date[1])
voting_outcomes <- left_join(kieswet1872, id, by = c("politician" = "names"))

distr <- find_district(voting_outcomes$polid, voting_outcomes$date[1])
voting_outcomes <- left_join(voting_outcomes, distr, by = c("polid" = "b1-nummer"))
#Correct districts
voting_outcomes[51,7] <- "Breda"

hallo <- get_all_variables(voting_outcomes, voting_outcomes$date[1])

modelkiesrecht1872 <- hallo %>%
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

a <- nobs(modelkiesrecht1872) ; b <-  modelkiesrecht1872$data$law[1]
n_voted <- length(voting_outcomes$vote)
modelkiesrecht1872 <- broom::tidy(modelkiesrecht1872) %>%
    cbind(law = b, n = a , n_voted = n_voted)

#Voting law 1887
source("./Code/Voting Data/VotingRecordsElectoralLaw1887.R")
id <- find_politician_id(kieswet1887$politician, kieswet1887$date[1])
voting_outcomes <- left_join(kieswet1887, id, by = c("politician" = "names"))

distr <- find_district(voting_outcomes$polid, voting_outcomes$date[1])
voting_outcomes <- left_join(voting_outcomes, distr, by = c("polid" = "b1-nummer"))
#Correct districts
voting_outcomes[48,7] <- "Delft"

hallo <- get_all_variables(voting_outcomes, voting_outcomes$date[1])

modelkiesrecht1887 <- hallo %>%
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

a <- nobs(modelkiesrecht1887) ; b <-  modelkiesrecht1887$data$law[1]
n_voted <- length(voting_outcomes$vote)
modelkiesrecht1887 <- broom::tidy(modelkiesrecht1887) %>%
    cbind(law = b, n = a , n_voted = n_voted)

#Kieswet 1892
source("./Code/Voting Data/VotingRecordsElectoralLaw1892.R")

id <- find_politician_id(kieswet1892$politician, kieswet1892$date[1])
voting_outcomes <- left_join(kieswet1892, id, by = c("politician" = "names"))

distr <- find_district(voting_outcomes$polid, voting_outcomes$date[1])
voting_outcomes <- left_join(voting_outcomes, distr, by = c("polid" = "b1-nummer"))
#Correct districts
voting_outcomes[c(28,77),7] <- c("Zutphen", "Waalwijk")

hallo <- get_all_variables(voting_outcomes, voting_outcomes$date[1])

modelkiesrecht1892 <- hallo %>%
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

a <- nobs(modelkiesrecht1892) ; b <-  modelkiesrecht1892$data$law[1]
n_voted <- length(voting_outcomes$vote)
modelkiesrecht1892 <- broom::tidy(modelkiesrecht1892) %>%
    cbind(law = b, n = a , n_voted = n_voted)

#Voting law 1896
source("./Code/Voting Data/VotingRecordsElectoralLaw1896.R")

id <- find_politician_id(kieswet1896$politician, kieswet1896$date[1])
voting_outcomes <- left_join(kieswet1896, id, by = c("politician" = "names"))

distr <- find_district(voting_outcomes$polid, voting_outcomes$date[1])
voting_outcomes <- left_join(voting_outcomes, distr, by = c("polid" = "b1-nummer"))
#Correct districts
voting_outcomes[c(7,11,30,32,34,53,64,67,72,73,78,90),7] <- c("Rotterdam",
                                                              "'s-Gravenhage",
                                                              "-s-Gravenhage",
                                                              "Franeker",
                                                              "Amsterdam",
                                                              "Amsterdam",
                                                              "Wolvega",
                                                              "Amsterdam",
                                                              "NA",
                                                              "Utrecht",
                                                              "Goes",
                                                              "Schoterland"
)

hallo <- get_all_variables(voting_outcomes, voting_outcomes$date[1])

modelkiesrecht1896 <- hallo %>%
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

a <- nobs(modelkiesrecht1896) ; b <-  modelkiesrecht1896$data$law[1]
n_voted <- length(voting_outcomes$vote)
modelkiesrecht1896 <- broom::tidy(modelkiesrecht1896) %>%
    cbind(law = b, n = a , n_voted = n_voted)


# Now, fiscal laws
source("./Code/Voting Data/VotingRecordsInkomstenbelasting1893.R")

id <- find_politician_id(inkomstenbelasting1893$politician, inkomstenbelasting1893$date[1])
voting_outcomes <- left_join(inkomstenbelasting1893, id, by = c("politician" = "names"))

distr <- find_district(voting_outcomes$polid, voting_outcomes$date[1])
voting_outcomes <- left_join(voting_outcomes, distr, by = c("polid" = "b1-nummer"))
#Correct districts
voting_outcomes[c(26,84),7] <- c("Zutphen", "Waalwijk")

hallo <- get_all_variables(voting_outcomes, voting_outcomes$date[1])

modelinkomstenbelasting1893 <- hallo %>%
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
modelinkomstenbelasting1893 <- broom::tidy(modelinkomstenbelasting1893) %>%
    cbind(law = b, n = a , n_voted = n_voted)


source("./Code/Voting Data/VotingRecordsSuccessiewet1911.R")

id <- find_politician_id(successiewet1911$politician, successiewet1911$date[1])
voting_outcomes <- left_join(successiewet1911, id, by = c("politician" = "names"))

distr <- find_district(voting_outcomes$polid, voting_outcomes$date[1])
voting_outcomes <- left_join(voting_outcomes, distr, by = c("polid" = "b1-nummer"))
#Correct districts
voting_outcomes[c(5,6,10,12,19,20,
                  21,23, 30, 31, 38, 39,
                  42,45,48,
                  52,53,54,
                  60,62),7] <- c("Breukelen", "Amsterdam V",
                                 "Appingedam",
                                 "Hoogezand",
                                 "Schiedam",
                                 "Utrecht I",
                                 "Tietjerksteradeel",
                                 "Schoterland",
                                 "Zutphen",
                                 "Rotterdam I",
                                 "Steenwijk",
                                 "Zevenbergen",
                                 "Helmond",
                                 "Zuidhorn",
                                 "Wijk bij Duurstede",
                                 "Goes",
                                 "Franeker",
                                 "Grave",
                                 "Amsterdam VI",
                                 "Venlo")

hallo <- get_all_variables(voting_outcomes, voting_outcomes$date[1])

modelsuccessiewet1911 <- hallo %>%
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
modelsuccessiewet1911 <- broom::tidy(modelsuccessiewet1911) %>%
    cbind(law = b, n = a , n_voted = n_voted)


source("./Code/Voting Data/VotingRecordsInkomstenbelasting1914.R")

id <- find_politician_id(inkomstenbelasting1914$politician, inkomstenbelasting1914$date[1])
voting_outcomes <- left_join(inkomstenbelasting1914, id, by = c("politician" = "names"))

distr <- find_district(voting_outcomes$polid, voting_outcomes$date[1])
voting_outcomes <- left_join(voting_outcomes, distr, by = c("polid" = "b1-nummer"))
#Correct districts
voting_outcomes[c(1,3,7:9, 11,
                  23,26,27,29,37,38,40,
                  45,50,52,53,55, 58:60, 65,
                  68, 71:74,
                  76:79),7] <- c("'s-Gravenhage I",
                                 "Amsterdam II",
                                 "Franeker", "Hoogezand", "Bergen op Zoom",
                                 "Zuidhorn", "Goes", "Haarlem",
                                 "Oostburg", "Venlo", "Deventer",
                                 "Amersfoort","Helmond", "Amsterdam III",
                                 "Zutphen", "Gorinchem", "Enschede", 
                                 "Appingedam", "Groningen", "Schiedam",
                                 "Rotterdam V", "Grave", "Emmen", "Breukelen",
                                 "Zevenbergen", "Wijk bij Duurstede","Steenwijk",
                                 "Breda", "Tilburg", "Tietjerksteradeel", "Sneek")

hallo <- get_all_variables(voting_outcomes, voting_outcomes$date[1]) 

modelinkomstenbelasting1914 <- hallo %>%
    glm(formula = as.numeric(vote) ~
            log(1+wealth_timevote) +
            agricul_share + 
            services_share +
        #    share_aandeel + 
        #    amount_strikes +
        #    Percentage +
         #   Turnout +
         #   days_to_next_el +
            age_of_vote +
            RK_pct +
            polparty, family=binomial(link = "logit")) 

a <- nobs(modelinkomstenbelasting1914) ; b <-  modelinkomstenbelasting1914$data$law[1]
n_voted <- length(voting_outcomes$vote)
modelinkomstenbelasting1914 <- broom::tidy(modelinkomstenbelasting1914) %>%
    cbind(law = b, n = a , n_voted = n_voted)


## Successiewet 1916
source("./Code/Voting Data/VotingRecordsSuccessiewet1916.R")

id <- find_politician_id(successiewet1916$politician, successiewet1916$date[1])
voting_outcomes <- left_join(successiewet1916, id, by = c("politician" = "names"))

distr <- find_district(voting_outcomes$polid, voting_outcomes$date[1])
voting_outcomes <- left_join(voting_outcomes, distr, by = c("polid" = "b1-nummer"))
#Correct districts
voting_outcomes[c(1,3,6,9,10,16, 17:19, 21, 23,
                  26,33, 36:38, 44, 47:49,
                  53:56,
                  61,63,64,67:69,
                  71:73, 75,76),7] <- c("Groningen", "Haarlem", "Deventer", 
                                        "Amersfoort", "Zuidhorn", "Zutphen",
                                        "Appingedam", "NA", "Amsterdam IX",
                                        "Oostburg", "Amsterdam VIII", "Gouda",
                                        "Hoogezand", "Rotterdam V", "Schiedam",
                                        "Winschoten", "Apeldoorn", "Enschede",
                                        "'s-Gravenhage I", "Emmen", "Breda",
                                        "Tilburg", "Breukelen", "Tietjerksteradeel",
                                        "Sneek", "Hontenisse", "Bergen op Zoom", 
                                        "Roermond", "Zevenbergen", "Grave",
                                        "Goes", "Nijmegen", "Steenwijk", "Helmond",
                                        "Wijk bij Duurstede")

hallo <- get_all_variables(voting_outcomes, voting_outcomes$date[1]) 

modelsuccessiewet1916 <- hallo %>%
    glm(formula = as.numeric(vote) ~
            log(1+wealth_timevote) +
        #    agricul_share + 
        #    share_aandeel + 
            amount_strikes +
            Percentage +
            Turnout +
            days_to_next_el +
            age_of_vote 
        #    RK_pct +
        #  +  polparty, 
            ,family=binomial(link = "logit")) 

a <- nobs(modelsuccessiewet1916) ; b <-  modelsuccessiewet1916$data$law[1]
n_voted <- length(voting_outcomes$vote)
modelsuccessiewet1916 <- broom::tidy(modelsuccessiewet1916) %>%
    cbind(law = b, n = a , n_voted = n_voted)


## Put everything in a data.frame and make a forest plot
results <- lapply(ls(pattern = "model"),get)
results <- results %>%
    purrr::reduce(bind_rows)

library(latex2exp)

p2 <- results %>%
    filter(term == "log(1 + wealth_timevote)") %>%
    ggplot(aes(x = law, y = estimate, 
               ymin = estimate - 1.65* std.error,
               ymax = estimate + 1.65* std.error)) + geom_pointrange() +
    coord_flip() + labs(title = "Effect of wealth on propensity to vote in favor of Laws", 
                        subtitle = TeX("Model: $y_i = \\alpha + \\beta_1 \\cdot log (1+W_i) + \\beta_2 \\cdot PolParty_i + Controls_i + Controls_d + \\epsilon_i$"),
                        y = "Coefficient magnitude", 
                        x = "Law") 

ggsave("./Figures/effectsizes_extensivemodel.png", p2, width = 25, units = "cm")

## Combine plots into one

#library(ggridExtra)
#p3 <- gridExtra::grid.arrange(p1, p2, nrow = 1)
#ggsave("./Figures/effectsizes_combined.png", p3, width = 35, height = 20, units = "cm")
