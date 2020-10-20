# Do all analyses in order:
source("./Code/find_politician_id.R")
source("./Code/find_district.R")
source("./Code/get_all_variables.R")

#Voting law 1872
source("./Code/Voting Data/Lower House/Electoral Law/VotingRecordsElectoralLaw1872.R")

id <- find_politician_id(kieswet1872$politician, kieswet1872$date[1])
voting_outcomes <- left_join(kieswet1872, id, by = c("politician" = "names"))

distr <- find_district(voting_outcomes$polid, voting_outcomes$date[1])
voting_outcomes <- left_join(voting_outcomes, distr, by = c("polid" = "b1-nummer"))
#Correct districts
voting_outcomes[51,7] <- "Breda"

hallo <- get_all_variables(voting_outcomes, voting_outcomes$date[1])

modelkiesrecht1872 <- hallo %>%
    glm(formula = as.numeric(vote) ~
            wealth_timevote +
            begin_periode +
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
            wealth_timevote +
            begin_periode +
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
            wealth_timevote +
            begin_periode +
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
            wealth_timevote +
            begin_periode +
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


results <- lapply(ls(pattern = "model"),get)
results <- results %>%
    purrr::reduce(bind_rows)

library(scales)
p1 <- results %>%
    filter(term == "begin_periode") %>%
    ggplot(aes(x = law, y = estimate, 
               ymin = estimate - 1.65* std.error,
               ymax = estimate + 1.65* std.error)) + geom_pointrange() +
    coord_flip() + labs(title = "Effect of first election time on propensity to vote in favor of Laws", 
                        subtitle = TeX("Model: $y_i = \\alpha + \\beta_1 \\cdot YearFirstElected_i + \\beta_2 \\cdot PolParty_i + Controls_i + Controls_d + \\epsilon_i$"),
                        y = "Coefficient magnitude", 
                        x = "Law") + scale_y_continuous(label=number_format(accuracy = 0.0001))

p2 <- results %>%
    filter(term == "age_of_vote") %>%
    ggplot(aes(x = law, y = estimate, 
               ymin = estimate - 1.65* std.error,
               ymax = estimate + 1.65* std.error)) + geom_pointrange() +
    coord_flip() + labs(title = "Effect of Age at Time of Vote on propensity to vote in favor of Laws", 
                        subtitle = TeX("Model: $y_i = \\alpha + \\beta_1 \\cdot Age_i + \\beta_2 \\cdot PolParty_i + Controls_i + Controls_d + \\epsilon_i$"),
                        y = "Coefficient magnitude", 
                        x = "Law") + scale_y_continuous(label=number_format(accuracy = 0.0001))

library(gridExtra)
p3 <- grid.arrange(p1, p2, nrow = 1)

ggsave("./Figures/effectsizes_FirstYearandAgeatVote.png", p3, width = 30, height = 20, units = "cm")
