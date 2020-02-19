# Voting Analysis

## This code chunck attempts to relation various voting outcomes
## to miscellaneous variables re: politicians, most importantly, wealth

# Loading Packages
library(readxl)
library(tidyverse)
library(stringdist)
library(janitor)
library(lubridate)
library(memisc)

# Now, import the sheet of all politicians - correctly matched
setwd("C:/Users/Machi003/RWD/UU_PhD_BasMachielsen")
parl <- read.csv("parl.csv")

parl <- clean_names(parl)

test1887 <- parl %>%
    filter(x1883_1884 == 1 | x1884_1886 == 1 | x1886_1887 == 1| x1887_1888 == 1)

# Filteren op begin en einde periode
test1887 <- test1887 %>%
    mutate(begin_periode = as.Date(begin_periode),
           einde_periode = as.Date(einde_periode)) %>%
    filter(begin_periode < "1887-03-23" & einde_periode > "1887-03-23") %>%
    mutate(va = paste(voorletters, achternaam, sep = " "))

# Now the voting results
## Yes votes
voting_outcomes <- data.frame(politician = c(
    "Greeve", "Van Alphen", 
     "Van Welderen Rengers", 
                     "Kielstra", "Willink", "Huber", 
                     "Van Osenbruggen", "Gildemeester", 
                     "Cremer", "Lieftinck",
                     "J.E.N. Schimmelpenninck van der Oye", 
                     "Van Diggelen", "De Bruyn Kops", 
                     "Van Asch van Wijk", "W. Van Dedem",
                     "Van Berckel", "Van den Biesen",
                     "Clercx", "Van Wassenaer Catwijck",
                     "Vos de Wael", "Van der Linden",
                     "Blussé", "Seret", "Visser van Hazerswoude",
                     "Van Delden", "Goekoop", "De Ranitz",
                     "Farncombe Sanders", "Viruly Verbrugge", 
                     "Rutgers van Rozenburg", "Goeman Borgesius",
                     "Smeenge", "De Vos van Steenwijk",
                     "A. Van Dedem", "Van der Kaay", "Van Kerkwijk",
                     "Gleichman", "Van der Goes van Dirxland", 
                     "Van der Sleyden", "Van Houten", "De Beaufort",
                     "Godin de Beaufort", "Hartogh", "Van Bylandt", 
                     "De Savornin Lohman", "Schaepman", "Smit",
                     "Fabius", "Reekers", "Ae. Mackay", "De Ruiter-Zylker",
                     "Kist", "De Meijier", "Verniers van der Loeff", 
                     "Mees", "Van Gennep", "Van der Feltz", 
                     "Meesters", "Van der Borch van Verwolde", "Schepel", 
                     "Buteux", "Van Aylva van Pallandt", "Buma",
                     "Th. Mackay", "Rooseboom", "Kolkman", "Zaaijer", 
                     "Cremers"))

voting_outcomes <- voting_outcomes %>%
    mutate(vote = rep(1, length(voting_outcomes$politician)))

## No votes
politician <- c("Van der Schrieck", "Smeele", "Borret", "Ruland",
                "Vermeulen", "Lambrechts", 
                "A. Schimmelpenninck van der Oye",
                "Ruys van Beerenbroek", "Haffmans", "Heldt", 
                "Keuchenius", "Brouwers", "Corver Hooft", 
                "De Geer van Jutfaas", "Reuther")

vote <- rep(0, length(politician))

## Combine
voting_outcomes <- rbind(voting_outcomes, 
                         cbind(politician, 
                               vote))


# Matching to wealth and politician data
matches <- amatch(voting_outcomes$politician, test1887$va, 
                  method = "jaccard", maxDist = Inf)

voting_outcomes <- voting_outcomes %>%
    mutate(name_match = test1887$va[matches], 
           id_match = test1887$id_match[matches])

## Correcting the mistakes manually
voting_outcomes[21,4] <- 828
voting_outcomes[35,4] <- 666      
voting_outcomes[57,4] <- 400

# Now, replace the names as well
newmatch <- match(voting_outcomes$id_match, test1887$id_match)

voting_outcomes <- voting_outcomes %>%
    mutate(name_match = test1887$va[newmatch])

# Now, we can match the voting outcomes with the wealth dataset
voting_outcomes <- left_join(voting_outcomes, parl, 
                             by = c("id_match" = "id_match"))

voting_outcomes <- voting_outcomes %>%
    mutate(vote = as.numeric(vote))

model1 <- glm(data = voting_outcomes, 
              vote ~ poldir + w_deflated2, family = "binomial")
model2 <- glm(data = voting_outcomes, 
              vote ~ poldir + log(1+w_deflated2), family = "binomial")


harnas <- voting_outcomes %>%
    filter(harnas2 == 1) 

model3 <- glm(data = harnas, 
              vote ~ poldir + w_deflated2, family = "binomial")
model4 <- glm(data = harnas, 
              vote ~ poldir + log(1+w_deflated2), family = "binomial")

mtable(model1, model2, model3, model4)
