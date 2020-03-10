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
library(stargazer)

# Now, import the sheet of all politicians - correctly matched
# setwd("C:/Users/Machi003/RWD/UU_PhD_BasMachielsen")
setwd("/home/bas/Documents/UU_PhD_BasMachielsen")
parl <- read.csv("Elections/Data/parl.csv")

parl <- clean_names(parl)

test1896 <- parl %>%
    filter(x1894_1897 == 1)

# Filteren op begin en einde periode
test1896 <- test1896 %>%
    mutate(begin_periode = as.Date(begin_periode),
           einde_periode = as.Date(einde_periode)) %>%
    filter(begin_periode < "1896-06-19" & einde_periode > "1896-06-19") %>%
    mutate(va = paste(voorletters, achternaam, sep = " "))

# Now the voting results
## Yes votes
voting_outcomes <- data.frame(politician = c(
    "Loeff", "Viruly Verbrugge", "Bool", 
    "van Deinse", "van Vlijmen", "Tydeman",
    "Hintzen", "de Ram", "Lely", 
    "Goekoop", "Conrad", "Drucker", 
    "C.J.E. van Bylandt", #Van Bylandt Gouda 
    "Travglino", "van Gijn", "Schaafsma", 
    "Meesters", "E. Smidt", "Rutgers van Rozenburg",
    "Kolkman", "Willinge", "Guyot",
    "Kerkdijk", "Cremer", "Rooijaards van den Ham",
    "van Delden", "Bouman", "van Gennep", 
    "Hesselink van Suchtelen", "Pijnacker Hordijk",
    "Borret", "Lieftinck", "Veegens", "Heldt", 
    "Farncombe Sanders", "Harte", "Lambrechts", 
    "Vos de Wael", "d'Ansembourg", "Goeman Borgesius",
    "de Beaufort", #De Beaufort Wijk bij duurstede
    "Smeenge", "Bahlmann", "Hartogh", "van Basten Batenburg", 
    "Mees", "Ferf", "de Beaufort", #de Beaufort Amsterdam
    "Michiels van Verduynen", "Smits van Oijen", 
    "Pijnappel", "Mutsaers", "Dobbelmann",
    "Zijp", "Rink", "Schaepman", 
    "Everts", "Haffmans", "Zijlma",
    "Truijen", "Vermeulen", "Houwing", "de Ras",
    "Knijff", "Gleichman"
    ))

voting_outcomes <- voting_outcomes %>%
    mutate(vote = rep(1, length(voting_outcomes$politician)))

## No votes
politician <- c("Hennequin", "Donner", 
                "van Borssele", "Van Alphen", 
                "Bastert", "Schimmelpenninck", 
                "Heemskerk", "van Kerkwijk", 
                "A. Smit", "de Savornin Lohman",
                "Roessingh", "Staalman", 
                "T. Mackay", "van Dedem", "\'t Hooft",
                "Lucasse", "van den Bergh van Heemstede",
                "Tijdens", "van Limbrug Stirum", 
                "Ae. Mackay", "Seret", "Pyttersen", 
                "F. van Bylandt") #Van Bylandt Apeldoorn 

vote <- rep(0, length(politician))

## Combine
voting_outcomes <- rbind(voting_outcomes, 
                         cbind(politician, 
                               vote))


# Matching to wealth and politician data
matches <- amatch(voting_outcomes$politician, test1896$va, 
                  method = "jaccard", maxDist = Inf)

voting_outcomes <- voting_outcomes %>%
    mutate(name_match = test1896$va[matches], 
           id_match = test1896$id_match[matches])


####
## VANAF HIER VERDERGAAN 

####


## Correcting the mistakes manually
voting_outcomes[8,4] <- 1079
voting_outcomes[31,4] <- 169
voting_outcomes[36,4] <- 522
voting_outcomes[68,4] <- 172      
voting_outcomes[79,4] <- 293

# Now, replace the names as well
newmatch <- match(voting_outcomes$id_match, test1896$id_match)

voting_outcomes <- voting_outcomes %>%
    mutate(name_match = test1896$va[newmatch])

# Now, we can match the voting outcomes with the wealth dataset
# But we filter out the duplicate observations in parl to account for 
# differences in name spelling! 
voting_outcomes <- left_join(voting_outcomes %>%
                                 group_by(id_match) %>%
                                 mutate(id = row_number()),
                             parl %>%
                                 group_by(id_match) %>%
                                 mutate(id = row_number()), 
                             by = c("id_match", "id"))

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

stargazer(model1, model2, model3, model4, 
          type = "latex", 
          header = FALSE,
          out = "1896results.tex",
          column.labels = c("Non-harnas", "Harnas"),
          column.separate = c(2,2)
)


mtable(model1, model2, model3, model4)

