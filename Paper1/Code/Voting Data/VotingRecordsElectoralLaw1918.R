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

test1918 <- parl %>%
  filter(x1918_1922 == 1)

# Filteren op begin en einde periode
test1918 <- test1918 %>%
  mutate(begin_periode = as.Date(begin_periode),
         einde_periode = as.Date(einde_periode)) %>%
  filter(begin_periode < "1919-05-09" & einde_periode > "1919-05-09") %>%
  mutate(va = paste(voorletters, achternaam, sep = " "))

# Now the voting results
## Yes votes
voting_outcomes <- data.frame(politician = c(
  "Marchant", "van Ryckevorsel", "Kolkman", "Kolthek", 
  "J. ter Laan", "Kuiper", "Gerhard",
  "Otto", "Kruyt", "J.E.W. Duijs", "Niemeijer", "Lely", "Bakker", "Schaper", 
  "Groeneweg", "Helsdingen", "Reijmer", "L. M. Hermans",
  "Ossendorp", "van Zadelhoff", "van Rappard", "Rutgers", "Ketelaar",
  "de Groot", "Albarda", "Nolens", "Kleerekoper", "de Muralt", "Sannes",
  "de Savornin Lohman", "Teenstra", "Ter Hall", "Bongaerts", "van de Laar", 
  "Treub", "Hugenholtz", "Oud", "de Buisonjé", "Bulten",
  "Rink", "Bos", "van Vuuren", "van Beresteyn", "de Zeeuw", "Loeff", 
  "van den Tempel", "Heijkoop", "Swane", "Rugge", "Smeenk", "van de Bilt",
  "Weitkamp", "Poels", "H. G. M. Hermans", "de Wijkerslooth de Weerdesteyn", 
  "van Wijnbergen", "Deckers", "de Jonge", "Haazevoet",
  "Visser van IJzendoorn", "de Geer", "Wijnkoop", "van Ravesteijn", 
  "Fock"
  ))

voting_outcomes <- voting_outcomes %>%
  mutate(vote = rep(1, length(voting_outcomes$politician)))

## No votes
politician <- c(
  "A.P. Staalman", "Schokking", 
  "Duymaer van Twist", "Zijlstra", 
  "de Monté verLoren", "de Wilde", "Snoeck Henkemans", "Beumer", 
  "van der Voort van Zijp"," Heemskerk"
)

vote <- rep(0, length(politician))

## Combine
voting_outcomes <- rbind(voting_outcomes, 
                         cbind(politician, 
                               vote))


# Matching to wealth and politician data
matches <- amatch(voting_outcomes$politician, test1918$va, 
                  method = "jaccard", maxDist = Inf)

voting_outcomes <- voting_outcomes %>%
  mutate(name_match = test1918$va[matches], 
         id_match = test1918$id_match[matches])

## Correcting the mistakes manually
voting_outcomes[17,4] <- 1105
voting_outcomes[32,4] <- 514 
voting_outcomes[34,4] <- 774
voting_outcomes[51,4] <- 121

# Now, replace the names as well
newmatch <- match(voting_outcomes$id_match, test1918$id_match)

voting_outcomes <- voting_outcomes %>%
  mutate(name_match = test1918$va[newmatch])

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

mtable(model1, model2, model3, model4)
stargazer(model1, model2, model3, model4, 
          type = "latex", 
          header = FALSE,
          out = "1919results.tex",
          column.labels = c("Non-harnas", "Harnas"),
          column.separate = c(2,2)
)

model5 <- glm(data = voting_outcomes,
              vote ~ poldir, family = "binomial")

model6 <- glm(data = harnas, 
              vote ~ poldir, family = "binomial")

stargazer(model5, model6,
          type = "latex", 
          header = FALSE,
          out = "1919results_2.tex",
          column.labels = c("Non-harnas", "Harnas")
)

mtable(model5, model6)
