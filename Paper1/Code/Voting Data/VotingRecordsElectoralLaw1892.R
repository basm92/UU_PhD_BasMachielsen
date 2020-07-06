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

test1892 <- parl %>%
  filter(x1891_1894 == 1 | x1894_1897 == 1)

# Filteren op begin en einde periode
test1892 <- test1892 %>%
  mutate(begin_periode = as.Date(begin_periode),
         einde_periode = as.Date(einde_periode)) %>%
  filter(begin_periode < "1894-03-09" & einde_periode > "1894-03-09") %>%
  mutate(va = paste(voorletters, achternaam, sep = " "))

# Now the voting results
# Yes is in favor of suffrage extension, no is against 
# This outcome is contrary to the actual voting outcome, but consistent with 
# the other analyses
## Yes votes
voting_outcomes <- data.frame(politician = c(
  "Donner", "Van Karnebeek", "Gerritsen",
  "E. Smidt", "Beelaerts van Blokland", 
  "Haffmans", "van Beuningen", "Lucasse",
  "Rutgers van Rozenburg", "Bahlmann",
  "Rink", "W.A. Feltz", "Hartogh", "Houwing",
  "Pyttersen", "Ferf", "Veegens", "de Kanter",
  "Smeenge", "Kerkdijk", "Seret", "Levy",
  "Tydeman", "Vrolik", "Zijp", "Zijlma", 
  "Valette", "Goeman Borgesius", "Cremer",
  "H.A. Velde", "Schepel", "Pijnacker Hordijk",
  "Farncombe Sanders", "Hennequin", "Heldt", 
  "A. Smit", "Land", "Tijdens", "Lieftinck", 
  "Roessingh", "van Kerkwijk"
))

voting_outcomes <- voting_outcomes %>%
  mutate(vote = rep(1, length(voting_outcomes$politician)))

## No votes
politician <- c(
  "van Gijn", "van Houten", "Geertsema", 
  "Schimmelpenninck van der Oye", "d'Ansembourg",
  "Guyot", "A. van Dedem", "van Alphen", 
  "Bevers", "Everts", "de Ras", "van den Berch van Heemstede",
  "Conrad", "P.G.J. Schrieck", "T. Mackay", "L.D.J.L. Ram",
  "Roell", "Walter", "E.A.M. Kun", "G. van Dedem", 
  "Kielstra", "Michiels van Verduynen", 
  "B.J. Lintelo baron de Geer van Jutphaas",
  "Harte van Tecklenburg", "Lambrechts", "Hintzen", "Heemskerk",
  "Smits van Oijen", "Dobbelmann", "De Beaufort", #De Beaufort Wijk bij Duurstede - missing
  "Reekers", "Vermeulen", "van Delden", "Roijaards van der Ham",
  "Kolkman", "Mutsaerts", "Travaglino", "Van Loben Sels", 
  "W.H. de Beaufort", #De Beaufort Amsterdam - 71
  "de Meijier", "de Savornin Lohman", "van Velzen", 
  "Huber", "van Vlijmen", "W. Kaay", "Th.L.M.H. Borret",
  "Mees", "Brantsen van de Zijp", "Bool", 
  "van Berckel", "Viruly Verbrugge", "van der Borch van Verwolde",
  "Ae. Mackay", "van Bylandt", "Plate", "Schaepman", 
  "Gleichman"
) 

vote <- rep(0, length(politician))

## Combine
voting_outcomes <- rbind(voting_outcomes, 
                         cbind(politician, 
                               vote))


# Matching to wealth and politician data
matches <- amatch(voting_outcomes$politician, test1892$va, 
                  method = "jaccard", maxDist = Inf)

voting_outcomes <- voting_outcomes %>%
  mutate(name_match = test1892$va[matches], 
         id_match = test1892$id_match[matches])


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
          out = "1892results.tex",
          column.labels = c("Non-harnas", "Harnas"),
          column.separate = c(2,2)
)


mtable(model1, model2, model3, model4)

