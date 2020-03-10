##Initial setup
#setwd("~/Documents/UU_PhD_BasMachielsen/Elections/Data")
#setwd("C:/Users/Machi003/RWD/UU_PhD_BasMachielsen/Elections/Data")

library(stringr)

allfiles <- dir()
allfiles <- allfiles[grepl("Uitslag(.+)", allfiles)]

#Code for reading the files

#Import all files
step1 <- data.frame()
step2 <- data.frame()
step3 <- data.frame()

for (i in allfiles) {
  step1 <- read.csv(i, sep = ";", encoding = "UTF-8")
  step2 <- cbind(step1, date = str_extract(i,"\\d.*[^\\.csv]+"))
  step3 <- rbind(step3, step2)
}

#Now, split all the dates according to general election and specifics
library(tidyverse)
step3 <- step3 %>%
  separate(col = date, into = c("main_election","sub_election"), sep = "_")

#Maybe remove (1) from string
step3$sub_election <- str_replace(step3$sub_election, "\\(1\\)","")

#Convert into dates
library(lubridate)
step3[,6] <- as.Date(step3[,6], format = "%Y%m%d")
step3[,7] <- dmy(step3[,7])

# Change variable name
names(step3)[1] <- "Regio"

#Then, attempt to match the kind of election, 
#the amount of seats and the amount of candidates. 
# 1. Make the dates
allelections <- read.csv("Data/allelections.csv")
allelections$date <- as.Date(paste(
    allelections$Jaar, 
    allelections$Maand, 
    allelections$Dag, sep = "-"))

#2. Numericize all other variables about election info
numchar <- function(x) { as.numeric(as.character(x))}
allelections[,7:10] <- sapply(allelections[,7:10], numchar) # Don't worry about the NA's
# These are observations with value '-', so they are automatically coerced to NA

# 3. Some observations have more turnout than electorate: 
allelections[allelections$Geldig > allelections$Electoraat,]
nieuw <- allelections[,c("District","date","Type", "Electoraat","Geldig")]


# 4. Now, attempt to match step3 to election info
step4 <- merge(x=step3, y=nieuw, 
               by.x = c("Regio", "sub_election"), 
               by.y = c("District", "date"))
step4 <- step4[order(step4[,2], step4[,1], step4[,8], step4[,4], step4[,6], 
                     decreasing = T),]

# 5. Create candidate positions, number of candidates, and the vote share
no_of_candidates <- step4 %>%
  group_by(Regio, sub_election, Type) %>%
  filter(Kandidaat != "") %>%
  count(sub_election)

step4 <- merge(step4, no_of_candidates)
names(step4)[11] <- "no_of_candidates"
#so far, correct.. 

# 6. Order it again: order should be: Main election, subelection, 8,2,1,3,5,7
step4 <- step4[order(step4[,8], step4[,2], step4[,1], step4[,3], step4[,5], step4[,7], 
                     decreasing = T),]

# 7. Make the numbers
step4 <- step4 %>%
  group_by(Regio, sub_election, Type) %>%
  mutate(
    number = ifelse(Kandidaat != "", seq_along(Kandidaat) -5,""), 
    voteshare = ifelse(Kandidaat != "", AantalStemmen/Geldig,""),
    number = as.numeric(number))

## Now, we ask ourselves: was it a close election?
## First, we get all politicians

allelected <- read.csv("Data/allelected.csv")

allelected <- allelected %>%
  mutate(name = paste(voornaam, achternaam, sep = " "))

allelected <- allelected %>%
  mutate(name = str_replace(name, "\\s\\s"," "))

politicians <- unique(allelected$name)

## Afterwards, we attempt to find whether the close election featured a runner-up (or second-runner-up) 
## that has never been into politics yet

#Before I do this, I attempt to clean the string of  step4$Kandidaat a little bit
#I run this step a couple of times, because each time, it replaces \.[a-z] with \.[a-z]\s

for (i in 1:3) {
step4$Kandidaat <- str_replace(step4$Kandidaat, "\\.[a-z]", 
            paste("\\.",
                  substr(
                    str_extract(
                      step4$Kandidaat, "\\.[a-z]"),
                          2,2)))
}


for (i in 1:3) {
  step4$Kandidaat <- str_replace(step4$Kandidaat, 
                "mr. |dr. |mr. dr. |dr. mr. |jhr. | jhr. mr. ",
                  "")
}


# Now make an indicator whether someone is a politician
step5 <- step4 %>%
  mutate(politician = ifelse(Kandidaat != "", ifelse(Kandidaat %in% politicians, 1, 0), ""))

# Then, we get the number of seats available to define close elections for 1 seat, 2 seats, and more seats.
allelected <- allelected %>%
  mutate(date = as.Date(
    paste(allelected$jaar, allelected$maand, allelected$dag, sep = "-")
  ))

seatsavailable <- allelected %>%
  group_by(districtsnaam, type.verkiezing,date) %>%
  summarise(seats = mean(zetels))


step5 <- merge(
  x=step5, y=seatsavailable, 
  by.x = c("Regio", "sub_election","Type"), 
  by.y = c("districtsnaam", "date","type.verkiezing"), all.x = TRUE)

#Order it again

step5 <- step5[order(step5[,8], step5[,2], step5[,1], step5[,3], step5[,5], step5[,7], 
                     decreasing = T),]


#Now find the appropriate margin
# Step 1 to do this: 
# In allelected Find the appropriate voter share per region, date, type for every elected guy
allelected <- merge(allelected, step5[,c(1,2,3,6,13)],
      by.x = c("districtsnaam", "type.verkiezing", "date", "name"),
      by.y = c("Regio", "Type", "sub_election", "Kandidaat"), 
      all.x = TRUE)


# Then, select the MINIMUM of elected share in that number, and compute the margin of all others with respect to this guy!

minvswinner <- allelected %>%
  group_by(districtsnaam, type.verkiezing, date) %>%
  summarise(minvswinner = min(voteshare))


# Then, merge it with step5! 
# All the elected politicians will have a positive margin, all the non-elected will have negative margin

step6 <- merge(step5, minvswinner,
      by.x = c("Regio", "Type", "sub_election"),
      by.y = c("districtsnaam", "type.verkiezing", "date"),
      all.x = TRUE)
      
step6 <- step6[order(step6[,8], step6[,2], step6[,1], step6[,3], step6[,5], step6[,7], 
                     decreasing = T),]

# If na, just use the margin w.r.t. top vote share

maxvswinner <- step6 %>%
  group_by(Regio, Type, sub_election) %>%
  summarise(maxvswinner = max(voteshare))

step6 <- merge(step6, maxvswinner)

for (i in 1:nrow(step6)) {
  if(is.na(step6$minvswinner[i])) {
    step6$minvswinner[i] <- step6$maxvswinner[i]
  }
}

# Finally, compute the margin, such that 
# All the elected politicians will have a positive margin, 
# all the non-elected will have negative margin
step6 <- step6 %>%
  mutate(voteshare = as.numeric(voteshare), minvswinner = as.numeric(minvswinner))

step6$margin <- step6$voteshare - step6$minvswinner

#This is a small check to see if they're not all the same (passed!) 
#step6[as.numeric(step6$minvswinner) != as.numeric(step6$maxvswinner),]

#Then, filter the dataset for the guys with the smallest possible negative margin
#First, a naive table: 0.4 > voteshare > 0.5 & politician == 0
naivelist <- step5[step5$voteshare > 0.4 & step5$politician == 0,]
write.csv(naivelist, "naivelist.csv")

#Now, a more nuanced list
nuancedlist <- step6[between(step6$margin,-0.15,-0.000001) & step6$politician == 0, ]
write.csv(nuancedlist, "nuancedlist.csv")


