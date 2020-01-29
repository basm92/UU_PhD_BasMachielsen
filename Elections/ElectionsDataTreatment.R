##Initial setup
setwd("~/Documents/UU_PhD_BasMachielsen/Elections")
#setwd("C:/Users/Machi003/RWD/UU_PhD_BasMachielsen/Elections")

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

#Then, attempt to match the kind of election, the amount of seats and the amount of candidates. 
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
step4 <- step4 %>%
  group_by(Regio, sub_election) %>%
  mutate(
    number = ifelse(Kandidaat != "", seq_along(Kandidaat)-5,""), 
    voteshare = ifelse(Kandidaat != "", AantalStemmen/Geldig,""))

step4 <- step4 %>%
  group_by(Regio, sub_election) %>%
  mutate(number = as.numeric(number),
         no_of_candidates = max(number, na.rm = TRUE))
    
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


testerinho <- merge(
  x=step5, y=seatsavailable, 
  by.x = c("Regio", "sub_election"), 
  by.y = c("districtsnaam", "date"), all.x = TRUE)
  

#HIER VERDER 


test <- test %>%
  mutate(margin = ifelse(no_of_candidates == "2", AantalStemmen/votes, NA))

test <- test %>%
  group_by(Regio, date) %>%
  mutate(maxShare = max(VoteShare))

test <- test %>%
  mutate(margintest = VoteShare - maxShare)

#Now a rule indicating how many to pick, because in some districts 1 (with 2 or 3 cand)
#in some districts 2 (with 4 or 5 candidates) or 3 (more candidates)

#Look at all elected persons lists
#download.file("http://resources.huygens.knaw.nl/verkiezingentweedekamer/databank/geaggregeerd/download.csv", destfile = "allelected.csv")
allelected <- read.csv("allelected.csv")
allelected$date <- as.Date(paste(allelected$jaar, allelected$maand, allelected$dag, sep = "-"))

eltesterinho <- allelected %>%
  filter(type.verkiezing == "algemeen") %>%
  group_by(date, districtsnaam) %>%
  summarise(seats = mean(zetels))

#Dit ook nog aanpassen 
closeelections0 <- kandidaten %>%
  filter(number == "2", VoteShare > 0.45)

closeelections1 <- kandidaten %>%
  filter(number == "2" | number == "3", VoteShare > 0.4)

closeelections2 <- kandidaten %>%
  filter(number == "2" | number == "3", VoteShare > 0.35)
  
#Match the election data to the parliament names
#I have to match step4 with c. 
library(stringdist)

source1.devices <- kandidaten
source2.devices <- c

# To make sure we are dealing with charts
source1.devices$name<-as.character(kandidaten$Kandidaat)
source2.devices$name<-as.character(c$politician)

# It creates a matrix with the Standard Levenshtein 
# distance between the name fields of both sources
# Run only one of these two, and run only once
# dist.name<-adist(
#      source1.devices$name,source2.devices$name, 
#      partial = TRUE, ignore.case = TRUE)
 dist.name <- stringdistmatrix(
 source1.devices$name, source2.devices$name,
 method = "lv") 
# We now take the pairs with the minimum distance
min.name<-apply(dist.name, 1, min)
# Also run this chunck one time
match.s1.s2<-NULL  
for(i in 1:nrow(dist.name))
{
  s2.i<-match(min.name[i],dist.name[i,])
  s1.i<-i
match.s1.s2<-rbind(data.frame(s2.i=s2.i,s1.i=s1.i,
                                  s2name=source2.devices[s2.i,]$name, 
                                  s1name=source1.devices[s1.i,]$name, 
                                  adist=min.name[i]),match.s1.s2)
}
# and we then can have a look at the results
View(match.s1.s2)
# write.csv(match.s1.s2,"matches.csv")
# match.s1.s2 <- read.csv("matches.csv")




#Some graphics about electability
library(hrbrthemes)
library(scales)

d1 <- step4 %>%
  filter(RegioUitslag == "Kiesgerechtigden") %>%
  group_by(date) %>%
  summarise(TotalKG = sum(AantalStemmen))

d2 <- step4 %>%
  filter(RegioUitslag == "AantalGeldigeStemmen") %>%
  group_by(date) %>%
  summarise(TotalVV = sum(AantalStemmen))

d3 <- cbind(d1,d2)
d3 <-d3[,-3]

d3 <- d3%>%
  mutate(date = as.Date(date,  format = "%Y%m%d"))

p1 <- d3 %>%
  pivot_longer(2:3,names_to = "Variable", values_to = "value") %>%
  ggplot(aes(x = date, y = value, group = Variable, color = Variable)) + 
  geom_line() + 
  theme_ipsum_tw() + 
  labs(x = "year", y = "Voters") + 
  scale_y_continuous(labels = comma) +
  ggtitle("Suffrage Expansion")


p2 <- d3 %>%
  mutate(Rate = ifelse(TotalVV/TotalKG > 1, 1, TotalVV/TotalKG)) %>%
  ggplot(aes(x = date, y = Rate)) + geom_line() + theme_ipsum_tw() + labs(x = "Year", y = "Percentage") + ggtitle("Electoral Turnout")

library(gridExtra)

png(file = "Plot01.png", width = 1024, height = 480)
grid.arrange(p1,p2, ncol = 2)
dev.off()


#Import all names in Parliament Data
library(readxl)
setwd("~/Downloads")
#setwd("C:/Users/Machi003/Downloads")


#Parliament data.. 
parl <- read_excel("Parlementen.xlsx")
parl <- parl[,c(seq(1,52,by=2))]

b <- data.frame()
c <- data.frame()

for (i in 1:ncol(parl)) {
  b <- cbind(parl[,i], year = names(parl)[i])
  names(b) <- c("politician","year")
  c <- rbind(c,b)
}

c <- c %>% 
  na.omit()

