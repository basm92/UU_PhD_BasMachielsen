#Initial setup
#setwd("/home/bas/Downloads/Elections")
setwd("C:/Users/Machi003/RWD/UU_PhD_BasMachielsen/Elections")
allfiles <- dir()
allfiles <- allfiles[2:27]

#Code for reading the files
library(stringr)

step1 <- data.frame()
step2 <- data.frame()
step3 <- data.frame()

for (i in allfiles) {
  step1 <- read.csv(i, sep = ";")
  step2 <- cbind(step1, date = str_extract(i,"\\d{8}"))
  step3 <- rbind(step3, step2)
}

#Step 3 contains all the elections
library(tidyverse)
names(step3)[1] <- "Regio"

#Create candidate positions
step4 <- step3 %>%
  group_by(Regio, date) %>%
  mutate(number = ifelse(Kandidaat != "", seq_along(Kandidaat)-5,""))

#Import all names in Parliament Data
library(readxl)
#setwd("~/Downloads")
setwd("C:/Users/Machi003/Downloads")

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

# My goal is to find who just won and just lost the elections. Let's find close elections first. 
kandidaten <- step4[step4$number != "",]

kandidaten <- kandidaten %>%
  group_by(Regio, date) %>%
  mutate(votes = sum(AantalStemmen), no_of_candidates = max(number))


### HIER VERDER
test <- kandidaten %>%
  mutate(VoteShare = AantalStemmen/votes)

test <- test %>%
  mutate(margin = ifelse(no_of_candidates == "2", AantalStemmen/votes, NA))

test <- test %>%
  group_by(Regio, date) %>%
  mutate(maxShare = max(VoteShare))

test <- test %>%
  mutate(margintest = VoteShare - maxShare)

#Now a rule indicating how many to pick, because in some districts 1 (with 2 or 3 cand)
#in some districts 2 (with 4 or 5 candidates) or 3 (more candidates)


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

