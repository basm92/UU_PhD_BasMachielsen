# This file constructs a dataset relating the wealth to HARNAS, and 

library(tidyverse)
library(readxl)
library(stringr)
library(hrbrthemes)
library(janitor)
library(stringdist)
library(gridExtra)
library(scales)

#setwd("C:/Users/Machi003/RWD/UU_PhD_BasMachielsen")

# Dataframe of all lower house members
parl <- read_excel("../Data/Parlementen.xlsx")

## Clean the dataframe
parl <- parl[c(seq(from = 1, 
                   to = 41, 
                   by = 2))]

parl <- pivot_longer(parl, 
                     1:21, 
                     names_to = "parliament", 
                     values_to = "name")

parl <- parl %>%
    filter(!is.na(name))

## Make indicator variables to create year dummies
parl <- parl %>%
    pivot_wider(names_from = 1, values_from = 1, 
                values_fn = list(parliament = length),
                values_fill = list(parliament = 0))

parl[parl == 2] <- 1

## Clean the names a bit
parl$name <- str_replace(parl$name, "\\|(.*)", "")

# Now the dataframe is finished. String matching with politicians data as follows
## Import the identified data
polnames <- read_excel("../Data/tk_1815tot1950uu.xlsx")

polnames <- polnames %>%
    unite(name, 4,2, sep = " ", remove = FALSE)

## Pretreat polnames
### Extract end year numbers
polnames <- polnames %>%
    separate(11, 
             c("year","month","day"), 
             sep = "-", 
             remove = FALSE)

## Filter on End year
polnames <- polnames %>%
    filter(year > 1860)

# Now, we do string matching
## Generate the matches
matches <- amatch(parl$name, polnames$name, 
                  method = "osa", 
                  maxDist = Inf)

## Merge the datasets and change the order 
parl <- parl %>% 
    mutate(name_match = polnames$name[matches], 
           id_match = polnames$`b1-nummer`[matches])

parl <- parl[,c(1,23,24,2:22)]

# Write this dataset to Excel
write.csv(parl,"match.csv")
# Manually match the rest

#First, I import the file were the exact matches are located (manually checked)
parl <- read.csv("../Data/match_corrected.csv", 
                 colClasses=c(rep("character",3), 
                              rep("numeric",21)))

# The following is not necessary
#parl$id_match <- str_pad(parl$id_match, 5, pad = "0")
#write.csv(parl, "match_corrected.csv", row.names = FALSE)

# Now, finally, merge parl with the analysis sheet with wealth using 
# the identifier number in both
wealth <-read_excel("Elections/Data/AnalysisFile.xlsx", sheet = "Analysis")
wealth <- wealth[,c(1:29, 35, 36)]

wealth <- clean_names(wealth)

parl <- left_join(parl, polnames, 
                  by = c("id_match" =  "b1-nummer")
                  )

parl <- left_join(parl, wealth, 
                  by = c("id_match" = "indexnummer")
                  )


parl <- parl %>%
    mutate(harnas2 = ifelse(yod - endyr < 3, 1, 0),
           harnas5 = ifelse(yod - endyr < 6, 1, 0)
           )

names(parl)[4:24] <- names(parl)[4:24] %>%
    str_replace("X","") %>%
    str_replace("\\.","-")

# Write a csv of this file to use in voting analysis
write.csv(parl, "parl.csv")


#Now generate the datasets for the plots

grid1 <- parl %>%
    pivot_longer(4:24, 
                 names_to = "parliament", 
                 values_to = "indicator") %>%
    filter(indicator == 1) %>%
    group_by(parliament) %>%
    summarise(mean = mean(w_deflated, na.rm = TRUE), 
              median = median(w_deflated, na.rm = TRUE), 
              count = n(),
              se = sd(w_deflated, na.rm = TRUE)/sqrt(count))


grid2 <- parl %>%
    pivot_longer(4:24, 
                 names_to = "parliament", 
                 values_to = "indicator") %>%
    filter(indicator == 1, !is.na(harnas2)) %>%
    group_by(parliament, harnas2) %>%
    summarise(mean = mean(w_deflated, na.rm = TRUE), 
              median = median(w_deflated, na.rm = TRUE), 
              count = n(),
              se = sd(w_deflated, na.rm = TRUE)/sqrt(count))

grid3 <- parl %>%
    pivot_longer(4:24, 
                 names_to = "parliament", 
                 values_to = "indicator") %>%
    filter(indicator == 1, !is.na(harnas5)) %>%
    group_by(parliament, harnas5) %>%
    summarise(mean = mean(w_deflated, na.rm = TRUE), 
              median = median(w_deflated, na.rm = TRUE), 
              count = n(),
              se = sd(w_deflated, na.rm = TRUE)/sqrt(count))


p1 <- grid1 %>%
    ggplot(aes(x= as.factor(parliament), y = mean, group = 1)) + 
    geom_line() + 
    geom_errorbar(aes(
        ymin = mean + -1.96*se, ymax = mean + 1.96*se), 
        width = 0.2) +
    #theme_ipsum_rc() + 
    ggtitle("Unconditional avg. wealth per parliament") +
    scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) + 
    xlab("Parliament") + ylab("Mean Wealth") 

p2 <- grid2 %>%
    ggplot(aes(x = parliament, 
               y = mean, 
               group = as.factor(harnas2), 
               linetype = as.factor(harnas2), 
                label = count)) + 
    geom_line() +
    geom_text() +
    ggtitle("Avg. wealth per parliament per value of HARNAS") +
    scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) + 
    xlab("Parliament") + ylab("Mean Wealth") + 
    scale_linetype_manual(name = "Harnas", 
                          values = c("dashed","solid"),
                          labels = c(">2 Years","<2 Years"))

p3 <- grid3 %>%
    ggplot(aes(x = parliament, 
               y = mean, 
               group = as.factor(harnas5), 
               linetype = as.factor(harnas5), 
               label = count)) + 
    geom_line() +
    geom_text() +
 #   theme_ipsum_rc() + 
    ggtitle("Avg. wealth per parliament per value of HARNAS") +
    scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) + 
    xlab("Parliament") + ylab("Mean Wealth") + 
    scale_linetype_manual(name = "Harnas", 
                          values = c("dashed","solid"),
                          labels = c(">5 Years","<5 Years"))


figure1 <- grid.arrange(p1,p2,p3, nrow = 3)
ggsave("Elections/Figures/figure1.png",figure1, height = 8, width = 9)


#To do

#Clean variable names 
#Make a list of influential politicians and repeat the above exercise
