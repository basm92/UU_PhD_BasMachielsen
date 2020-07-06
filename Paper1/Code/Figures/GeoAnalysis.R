#Set the working directory appropriately
#setwd("C:/Users/Machi003/RWD/UU_PhD_BasMachielsen/Paper1")


#Primary source for shapefiles
#https://easy.dans.knaw.nl/ui/datasets/id/easy-dataset:44426/tab/2
#Load libraries
library(readxl)
library(tidyverse)
library(rgdal)
library(lubridate)
library(stringdist)
library(fuzzyjoin)


x <- c("ggmap", "rgdal", "rgeos", "maptools", "tmap")
lapply(x, library, character.only = TRUE)

#Load the politicians files
politicians <- read_xlsx("../Data/tk_1815tot1950uu.xlsx", sheet = 1)
politicians2 <- read_xlsx("../Data/tk_1815tot1950uu.xlsx", sheet = 2)

birth <- politicians2 %>%
    filter(rubriek == "3010") %>%
    select(1:4)

death <- politicians2 %>%
    filter(rubriek == "3020") %>%
    select(1:4)

politicians <- left_join(politicians, birth, by = 'b1-nummer')
names(politicians)[12:13] <- c("birthplace", "birthdate")

politicians <- left_join(politicians, death, by = 'b1-nummer')
names(politicians)[15:16] <- c("deathplace", "deathdate")

politicians <- politicians %>%
    mutate(deathdate = dmy(deathdate), 
           birthdate = dmy(birthdate),
           `begin periode` = ymd(`begin periode`),
           `einde periode` = ymd(`einde periode`))

#How many pol died in Den Haag? 
politicians %>%
    filter(deathdate < "1860-01-01") %>%
    group_by(deathplace) %>%
    summarise(count = n())


## Politicians active before 1887
before1887 <- politicians %>%
    filter(`begin periode` > "01-01-1860", `begin periode` < "1887-03-05") %>%
    group_by(birthplace) %>%
    summarise(count = n()) #%>%
#    mutate(birthplace = str_replace_all(birthplace, "\\s\\((.+)\\)", ""))

between18871917 <- politicians %>%
    filter(`begin periode` > "1887-03-05", `begin periode` < "1917-01-01") %>%
    group_by(birthplace) %>%
    summarise(count = n()) #%>%
#    mutate(birthplace = str_replace_all(birthplace, "\\s\\((.+)\\)", ""))

between19179140 <- politicians %>%
    filter(`begin periode` > "1917-01-01", `begin periode` < "1940-01-01") %>%
    group_by(birthplace) %>%
    summarise(count = n()) %>%
    mutate(birthplace = str_replace_all(birthplace, "\\s\\((.+)\\)", ""))


#Geographical databases: 

nl1880 <- readOGR(dsn = "Data", layer = "nl_1880")
nl1900 <- readOGR(dsn = "Data", layer = "nl_1900")
nl1920 <- readOGR(dsn = "Data", layer = "nl_1920")


#nl1880@data <- 
#nl1880@data <- 
nl1880@data <- left_join(nl1880@data, before1887, by = c("GM_NAAM" = "birthplace"), )

test <- nl1880@data %>%
    stringdist_join(before1887, by = c(GM_NAAM = "birthplace"), 
                         method = "osa", mode = "left",
                         max_dist = 1)

nl1900@data <- left_join(nl1900@data, between18871917, by = c("GM_NAAM" = "birthplace"))

nl1920@data <- left_join(nl1920@data, between19179140, by = c("GM_NAAM" = "birthplace"))

#Birthplace of politicians (municipality)

#Politicians before 1887
p1 <- tm_shape(nl1880)+  tm_polygons(col='count', 
                               palette='Reds', 
                               text = "count", 
                               alpha=0.4, 
                               border.col = 'transparent', 
                               breaks = c(1,5,10,15,20,150),
                               textNA = "0"
                               ) + 
    tm_legend(show=TRUE) + 
    tm_text(text = "count", 
            col = "black",
            scale = 0.8, 
            alpha = 0.8) +
    tm_layout(title = "1860 to 1887")

#Politicians 1887-1917
p2 <- tm_shape(nl1900) + tm_polygons(col='count', 
                             palette='Reds', 
                             text = "count", 
                             alpha=0.4, 
                             border.col = 'transparent', 
                             breaks = c(1,5,10,15,20,150),
                             textNA = "0"
) + 
    tm_legend(show=TRUE) + 
    tm_text(text = "count", 
            col = "black",
            scale = 0.8, 
            alpha = 0.8) +
    tm_layout(title = "1887 to 1917")


#Politicians 1917-1940
p3 <- tm_shape(nl1920) + tm_polygons(col='count', 
                               palette='Reds', 
                               text = "count", 
                               alpha=0.4, 
                               border.col = 'transparent', 
                               breaks = c(1,5,10,15,20,150),
                               textNA = "0"
) + 
    tm_legend(show=TRUE) + 
    tm_text(text = "count", 
            col = "black",
            scale = 0.8, 
            alpha = 0.8) +
    tm_layout(title = "1917 to 1940")

png("birthplace.png")
tmap_arrange(p1,p2,p3, ncol = 3)
dev.off()

