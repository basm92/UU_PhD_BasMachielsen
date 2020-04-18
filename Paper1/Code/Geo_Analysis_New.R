#Set the working directory appropriately
#setwd("C:/Users/Machi003/RWD/UU_PhD_BasMachielsen/Paper1")

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
politicians <- read_xlsx("./Data/tk_1815tot1950uu.xlsx", sheet = 1)
politicians2 <- read_xlsx("./Data/tk_1815tot1950uu.xlsx", sheet = 2)

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

nl1880@data <- left_join(nl1880@data, before1887, by = c("GM_NAAM" = "birthplace"), )

test <- nl1880@data %>%
    stringdist_join(before1887, by = c(GM_NAAM = "birthplace"), 
                    method = "osa", mode = "left",
                    max_dist = 1)

nl1900@data <- left_join(nl1900@data, between18871917, by = c("GM_NAAM" = "birthplace"))

nl1920@data <- left_join(nl1920@data, between19179140, by = c("GM_NAAM" = "birthplace"))

#Birthplace of politicians (municipality)

proj4string(nl1880) <- CRS("+init=epsg:24383")

cartogram <- cartogram_cont(nl1880, "count")

ggplot() + 
    geom_sf(data = st_union(sf::st_as_sf(nl1880)), fill = "grey", alpha = 0.5, color = "black") + 
    geom_sf(data = sf::st_as_sf(cartogram), aes(fill = count)) + 
    theme_void() +
    scale_fill_viridis_c(name = "Privet druzya", option = "plasma") + 
    labs(title = "Origin of Politicians", subtitle = "from 1880") +
    theme(
        text = element_text(color = "#22211d"), 
        plot.background = element_rect(fill = "transparent", color = "black"), 
        panel.background = element_rect(fill = "transparent", color = NA), 
        legend.background = element_rect(fill = "transparent", color = NA),
        plot.title = element_text(size= 24, 
                                  color = "#4e4d47"
        ),
        plot.subtitle = element_text(size = 18,
                                     color = "#4e4d47"
        )
    )

