#Loading packages
library(jsonlite)
library(janitor)
library(stringr)
library(tidyverse)
library(hrbrthemes)
library(lubridate)
library(rvest)

#Reading in the data
Metadata <- read_json(
    "https://opendata.cbs.nl/ODataApi/OData/70222ned/TableInfos", 
    simplifyVector = TRUE)$value

DataProperties <- read_json(
    "https://opendata.cbs.nl/ODataApi/OData/70222ned/DataProperties", 
    simplifyVector = TRUE)$value

Periods <- read_json(
    "https://opendata.cbs.nl/ODataApi/OData/70222ned/Perioden",
    simplifyVector = TRUE)$value

Data1 <- read_json(
    "https://opendata.cbs.nl/ODataApi/OData/70222ned/UntypedDataSet",
    simplifyVector = TRUE)$value

Data2 <- read_json(
    "https://opendata.cbs.nl/ODataApi/OData/70222ned/TypedDataSet",
    simplifyVector = TRUE)$value

#Data2 is the right one

#Cleaning
Data2 <- clean_names(Data2)

Data2 <- Data2 %>%
    mutate(
        perioden = as.Date(paste(as.numeric(str_extract(perioden,"[0-9]+")),"-01-01", sep = "")))

#Plot
Data2 %>%
    filter(perioden < "1950-01-01") %>%
    ggplot(aes(x = perioden, y = totaal_overheidsuitgaven_voor_onderwijs_1)) +
    geom_line() +
    labs(x = "Year", y = "Total Education Expenses") + 
    geom_vline(xintercept = as.Date("1918-01-01"), linetype = "dashed") +
    theme_ipsum_rc()

#Now merge with government data
Governments <- read_html(
    "https://nl.wikipedia.org/wiki/Nederlandse_kabinetten_van_1848_t/m_WO_II") %>%
    html_nodes("#mw-content-text > div > table:nth-child(6)") %>%
    html_table(fill = TRUE)
Governments <- Governments[[1]]
Governments <- Governments[,-4]
Governments$`Politieke kleur`[Governments$`Politieke kleur`=="Christelijk"] <- "Confessioneel"

Governments$`Politieke kleur` <- gsub(
    "Liberaal(.)+","Liberaal", 
    Governments$`Politieke kleur`)

Governments$`Politieke kleur` <- gsub(
    "Conservatief","Confessioneel", 
    Governments$`Politieke kleur`)

temp <- read_html(
    "https://nl.wikipedia.org/wiki/Nederlandse_kabinetten_van_1848_t/m_WO_II") %>%
    html_nodes("#mw-content-text > div > table:nth-child(9)") %>%
    html_table(fill = TRUE)

temp <- temp[[1]]                            
temp <- temp[,-c(2,5,7)]
temp$Partijen <- rep("Confessioneel", 10)
names(temp) <- names(Governments)

#Final dataset
Governments <- rbind(Governments, temp)

#Convert dates
Governments$Aantreden <- dmy(Governments$Aantreden)
Governments$Aftreden <- dmy(Governments$Aftreden)

#Filter governments
GovernmentsNew <- Governments %>%
    filter(Aantreden > as.Date("1897-01-01")) 

#Final plot
Data2 %>%
    filter(perioden < as.Date("1945-01-01")) %>%
    ggplot() +
    labs(x = "Year", y = "Total Education Expenses")  +
    geom_rect(data = GovernmentsNew,
                   aes(xmin = Aantreden, 
                          xmax = Aftreden,
                          ymin = -Inf, 
                          ymax = Inf,
                          fill = `Politieke kleur`), 
                     alpha = 0.5) + 
    scale_fill_manual(values = c("dark blue", "orange"), name = "Political color") + 
    geom_vline(xintercept = as.Date("1918-01-01"), linetype = "dotted") +
    geom_line(aes(
        x = perioden, 
        y = totaal_overheidsuitgaven_voor_onderwijs_1,
        linetype = "Nominal")) +
    geom_line(aes(
        x = perioden, 
        y = per_hoofd_van_de_bevolking_23,
        linetype = "Per Capita")) +
    scale_linetype_manual(values = c("solid", "dashed"), name = "Variable") +
    theme_ipsum_rc() + ggtitle("Educational Expenses and Government Orientation")


