# This file contains the code to investigate government finances
# First, load packages
library(readxl)
library(rvest)
library(jsonlite)
library(tidyverse)
library(stringr)
library(hrbrthemes)
library(janitor)
library(gridExtra)
library(lubridate)

# Second, download the excel file from:

#url <- "https://www.cpb.nl/sites/default/files/omnidownload/Langetijdreeksen-overheidsfinancien-mev2018_publicatie.xlsx"
#download.file(destfile = "Langetijdreeksen-overheidsfinancien-mev2018_publicatie.xlsx", 
#              url= url)

setwd("/home/bas/Documents/UU_PhD_BasMachielsen/")
# Third, read file:
govtfin <- read_xlsx(
    "Elections/Data/Langetijdreeksen-overheidsfinancien-mev2018_publicatie.xlsx"
)

# Clean the file
names(govtfin)[2] <- "variable"
govtfin <- govtfin[,-1]
govtfin <- govtfin[!is.na(govtfin$variable),] 
govtfin[9,1] <- "GDP, current prices, bln euro"
govtfin[10,1] <- "GDP volume change"
govtfin[11,1] <- "GDP price change"
govtfin <- govtfin[-8,]

govtfin <- govtfin %>%
    pivot_longer(2:ncol(govtfin), 
                 names_to = "year", 
                 values_to = "value") %>%
    pivot_wider(names_from = 1, 
                values_from = 3)

govtfin <- clean_names(govtfin)

govtfin <- govtfin %>%
    mutate(year = as.Date(paste(year,"01-01", sep="-")))

# Now, the data is clean

# Merge with government data
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
    filter(Aantreden > as.Date("1860-01-01")) 

#Now, the data is clean. 

# And finally the educational data
Data2 <- read_json(
    "https://opendata.cbs.nl/ODataApi/OData/70222ned/TypedDataSet",
    simplifyVector = TRUE)$value

## Cleaning
Data2 <- clean_names(Data2)

Data2 <- Data2 %>%
    mutate(
        perioden = as.Date(paste(as.numeric(str_extract(
            perioden,
            "[0-9]+")
            ),
            "-01-01", 
            sep = ""))
        )


# We can start to make graphs

#Figure 1
fig1 <- govtfin %>%
    filter(year >= "1860-01-01", year <= "1939-01-01") %>%
    select(year, gross_government_expenditure, interest, defense) %>%
    mutate(net_government_expenditure = 
               gross_government_expenditure - interest - defense) %>%
    select(year, 
           gross_government_expenditure, 
           net_government_expenditure) %>%
    pivot_longer(2:3, 
                 names_to = "variable", 
                 values_to = "value")
p1 <- ggplot() +
    geom_rect(data = GovernmentsNew,
              aes(xmin = as.Date(Aantreden), 
                  xmax = as.Date(Aftreden),
                  ymin = -Inf, 
                  ymax = Inf,
                  fill = `Politieke kleur`), 
              alpha = 0.3) + 
    scale_fill_manual(values = c("darkblue", "purple", "orange"), 
                      name = "Political color") +
    geom_line(data = fig1, 
              aes(x = year, 
                  y = value, 
                  linetype = variable, 
                  group = variable), 
              size = 0.5) +
    ggtitle("Government Expenditures") +
    scale_linetype_manual(breaks = c("gross_government_expenditure", 
                                     "net_government_expenditure"), 
                          name = "Type", 
                          values = c("solid", "dashed"),
                          labels = c("Gross Expenditures","Net Expenditures")) +
 #   theme_ipsum_rc() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()
    ) + xlab("Year") + ylab("Value")

#Figure 2
fig2 <- govtfin %>%
    filter(year >= "1860-01-01", year <= "1939-01-01") %>%
    select(year, 
           financial_balance_general_government_emu, 
           tax_premium_burden_1814_1939_only_taxes) %>%
    pivot_longer(2:3,
                 names_to = "variable",
                 values_to = "value")

p2 <- ggplot() + geom_rect(data = GovernmentsNew,
              aes(xmin = as.Date(Aantreden), 
                  xmax = as.Date(Aftreden),
                  ymin = -Inf, 
                  ymax = Inf,
                  fill = `Politieke kleur`), 
              alpha = 0.3) + 
    scale_fill_manual(values = c("darkblue", "purple", "orange"), 
                      name = "Political color") +
    geom_line(data = fig2, 
              aes(x = year, 
                  y = value, 
                  linetype = variable, 
                  group = variable), size = 0.5) +
    ggtitle("Government Fiscal Policy") +
    scale_linetype_manual(values = c("solid", "dashed"),
                       "Series (% GDP)", 
                       labels = c("Financial Balance", "Taxes")) +
  #  theme_ipsum_rc() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()
    ) + xlab("Year") + ylab("Value")

fig3 <- govtfin %>%
    filter(year >= "1860-01-01", year <= "1939-01-01") %>%
    select(year, 
           gross_debt_general_government_emu,
           gdp_current_prices_bln_euro) %>%
    mutate(gdp_current_prices_bln_euro = gdp_current_prices_bln_euro * 100) %>%
    pivot_longer(2:3,
                 names_to = "variable",
                 values_to = "value")

p3 <- ggplot() + geom_rect(data = GovernmentsNew,
                     aes(xmin = as.Date(Aantreden), 
                         xmax = as.Date(Aftreden),
                         ymin = -Inf, 
                         ymax = Inf,
                         fill = `Politieke kleur`), 
                     alpha = 0.3) + 
    scale_fill_manual(values = c("darkblue", "purple", "orange"), 
                      name = "Political color") +
    geom_line(data = fig3, aes(x = year, 
                  y = value, 
                  linetype = variable, 
                  group = variable), 
              size = 0.5) +
    ggtitle("Government Debt and GDP") +
    scale_linetype_manual(values = c("solid", "dashed"),
                          "Series", 
                          labels = c("GDP (mln)","Govt Debt (% GDP)")) +
 #   theme_ipsum_rc() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()
    ) + xlab("Year") + ylab("Value")


fig4 <- Data2 %>%
    filter(perioden < as.Date("1945-01-01")) %>%
    select(perioden, 
           totaal_overheidsuitgaven_voor_onderwijs_1,
           per_hoofd_van_de_bevolking_23) %>%
    pivot_longer(2:3,
                 names_to = "variable",
                 values_to = "value"
    ) 

fig4 <- fig4 %>%
    mutate(variable = factor(
        fig4$variable, 
        levels = c("totaal_overheidsuitgaven_voor_onderwijs_1", 
                   "per_hoofd_van_de_bevolking_23")
        )
        )

GovernmentsNew1 <- GovernmentsNew %>%
    filter(Aantreden > "1897-01-01")

p4 <- ggplot() +
    geom_rect(data = GovernmentsNew1,
          aes(xmin = as.Date(Aantreden), 
              xmax = as.Date(Aftreden),
              ymin = -Inf, 
              ymax = Inf,
              fill = `Politieke kleur`), 
          alpha = 0.3) + 
    scale_fill_manual(values = c("darkblue", "orange"), 
                      name = "Political color") +
    geom_line(data = fig4, 
              aes(
                  x = perioden, 
                  y = value,
                  group = variable,
                  linetype = variable),
              size = 0.5) +
    scale_linetype_manual(
        values = c("solid", "dashed"), 
        name = "Variable",
        labels = c("Total", "Per Capita")) +
#    theme_ipsum_rc() + 
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()
    ) + ggtitle("Educational Expenses and Government Orientation")


png("Elections/Figures/FigureX1.png", height = 768, width = 1280)
grid.arrange(p1, p2, p3, p4, nrow =2)
dev.off()

