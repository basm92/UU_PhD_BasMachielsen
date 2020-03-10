# This file contains the code to investigate government finances
# First, load packages
library(readxl)
library(rvest)
library(jsonlite)
library(tidyverse)
library(hrbrthemes)
library(janitor)
library(gridExtra)

# Second, download the excel file from:

url <- "https://www.cpb.nl/sites/default/files/omnidownload/Langetijdreeksen-overheidsfinancien-mev2018_publicatie.xlsx"
download.file(destfile = "Langetijdreeksen-overheidsfinancien-mev2018_publicatie.xlsx", 
              url= url)

# Third, read file:
govtfin <- read_xlsx(
    "Langetijdreeksen-overheidsfinancien-mev2018_publicatie.xlsx"
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
# We can start to make graphs

fig1 <- govtfin %>%
    filter(year >= "1860-01-01", year <= "1939-01-01") %>%
    select(year, gross_government_expenditure, interest, defense) %>%
    mutate(net_government_expenditure = 
               gross_government_expenditure - interest - defense)
    
p1 <- fig1 %>%
    select(year, 
           gross_government_expenditure, 
           net_government_expenditure) %>%
    pivot_longer(2:3, 
                 names_to = "variable", 
                 values_to = "value") %>%
    ggplot(aes(x=year, y = value, linetype = variable, group = variable)) + 
    geom_line(size = 1) +
    ggtitle("Government Expenditures") +
    scale_linetype_manual(breaks = c("gross_government_expenditure", 
                                     "net_government_expenditure"), 
                          name = "Type", 
                          values = c("solid", "dashed"),
                          labels = c("Gross Expenditures","Net Expenditures")) +
    theme_ipsum_rc() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()
    ) 

fig2 <- govtfin %>%
    filter(year >= "1860-01-01", year <= "1939-01-01") %>%
    select(year, 
           financial_balance_general_government_emu, 
           tax_premium_burden_1814_1939_only_taxes)

p2 <- fig2 %>%
    pivot_longer(2:3,
                 names_to = "variable",
                 values_to = "value") %>%
    ggplot(aes(x = year, y = value, color = variable, group = variable)) +
    geom_line(size = 1) + 
    ggtitle("Government Fiscal Policy") +
    scale_color_manual(values = c("black", "grey"),
                       "Series (% GDP)", 
                       labels = c("Financial Balance", "Taxes")) +
    theme_ipsum_rc() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()
    ) 

fig3 <- govtfin %>%
    filter(year >= "1860-01-01", year <= "1939-01-01") %>%
    select(year, 
           gross_debt_general_government_emu,
           gdp_current_prices_bln_euro) %>%
    mutate(gdp_current_prices_bln_euro = gdp_current_prices_bln_euro * 100)

p3 <- fig3 %>%
    pivot_longer(2:3,
                 names_to = "variable",
                 values_to = "value")  %>%
    ggplot(aes(x = year, 
               y = value, 
               linetype = variable, 
               group = variable)) + 
    geom_line(size = 1) +
    ggtitle("Government Debt and GDP") +
    scale_linetype_manual(values = c("solid", "dashed"),
                          "Series", 
                          labels = c("GDP (mln)","Govt Debt (% GDP)")) +
    theme_ipsum_rc() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()
    ) 

png("FigureX1.png", height = 1024, width = 768)
grid.arrange(p1, p2, p3, nrow =3)
dev.off()

