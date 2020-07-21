## Auxiliary code to write the datasets used in find_econcontrols.R

library(hdng)

#First, find the appropriate variables

#Welvaart
##agpb = share of municipality in total taxes
##prbd = share of hoogstaangeslagenen as a percentage of beroepsbevolking
econdata <- hdng::hdng_get_data(c("agpb", "prbd"), col.names = T)

econdata$naam <- plyr::mapvalues(econdata$naam, 
                                 c("'S HERTOGENBOSCH", "'S GRAVENHAGE"), 
                                 c("'S-HERTOGENBOSCH", "'S-GRAVENHAGE")
)
##Still need to find out the scale of these variables, but other than that, 
##they're ready for use
write.csv(econdata, "./Data/econ_controls_1.csv", row.names = F)

#Beroepen
## Extract all variables
hdng::hdng_names_cats("Beroepen", from = 1889, to = 1930) %>%
    filter(`1889` == 1, `1899` == 1, `1930` == 1) -> vars

vars <- lapply(vars[4], as.character)[[1]]

econdata2 <- hdng_get_data(vars, col.names = T) %>%
    filter(year < 1931)


detach("package:hdng", unload = TRUE)

## Now add the different measurements into variables of several categories

### Services
### Agriculture and Fishery
### Industry

econdata2 <- econdata2 %>%
    group_by(naam, year, cbsnr, acode) %>%
    summarise(industry = rowSums(across(matches(c("Aardewerk.+Pos", 
                                                  "Bouwbedrijven.+Pos", 
                                                  "Chemisch.+Pos",
                                                  "Diamant.+Pos",
                                                  "Hout.+Pos",
                                                  "Textiel.+Pos",
                                                  "Kunstnijverheid.+Pos",
                                                  "Leder.+Pos",
                                                  "Metaalbewerking.+Pos"
    ))), 
    na.rm = T),
    services = rowSums(across(matches(c("Drukkersbedrijven.+Pos",
                                        "Huiselijke diensten",
                                        "Godsdienst",
                                        "Kleding.+Pos",
                                        "Onderwijs",
                                        "Verkeer.+Pos",
                                        "Voedings-.+Pos",
                                        "Verzekering.+Pos",
                                        "Handel.+Pos",
                                        "Krediet.+Pos",
                                        "Papier.+Pos",
                                        "Vrije Beroepen"
    ))),
    na.rm = T),
    agricul = rowSums(across(matches(c("Visserij.+Pos",
                                       "Landbouw.+Pos"))),
                      na.rm = T),
    total = rowSums(across(c(industry, services, agricul))),
    across(c(industry, services, agricul), ~ . / total, .names = "{col}_share"))

econdata2$naam <- plyr::mapvalues(econdata2$naam, 
                                  c("'S HERTOGENBOSCH", "'S GRAVENHAGE"), 
                                  c("'S-HERTOGENBOSCH", "'S-GRAVENHAGE")
)

write.csv(econdata2, "./Data/econ_controls_2.csv", row.names = F)

## Education
##hdng::hdng_names_cats("Onderwijs", from = 1870, to = 1891)
##hdng_get_data("uilo")
