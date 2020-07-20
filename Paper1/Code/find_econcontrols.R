#find_econcontrols(district, year)

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

# Now the function using these two datasets

find_econcontrols <- function(district, yearinput, absrel = "both"){
    
    #Read in the data
    econdata <- read.csv("./Data/econ_controls_1.csv")
    econdata2 <- read.csv("./Data/econ_controls_2.csv")
    
    #Calculate shares
    econdata <- econdata %>%
        group_by(year) %>%
        mutate(share_aandeel = Aandeel.der.gemeente.in.s.Rijks.personele.belasting/sum(Aandeel.der.gemeente.in.s.Rijks.personele.belasting))
    
    #Find closest available year in each dataset
    query1 <- econdata %>%
        group_by(naam) %>%
        slice_min(abs(yearinput - year))
    
    query1 <- query1 %>%
        filter(tolower(naam) %in% tolower(district)) %>%
        select(c(2,5,7,8))
    
    colnames(query1) <- c("naam", "year_eccontrls1", "no_highest_tax", "share_aandeel")
    
    
    query2 <- econdata2 %>%
        group_by(naam) %>%
        slice_min(abs(yearinput - year)) %>%
        filter(tolower(naam) %in% tolower(district)) 
    
    colnames(query2)[2] <- "year_eccontrls2"
    
    if(absrel == "abs") {
        
        query2 <- query2 %>%
            select(1:8)
    } else if(absrel == "rel"){
        
        query2 <- query2 %>%
            select(c(1:4, 9:11))
    }
    
    #Merge the two datasets on the basis of district
    merge(query1, query2, by = "naam")
}
