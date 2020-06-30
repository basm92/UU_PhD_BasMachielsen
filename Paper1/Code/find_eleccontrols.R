# From the district and time, find the electoral variables
## Nearest socialists, socialist dummy, show-up rate, 
## nearest competitor, amount of votes, percentage of votes, showup rate
## Margin above 2nd candidate


### Read in an elections dataset

files <- dir("./Data")
allelections <- files[grepl("Uitslag_TK", files)]

b <- NULL
for(i in allelections){
  a <- read.csv(paste("./Data/",i, sep = ""), sep = ";")
  
  title <- readr::parse_factor(i)
  
  a <- cbind(a, title)
  b <- rbind(b,a)
}

### Clean the dataset
allelections <- b %>%
  separate(title, into = c("a", "main", "side"), sep = "_") %>%
  select(-a) %>%
  mutate(main = str_remove(main, "TK"), 
         side = str_remove(side, "\\.csv"), 
         side = str_remove(side, "\\(1\\)")) %>%
  separate(main, sep = c(4,6), into = c("year","month", "day")) %>%
  unite("main", c("year","month", "day"), sep = "-") %>%
  mutate(main = ymd(main), side = dmy(side))


### Find the districts for which we want to find electoral information

find_district(politicians$`b1-nummer`, "1911-01-14") -> districtstest

district <- districtstest$toelichting
date <- ymd("1911-01-14")

## Derive the electoral information on the basis of these districts
find_eleccontrols <- function(polid, district, date){
  
  
  districtfilter <- allelections %>%
    filter(Regio %in% district) %>%
    mutate(Kandidaat = as.character(Kandidaat)) %>%
    group_by(Regio, side) %>%
    filter(any(Kandidaat != ""))
  
  #Find the date of the most recent election since the date we've entered per district
  #And filter the dataframe to exactly those elections
  elecinfo <- districtfilter %>%
    group_by(Regio) %>%
    mutate(diff = date - side) %>%
    filter(diff > 0) %>% #Because it has to be an election in the past
    slice_min(diff)
  
  # General election characteristics
  generalinfo <- elecinfo %>%
    filter(RegioUitslag == "Kiesgerechtigden" | RegioUitslag == "Opkomst") %>%
    select(Regio, RegioUitslag, AantalStemmen) %>%
    pivot_wider(names_from = RegioUitslag, values_from = AantalStemmen) %>%
    mutate(Turnout = Opkomst/Kiesgerechtigden)
    
  
  # Characteristics of politicians
  ## Get the names of the politicians
  
  ## String-match them to the candidates per district
  ## Find amount of votes and margin
}
