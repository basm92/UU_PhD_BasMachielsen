# From the district and time, find the electoral variables
## Nearest socialists, socialist dummy, show-up rate, 
## nearest competitor, amount of votes, percentage of votes
## Margin above 2nd candidate

## Derive the electoral information on the basis of these districts
## (I can match the politicians on the basis of district-data uniqueness), 
## and if I have politicians loaded
## Put the corrected vector in districtstest$toelichting! 

find_eleccontrols <- function(datafreem, date, mindist = 0){
  
  ## Read in the  datasets
  politicians3 <- read_excel("./Data/tk_1815tot1950uu.xlsx")
  allcandidates <- read.csv("./Data/functiondata_allcandidates.csv") %>%
    mutate(Date = ymd(Date))
  allelections <- read.csv("./Data/functiondata_allelections.csv") %>%
    mutate(main = ymd(main), side = ymd(side))
  
  # convert the date to real date
  date <- lubridate::ymd(date)
  
  districtfilter <- allelections %>%
    filter(Regio %in% datafreem$toelichting) %>%
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
    select(Regio, RegioUitslag, AantalStemmen, side) %>%
    pivot_wider(names_from = RegioUitslag, values_from = AantalStemmen) %>%
    mutate(Turnout = Opkomst/Kiesgerechtigden)
  
  # Characteristics of politicians
  ## Get the names of the politicians
  generalinfo <- generalinfo %>%
    merge(datafreem %>%
            select(toelichting, polid), ###TODO: THIS DOESNT WORK
          by.x = "Regio",
          by.y = "toelichting") 
   
  generalinfo <- generalinfo %>%
    merge(
      politicians3 %>%
        select(`b1-nummer`, voorletters, prepositie, achternaam),
      by.x = "polid",
      by.y = "b1-nummer")
  
  polchars <- allcandidates %>%
    filter(District %in% datafreem$toelichting) %>% #smallcase district and smallcase date are function arguments
    group_by(District) %>%
    mutate(diff = date - Date) %>%
    filter(diff > 0) %>%
    slice_min(diff) %>%
    mutate(name = str_trim(name))
  
  
  polchars %>%
    group_by(District) %>%
    mutate(socialistdum = ifelse(any(grepl("*.?SDAP*.?", Aanbevolen.door)), "1", "0"),
           socialistpercentage = sum(Percentage[grepl("*.?SDAP*.?", Aanbevolen.door)])) %>%
    group_by(District) %>%
    merge(generalinfo, #%>%
            #select(Regio, `b1-nummer`, voorletters, prepositie, achternaam), 
          by.x = "District", 
          by.y = "Regio") %>%
    as_tibble() %>%
    unite("truename", voorletters, prepositie, achternaam, 
          na.rm = TRUE, sep = " ") %>%
    mutate(dist = stringdist::stringsim(name, truename, method = "jaccard")) %>%
    group_by(truename) %>%
    #mutate(elected = ifelse(dist == max(dist), "1", "0")) %>%
    mutate(elected = ifelse(dist == max(dist), "1", "0")) %>%
    ungroup() %>%
    group_by(District) %>%
    mutate(nearestcompetitormargin = ifelse(Percentage - max(Percentage[elected != "1"]) == Inf,
                                            0, 
                                            Percentage - max(Percentage[elected != "1"]))
           ) %>%
    filter(elected == "1") %>%
    filter(dist > mindist) %>%
    select(-elected, -dist) %>%
    relocate(polid, truename, District, Date) %>%
    distinct()
    
    #socialist margin, socialist dummy, nearest competitor margin, amount of votes, total margin, 
  
}
