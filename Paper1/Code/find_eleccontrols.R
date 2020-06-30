# From the district and time, find the electoral variables
## Nearest socialists, socialist dummy, show-up rate, 
## nearest competitor, amount of votes, percentage of votes
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


## Read in the political party and candidates dataset
politicians <- read_excel("./Data/tk_1815tot1950uu.xlsx")

## Read in the electoral results with parties and names dataset
files <- dir("./Data")
allcandidates <- files[grepl("allcandidates", files)]

b <- NULL

for(i in allcandidates) {
  a <- read.csv(paste("./Data/", i, sep = ""))
  
  b <- rbind(b, a)
}

allcandidates <- b %>%
  select(-1) %>%
  na.omit() %>%
  mutate(name = as.character(V5)) %>%
  mutate(name = gsub("'", "", name)) %>%
  select(-V5) %>%
  as_tibble() %>%
  separate("District", into = c("District", "Date"), sep = "\\(") %>%
  mutate(Date = str_replace(Date, "\\)", ""), 
         Aanbevolen.door = as.character(Aanbevolen.door),
         Percentage = as.numeric(str_replace(Percentage, "%", "")),
         Aantal.stemmen = as.numeric(Aantal.stemmen),
         Date = dmy(Date))

### Find the districts for which we want to find electoral information
### starting from the numbers of the politicians, and the dates
find_district(politicians$`b1-nummer`, "1911-01-14") -> districtstest

district <- districtstest$toelichting
date <- ymd("1911-01-14")

## Derive the electoral information on the basis of these districts
## (I can match the politicians on the basis of district-data uniqueness), 
## and if I have politicians loaded
find_eleccontrols <- function(district, date){
  
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
    mutate(Turnout = Opkomst/Kiesgerechtigden) %>%
    cbind(date = date)
  
  # Characteristics of politicians
  ## Get the names of the politicians
  generalinfo <- generalinfo %>%
    merge(districtstest, 
          by.x = "Regio",
          by.y = "toelichting") 
   
  generalinfo <- generalinfo %>%
    merge(
      politicians %>%
        select(`b1-nummer`, achternaam),
      by.x = "b1-nummer",
      by.y = "b1-nummer")
  
  polchars <- allcandidates %>%
    filter(District %in% district) %>% #smallcase district and smallcase date are function arguments
    group_by(District) %>%
    mutate(diff = date - Date) %>%
    filter(diff > 0) %>%
    slice_min(diff)  ## TODO: Extract the necessary variables from this and merge with generalinfo
  #socialist margin, socialist dummy, nearest competitor margin, amount of votes, total margin, 
  
  ## String-match them to the candidates per district
  ## Find amount of votes and margin
  
}
