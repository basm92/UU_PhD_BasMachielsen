#find_district

#This function works with dplyr 0.8.5, but not with 1.0.0
#From the politician number, find the district


find_district <- function(polid, date){
  
  politicians <- read_excel("./Data/tk_1815tot1950uu.xlsx", sheet = 1)
  career <- read_excel("./Data/tk_1815tot1950uu.xlsx", sheet = 2)
  
  career <- career %>%
    separate(datum, sep = "/", into = c("from", "to")) %>%
    mutate(from = dmy(from), to = dmy(to)) %>%
    filter(grepl("Tweede Kamer", waarde), 
           from > "1860-01-01", to < "1950-01-01") %>%
    mutate(toelichting = str_replace(toelichting, "voor het kiesdistrict ", ""))
  
  career %>%
    filter(`b1-nummer` %in% polid) %>%
    filter(from < ymd(date), to > ymd(date)) %>%
    select(`b1-nummer`, toelichting) %>%
    filter(!is.na(toelichting))
  
}