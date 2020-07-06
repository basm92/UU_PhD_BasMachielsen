#find_strikes

## Aggregate per district-year
find_strikes <- function(districts = NULL, year) {
  strikespd <- read.csv("./Data/strikesperdistrict.csv") %>%
    select(-1) %>%
    na.omit()
  
  if(!is.null(districts)){
    strikespd %>%
      group_by(district, JAAR) %>%
      summarise(amount = sum(howmuch)) %>%
      filter(district %in% districts, JAAR == year)
  }
  else {
    strikespd %>%
      group_by(district, JAAR) %>%
      summarise(amount = sum(howmuch)) %>%
      filter(JAAR == year)
  }
  
}