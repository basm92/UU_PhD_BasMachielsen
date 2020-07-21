#find_strikes

## Aggregate per district-year
find_strikes <- function(districts = NULL, datum) {
  strikespd <- read.csv("./Data/strikesperdistrict.csv") %>%
    select(-1) %>%
    na.omit()
  
  datum <- str_extract(datum, "\\d{4}")
  
  if(!is.null(districts)){
    strikespd %>%
      group_by(district, JAAR) %>%
      summarise(amount = sum(howmuch)) %>%
      filter(district %in% districts, JAAR == datum) %>%
      rename(amount_strikes = amount)
  }
  else {
    strikespd %>%
      group_by(district, JAAR) %>%
      summarise(amount = sum(howmuch)) %>%
      filter(JAAR == datum)
  }
  
}