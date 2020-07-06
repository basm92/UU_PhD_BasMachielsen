# Make the dataset for the function find_strikes
#From the district and time, find the strikes
## Read in the strikes (municipality level) and clean it
strikes <- read.csv("./Data/howmuch.csv") %>%
  filter(JAAR > 1870, JAAR < 1919, GEMEENTE != "", GEMEENTE != "nvt") %>%
  group_by(JAAR, GEMEENTE, PROVINCIE) %>%
  summarise(howmuch = sum(howmuch))

## Read in the municipality to district
municipalities_to_districts <- read.csv("./Data/Municipalities_and_districts.csv") %>%
  select(-1)

names(municipalities_to_districts) %>%
  str_replace("X", "") -> names(municipalities_to_districts)

municipalities_to_districts <- municipalities_to_districts %>%
  pivot_longer(2:5, "year") %>%
  filter(value == 1) %>%
  select(-value) %>%
  mutate(year = as.numeric(year))

## Match the appropriate district with each municipality-year in strikes

match_district <- function(strikesdf) {
  
  ## where to look?
  strikesdf$temp <- ifelse(strikesdf$JAAR < 1848, 
                           1848, 
                           ifelse(
                             between(strikesdf$JAAR, 1848, 1850),
                             1848,
                             ifelse(between(strikesdf$JAAR, 1850, 1888),
                                    1850,
                                    ifelse(between(strikesdf$JAAR, 1888, 1897),
                                           1888, 
                                           1897)
                             )
                           )
  )
  
  df <- left_join(strikesdf, municipalities_to_districts, 
                  by = c("GEMEENTE" = "gemeente", "temp" = "year"))
  df
  
}

## Manually edit most important NA's:
## Amsterdam Rotterdam, Utrecht, Den Haag, Tilburg, Almelo, Deventer
match_district(strikes) -> strikespd

strikespd <- strikespd %>%
  ungroup() %>%
  mutate(GEMEENTE = as.character(GEMEENTE), district = as.character(district))

changes <- c("Amsterdam", "Utrecht", "Rotterdam", "Den Haag", "Almelo", "Deventer")

strikespd[is.na(strikespd$district) & strikespd$GEMEENTE %in% changes,]$district <- strikespd[is.na(strikespd$district) & strikespd$GEMEENTE %in% changes,]$GEMEENTE

write.csv(strikespd, "./Data/strikesperdistrict.csv")
