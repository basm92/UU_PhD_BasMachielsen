#find_demographics
#From the polid, find the demographic control variables

## Tenure, age of death, age of entrance, electoral horizon (next election and until pension), party affiliation

### This is an auxiliary Function: Load elections, and filter to the following information: 
### number, district, days to next election in that district
### To be used within find_demographcis
source("./Code/aux_which_elections.R")


## Then, get the demographic variables
find_demographics <- function(distrpoliddate) {
  
  politicians2 <- read_excel("./Data/tk_1815tot1950uu.xlsx", sheet = 1)
  career2 <- read_excel("./Data/tk_1815tot1950uu.xlsx", sheet = 2)
  
  career2 <- career2 %>%
    filter(rubriek == "3010" |rubriek == "3020") %>%
    dplyr::select(c(1:2, 4)) %>%
    dplyr::group_split(rubriek)
  
  together <- merge(politicians2, career2[[1]]) %>%
    merge(career2[[2]], by = "b1-nummer")
  
  colnames(together)[c(1,12,14)] <- c("b1_nummer", "dateofbirth", "dateofdeath")
  
  together <- together %>%
    janitor::clean_names() %>%
    as_tibble() %>%
    mutate(across(contains("periode"), ymd), across(contains("date"), dmy))
  
  date <- ymd(distrpoliddate$date)[1]
  
  data <- together %>%
    filter(b1_nummer %in% distrpoliddate$b1_nummer) %>%
    mutate(tenure = date - begin_periode,
           age_of_death = dateofdeath-dateofbirth,
           age_of_entrance = begin_periode - dateofbirth,
           age_of_vote = date - dateofbirth,
           long_elec_horiz = einde_periode - date
    )
  
  #Merge the dataset with the election with the data set here ^^
  temp <- which_elections(distrpoliddate)
  data <- merge(data, temp, 
                by.x = "b1_nummer", 
                by.y = "b1_nummer")
  
  ## A conversion table for party affiliation (so i can create that variable)
  temp <- read.csv("./Data/key_politicalparty_category.csv") %>%
    select(-1) %>%
    rename(polparty=class)
  
  ## Merge this variable in the results, and finalize the output
  merge(data, temp,
        by.x = "partij_en_fractie_s",
        by.y = "partys") %>%
    distinct() %>%
    as_tibble()
}
