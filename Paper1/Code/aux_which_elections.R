which_elections <- function(distrpoliddate) {
  
  #Read in all elections
  elections <- read.csv("./Data/allelections.csv") %>%
    janitor::clean_names() %>%
    as_tibble() %>%
    mutate(across(c(dag,maand,jaar), as.numeric)) %>%
    unite("date", 2:4, sep = "-") %>%
    mutate(date = dmy(date))
  
  ### Filter it for the right elections (closest to the date of interest, and in the future)
  elections <- elections %>%
    filter(district %in% distrpoliddate$toelichting) %>%
    mutate(diff = date - distrpoliddate$date) %>%
    group_by(district) %>%
    filter(diff > 0) %>%
    slice(which.min(abs(diff))) ## TODO: merge this dataset with the b1_nummer (otherwise unmergeable)
  
  elections <- merge(elections, distrpoliddate, 
                     by.x = "district", 
                     by.y = "toelichting") %>%
    select(`b1-nummer`, district, diff) %>%
    rename(days_to_next_el = diff)
  
  elections 
  
}