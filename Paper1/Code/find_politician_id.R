#find_politician_id

find_politician_id <- function(names, date) {
  politicians <- readxl::read_excel("./Data/tk_1815tot1950uu.xlsx", sheet = 1)
  date <- lubridate::ymd(date)
  
  temp <- politicians %>%
    janitor::clean_names() %>%
    mutate(begin_periode = ymd(begin_periode), 
           einde_periode = ymd(einde_periode)) %>%
    filter(begin_periode < date, einde_periode > date)
  
  #remove the Van Der's from the last name..
  names2 <- str_replace(names, "Van De |Van Der |van de |van der |van den |van |Van der |Van |de ", "")
  
  #Split up between names without first letter..
  names3 <- names2[!grepl("\\.", names2)]
  matches <- stringdist::amatch(names3, temp$achternaam, method = "jw", maxDist = 10)
  #dataframe with first
  first <- as.data.frame(cbind(
    names = as.character(names[!grepl("\\.", names2)]), 
    polnames = temp$achternaam[matches], 
    polid = temp$b1_nummer[matches])
  )
  
  #And names with first letter
  names4 <- names2[grepl("\\.", names2)]
  matches_point <- stringdist::amatch(names4, 
                                      str_c(temp$voorletters," ",
                                            temp$achternaam), 
                                      method = "jw", 
                                      maxDist = 10)
  #dataframe with second
  second <- as.data.frame(cbind(
    names = as.character(names[grepl("\\.", names2)]), 
    polnames = temp$achternaam[matches_point], 
    polid = temp$b1_nummer[matches_point])
  )
  
  # merge the two dataframes
  rbind(first, second)
}    
