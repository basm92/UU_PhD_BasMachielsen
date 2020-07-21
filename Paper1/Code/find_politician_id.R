#find_politician_id

find_politician_id <- function(names, date) {
  politicians <- readxl::read_excel("./Data/tk_1815tot1950uu.xlsx", sheet = 1)
  date <- lubridate::ymd(date)
  
  temp <- politicians %>%
    janitor::clean_names() %>%
    mutate(begin_periode = ymd(begin_periode), 
           einde_periode = ymd(einde_periode)) %>%
    filter(date > begin_periode, date < einde_periode)
  
  #remove the Van Der's from the last name..
  names2 <- str_replace(names, "Van De |Van Der |van de |van der |van |Van der |Van ", "")
  
  matches <- stringdist::amatch(names2, temp$achternaam, method = "jw", maxDist = 10)
  
  data.frame(names = names, 
             polnames = temp$achternaam[matches],
             polid = temp$b1_nummer[matches]
  )
}
