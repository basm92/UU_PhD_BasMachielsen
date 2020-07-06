#find_politician_id

find_politician_id <- function(names, date) {
  politicians <- readxl::read_excel("./Data/tk_1815tot1950uu.xlsx", sheet = 1)
  
  temp <- politicians %>%
    janitor::clean_names() %>%
    mutate(begin_periode = ymd(begin_periode), 
           einde_periode = ymd(einde_periode)) %>%
    filter(date > begin_periode, date < einde_periode)
  
  matches <- stringdist::amatch(names, temp$achternaam, method = "jw", maxDist = 10)
  
  data.frame(names = names, 
             polnames = temp$achternaam[matches],
             polid = temp$b1_nummer[matches]
  )
}