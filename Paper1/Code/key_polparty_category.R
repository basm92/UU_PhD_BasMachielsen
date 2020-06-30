setwd("/home/bas/Documents/git/UU_PhD_BasMachielsen/Paper1")

polparty <- read_excel("./Data/tk_1815tot1950uu.xlsx", sheet = 1) %>%
  janitor::clean_names() %>%
  mutate(across(contains("periode"), ymd)) %>%
  filter(begin_periode > "1860-01-01", einde_periode < "1950-01-01") %>%
  select(b1_nummer, partij_en_fractie_s)

partys <- unique(polparty$partij_en_fractie_s)

partys <- data.frame(partys)  

partys <- partys %>%
  mutate(class = ifelse(
    str_detect(partys, ".*?iber*.?|.*?vrijzin.*?"),
    "liberal",
    ""),
    class = ifelse(
      str_detect(partys,".*?athol.*?"),
      "confessional",
      class
    ),
    class = ifelse(
      str_detect(partys, ".*?onserva.*?"),
      "confessional",
      class
    ),
    class = ifelse(
      str_detect(partys, ".*?ntirevol.*?"),
      "confessional",
      class
      ),
    class = ifelse(
      str_detect(partys, ".*?AR.*?"),
                 "confessional",
                 class
      ),
    class = ifelse(
      str_detect(partys, ".*?CH.*?|.*?c\\.h\\..*?|.*?a\\.r\\..*?"),
      "confessional",
      class
    ),
    class = ifelse(
      str_detect(partys, "Schaepman.*?|Putt.*?|Kappey.*?|Bahlman.*?|KVP"),
      "confessional",
      class
    ),
    class = ifelse(
      str_detect(partys, "Thorb.*?|.*?VVD.*?"),
      "liberal",
      class
    ),
    class = ifelse(
      str_detect(partys, ".*?RK.*?"),
      "confessional",
      class
    ),
    class = ifelse(
      str_detect(partys, ".*?VDB.*?|.*?SDAP.*?|.*?SDB.*?|.*?Sociali.*?|Vrije Socialist|Radicale Bond"),
      "socialist",
      class
    ),
    class = ifelse(
      str_detect(partys, ".*?eutr.*?|.*?lattelan.*?|.*?partijloo.*?|MP S&L|NBTMP|VNH"),
      "neutral",
      class
    ),
    class = ifelse(
      str_detect(partys, "CSP|CPH|.*?CPN.*?|CDU|.*?PvdA.*?|RSP;RSAP|SP \\(voor 1940\\)"),
      "socialist",
      class
    ),
    class = ifelse(
      str_detect(partys, "SGP|HGSP"),
      "confessional",
      class
    ),
    class = ifelse(
      str_detect(partys, "NSB"),
      "national-socialist",
      class
    ),
    class = ifelse(
      str_detect(partys, "Volkspartij|Vrijheidsbond.*?"),
      "liberal",
      class
    )
  )

write.csv(partys, "./Data/key_politicalparty_category.csv")              
