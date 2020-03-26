library(rvest)
library(stringr)
library(tidyverse)

urls <- paste("http://resources.huygens.knaw.nl/verkiezingentweedekamer/databank/zoek_district/samenstelling_per_district/samenstelling_per_district?District_ID=", seq(10,150), sep = "")
  
#Initialization
out <- NULL
for(i in 1:length(urls)) {
  districtname <- read_html(urls[i]) %>%
    html_nodes("#content-text > h1:nth-child(2)") %>%
    html_text()
  interim <- read_html(urls[i]) %>%
    html_nodes(".vertical") %>%
    html_table(fill = TRUE, header = TRUE) %>%
    as.data.frame()
  colnames(interim) <- interim[1,]
  districtdata <- interim[-1,]
  data <- try(data.frame(districtdata, districtname))
  out <- rbind(out, data)
  Sys.sleep(0.5)
}


out <- out[!is.na(out$districtname),]

final <- out %>%
  mutate(districtname = as.character(districtname),
         bevolkingsomvang = as.numeric(str_replace(bevolkingsomvang, ",","")),
         jaar = as.numeric(jaar)) %>%
  separate(RK, into = c("RK_number", "RK_pct"), sep = "\\s") %>%
  separate(Hervormd, into = c("Hervormd_number", "Hervormd_pct"), sep = "\\s") %>%
  separate(Gereformeerd, into = c("Gereformeerd_number", "Gereformeerd_pct"), sep = "\\s") %>%
  separate(Overig, into = c("Overig_number", "Overig_pct"), sep = "\\s")

final <- final %>%
  mutate_at(c(4,6,8,10), str_extract, "[0-9]{1,}\\.[0-9]{1,}") %>%
  mutate_at(c(4,6,8,10), as.numeric) %>%
  mutate_at(c(3,5,7,9), str_replace, ",", "") %>%
  mutate_at(c(3,5,7,9), as.numeric)

write.csv(final, "../Data/Religion_inhabitants_per_district.csv")
