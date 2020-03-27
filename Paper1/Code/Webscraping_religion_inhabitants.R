# There are two scripts in this file
# One: religious composition per district 
# Two: Municipalities per district conversion

#Libraries
library(rvest)
library(XML)
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


# Now, repeat the task for municipalities per districts
urls <- paste("http://resources.huygens.knaw.nl/verkiezingentweedekamer/databank/zoek_district/gemeenten_per_district?District_ID=", seq(10,150), sep = "")

#Initialize
out <- NULL

for (i in 1:length(urls)) {
table <- read_html(urls[i]) %>%
  html_nodes(".vertical") %>%
  html_table(fill = TRUE, ) %>%
  as.data.frame()
districtname <- str_replace(table[1,1], pattern = "Gemeenten die deel uitmaakten van het district ", "")
table <- table <- table[-1,]
colnames(table) <- table[1,]
table <- table[-1,]
data <- data.frame(table, districtname)
out <- rbind(out, data)
Sys.sleep(0.5)
}

#Write final document
final <- out %>%
  select(-c(2:5))

write.csv(final, "../Data/Municipalities_and_districts.csv")


#New version with indicators when a municipality belonged to a certain district
out <- NULL

for (i in 1:length(urls)) {
nodes <- read_html(urls[i]) %>%
  html_nodes(".vertical")

if (length(nodes) == 0) {
  next
}
nodes <- gsub(nodes,pattern = "<td class=\"box-mark\">", replacement = "<td class=\"box-mark\"> 1")

table <- nodes %>%
  readHTMLTable() %>%
  as.data.frame()

colnames(table) <- c("a","b","c","d","e")
table <- table[-1,]

districtname <- read_html(urls[i]) %>%
  html_nodes("#content-text > h1:nth-child(2)") %>%
  html_text()

data <- data.frame(table, districtname)

out <- rbind(out, data)

Sys.sleep(0.5)
}

out %>%
  mutate_all(str_replace, "Â", "") %>%
  mutate_at(c(2,3,4,5), str_trim) %>%
  mutate_at(c(2,3,4,5), as.numeric)
  