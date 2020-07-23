#Kieswet 1872

#VOOR kiesrechtuitbreiding

kieswet1872 <- data.frame(politician = c("Dullert","Moens","de Bruyn Kops","van Kerkwijk","Mees"," de Jong","Oldenhuis Gratatna","Bredius"," Rombach"," Brouwer","Lenting","Dam",
                                         "Idzerda"," Gevers Deynoot","Mirandolle",
                                         "van Akerlaken","de Ruiter Zylker",
                                         "Schepel","Wybenga","Storm van 'sGravesande",
                                         "Godefroi"," Cremers","van Houten","Blusse",
                                         "Rutgers van Rozenburg","de Lange",
                                         "Teding van Berkhout"," Smidt"," Mackay",
                                         "Kappeyne van de Coppello"," Stieltjes","Tak van Poortvliet")) %>%
    mutate(politician = stringr::str_trim(politician))

kieswet1872 <- cbind(kieswet1872, vote = rep(1, length(kieswet1872$politician)))
#TEGEN kiesrechtuitbreiding:

politician = c("van Zinnicq Bergmann"," Schimmelpenninck van Nijenhuis"," de Bieberstein "," van Loon",
                " Heydenrijck"," Schimmelpenninck van der Oye "," Insinger"," Sandberg"," 
               Kerens de Wyire"," van Kuyk"," van den Berch van Heemstede"," 
               van den Heuvel"," Haffmans"," Hingst"," Bergsma"," 
               Bichon van IJsselmonde"," 
               van Baar"," Luyben"," C. van Nispen tot Sevenaer"," 
               van Harinxma thoe Slooten"," Lambrechts"," 
               van Naamen van Eemnes"," van Zuijlen van Nyevelt"," 
               Wintgens"," Jonckbloet"," Begram"," van Eek"," 
               de Roo van Alderwerelt"," Nierstrasz"," Kuiper"," 
               Kien"," Smitz "," Arnoldts"," Saaymans Vader"," Fabius"," 
               van Reenen "," Verheyen"," 's Jacob", "Viruly Verbrugge") %>%
    stringr::str_trim()

vote <- rep(0, length(politician))

## Combine
kieswet1872 <- rbind(kieswet1872, 
                     cbind(politician, 
                           vote))

kieswet1872 <- kieswet1872 %>%
    mutate(law = "Kieswet 1872", date = "1874-06-19")
