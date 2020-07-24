## TEGEN is 1, voor is 0

# Against is the progressive position

hogeronderwijswet1904 <- data.frame(politician = c("Verhey"," ter Laan"," van Gijn"," Pijnacker Hordijk"," 
                                           Röell"," Troelstra"," Drucker"," Schaper","
                                           den Tex"," Hugenholtz"," Melchers"," Nolting"," 
                                           Lieftinck"," de Klerk"," Rink"," Ketelaar"," van der Zwaag"," 
                                           Helsdingen"," Smidt"," van Karnebeek"," Ferf"," 
                                           van Styrum"," Smeenge"," de Visser"," Cremer"," Dolk","
                                           Bos"," Goeman Borgesius"," Tydeman"," Hennequin"," Hubrecht","
                                           Zijlma"," van Kol"," Goekoop"," Roessingh"," Willinge"," vau Foreest","
                                           de Boer"," van der Vlugt"," Mees", "Fock")) %>%
    mutate(politician = stringr::str_squish(politician), vote = rep(1, length(politician)))

politician <- c("
Schokking"," van Vliet","
Nolens"," Brummmelkamp"," Krap"," Travaglino"," Lucasse"," van de Berch van Heemstede"," 
Kolkman"," de Waal Malefijt"," Byleveld"," van Sasse van Ysselt"," 
Duymaer van Twist"," vau ldsinga"," de Ridder"," van Wassenaer van Catwtjck"," 
Michiels van Verduynen "," de Ram"," van der Kun"," de Vries"," Aalberse"," van Limburg Stirum"," 
Sluis"," de Stuers"," Staalman"," Beckers"," van den Heuvel","
Mutsaers"," Janssen"," Raymakers"," van der Borch van Verwolde"," 
Friesen"," Merckelbach"," van Asch van Wyek"," van Löben Sels","
van Nispen tot Sevenaer"," van Wichen"," Talma"," van Dedem"," Pompe van Meerdervoort"," 
de Savornin Lohman"," van Veen"," van Vlijmen"," Arts"," Okma"," Bolsius"," Brants"," 
Heemskerk"," van Bylandt"," van de Velde"," van Heemstra"," van Alphen"," Fruytier","
Passtoors"," Seret"," Mackay") %>%
    str_squish()

vote <- rep(0, length(politician))

hogeronderwijswet1904 <- rbind(hogeronderwijswet1904, cbind(politician, vote)) %>%
    mutate(law = "Hoger Onderwijswet 1904", date = "1904-03-24")
