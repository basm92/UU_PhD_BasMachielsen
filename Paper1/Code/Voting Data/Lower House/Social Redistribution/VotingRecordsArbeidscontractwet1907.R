## This is a decisive amendment, the law is subsequently accepted unanimously
## The progressive position is against (the amendement would give more rights to employers)
## (as opposed to employees)

arbeidscontractwet1907 <- data.frame(politician = c("Nolting"," Roessingh"," Limburg"," Thomson"," Reyne","
                           Schaper"," van Deventer"," Smidt"," Hubrecht"," 
                           van Doorn"," Patijn"," Drucker"," ter Laan"," 
                           Marchant"," Goeman Borgesius"," Bos"," Treub"," 
                           van Karnebeek"," de Klerk"," Tydeman"," Janssen"," 
                           IJzerman"," van Gijn"," de Boer"," Lely"," 
                           van Kol"," Plate"," Zijlma"," Jannink"," de Beaufort"," 
                           Troelstra"," Pierson"," Jansen"," van Foreest"," 
                           van der Zwaag"," Blooker"," Smeenge"," Ketelaar"," 
                           Mees"," Verhey"," S. van den Bergh"," Lieftinck"," 
                           van Styrum"," Roell")) %>%
    mutate(politician = stringr::str_squish(politician), vote = rep(1, length(politician)))


politician <- c("van Veen"," de Ram"," 
Nolens' van Heemstra"," van Saste van Ysselt"," 
Loeft'"," Heemskerk"," van Vuuren"," Duymaer van Twist"," 
Kolkman"," van den Berch van Heemstede"," 
Kooien"," van de Velde"," de Waal Malefijt"," Brummelkamp"," 
Bogaardt"," Aalberse"," van Limburg Stirum"," Passtoors"," 
Duynstee"," van Vlijmen"," van den Heuvel"," Beckers"," 
Ruys de Beerenbrouck"," van Wijnbergen"," Bolsius"," 
Regout"," Talma"," Okma"," van Alphen"," de Savornin Lobman"," 
van Nispen tot Sevenaer"," van|Wassenaer van Catwijck"," 
van der Borch van Verwolde"," Fruytier"," van Idsinga"," 
van Dedem"," Brants"," Lucasse"," 
van Bylandt"," van Vliet"," Schokking")  %>%
    str_squish()

vote <- rep(0, length(politician))

arbeidscontractwet1907 <- rbind(arbeidscontractwet1907, cbind(politician, vote)) %>%
    mutate(law = "Arbeidscontractwet 1907", date = "1906-06-21")

