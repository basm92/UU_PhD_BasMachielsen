antistakingswet1903 <- data.frame(politician = c("
                                  Smeenge"," Krap"," van Dedem","
Fock"," van Limburg Stirum"," de Vries"," Hennequin"," van de
Velde"," Goekoop"," de Ram"," Travaglino"," Talma"," van Vliet","
Schokking "," van Asch van Wjjck "," Rink "," Fruytier "," Tydeman ","
Dolk"," Pompe van Meerdervoort"," Brants"," Cremer"," van Veen","
van Wijck"," Willinge"," van Wassenaer van Catwijck"," Seret","
Bnunmelkamp"," Röell"," van Sasse van Ysselt"," van den Berch
van Heemstede"," van Löben Sels"," Friesen"," van Nispen tot Sevenaer","
Kolkman"," van der Vlugt"," Roessingh"," Ükma"," Goeman
Borgesius"," Michiels van Verduynen"," Lucasse"," Duymaer van Twist","
van Idsinga", "Heemskerk "," van Karnebeek "," de Klerk "," 
Lieftinck"," de Visser"," den Tex"," Hubrecht"," deStuers"," Passtoors"," 
de Boer"," Mutsaers"," vau den Heuvel"," Mees"," Merckelbach"," Janssen","
Raymakers"," van der Borch van Verwolde"," Arts"," van Heemstra","
Ferf"," van Vlijmen"," Verhey "," van VVichen"," van der Kun"," Zijlma","
Sluis"," de Waal Malefijt"," van Styrum"," van Alphen"," Bolsius","
de Savornin Lohman "," Nolens"," van Bylandt"," Staalman"," 
Aalberse"," van Gijn "," Bijleveldt"," Mackay")) %>%
    mutate(politician = str_squish(politician), vote = rep(1, length(politician)))

politician <- c("Nolting"," Marchant"," 
Hugenholtz"," Smidt"," van Raalte"," 
van der Zwaag"," Drucker"," Helsdingen "," 
Troelstra "," Schaper"," 
ter Laan "," Melchers "," 
Bos"," Ketelaar") %>%
    stringr::str_squish()

vote <- rep(0, length(politician))

antistakingswet1903 <- rbind(antistakingswet1903, cbind(politician, vote)) %>%
    mutate(law = "Antistakingswet 1893", date = "1903-04-09")
