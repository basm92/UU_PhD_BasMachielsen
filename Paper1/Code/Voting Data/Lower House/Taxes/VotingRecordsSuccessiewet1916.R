### Wijziging Successiewet 1916
successiewet1916 <- data.frame(politician = c("Limburg"," Eerdmans"," Fock"," 
                                               Bichon van IJsselmonde"," Ankerman"," 
                                               Marchant"," Spiekman"," van Kaalte"," de Beaufort"," 
                                               Teenstra"," van Leeuwen"," Heeres"," Boissevain"," 
                                               de Meester"," van Hamel"," Lieftinck"," Schaper"," 
                                               van den Tempel"," Sannes"," de Muralt"," 
                                               Drion"," Kleerekoper"," Duijs"," de Jong"," 
                                               van Doorn"," Jansen"," Jannink"," Hubrecht"," 
                                               Roodenburg"," ter Spill"," Smeenge"," Rink"," 
                                               van Foreest"," Rutgers"," J. ter Laan"," 
                                               de Geer"," van Beresteyn"," Koster"," Schim van der Loeff"," 
                                               Nierstrasz"," Knobel"," Patijn"," Snoeck Henkemans"," 
                                               Eland"," Tydeman"," Albarda"," 
                                               K. ter Laan"," Goeman Borgesius")
                               ) %>%
    mutate(politician = stringr::str_trim(politician))

successiewet1916 <- successiewet1916 %>%
    mutate(vote = rep(1, length(successiewet1916$politician)))

#Tegen
politician <- c(
    "Gerretson"," van Best"," Ruys de Beerenbrouck"," 
    Bogaardt"," Arts"," de Monté ver Loren"," van der Voort van Zijp"," 
    van Wijnbergen"," Janssen"," van Sasse van Ysselt"," 
    de Visser"," Scheurer"," van Vliet"," Fruytier"," Juten"," 
    van Nispen tot Sevenaer"," Brummelkamp"," Bongaerts"," 
    van Vuuren"," Kooien"," Schimmelpenninck"," de Savornin Lohman"," 
    Kolkman"," Duymaer van Twist"," van Veen"," Fleskens"," 
    de Wijkerslooth de Weerdesteyn"," van Wichen"," van de Velde"
) %>% stringr::str_trim()

vote <- rep(0, length(politician))

successiewet1916 <- rbind(successiewet1916, cbind(politician, vote)) %>%
    mutate(law = "Successiewet 1916", date = "1916-07-18")
