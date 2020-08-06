#a.k.a. Ouderdomsrenten

staatspensioen1916 <- data.frame(politician = c(
  "Jannink"," van den Tempel",
  "Duys"," Fook"," de Beaufort"," de Jong"," J. ter Laan","
  Schaper"," Albarda"," ter Spill"," Utto"," Nierstrasz"," van Doorn","
  Roodenburg"," Tydeman"," Kleerekoper"," Hubrecht"," Troelstra","
  de Muralt"," Eerdmans"," Smeenge"," Lieftinck"," Hugenkoltz","
  Jansen (den Haag)"," van Foreest"," Knobel"," van Leeuwen","
  Heeres"," Gerkard"," Visser van IJzendoorn"," K. ter Laan"," 
  Patijn"," Helsdingen"," Eland"," Drion"," Limburg"," Schim van der Loeff",
  " Boissevain"," van Hamel"," Koster"," Marchant","
de Meester"," Teenstra"," Roodkuyzen"," Rink"," Ketelaar"," Mendels","
Sannes"," van Raalte"," Spiekman"," Goeman Borgesius"
)) %>%
  mutate(politician = stringr::str_trim(politician), vote = rep(1, length(politician)))

politician <- c(
  "Schimmelpenninck"," de Monté ver Loren"," de Geer"," de Savornin Lokman",
  " van der Molen"," van de Velde"," Duymaer van Twist"," Snoeck Henkemans"," Rutgers"," Üosterbaan"," de Wijkerslootk de Weerdesteyn",
  " Beumer"," Arts"," Bogaardt"," Scheurer"," Duynstee"," Fruytier",
  " Van Veen"," van Wijnbergen"," de Visser"," van Idsinga","Janssen"," Beckers",
  " van Wichen"," Aalberse"," Bichon van Ijsselmonde",
  "van der Voort van Zijp"," Kooien"," Aukerman"," Bongaerts"," Kolkman"," Gerretson",
  "Ruys de Beerenbrouck"," Brummelkamp"
) %>%
  stringr::str_trim()

vote <- rep(0, length(politician))

staatspensioen1916 <- rbind(staatspensioen1916, cbind(politician, vote)) %>%
  mutate(law = "Staatspensioen 1916", date = "1916-05-18")
  