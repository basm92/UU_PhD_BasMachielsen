successiewet1921 <- data.frame(politician = c(
  "de Groot"," Fleskens"," L.M. Hermans"," Schokking"," Treub"," 
  Ossendorp"," Bomans"," Reijmer","
  Kleerekoper"," van Zadelhoff"," Haazevoet"," Deckers"," Ter Hall","
  H. Hermans"," Nolens"," Bongaerts"," A.P. Staalman"," Duys","
  Zijlstra"," Kolkman"," Stulemeijer"," de Wilde"," van Stapele",
  " de Muralt"," van Schaik"," van Rijckevorsel"," Duymaer van Twist","
  Oud"," Rutgers"," Scheurer"," van de Bilt"," Rugge"," Wintermans","
  Poels"," van der Molen"," Smeenk"," de Jonge"," van Vuuren","
  G.J. ter Laan"," Schaper"," Troelstra"," Groeneweg"," 
  van den Tempel"," K. ter Laan"," Ketelaar"," Teenstra","
  Engels"," Bulten"," van Wijnbergen"," Bakker"," van Beresteyn","
  Marchant"," Hugenholtz"," Schouten"," van Dijk", "Albarda"
)) %>%
  mutate(politician = stringr::str_trim(politician),
         vote = rep(1, length(politician)))

politician <- c(
  "Drion"," Gerretson"," Rink","
  Dresselhuys"," A. Staalman"," de Monté ver Loren"," de Kanter","
  Heemskerk"," van Rappard"," Fruytier"," de Wijkerslooth de Weerdesteyn"," van Sasse van Ysselt"," Swane"," Braat"," Beumer","
Weitkamp"," Snoeck Henkemans"," Koelen"
) %>%
  stringr::str_trim()

vote <- rep(0, length(politician))

successiewet1921 <- rbind(successiewet1921, cbind(politician, vote)) %>%
  mutate(law = "Successiewet 1921", date = "1921-04-14")
