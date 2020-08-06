# ziektwet 1913
# progressive position is in favor

ziektewet1913 <- data.frame(politician = c("
                            van Sasse van Ysselt"," Fleskens"," Schimmelponninck"," 
                            Fruytier"," J.W.J.C.M. van Nispen tot Sevenaer"," 
                            Ruys de Beerenbrouck"," van Lennep"," Troelstra"," 
                            van Bylandt"," Snoeck Henkemans"," Middelberg"," 
                            Helsdingen"," Loeff"," de Vlugt"," 
                            van der Borch van Verwolde"," Passtoors"," 
                            Schaper"," Duijs"," Pollema"," Hugenholtz"," ter Laan"," 
                            Kooien"," van der Molen"," do Visser"," Oosterbaan"," 
                            van der Voort van Zijp", "Elhorst"," Duynstee"," 
                            de Monté ver Loren"," van Hoogstraten","
                            Nolens"," van Vliet"," van Veen"," van Asch van Wijck"," 
                            van Wijnbergen"," de Ram"," Heemskerk"," 
                            Duymaer van Twist"," van den Berch van Heemstede"," 
                            Aalberse"," van Lynden van Sandenburg"," Ankerman"," 
                            Rutgers"," van Vuuren"," Brummelkamp"," de Geer"," 
                            van Best"," Vliegen"," Janssen"," Beckers"," Scheurer"," 
                            Arts"," van Vlijmen"," van Dedem"," Blum"," 
                            van de Velde"," Bogaardt"," O.F.A.M. van Nispen van Sevenaer")) %>%
    mutate(politician = str_squish(politician), vote = rep(1, length(politician)))

politician <- c("
                Eland"," Bichon van IJsselmonde"," Vorsterman van Oyen"," 
                van Hamel"," Dolk"," Teenstra"," de Meester"," 
                Roodhuyzen"," de Beaufort"," Jansen"," 
                de Klerk"," Jannink"," Patijn"," Bos"," Drucker"," 
                de Kanter"," Limburg"," Smidt"," Marchant"," 
                Thomson"," Tydeman"," de Savornin Lohman") %>%
    str_squish()

vote <- rep(0, length(politician))

ziektewet1913 <- rbind(ziektewet1913, cbind(politician, vote)) %>%
    mutate(law = "Ziektewet 1913", date = "1913-04-25")
