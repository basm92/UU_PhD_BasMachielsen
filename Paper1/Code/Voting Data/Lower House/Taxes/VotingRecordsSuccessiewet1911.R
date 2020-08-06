# successiewet 1911

#voor

successiewet1911 <- data.frame(politician = c("Elhorst","  
                                              O.F.A.M. van Nispen tot Sevenaer"," 
                                              ter Laan"," Patijn"," 
de Monte verLoren"," Ketelaar"," van Hamel"," Ferf"," Dolk"," 
Schaper"," Aalberse"," Rink"," Vliegen"," Snoeck Henkemans"," 
Roodkuyzen"," Smidt"," de Klerk"," Heemskerk"," de Geer"," van Karnebeek"," 
van der Voort van Zijp"," J.W.J.C.M. van Nispen tot Sevenaer"," 
Limburg"," de Ram"," Ankerman"," Ros"," Smeenge"," Hubrecht"," Thomson"," Lieftinck"," 
Goeman Borgesius"," de Visser"," de Vlugt"," de Kanter"," Kuyper"," Duys"," van Best"," 
Duymaer van Twist"," van Vuuren"," van Wijnbergen"," Drucker"," 
Fleskens"," Jannink"," van Sasse van Ysselt","
Teenstra"," van Foresst"," van Lennep"," de Wijkorslooth de Weerdesteyn"," 
Eland"," Middelberg"," de Jongh", "de Savornin Lohman"," 
Helsdingen"," Kooien"," van Vliet"," vau Veen"," Duynstee","
Roessingh"," Verheij"," de Beaufort"," Loeff"," Nolens"," Treub"," van Bylandt")) %>%
    mutate(politician = stringr::str_trim(politician))

successiewet1911 <- successiewet1911 %>%
    mutate(vote = rep(1, length(successiewet1911$politician)))

politician <- c("van Idsinga", "van Dedem", "van Wassenaer van Catwijck",
                "van Lynden van Sandenburg", "van Hoogstrate")

vote <- rep(0, length(politician))

successiewet1911 <- rbind(successiewet1911, cbind(politician, vote)) %>%
    mutate(law = "Successiewet 1911", date = "1911-03-10")
