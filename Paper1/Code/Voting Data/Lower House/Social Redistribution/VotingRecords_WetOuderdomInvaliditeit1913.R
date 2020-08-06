## Against the law was the progressive position

wetouderdominvaliditeit1913 <- data.frame(politician = c("
     Jansen"," van Hamel"," Drucker"," Roodhuyzen"," Helsdingen"," van Foreest"," 
     Verhey"," Vorsterman van Oijen"," de Beaufort"," Eland"," 
     Schaper"," Duijs"," Troelstra"," Teenstra"," Jannink"," ter Laan"," 
     Treub"," de Meester"," Limburg"," Smeenge"," Smidt"," van Karnebeek"," 
     Dolk"," Rink"," de Jongh"," Vliegen"," Lieftinck"," 
     Marchant"," de Kanter"," de Klerk"," Bos"," Patijn"," 
     Goeman Borgesius"," Hugenholtz"," Tydema ")) %>%
    mutate(politician = stringr::str_squish(politician), vote = rep(1, length(politician)))

politician <- c("Arts"," Bolsius"," Ruys de Beerenbrouck"," van Best"," 
            Scheurer"," Duynstee"," van Lennep"," Passtoors"," 
            Duymaer van Twist"," van Vuuren"," Snoeck Henkemans"," 
            van Vlijmen"," Rutgers"," van Bylandt"," de Geer"," Oosterbaan"," 
            Elhorst"," van de Velde"," Kooien"," van der Molen"," van Vliet"," 
            de Wijkerslooth de Weerdesteyn"," de Ram"," 
            de Visser"," Aalberse"," Nolens"," de Savornin Lohman"," 
            Brummelkamp"," van Dedem"," Heemskerk"," de Monté verLoren","
            van Hoogstraten"," van Veen"," van Wassenaer van Catwijck"," 
            van Asch van Wijck"," van Lynden van Sandenburg"," 
            van der Borch van Verwolde"," 
            J.W.J.C.M. van Nispen tot Sevenaer"," Bogaardt"," Fleskens"," van den Berch van Heemstede"," 
            de Vlugt"," Pollema"," van Sasse van Ysselt"," Schimmelpenninck"," 
            van der Voort van Zijp"," Ankerman"," Middelberg"," 
            Janssen"," Loeff"," Beckers"," van Wichen",
                " van Wijnbergen"," O.F.A.M. van Nispen tot Sevenaer") %>%
    stringr::str_squish()

vote <- rep(0, length(politician))

wetouderdominvaliditeit1913 <- rbind(wetouderdominvaliditeit1913, cbind(politician, vote)) %>%
    mutate(law = "Wet Ouderdom Invaliditeit 1913", date = "1913-03-07")
