# Amendement Rutgers

## Voor = tegen presentiegeld
## But coded with 0, because conventions (left-wing is in favor)

amendementrutgers <- data.frame(politician = c(
    "Kleerekoper","  de Meester","  Helsdingen","  de Jong","  van de Velde","  
    Kolkman","  van Leeuwen","  Kooien","  van Wijnbergen","   
    Schaper","  Jannink","  van der Voort van Zijp","   Ruys de Beerenbrouck","   
    Nolens","   Marchant","   van Veen","   Knobel","  Albarda","   
    Heeres","  Patijn","   Loeff","   Ankerman","   
    Hugenholtz","    Brummelkamp","    Koster","    
    Rink","    Fock","    Bongaerts","  de  Geer","  
    van  Beresteyn","  Duynstee","  van Raalte","  
    Roodenburg","  van Rijckevorsel","  Beumer","  
    Visser van IJzendoorn","   de Visser","  Scheurer","  Sannes","  Roodhuyzen","  
    Drion","   Boissevain","   Teenstra","  Duymaer van Twist","  
    Smeenge"," van Hamel")) %>%
    mutate(politician = stringr::str_trim(politician))

amendementrutgers <- amendementrutgers %>%
    mutate(vote = rep(0, length(amendementrutgers$politician)))

politician <- c(
    "Juten","  Otto","  Eerdmans","  Gerhard","   Eland","   
    van Doorn","   Schimmelpenninck","   Limburg","   
    K. ter Laan","  Spiekman","   Fruytier","   
    de Muralt","   de Savornin Lohman"," 
    Rutgers","  van Vliet","  de Monté verLoren","  Engels","  
    Duys","  Snoeck Henkemans","  van Nispen tot Sevenaer","  
    J. ter Laan","  van Groenendael","  van  Idsinga", 
    "Oosterbaan","   van Best","   Bogaardt","    
    Mendels","   Fleskens","   van den Tempel","  Arts"," 
    ter Spill","   Ketelaar ") %>%
    stringr::str_trim()

vote <- rep(1, length(politician))

amendementrutgers <- rbind(amendementrutgers, cbind(politician, vote)) %>%
    mutate(law = "Amendement Rutgers: Presentiegeld", date = "1916-11-17")
