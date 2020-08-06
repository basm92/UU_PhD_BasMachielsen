#Kinderwet van houten


kinderwet1874 <- data.frame(politician = c(
    "C.  van  Nispen tot Sevenaer","  
    Schimmelpenninck van der  Oye","  
    Sandberg  "," 
    van  Kerkwijk","  Brouwer","  Bredius","  van  Baar","    
    de  Jong","   van den Berch van Heemstede"," 
    Hingst","   van  Loon","  Rombach","  Heydenryck","  
    Bichon  van  IJsselmonde","  van  Houten  "," Lenting"," 
    Oldenhuis  Gratama","  Jonckbloet","    
    van   Lynden   van   Sandenburg","   
    Insinger","   van   Eek","   van  Zuijlen  van Nyevelt","  
    Mirandolle","  Blom  "," Cremers"," Godefroi","   
    Wintgens","  Borret","  Blussé"," Fabius","  van  Reenen","   
    Kerens    de   Wijlre","    Dam","   Haffmans","   Arnoldts","   
    van den Heuvel","  Lambrechts","  
    Teding   van  Berkhout","   van  Foreest","   
    Gevers   Deynoot","
    van   der  Does   de  Willebois","  Kion  ","   
    van   Nispen   van   Sevenaer","   
    Wybenga","   van  Naamen   van   Eemnes","  van  Wasseneer   van   Catwijck","    
    Nierstrasz","   Bergsma","  
    Rutgers   van   Rozenburg","   
    Stieltjes","   Moens","  's Jacob","   
    Kappeyne   van   de  Coppollo","  
    de  Bruyn    Kops","    Mees","   Smitz","   de  Ruiter   Zylker","   Mackay","   
    de  Bieberstein  ","   Smidt"," van   Kuyk  ","  
    Schimmelpenninck", " van   Zinnicq   Bergmann"
))

kinderwet1874 <- kinderwet1874 %>%
    mutate(politician = str_squish(politician)) %>%
    mutate(vote = rep(1, length(kinderwet1874$politician)))

politician <- c(
    "Verheijen","   
    Begram","   
    Idzerda","  
    Kuyper","   
    Luyben","
    Saaymans Vader"
) %>%
    stringr::str_trim()

vote <- rep(0, length(politician))

kinderwet1874 <- rbind(kinderwet1874, cbind(politician, vote))
kinderwet1874 <- kinderwet1874 %>%
    mutate(law = "Kinderwetje 1874", date = "1874-05-05")
    