# Inkomstenbelasting 1914

#Voor

inkomstenbelasting1914 <- data.frame(politician = c(
    "K. ter Laan"," Duys"," Gerhard", #K. ter Laan Den Haag 
    "Nierstrasz"," van Vliet"," Roodenburg"," Helsdingen"," 
    Rink"," Juten"," van Wijnbergen"," Teenstra"," Vliegen"," 
    Hugenholtz"," Patijn"," Aalberse"," Heeres"," Eerdmans"," 
    van den Berch van Heemstede"," Jannink"," Ruys de Beerenbrouck"," 
    Beumer"," van Deventer"," de Savornin Lohman"," Dolk"," Roodhuyzen"," Fock"," 
    de Muralt"," van Raalte"," Nolens"," van Vollenhoven"," Bos"," Schim van der Loeff"," 
    J.W.J.C.M. van Nispen tot Sevenaer","
    Hubrecht"," de Jong"," O.F.A.M. van Nispen tot Sevenaer"," 
    Marchant"," de Beaufort"," Rutgers"," Eleskens"," Duynstee"," 
    Janssen"," de Meester"," Boissevain"," Otto"," Gerretson"," 
    Tydeman"," van Wichen"," Smeenge"," Lieftinck"," Schimmelpenninck"," 
    Visser van IJzendcorn"," Albarda"," van de Velde"," Schaper"," 
    Jansen"," ter Spul"," Limburg"," de Geer"," 
    J. ter Laan", # Jan ter Laan SDAP Rotterdam
    "van Bijlandt"," Sannes"," Knobel"," Bichon van IJsselmonde"," 
    Koolen"," Oosterbaan"," Spiekman"," Goeman Borgesius")) %>%
    mutate(politician = stringr::str_trim(politician))

inkomstenbelasting1914 <- inkomstenbelasting1914 %>%
    mutate(vote = rep(1, length(inkomstenbelasting1914$politician)))
  

# TEGEN  
politician <- c("Brummelkamp"," Bongaerts"," de Monte verLoren"," 
                 van Vuuren"," de Wijkerslooth de Weerdesteyn"," 
                 Duymaer van Twist"," Beckers"," Bogaardt"," 
                 Arts"," van der Voort van Zijp"," 
                 Scheurer"," van Vlijmen") %>%
    str_trim()

vote <- rep(0, length(politician))

#Samen 
inkomstenbelasting1914 <- rbind(inkomstenbelasting1914, cbind(politician, vote)) %>%
  mutate(law = "Inkomstenbelasting 1914", date = "1914-06-03")

    
    
