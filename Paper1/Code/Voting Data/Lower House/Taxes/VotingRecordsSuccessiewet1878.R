successiewet1878 <- data.frame(politician = c("de Jong "," de Beaufort","
                                              Idzerda"," Verniers van der Loeff"," Bredius jr. "," Patijn"," 
                                              Rombach"," Hingst"," de Vos van Steenwijk "," Blussé "," van der Kaay","
Sandberg","Stieltjes"," Mees"," Viruly Verbrugge "," van de Werk "," van Stolk"," van der Feltz","
van Harinxma thoe Slooten"," 
Oldenhuis gratama"," Sickesz"," Bastert"," Gevers Deynoot"," 
Luyben"," Moens"," Bredius"," van Delden"," Schepel","
  Lenting"," Godefroi"," de Meijier"," Goeman Borgesius"," Bergsma",
 "de Ruiter Zylker"," van Tienlioven"," Dijckmeester"," Roell","
  Vening Meinesz"," van Houten"," Cremers"," Mirandolle",
  " Fransen van de Putte"," de Bruyn Kops"," Wybenga"," van Heukelom","
  Holtzman"," van Eek"," Dullert"
)) %>%
  mutate(politician = stringr::str_trim(politician), vote = rep(1, length(politician)))


politician <- c("van den Berch van Heemstede"," Schimmelpenninck van der Oye"," Bichon van IJsselmonde"," 
                Reekers"," van Asch van Wijck"," Heydenrijck","
                de Caseinbroot"," Korens de Wijlre"," van Kerkwijk "," 
                de Bieberstein"," van Nispen tot Sevenaer"," 
                Corver Hooft"," Begram"," van der Schrieck"," Tedinjg van Berkhout"," Haffmans"," 
                van Baar"," Borret"," Verheijen"," Maekay"," Lambrechts"," 
                van Zinnicq Bergmann"," Saaymans Vader"," Insinger"," 
                Schagen van Leeuwen"," de Jonge"," 
                Schimmelpenninck"," des Amorie van der Hoeven"," Arnoldts"," 
                Brouwers"," van Wassenaer van Catwijck"," Wintgens") %>%
  stringr::str_trim()

vote <- rep(0, length(politician))

successiewet1878 <- rbind(successiewet1878, cbind(politician, vote)) %>%
  mutate(law = "Successiewet1878", date = "1878-05-24")

