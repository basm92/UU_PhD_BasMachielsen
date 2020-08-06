woningwet1901 <- data.frame(politician = c("
                           Merckelbach"," de Bieberstein"," 
                                           van de Velde"," Bolsius"," van Dedem"," de Boer"," Schaper"," 
                                           van der Borch van Verwolde"," 
                                           van Kol"," Zijlma"," Meesters"," Rink"," 
                                           Knijff"," 
                                           Drucker"," Lieftinck"," Schaafsma"," Jansen"," de Klerk"," 
                                           van Vlijmen"," 
                                           de Bas"," Smeenge"," de Ram"," Nolting"," Ketelaar"," 
                                           Mees"," de Visser","
                                           Roessingh"," Pijnappel"," Geertsema"," Schepel"," van Alphen. Ferf"," 
                                           van Deinse"," den Hertog"," Pijnacker Hordijk"," Travaglino"," 
                                           Nolens"," 
                                           Goekoop"," Conrad"," Michiels van Verduynen"," Vermeulen"," Verhey"," 
                                           C.J.E. van Bylandt"," Dobbelmann"," van Gilse"," van Gijn"," 
                                           Heemstra"," Goeman Borgesius"," van Kerkwijk"," Lely",
                                           " van den Berch van Heemstede"," Pastoors",
                                           " Marchant"," Brummelkamp"," 
                                           Kerdyk"," 
                                           van Karnebeek"," Fokker"," Kolkman"," Heldt"," Kuyper"," Tydeman"," 
                                           Groen van Waarder"," Veegens"," Tijdens"," Mackay"," Houwing"," 
                                           Mutsaers"," Kool"," Troelstra"," van den Heuvel"," de Waal Malefijt","
                                           Gleichman")) %>%
    mutate(politician = stringr::str_squish(politician), vote = rep(1,length(politician)))


politician <- c("W.K.F.P. van Bylandt"," Schimmelpenninck"," 
                 de Savornin Lohman","
                 van Limburg Stirum") %>%
    stringr::str_squish()

vote <- rep(0, length(politician))

woningwet1901 <- rbind(woningwet1901, cbind(politician, vote)) %>%
    mutate(law = "Woningwet 1901", date = "1901-04-19")
