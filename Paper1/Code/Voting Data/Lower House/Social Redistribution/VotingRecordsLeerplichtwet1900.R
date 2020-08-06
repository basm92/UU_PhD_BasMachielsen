leerplichtwet1901 <- data.frame(politician = c("Lieftinck"," Van Gijn"," Groen van Waarder"," Smeenge","
                                Hennequin"," De Beaufort"," Kerdijk"," Bouman"," 
                                Fokker"," Pijnacker Hordijk"," Hartogh"," 
                                van Raalte"," Schepel"," Kolkman"," Lely"," 
                                Goeman Borgesius"," De Klerk"," Heldt"," 
                                Veegens"," Tijdeman"," Rink"," Tak van Poortvliet"," 
                                Conrad"," Goekoop"," van Kerkwijk"," Zijlma"," 
                                Tijdens"," Schaafsma"," Houwing"," Drucker"," 
                                Schaepman"," Kool"," Ferf"," de Boer"," Willinge"," 
                                Pyttersen"," Mees"," Knijff"," van Deinse"," 
                                C.J.E. Van Bylandt"," Geertsema"," Rethaan Macaré"," 
                                Van Gilse"," Roessingh"," Verhey"," Ketelaar"," 
                                Nolting"," Meesters"," Hesselink van Suchtelen"," Gleichman
                                ")) %>%
    mutate(politician = stringr::str_squish(politician), vote = rep(1, length(politician)))


politician <- c("Van Basten Batenburg"," 
Van der Borch van Verwolde"," Van der Kun"," 't Hooft"," Brummelkamp"," 
Schaper"," H.M.J. Van Asch van Wijck"," 
Merkelbach"," Van Heemstra"," Krap"," De Ram"," Dobbelmann"," 
Truijen"," De Ras"," Van den Berch van Heemstede"," 
Van Limburg Stirum"," Harte van Tecklenburg"," 
Van Kempen"," Van Karnebeek"," De Bieberstein"," Michiels van Verduynen"," 
Seret"," Vermeulen"," Troelsta"," Van der Zwaag"," 
De Visser"," Van Dedem"," Loeff"," Jansen"," Lucasse"," 
W.K.F.P. van Bylandt"," L.H.J.M. Van Asch van Wijck"," 
Van Kol"," Pijnappel"," Kuyper"," Bastert"," De Waal Malefijt"," 
Mutsaers"," Travaglino"," Mackay"," Van de Velde"," 
De Savornin Lohman"," Van Vlijmen"," Van den Heuvel"," 
Everts"," Donner"," Staalman"," Van Alphen", "Nolens") %>%
    stringr::str_squish()

vote <- rep(0, length(politician))

leerplichtwet1901 <- rbind(leerplichtwet1901, cbind(politician, vote)) %>%
    mutate(law = "Leerplichtwet 1901", date = "1900-03-30")
