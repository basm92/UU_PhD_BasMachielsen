ongevallenwet1901 <- data.frame(politician = c("
                                    Zijlma"," van Deinse"," Domier"," 
                                    van Kol"," Schaper"," C.J.E. van Bylandt"," Dobbelmann","
                                    Meesters"," Trujjen "," Fokker"," Rethaan Macaré","
                                    van Kempen"," Nolting"," van Gijn "," Pijnappel"," Heldt"," 
                                    Merckelbach "," van der Zwaag ","
Nolens "," Goeman Borgesius "," Bouman "," Troelstra "," 
van Gilse "," Michiels van Verduynen "," Groen van Waarder"," 
Hennequin ","de Klerk"," van Heemstra"," de Ras"," Tydeman "," Lely"," 
Kolkman"," de Beaufort"," Verhey "," van Raalte "," Tak van Poortvliet"," 
Brummelkamp "," Roessingh "," Goekoop"," de Kam "," L.H.J.M. van Asch van Wijck"," 
Loeff"," van der Kun"," H.M.J. van Asch van Wijck"," 
van Basten Batenburg"," Hink"," Kuyper"," Lieftinck"," Veegens "," 
Conrad "," Harte van Tecklenburg"," 't Hooft"," Smeenge"," Schaepman "," 
Ferf"," de Bieberstein"," Travaglino"," Pijnacker Hordijk"," van Vlijmen"," Krap"," 
Geertsema"," Pyttersen"," Jansen"," van de Velde"," Knijff̈"," Ketelaar
"," Hesselink van Suchtelen"," de Boer"," Staalman"," Houwing"," Kool","
Drucker"," de Waal Malefijt"," van Alphen "," Lucasse "," 
Mees "," Schaafsma "," Seret"," Gleichman")) %>%
    mutate(politician = stringr::str_squish(politician), vote = rep(1, length(politician)))
                                
politician <- c("van Kerkwijk"," de Visser"," van den Berch van Heemstede"," 
de Savornin Lohman"," Schimmelpenninck "," 
Bastert"," Mackay"," van Karnebeek"," Vermeulen"," 
van den Heuvel"," Mutsaers"," van Dedem"," 
W.K.F.P. van Bylandt"," Everts") %>%
    stringr::str_squish() 

vote <- rep(0, length(politician))

ongevallenwet1901 <- rbind(ongevallenwet1901, cbind(politician, vote)) %>%
    mutate(law = "Ongevallenwet 1901", date = "1899-12-13")
                                               
 