# Inkomstenbelasting 1893

##Voor
inkomstenbelasting1893 <- data.frame(politician = c("Plate"," Smit"," Goekoop","
Roessingh"," Ferf"," van Houten"," van der Feltz"," van Gijn","
W.H. de Beaufort"," Lucasse"," Pijnacker Hordijk"," Rutgers van Rozenburg"," Cremer", #Beaufort Amsterdam
"van Delden"," A. van Dedem"," Rink"," Levy"," Guyot"," 
Schimmelpenninck van der Oye"," van der Kaay"," Tydeman"," 
Conrad"," Hartogh"," Ae. Mackay"," Roijaards van den Ham"," 
Goeman Borgesius"," Kerdijk"," Lieftinck"," Bahlmann"," 
Kolkman"," Schepel"," Kielstra"," Veegens"," Valette"," Hintzen"," 
Mees"," Roell"," Land"," van Alphen"," Schaepman"," 
Tijdens"," de Kanter"," de Meijier"," van Beuningen"," van Berckel"," 
Bool"," Smeenge"," van Kerkwijk"," Zijp"," Houwing"," Heldt"," Hennequin"," 
van Velzen"," Viruly Verbrugge"," Gleichman")) %>%
    mutate(politician = stringr::str_trim(politician))

inkomstenbelasting1893 <- inkomstenbelasting1893 %>%
    mutate(vote = rep(1, length(inkomstenbelasting1893$politician)))

## Tegen

politician = c("Pyttersen"," Brantsen van de Zijp"," Seret"," 
van der Borch van Verwolde"," Travaglino"," 
Donner"," Bevers"," Walter"," Lambrechts"," van der Schrieck","
Keuchenius"," van der Kun"," Smits van Oijen"," T. Mackay"," 
van de Velde"," Reekers"," Poelman"," Dobbelmann"," Gerritsen","
Farncombe Sanders"," Heemskerk"," Harte"," de Ras"," G. 
Dedem"," van Karnebeek"," Vermeulen"," van Löben Sels","
van den Berch van Heemstede"," Mutsaers"," de Geer 
Jutfaas"," van Vlijmen "," van Bylandt"," Haffmans"," Beelaerts van Blokland") %>%
    str_trim()

vote = rep(0, length(politician))

inkomstenbelasting1893 <- rbind(inkomstenbelasting1893, cbind(politician, vote))

inkomstenbelasting1893 <- inkomstenbelasting1893 %>%
    mutate(law = "Inkomstenbelasting 1893", date = "1893-06-23")

