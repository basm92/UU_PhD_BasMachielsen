# This outcome is contrary to the actual voting outcome, but consistent with 
# the other analyses
## Yes votes
kieswet1892 <- data.frame(politician = c(
  "Donner", "Van Karnebeek", "Gerritsen",
  "E. Smidt", "Beelaerts van Blokland", 
  "Haffmans", "van Beuningen", "Lucasse",
  "Rutgers van Rozenburg", "Bahlmann",
  "Rink", "W.A. Feltz", "Hartogh", "Houwing",
  "Pyttersen", "Ferf", "Veegens", "de Kanter",
  "Smeenge", "Kerkdijk", "Seret", "Levy",
  "Tydeman", "Vrolik", "Zijp", "Zijlma", 
  "Valette", "Goeman Borgesius", "Cremer",
  "H.A. Velde", "Schepel", "Pijnacker Hordijk",
  "Farncombe Sanders", "Hennequin", "Heldt", 
  "A. Smit", "Land", "Tijdens", "Lieftinck", 
  "Roessingh", "van Kerkwijk"
))

kieswet1892 <- kieswet1892 %>%
  mutate(vote = rep(1, length(kieswet1892$politician)))

## No votes
politician <- c(
  "van Gijn", "van Houten", "Geertsema", 
  "Schimmelpenninck van der Oye", "Marchant et d'Ansembourg",
  "Guyot", "A. van Dedem", "van Alphen", 
  "Bevers", "Everts", "de Ras", "van den Berch van Heemstede",
  "Conrad", "P.G.J. Schrieck", "T. Mackay", "L.D.J.L. Ram",
  "Roell", "Walter", "E.A.M. Kun", "G. van Dedem", 
  "Kielstra", "Michiels van Verduynen", 
  "B.J. Lintelo baron de Geer van Jutphaas",
  "Harte van Tecklenburg", "Lambrechts", "Hintzen", "Heemskerk",
  "Smits van Oijen", "Dobbelmann", "De Beaufort", #De Beaufort Wijk bij Duurstede - missing
  "Reekers", "Vermeulen", "van Delden", "Roijaards van der Ham",
  "Kolkman", "Mutsaerts", "Travaglino", "Van Loben Sels", 
  "W.H. de Beaufort", #De Beaufort Amsterdam - 71
  "de Meijier", "de Savornin Lohman", "van Velzen", 
  "Huber", "van Vlijmen", "W. Kaay", "Th.L.M.H. Borret",
  "Mees", "Brantsen van de Zijp", "Bool", 
  "van Berckel", "Viruly Verbrugge", "van der Borch van Verwolde",
  "Ae. Mackay", "van Bylandt", "Plate", "Schaepman", 
  "Gleichman"
) 

vote <- rep(0, length(politician))

## Combine
kieswet1892 <- rbind(kieswet1892, 
                         cbind(politician, 
                               vote))


kieswet1892 <- cbind(kieswet1892, law = "Kieswet 1892") %>%
  mutate(date = "1894-03-09")
