# Now the voting results
## Yes votes
kieswet1896 <- data.frame(politician = c(
    "Loeff", "Viruly Verbrugge", "Bool", 
    "van Deinse", "van Vlijmen", "Tydeman",
    "Hintzen", "de Ram", "Lely", 
    "Goekoop", "Conrad", "Drucker", 
    "C.J.E. van Bylandt", #Van Bylandt Gouda 
    "Travglino", "van Gijn", "Schaafsma", 
    "Meesters", "E. Smidt", "Rutgers van Rozenburg",
    "Kolkman", "Willinge", "Guyot",
    "Kerkdijk", "Cremer", "Rooijaards van den Ham",
    "van Delden", "Bouman", "van Gennep", 
    "Hesselink van Suchtelen", "Pijnacker Hordijk",
    "Borret", "Lieftinck", "Veegens", "Heldt", 
    "Farncombe Sanders", "Harte van Tecklenburg", "Lambrechts", 
    "Vos de Wael", "Marchant et d'Ansembourg", "Goeman Borgesius",
    "de Beaufort", #De Beaufort Wijk bij duurstede
    "Smeenge", "Bahlmann", "Hartogh", "van Basten Batenburg", 
    "Mees", "Ferf", "de Beaufort", #de Beaufort Amsterdam
    "Michiels van Verduynen", "Smits van Oijen", 
    "Pijnappel", "Mutsaers", "Dobbelmann",
    "Zijp", "Rink", "Schaepman", 
    "Everts", "Haffmans", "Zijlma",
    "Truijen", "Vermeulen", "Houwing", "de Ras",
    "Knijff", "Gleichman"
    ))

kieswet1896 <- kieswet1896 %>%
    mutate(vote = rep(1, length(kieswet1896$politician)))

## No votes
politician <- c("Hennequin", "Donner", 
                "van Borssele", "Van Alphen", 
                "Bastert", "Schimmelpenninck", 
                "Heemskerk", "van Kerkwijk", 
                "A. Smit", "de Savornin Lohman",
                "Roessingh", "Staalman", 
                "T. Mackay", "van Dedem", "\'t Hooft",
                "Lucasse", "van den Bergh van Heemstede",
                "Tijdens", "van Limbrug Stirum", 
                "Ae. Mackay", "Seret", "Pyttersen", 
                "F. van Bylandt") #Van Bylandt Apeldoorn 

vote <- rep(0, length(politician))

## Combine
kieswet1896 <- rbind(kieswet1896, 
                         cbind(politician, 
                               vote))

kieswet1896 <- cbind(kieswet1896, law = "Kieswet 1896", date = "1896-06-19")
