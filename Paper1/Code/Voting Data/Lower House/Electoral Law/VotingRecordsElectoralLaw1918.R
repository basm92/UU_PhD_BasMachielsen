## Yes votes
kieswet1918 <- data.frame(politician = c(
  "Marchant", "van Ryckevorsel", "Kolkman", "Kolthek", 
  "J. ter Laan", "Kuiper", "Gerhard",
  "Otto", "Kruyt", "J.E.W. Duijs", "Niemeijer", "Lely", "Bakker", "Schaper", 
  "Groeneweg", "Helsdingen", "Reijmer", "L. M. Hermans",
  "Ossendorp", "van Zadelhoff", "van Rappard", "Rutgers", "Ketelaar",
  "de Groot", "Albarda", "Nolens", "Kleerekoper", "de Muralt", "Sannes",
  "de Savornin Lohman", "Teenstra", "Ter Hall", "Bongaerts", "van de Laar", 
  "Treub", "Hugenholtz", "Oud", "de Buisonjé", "Bulten",
  "Rink", "Bos", "van Vuuren", "van Beresteyn", "de Zeeuw", "Loeff", 
  "van den Tempel", "Heijkoop", "Swane", "Rugge", "Smeenk", "van de Bilt",
  "Weitkamp", "Poels", "H. G. M. Hermans", "de Wijkerslooth de Weerdesteyn", 
  "van Wijnbergen", "Deckers", "de Jonge", "Haazevoet",
  "Visser van IJzendoorn", "de Geer", "Wijnkoop", "van Ravesteijn", 
  "Fock"
  ))

kieswet1918 <- kieswet1918 %>%
  mutate(vote = rep(1, length(kieswet1918$politician)))

## No votes
politician <- c(
  "A.P. Staalman", "Schokking", 
  "Duymaer van Twist", "Zijlstra", 
  "de Monté verLoren", "de Wilde", "Snoeck Henkemans", "Beumer", 
  "van der Voort van Zijp"," Heemskerk"
)

vote <- rep(0, length(politician))

## Combine
kieswet1918 <- rbind(kieswet1918, 
                         cbind(politician, 
                               vote))
kieswet1918 <- cbind(kieswet1918, law = "Kieswet 1918", date = "1919-05-09")
