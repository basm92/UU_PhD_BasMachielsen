## Yes votes
kieswet1887 <- data.frame(politician = c(
    "Greeve", "Van Alphen", 
    "Van Welderen Rengers", 
    "Kielstra", "Willink", "Huber", 
    "Van Osenbruggen", "Gildemeester", 
    "Cremer", "Lieftinck",
    "J.E.N. Schimmelpenninck van der Oye", 
    "Van Diggelen", "De Bruyn Kops", 
    "Van Asch van Wijk", "W. Van Dedem",
    "Van Berckel", "Van den Biesen",
    "Clercx", "Van Wassenaer Catwijck",
    "Vos de Wael", "Van der Linden",
    "Blussé van Oud Alblas", "Seret", "Visser van Hazerswoude",
    "Van Delden", "Goekoop", "De Ranitz",
    "Farncombe Sanders", "Viruly Verbrugge", 
    "Rutgers van Rozenburg", "Goeman Borgesius",
    "Smeenge", "De Vos van Steenwijk",
    "A. Van Dedem", "Van der Kaay", "Van Kerkwijk",
    "Gleichman", "Van der Goes van Dirxland", 
    "Van der Sleyden", "Van Houten", "De Beaufort",
    "Godin de Beaufort", "Hartogh", "Van Bylandt", 
    "De Savornin Lohman", "Schaepman", "Smit",
    "Fabius", "Reekers", "Ae. Mackay", "De Ruiter-Zylker",
    "Kist", "De Meijier", "Verniers van der Loeff", 
    "Mees", "Van Gennep", "Van der Feltz", 
    "Meesters", "Van der Borch van Verwolde", "Schepel", 
    "Buteux", "Van Aylva van Pallandt", "Buma",
    "Th. Mackay", "Rooseboom", "Kolkman", "Zaaijer", 
    "Cremers"))

kieswet1887 <- kieswet1887 %>%
    mutate(vote = rep(1, length(kieswet1887$politician)))

## No votes
politician <- c("Van der Schrieck", "Smeele", "Borret", "Ruland",
                "Vermeulen", "Lambrechts", 
                "A. Schimmelpenninck van der Oye",
                "Ruys van Beerenbroek", "Haffmans", "Heldt", 
                "Keuchenius", "Brouwers", "Corver Hooft", 
                "Lintelo De Geer van Jutfaas", "Reuther")

vote <- rep(0, length(politician))

## Combine
kieswet1887 <- rbind(kieswet1887, 
                         cbind(politician, 
                               vote))

kieswet1887 <- kieswet1887 %>%
    mutate(law = "Kieswet 1887", date = "1887-03-23")