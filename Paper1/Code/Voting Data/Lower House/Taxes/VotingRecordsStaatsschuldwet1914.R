#Dividendbelasting 1917, TK

#voor

staatsschuldwet1914 <- data.frame(
    politician = c(
      "Vliet",
      " Eland",
      " van Doorn",
      " van de Velde",
      " Juten",
      " Fock",
      " Nolens",
      " van Bylandt",
      " Jannink",
      " Loeff",
      " Duynstee",
      " van Wichen",
      " Heercs",
      " van den  Berch van Heemstede",
      " de Visser",
      " Beumer",
      " van Sasse van Ysselt",
      " Fleskens",
      " van Best",
      " Boissevain",
      " van Hamel",
      " van Idsinga",
      " Jansen",
      " Lieftiuck",
      " Gerretson",
      " van derVoort van Zijp",
      " J.W.J.C.M. van Nispen tot Sevenaer",
      " Smeenge",
      "   Hubrecht",
      " de Geer",
      " Visser van IJzendoorn",
      " Duymaer van Twist",
      " van Veen",
      " Rutgers",
      " Roodhuizen",
      " Drion",
      " Rink",
      " de Meester",
      " van Raalte",
      " van Foreest",
      " Oosterbaan",
      " ter Spill",
      " van Vuuren",
      " de Monté ver Loren",
      " Bichon van Ijsselmonde",
      " Patijn",
      " Bongaerts",
      " Tydeman",
      " Schim van der Loeff",
      " van Deventer",
      " O.F.A.M. van Nispen tot Sevenaer",
      " Bogaardt",
      " Brummelkamp",
      " Knobel",
      " van Vollenhoven",
      " Beckers",
      " Aalberse",
      " Dolk",
      " de Beaufort",
      " van Wijnbergen",
      "Goeman Borgesius"
    )
  ) %>%
  mutate(politician = stringr::str_trim(politician))

staatsschuldwet1914 <- staatsschuldwet1914 %>%
  mutate(vote = rep(1, length(staatsschuldwet1914$politician)))


politician <- c(
  "Bos",
  " Teenstra",
  " Troelstra",
  "
                Limburg",
  " ter Laan (Rotterdam)",
  " Spiekman",
  " van Leeuwen",
  "
                Mendels",
  " Kleerekoper",
  " Otto",
  " de Jong",
  " Hugenholtz",
  " Schaper",
  "
                Albarda",
  " Roodenburg",
  " Marchant",
  " Vliegen",
  " de Muralt",
  " Gerhard",
  " Ketelaar",
  " Helsdingen",
  "ter Laan"
) %>%
  stringr::str_trim()

vote <- rep(0, length(politician))

staatsschuldwet1914 <- rbind(staatsschuldwet1914, cbind(politician, vote)) %>%
  mutate(law = "Staatsschuldwet 1914", date = "1914-12-11")
