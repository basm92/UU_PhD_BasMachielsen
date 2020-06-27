library(Hmisc)
library(dplyr)

dir()
setwd("./Downloads")
df <- mdb.get("netherlands.mdb")

strikes <- df$tblActies

strikes <- left_join(strikes, df$tblActies_Plaats, by = c("ID" = "ActieID"))

strikes <- strikes %>%
  left_join(df$tblPlaats, by = c("PlaatsID" = "ID"))

howmuch <- strikes %>%
  group_by(JAAR, GEMEENTE, PlaatsID, PROVINCIE) %>%
  summarise(howmuch = n())

