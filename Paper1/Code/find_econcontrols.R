#find_econcontrols(district, year)

library(hdng)

hdng::hdng_names_cats("Welvaart", from = 1879, to = 1933)


#First, find the appropriate variables

#Welvaart
#agpb = share of municipality in taxes
#prbd = share of hoogstaangeslagenen as a percentage of beroepsbevolking
econdata <- hdng::hdng_get_data(c("agpb", "prbd"), col.names = T)
#Still need to find out the scale of these variables, but other than that, 
#they're ready for us

#Beroepen
## Extract all variables
hdng::hdng_names_cats("Beroepen", from = 1889, to = 1930) %>%
    filter(`1889` == 1, `1899` == 1, `1930` == 1) -> vars

vars <- lapply(vars[4], as.character)[[1]]

econdata2 <- hdng_get_data(vars, col.names = T) %>%
    filter(year < 1931)


detach("package:hdng", unload = TRUE)

## Now add the different measurements into one variable
econdata2 %>%
    group_by(naam, year) %>%
    summarise(across(contains("Aardewerk"), ))

# Now the function using these two datasets

find_econcontrols <- function(district, year){
    
    
    
}



library(binman)
