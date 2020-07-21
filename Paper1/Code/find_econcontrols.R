#find_econcontrols(district, year)
# Now the function using these two datasets

find_econcontrols <- function(district, yearinput, absrel = "both"){
    
    #Read in the data
    econdata <- read.csv("./Data/econ_controls_1.csv")
    econdata2 <- read.csv("./Data/econ_controls_2.csv")
    
    #Calculate shares
    econdata <- econdata %>%
        group_by(year) %>%
        mutate(share_aandeel = Aandeel.der.gemeente.in.s.Rijks.personele.belasting/sum(Aandeel.der.gemeente.in.s.Rijks.personele.belasting))
    
    #Find closest available year in each dataset
    query1 <- econdata %>%
        group_by(naam) %>%
        slice_min(abs(yearinput - year))
    
    query1 <- query1 %>%
        filter(tolower(naam) %in% tolower(district)) %>%
        select(c(2,5,7,8))
    
    colnames(query1) <- c("naam", "year_eccontrls1", "no_highest_tax", "share_aandeel")
    
    
    query2 <- econdata2 %>%
        group_by(naam) %>%
        slice_min(abs(yearinput - year)) %>%
        filter(tolower(naam) %in% tolower(district)) 
    
    colnames(query2)[2] <- "year_eccontrls2"
    
    if(absrel == "abs") {
        
        query2 <- query2 %>%
            select(1:8)
    } else if(absrel == "rel"){
        
        query2 <- query2 %>%
            select(c(1:4, 9:11))
    }
    
    #Merge the two datasets on the basis of district
    merge(query1, query2, by = "naam")
}
