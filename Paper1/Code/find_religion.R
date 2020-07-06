# find_religion

#Religion on a district level
## Dataset religion and inhabitants per district

find_religion <- function(district, year, append = TRUE) {
  
  religion <- read.csv("./Data/Religion_inhabitants_per_district.csv") %>%
    select(-1)
  
  distance <- min(abs(unique(religion$jaar) - year))# %>%
  check <- (year + distance) %in% unique(religion$jaar)
  
  if(check == TRUE){
    
    target <- year + distance
    
  } else {
    
    target <- year - distance
    
  }
  
  data <- religion %>%
    filter(districtname %in% district & jaar == target)
  
  if(append == TRUE) {
    #Get the second year you want
    temp <- abs(unique(year - religion$jaar)) %>%
      sort() %>%
      magrittr::extract(2)
    
    check <- (year + temp) %in% unique(religion$jaar)
    
    if(check == TRUE) {
      
      target2 <- year + temp
      
    } else {
      
      target2 <- year - temp
      
    }
    
    #Add the missing districts
    district2 <- setdiff(district, data$districtname)
    
    data <- religion %>%
      filter((districtname %in% district & jaar == target) | 
               (districtname %in% district2 & jaar == target2)
      )
    
    
    #Third iteration, if there are still missing data:
    temp <- abs(unique(year - religion$jaar)) %>%
      sort() %>%
      magrittr::extract(3)
    
    check <- (year + temp) %in% unique(religion$jaar)
    
    if(check == TRUE) {
      
      target3 <- year + temp
      
    } else {
      
      target3 <- year - temp
      
    }
    
    district3 <- setdiff(district2, data$districtname)
    
    data <- religion %>%
      filter((districtname %in% district & jaar == target) | 
               (districtname %in% district2 & jaar == target2) | 
               (districtname %in% district3 & jaar == target3)
      )
  }
  
  data
}
