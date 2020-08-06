# clean_districts

clean_districts <- function(district, datum) {
  
  goed <- ifelse(!str_detect(district, "\\d"), district, NA)
  bad <- ifelse(str_detect(district, "\\d"), district, NA)
  
  #Split up bad between easy and hard
  
  hardbad <- ifelse(str_detect(bad, ",|;"), bad, NA)
  easybad <- ifelse(!str_detect(bad, ",|;"), bad, NA)
  
  #Clean up easybad
  easybad <- easybad %>%
    str_replace("in ", "") %>%
    str_replace("\\d{4}-.*\\d{4} ", "")
  
  #Clean up hardbad with the help of year
  hardbad <- hardbad %>%
    str_replace(";(.+)", "") %>%
    str_split(",| en ") 
  
  #Make a buffer variable for which hardbad to clean up later
  hardbad2 <- hardbad
  
  #Extract the years
  hardbad <- lapply(hardbad, str_extract, "\\d{4}-\\d{4}") %>%
    lapply(str_split, "-") 
  
  year <- str_extract(datum, "\\d{4}")
   
  #Make an empty vector 
  whichones <- vector(length = length(hardbad)) 
  
  #Fill the vector where the year (function argument) is in the period)
  for(i in 1:length(hardbad)){
 #   print(i)
    
    if(is.na(hardbad[[i]][1])){
 #       print("lengte een")
        whichones[i] <- NA
        
      } else{
        
        for(j in 1:length(hardbad[[i]])){
          
  #        print(j)
          begin <- magrittr::extract(hardbad[[i]][[j]], 1)
          end <- magrittr::extract(hardbad[[i]][[j]], 2)
          
          period <- seq(begin, end)
          
          if(year %in% period){
            whichones[i] <- j
          }
        }
    
      }
    
    whichones
  }
 
  temp <- coalesce(goed, easybad) 
  
  hardbad2corrected <- vector(length = length(hardbad2))
  
  for(i in 1:length(hardbad2)){
    hardbad2corrected[i] <- hardbad2[[i]][whichones[i]]
  }
  
  hardbad2corrected <- hardbad2corrected %>%
    str_remove("in ") %>%
    str_remove("\\d{4}-\\d{4}") %>%
    str_remove("voor het kiesdistrict") %>%
    str_remove_all(":|\\(|\\)") %>%
    str_trim()
  
  coalesce(temp, hardbad2corrected)
}


