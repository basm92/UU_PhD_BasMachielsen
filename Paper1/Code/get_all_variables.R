get_all_variables <- function(dataframe, datum) {
    
    # Source all functions
    funcs <- list(
        "find_strikes.R",
        "find_religion.R",
        "find_demographics.R",
        "find_eleccontrols.R",
        "find_econcontrols.R",
        "find_wealth.R")
    
    map(paste0("./Code/", funcs), source)
    
    voting_outcomes <- dataframe
    
    strikes <- find_strikes(voting_outcomes$toelichting, datum)
    voting_outcomes <- left_join(voting_outcomes, strikes, by=c("toelichting" = "district")) %>%
        replace_na(list(amount_strikes = 0))
    
    religion <- find_religion(voting_outcomes$toelichting, datum)
    voting_outcomes <- left_join(voting_outcomes, religion, by = c("toelichting" = "districtname"))
    
    poliddistrid <- data.frame(b1_nummer = voting_outcomes$polid, 
                               toelichting = voting_outcomes$toelichting, 
                               date = datum)
    
    demographics <- find_demographics(poliddistrid) 
    voting_outcomes <- left_join(voting_outcomes, demographics, by = c("polid" = "b1_nummer"))
    
    elecctrls <- find_eleccontrols(voting_outcomes, datum) 
    voting_outcomes <- left_join(voting_outcomes, elecctrls, by = c("polid" = "polid")) 
    
    econcontrols <- find_econcontrols(voting_outcomes$toelichting, datum)
    
    voting_outcomes <- left_join(voting_outcomes %>%
                                     mutate(toelichting = toupper(toelichting)), econcontrols, by = c("toelichting" = "naam"))
    
    wealth <- find_wealth(voting_outcomes$polid, datum)
    voting_outcomes <- left_join(voting_outcomes, wealth, by = c("polid" = "b1_nummer"))
    
    voting_outcomes
}