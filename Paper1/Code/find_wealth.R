#find_wealth(polid, datum)

if(Sys.info()[[8]] == "bas"){
    setwd("/home/bas/Documents/git/UU_PhD_BasMachielsen/Paper1")
}else{
    setwd("/Users/basmachielsen/Documents/git/UU_PhD_BasMachielsen/Paper1")
}

find_wealth <- function(polid, votedatum) {
    
    #Load in the file
    wealth <- read_xlsx("./Data/AnalysisFile.xlsx", sheet = "Analysis")
    
    #Calculate the shares
    ## Shares of interest as variables (share_) and shares as for retrospective estimate (xshare)
    wealth <- wealth %>%
        rowwise() %>%
        mutate(sum_all = sum(Re, Dugobo, Fogobo, Duprbo, Foprbo, Dush, Fosh, Cash, Misc),
               share_re = Re / sum_all ,
               share_bonds = sum(Dugobo, Fogobo, Duprbo, Foprbo)/sum_all,
               share_shares = sum(Dush, Fosh)/ sum_all, 
               share_domestic = sum(Dugobo, Duprbo, Dush) / sum_all,
               share_foreign = sum(Fogobo, Foprbo, Fosh) / sum_all,
               across(c(Re:Misc), ~ .x / sum_all, .names = "xshare_{col}")
               )
    
    # Import the RoRE Dataset and clean it
    roroe <- read_xlsx("./Data/rateofreturnoneveryting_data.xlsx", sheet = "Data") %>%
        group_split(iso == "NLD") %>%
        lapply(select, year, country, iso, bond_rate, housing_rent_rtn, ltrate, eq_tr, safe_tr) %>%
        lapply(mutate, eq_tr = coalesce(eq_tr, safe_tr)) #
    
    #make a foreign portfolio
    weights_20 <- c("DEU", "FRA")
    weights_10 <- c("BEL", "USA", "GBR", "ITA")
    weights_2 <- c("AUS", "CAN", "DNK", "FIN", "JPN", "NOR", "PRT", "ESP", "SWE", "CHE")
    
    hi <- list(weights_20, weights_10, weights_2)
    
    bonds <- map(hi, ~ paste("bond_rate_", .x, sep = ""))
    housing <- map(hi, ~ paste("housing_rent_rtn_", .x, sep = "")) #doesnt exist, so doesnt matter
    prbonds <- map(hi, ~ paste("ltrate_", .x, sep = ""))
    shares <- map(hi, ~ paste("eq_tr_", .x, sep = ""))
    
    #compute the foreign returns
    foreign <- roroe[[1]] %>%
        select(-country) %>%
        pivot_wider(names_from = iso, 
                    values_from = c(bond_rate:safe_tr)) %>%
        group_by(year) %>%
        summarize(foreign_bond_ret = 
                      (0.2 * sum(across(bonds[[1]])) + 
                           0.1 * sum(across(bonds[[2]])) +
                           0.05 * sum(across(bonds[[3]]), na.rm = T)),
                  foreign_prbonds = 
                      ((0.2 * sum(across(prbonds[[1]]), na.rm = T) +
                           0.1 * sum(across(prbonds[[2]]), na.rm = T) +
                           0.05 * sum(across(prbonds[[3]]), na.rm = T)) / 100),
                  
                  foreign_shares = 
                      (0.2 * sum(across(shares[[1]]), na.rm = T) +
                           0.1 * sum(across(shares[[2]]), na.rm = T) +
                           0.05 * sum(across(shares[[3]]), na.rm = T))
        )
    
    #domestic
    domestic <- roroe[[2]] %>%
        select(-country)
    
    #find age of death and  compute the YEARS between death and vote
    years <- read_csv("./Data/b1no_dod.csv", col_types = list(
        col_character(), 
        col_character(), 
        col_character())
        ) %>%
        select(-1) %>%
        janitor::clean_names() %>%
        mutate(dateofdeath = as.integer(str_extract(dateofdeath, "\\d{4}"))) %>%
        rowwise() %>%
        mutate(diff = list(
            seq(
                from = as.integer(str_extract(votedatum, "\\d{4}")), 
                to = dateofdeath)
            )
            ) #This gives me a list of years for each b1_number
    
    ## Merge the list with the wealth datasets
    portfolio <- left_join(wealth, years, by = c("Indexnummer" = "b1_nummer")) %>%
        filter(Indexnummer %in% polid) #Here is the polid filter
    
    
    ## Use the foreign and domestic datasets to retrieve returns
    portfolio <- portfolio %>%
        rowwise() %>%
        mutate(foreign_bond_ret = sum(foreign$foreign_bond_ret[which(foreign$year %in% diff)], na.rm = T),
               foreign_shares_ret = sum(foreign$foreign_shares[which(foreign$year %in% diff)], na.rm = T),
               foreign_prbond_ret = sum(foreign$foreign_prbonds[which(foreign$year %in% diff)], na.rm =T),
               dutch_re_ret = sum(domestic$housing_rent_rtn[which(domestic$year %in% diff)], na.rm = T),
               dutch_bond_ret = sum(domestic$bond_rate[which(domestic$year %in% diff)], na.rm = T),
               dutch_prbond_ret = sum(domestic$ltrate[which(domestic$year %in% diff)], na.rm = T)/100,
               dutch_sh_ret = sum(domestic$eq_tr[which(domestic$year %in% diff)], na.rm = T))
    
    
    ## Finally, compute the net return on the portfolio using the composition variables
    ## Use compound interest exp^-(previously calculated sum) to calculate back in time to the vote
    
    
    ## Select the variables which I want in the analysis, rename Indexnummer to b1_nummer
    
}


#Check
foreign %>%
    pivot_longer(2:4) %>%
    ggplot(aes(x = year, y = value, group = name, color = name)) + geom_line()

