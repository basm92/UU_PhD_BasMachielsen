#find_wealth(polid, datum)

if(Sys.info()[[8]] == "bas"){
    setwd("/home/bas/Documents/git/UU_PhD_BasMachielsen/Paper1")
}else{
    setwd("/Users/basmachielsen/Documents/git/UU_PhD_BasMachielsen/Paper1")
}

find_wealth <- function(polid, datum) {
    
    #Load in the file
    wealth <- read_xlsx("./Data/AnalysisFile.xlsx", sheet = "Analysis")
    
    #Calculate the shares
    ## Shares of interest as variables (share_) and shares as for retrospective estimate (xshare)
    wealth %>%
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
}


#Check
foreign %>%
    pivot_longer(2:4) %>%
    ggplot(aes(x = year, y = value, group = name, color = name)) + geom_line()

