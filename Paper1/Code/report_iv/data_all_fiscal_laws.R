#all tax laws

#Successiewet 1878
id <- find_politician_id(successiewet1878$politician, successiewet1878$date[1])
voting_outcomes <- left_join(successiewet1878, id, by = c("politician" = "names"))
distr <- find_district(voting_outcomes$polid, voting_outcomes$date[1])
voting_outcomes <- left_join(voting_outcomes, distr, by = c("polid" = "b1-nummer"))
#districts
voting_outcomes$toelichting <- clean_districts(voting_outcomes$toelichting, voting_outcomes$date[1])
sw1878 <- get_all_variables(voting_outcomes, voting_outcomes$date[1])

#Successiewet1911
id <- find_politician_id(successiewet1911$politician, successiewet1911$date[1])
voting_outcomes <- left_join(successiewet1911, id, by = c("politician" = "names"))
distr <- find_district(voting_outcomes$polid, voting_outcomes$date[1])
voting_outcomes <- left_join(voting_outcomes, distr, by = c("polid" = "b1-nummer"))
#districts
voting_outcomes$toelichting <- clean_districts(voting_outcomes$toelichting, voting_outcomes$date[1])
sw1911 <- get_all_variables(voting_outcomes, voting_outcomes$date[1])


#successiewet1916
id <- find_politician_id(successiewet1916$politician, successiewet1916$date[1])
voting_outcomes <- left_join(successiewet1916, id, by = c("politician" = "names"))
distr <- find_district(voting_outcomes$polid, voting_outcomes$date[1])
voting_outcomes <- left_join(voting_outcomes, distr, by = c("polid" = "b1-nummer"))
#districts
voting_outcomes$toelichting <- clean_districts(voting_outcomes$toelichting, voting_outcomes$date[1])
sw1916 <- get_all_variables(voting_outcomes, voting_outcomes$date[1])


#inkomstenbelasting1893
id <- find_politician_id(inkomstenbelasting1893$politician, inkomstenbelasting1893$date[1])
voting_outcomes <- left_join(inkomstenbelasting1893, id, by = c("politician" = "names"))
distr <- find_district(voting_outcomes$polid, voting_outcomes$date[1])
voting_outcomes <- left_join(voting_outcomes, distr, by = c("polid" = "b1-nummer"))
#districts
voting_outcomes$toelichting <- clean_districts(voting_outcomes$toelichting, voting_outcomes$date[1])
ib1893 <- get_all_variables(voting_outcomes, voting_outcomes$date[1])


#inkomstenbelasting1914
id <- find_politician_id(inkomstenbelasting1914$politician, inkomstenbelasting1914$date[1])
voting_outcomes <- left_join(inkomstenbelasting1914, id, by = c("politician" = "names"))
distr <- find_district(voting_outcomes$polid, voting_outcomes$date[1])
voting_outcomes <- left_join(voting_outcomes, distr, by = c("polid" = "b1-nummer"))
#districts
voting_outcomes$toelichting <- clean_districts(voting_outcomes$toelichting, voting_outcomes$date[1])
ib1914 <- get_all_variables(voting_outcomes, voting_outcomes$date[1])


#staatsschuldwet1914
id <- find_politician_id(staatsschuldwet1914$politician, staatsschuldwet1914$date[1])
voting_outcomes <- left_join(staatsschuldwet1914, id, by = c("politician" = "names"))
distr <- find_district(voting_outcomes$polid, voting_outcomes$date[1])
voting_outcomes <- left_join(voting_outcomes, distr, by = c("polid" = "b1-nummer"))
#districts
voting_outcomes$toelichting <- clean_districts(voting_outcomes$toelichting, voting_outcomes$date[1])
ss1914 <- get_all_variables(voting_outcomes, voting_outcomes$date[1])


fiscal <- bind_rows(ib1893, ib1914, sw1878, sw1911, sw1916, ss1914)
