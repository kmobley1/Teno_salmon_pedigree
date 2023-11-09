#parentage dataset default 21.06.18

####packages####
library (tidyverse)

#database
UtsSNP <- read.csv("Data/UtsSNP_21.04.13.csv")
Uts_Birthyear_Calc <- read.csv("Data/Uts_Birthyear_Calc_21_06_22.csv")
Uts_parentage_default <- read.csv("Data/Uts_parentage_default_21.06.22.csv")

#select info for adult class.cor, year, birthyear.int, respawner
Uts_Birthyear_Calc_info <- Uts_Birthyear_Calc %>%
  select(ID, type, class.cor, year, birthyear.int, Scale.smoltage, scale.seaage, InterpAge, Respawner, respawner.info) %>%
  mutate(Respawner = replace_na(Respawner, 0)) %>%
  mutate(respawner.info = ifelse(respawner.info == "", NA, respawner.info))

#starting sum of offspring
#dams
Uts_parentage_default %>% filter(!is.na(dam)) %>% ungroup() %>% count()
#sires
Uts_parentage_default %>% filter(!is.na(sire)) %>% ungroup() %>% count()

####working with dams####
#summarize (count) all offspring per dam 
Uts_default_dams <- Uts_parentage_default %>%
  group_by(dam, class.cor.off, birthyear.int.off) %>%
  summarise_at(c("type.off"), funs(n.off = sum(!is.na(.))))

#pivot wider
Uts_default_dams_wide <- Uts_default_dams %>%
  pivot_wider(
    names_from = c(class.cor.off),
    names_sep = "_",
    values_from = n.off)

#remove rows where dam = NA, replace NA with 0
Uts_default_dams_wide_rmNA <- Uts_default_dams_wide %>%
  filter(dam != "NA")%>%
  replace(is.na(.), 0)

#sum individuals across rows
Uts_default_dams_total <- Uts_default_dams_wide_rmNA %>%
  rowwise() %>% 
  mutate(n.offspring = rowSums(across(c("0+", "1+", "2-3+", "Adult")))) %>%
  rename(cohort.year = birthyear.int.off)

#check total offspring
Uts_default_dams_total %>% ungroup() %>% tally(n.offspring)

#join datasets
Uts_default_dams_all <- left_join(Uts_default_dams_total, Uts_Birthyear_Calc_info, by = c("dam" = "ID")) %>%
  relocate(c(8:16), .after = dam)

#find minimum cohort year
Uts_default_dams_cohort.min <- Uts_default_dams_all %>%
  group_by(dam) %>%
  summarize(firstcohort = ifelse(type == "Adult" & Respawner == "0", min(cohort.year),
                                 ifelse(type == "Adult" & Respawner == "1", min(cohort.year), 
                                       ifelse(type == "Offspring", min(cohort.year), NA)))) %>%
  distinct()

#combine min fix with dams all
Uts_default_dams_min.combine <- left_join(Uts_default_dams_all, Uts_default_dams_cohort.min, by = "dam")

#check, how many females had cohort that was before collection?
Uts_default_dams_min.combine.check <- Uts_default_dams_min.combine %>%
  rowwise() %>% 
  mutate(respawn.check = ifelse(Respawner == 0 & firstcohort <= year, 1, 
                                ifelse(Respawner == 1 & firstcohort <= year-2, 1, 0)))

#reset firstcohort for Uts_default_dams_min.combine for individuals that had a few offspring in an earlier chort year
Uts_default_dams_min.combine.fix <- Uts_default_dams_min.combine.check %>%
  mutate(firstcohort = ifelse(respawn.check == 1, firstcohort+1, firstcohort))

#add fw age (smolt.scale.age) for individuals missing age
Uts_default_dams_fwage <- Uts_default_dams_min.combine.fix %>%
  mutate(Scale.smoltage = replace_na(Scale.smoltage, 4)) 
  
#caculate age at maturity
Uts_default_dam_cohort_AAM <- Uts_default_dams_fwage %>%  
  group_by(dam) %>%
  mutate(seaageatmaturity = ifelse(class.cor == "0+", firstcohort-birthyear.int-Scale.smoltage,  
                                   ifelse(class.cor == "1+", firstcohort-birthyear.int-Scale.smoltage,      
                                          ifelse(class.cor == "2-3+", firstcohort-birthyear.int-Scale.smoltage, 
                                                 ifelse(Respawner == "0" & class.cor == "Adult", InterpAge,
                                                        ifelse(Respawner == "1" & respawner.info == "1S1", 1, 
                                                               ifelse(Respawner == "1" & respawner.info == "2S1", 2,
                                                                      ifelse(Respawner == "1" & respawner.info == "3S1", 3, NA)))))))) 



#combine into cohorts and assign a cohort number
Uts_default_dams_cohort_assignment <- Uts_default_dam_cohort_AAM %>%
  ungroup() %>%
  rowwise(dam) %>%
  mutate(cohort = ifelse(cohort.year == firstcohort-1, 1, 
                      ifelse(cohort.year == firstcohort, 1,   
                         ifelse(cohort.year == firstcohort +1, 1, 
                                ifelse(cohort.year == firstcohort +2, 2, 
                                       ifelse(cohort.year == firstcohort +3, 2, 
                                              ifelse(cohort.year == firstcohort +4, 3, 
                                                     ifelse(cohort.year == firstcohort +5, 3,  
                                                            ifelse(cohort.year == firstcohort +6, 4,
                                                                   ifelse(cohort.year == firstcohort +7, 4, 
                                                                          ifelse(cohort.year == firstcohort +8, 5,
                                                                                 ifelse(cohort.year == firstcohort +9, 5, 
                                                                                        ifelse(cohort.year == firstcohort +10, 6,
                                                                                               ifelse(cohort.year == firstcohort +11, 6, NA))))))))))))))

#calculate the year for 1st-4th cohort based on the number of offspring ranked (descending)
#females have up to 4 cohorts
Uts_default_dams_cohort_year_rank <-Uts_default_dams_cohort_assignment %>%
 group_by(dam, cohort) %>%
     mutate(cohort.year.rank = ifelse(cohort == 1, dense_rank(desc(n.offspring)), 
                                      ifelse(cohort == 2, dense_rank(desc(n.offspring)), 
                                          ifelse(cohort ==3, dense_rank(desc(n.offspring)),
                                            ifelse(cohort == 4, dense_rank(desc(n.offspring)), NA)))))  

#issues with ties in dense_rank?
Uts_default_dams_cohort_year_rank %>% group_by(dam, cohort) %>% filter(cohort == "1") %>% count(cohort.year.rank) %>% filter(n > 1)
Uts_default_dams_cohort_year_rank %>% group_by(dam, cohort) %>% filter(cohort == "2") %>% count(cohort.year.rank) %>% filter(n > 1)
Uts_default_dams_cohort_year_rank %>% group_by(dam, cohort) %>% filter(cohort == "3") %>% count(cohort.year.rank) %>% filter(n > 1)
Uts_default_dams_cohort_year_rank %>% group_by(dam, cohort) %>% filter(cohort == "4") %>% count(cohort.year.rank) %>% filter(n > 1)
Uts_default_dams_cohort_year_rank %>% group_by(dam, cohort) %>% filter(cohort == "5") %>% count(cohort.year.rank) %>% filter(n > 1)

#fix issue with Uts_12_0y_119 - first cohort in 2014, 2nd in 2015
#fis issue with Uts_14A_024
Uts_default_dams_cohort_year_fix <- Uts_default_dams_cohort_year_rank %>%
      mutate(cohort.year.rank = ifelse(dam == "Uts_12_0y_119" & cohort.year == "2015", 2, cohort.year.rank)) %>%
      mutate(cohort.year.rank = ifelse(dam == "Uts_14A_024" & cohort.year == "2016", 3, cohort.year.rank))

#estimate the 2nd, 3rd and 4th cohort year
Uts_default_dams_cohort_year <- Uts_default_dams_cohort_year_fix %>%
  mutate(cohort.year.all = ifelse(cohort == "1" & cohort.year.rank == "1", cohort.year, 
                         ifelse(cohort == "2" & cohort.year.rank == "1", cohort.year, 
                             ifelse(cohort == "3" & cohort.year.rank == "1", cohort.year, 
                                 ifelse(cohort == "4" & cohort.year.rank == "1", cohort.year, NA))))) 

#fill down cohort.year
Uts_default_dams_cohort_year_fill <-Uts_default_dams_cohort_year %>% 
  group_by(dam, cohort) %>%
  fill(cohort.year.all)

#fix issues
#issue with Uts_11A_37, - cohort year not assigned to 2009
#issue with Uts_12_0y_119 - cohort year not assigned to 2014
#issue with Uts_13A_047 - cohort year not assigned to 2017
Uts_default_dams_cohort_year_fix <- Uts_default_dams_cohort_year_fill %>%
  mutate(cohort.year.all = ifelse(dam == "Uts_11A_37" & is.na(cohort.year.all), 2010, cohort.year.all)) %>%
  mutate(cohort.year.all = ifelse(dam == "Uts_12_0y_119" & is.na(cohort.year.all), 2014, cohort.year.all)) %>%
  mutate(cohort.year.all = ifelse(dam == "Uts_13A_047" & is.na(cohort.year.all), 2018, cohort.year.all))

#reset first cohort based on ranked 1st cohort
Uts_default_dams_cohort.min.fix <- Uts_default_dams_cohort_year_fix %>%
  group_by(dam) %>%
  mutate(firstcohort = ifelse(cohort == "1" & cohort.year.rank == "1", cohort.year.all, NA)) %>%
  fill(firstcohort) %>%
  mutate(firstcohort = ifelse(dam == "Uts_11A_37", 2010, firstcohort)) %>%
  fill(firstcohort)
         
#are there any where cohort.year.rank 1 =/ firstchort?
Uts_default_dams_cohort.min.fix %>% ungroup %>% filter(cohort == "1", cohort.year.rank == "1") %>% count(firstcohort == cohort.year)

#combine for cohorts
dam_default_cohort_year_combine <- Uts_default_dams_cohort.min.fix %>%
  group_by(dam, cohort, cohort.year.all) %>%
  summarise(across(11:15, sum))

#check distribution
dam_default_cohort_year_combine %>% ungroup() %>% summarise(across(4:8, sum))

#select row info from Uts_default_dams_cohort_year
dams_default_cohort_AAM_info <- Uts_default_dam_cohort_AAM %>%
  select(dam, type, class.cor, year, birthyear.int, Scale.smoltage, scale.seaage, InterpAge, Respawner, respawner.info, firstcohort, seaageatmaturity)

#select distinct row info from Uts_default_dams_cohort_AAM
dams_default_cohort_AAM_distinct <- Uts_default_dam_cohort_AAM %>%
  distinct(dam, type, class.cor, year, birthyear.int, Scale.smoltage, scale.seaage, InterpAge, Respawner, respawner.info, firstcohort, seaageatmaturity) %>%
  ungroup()

#join datasets
dam_default_cohort_total <- left_join(dams_default_cohort_AAM_distinct, dam_default_cohort_year_combine, by = "dam")

#tally cohorts
dam_default_cohort_total_count <- dam_default_cohort_total %>%
  group_by(dam) %>%
  count() %>%
  rename(cohort.total = n) 

#designate semelparous and iteroparous
dam_default_cohort_total_RESP <- dam_default_cohort_total_count %>%
  mutate(respawner.gen = ifelse(cohort.total > 1, "iteroparous", "semelparous")) %>%
  ungroup()

#combine sire and cohort counts
dam_default_cohort_all <- left_join(dam_default_cohort_total, dam_default_cohort_total_RESP, "dam")

#find minimum cohort #
dam_default_cohort_min.cohort <- dam_default_cohort_all %>%
  group_by(dam) %>%
  mutate(min.cohort = min(cohort))

#rank cohorts
dam_default_cohort_total_rank <- dam_default_cohort_min.cohort %>%
  group_by(dam) %>%
  mutate(cohort.rank = ifelse(min.cohort == 1, dense_rank(cohort),
                              ifelse(min.cohort == 2, dense_rank(cohort)+1, NA)))

#check total offspring
dam_default_cohort_total_rank %>% ungroup() %>% tally(n.offspring)

#UtsSNP just vips
UtsSNP_VIP <- UtsSNP %>%
  select(ID, AKAP11_4_fix, c25_1441_SAC)

#bring in SNP data
dam_default_cohort_SNP <- left_join(dam_default_cohort_total_rank, UtsSNP_VIP, c("dam" = "ID"))


####working with sires####
#summarize (count) all offspring per sire 
Uts_default_sires <- Uts_parentage_default %>%
  group_by(sire, class.cor.off, birthyear.int.off) %>%
  summarise_at(c("type.off"), funs(n.off = sum(!is.na(.))))

#pivot wider
Uts_default_sires_wide <- Uts_default_sires %>%
  pivot_wider(
    names_from = c(class.cor.off),
    names_sep = "_",
    values_from = n.off)

#remove rows where sire = NA, replace NA with 0
Uts_default_sires_wide_rmNA <- Uts_default_sires_wide %>%
  filter(sire != "NA")%>%
  replace(is.na(.), 0)

#sum individuals across rows
Uts_default_sires_total <- Uts_default_sires_wide_rmNA %>%
  rowwise() %>% 
  mutate(n.offspring = rowSums(across(c("0+", "1+", "2-3+", "Adult")))) %>%
  rename(cohort.year = birthyear.int.off)

#join datasets
Uts_default_sires_all <- left_join(Uts_default_sires_total, Uts_Birthyear_Calc_info, by = c("sire" = "ID")) %>%
  relocate(c(8:16), .after = sire) 

#combine into cohorts and assign a cohort number
Uts_default_sires_cohort.min <- Uts_default_sires_all %>%
  group_by(sire) %>%
  summarize(firstcohort = ifelse(type == "Adult" & Respawner == "0", min(cohort.year),
                                 ifelse(type == "Adult" & Respawner == "1", min(cohort.year), 
                                        ifelse(type == "Offspring", min(cohort.year), NA)))) %>%
  
  distinct()

#combine minimum cohort with Uts_defaultsires_MMP, fix respawner info 
Uts_default_sires_min.combine <- left_join(Uts_default_sires_all, Uts_default_sires_cohort.min, "sire")

#check, how many females had cohort that was before collection?
Uts_default_sires_min.combine.check <- Uts_default_sires_min.combine %>%
  rowwise() %>% 
  mutate(respawn.check = ifelse(Respawner == 0 & firstcohort <= year, 1, 
                                ifelse(Respawner == 1 & firstcohort <= year-2, 1, 0)))

#set respawner.info = NA for individuals that aren't respawners
Uts_default_sires_min.combine.fix <- Uts_default_sires_min.combine.check %>%
  mutate(respawner.info = ifelse(respawner.info == "1S1", respawner.info, NA)) 

#reset firstcohort for Uts_default_sires_min.combine for individuals that had a few offspring in an earlier chort year
#issue with Uts_11A_17, seaage = 3, no information on repeat spawning from scale, first cohort in 2010, defaultistent with 1S1 repeat spawner, 1 adult offspring
#issue with Uts_11A_40, seaage = 4, no information on repeat spawning from scale info, first cohort in 2007, defaultistent with 0S1 (mmp) or 1S1 if offspring aged incorrectly, 1 adult offspring
#issue with Uts_13A_014, seaage = 1, no information on repeat spawning from scale, first cohort in 2012, defaultistent with 0S1 (mmp) or 1S1 if offspring aged incorrectly, 1 1+ offspring
#issue with Uts_13A_024, seaage = 3, no information on repeat spawning from scale data, first  cohort 2012, defaultistent with 1S1, many offspring 
#issue with Uts_13A_043, seaage = 3, no information on repeat spawning from scale data, first cohort 2012, defaultistent with 1S1, many offspring
#issue with Uts_16A_041, seaage = 2, no information on repeat spawning from scale data, first cohort 2015, defaultistent with 0S1 (mmp), 2 offspring
#issue with Uts_17_071, seaage = 3, no information on repeat spawning from scale data, first cohort 2014, defaultistent with 0S1 (mmp), 1 offspring
Uts_default_sires_min.combine.fix2 <- Uts_default_sires_min.combine.fix %>%
     mutate(respawner.info = ifelse(sire == "Uts_11A_17", "1S1", 
                                 ifelse(sire == "Uts_13A_024", "1S1",
                                     ifelse(sire == "Uts_13A_043", "1S1", respawner.info)))) %>%
  mutate(Respawner = ifelse(sire == "Uts_11A_17", 1, 
                                 ifelse(sire == "Uts_13A_024", 1,
                                        ifelse(sire == "Uts_13A_043", 1, Respawner))))

#reset firstcohort for Uts_default_dams_min.combine for individuals that had a few offspring in an earlier cohort year                                                      
Uts_default_sires_min.combine.fix3 <- Uts_default_sires_min.combine.fix2 %>%
      mutate(firstcohort = ifelse(respawn.check == 1 & is.na(respawner.info), firstcohort+1, firstcohort))
 
#reset individuals noted above that may be reproducting as mature male parr
Uts_default_sires_min.combine.fix4 <- Uts_default_sires_min.combine.fix3 %>%
  mutate(firstcohort = ifelse(sire == "Uts_11A_40", firstcohort-1, 
                              ifelse(sire == "Uts_13A_014", firstcohort-1, 
                                     ifelse(sire == "Uts_16A_041", firstcohort-1, 
                                            ifelse(sire == "Uts_17_071", firstcohort-1, firstcohort)))))

#add fw age (smolt.scale.age) for individuals missing age
Uts_default_sires_fwage <- Uts_default_sires_min.combine.fix4 %>%
  mutate(Scale.smoltage = replace_na(Scale.smoltage, 4)) 

#calculate seaage at maturity for first cohort
Uts_default_sire_cohort_AAM <- Uts_default_sires_fwage %>%  
  group_by(sire) %>%
  mutate(seaageatmaturity = ifelse(class.cor == "0+", firstcohort-birthyear.int-Scale.smoltage,
                                   ifelse(class.cor == "1+", firstcohort-birthyear.int-Scale.smoltage,    
                                          ifelse(class.cor == "2-3+", firstcohort-birthyear.int-Scale.smoltage,
                                                 ifelse(Respawner == "0" & class.cor == "Adult", InterpAge,
                                                        ifelse(Respawner == "1" & respawner.info == "1S1", 1, NA))))))

#assign cohorts based on minimum cohort
Uts_default_sires_cohort_assignment <- Uts_default_sire_cohort_AAM %>%
  rowwise() %>%
  mutate(cohort = ifelse(cohort.year == firstcohort-1, 1, 
                         ifelse(cohort.year == firstcohort, 1,   
                                ifelse(cohort.year == firstcohort +1, 1, 
                                       ifelse(cohort.year == firstcohort +2, 2, 
                                              ifelse(cohort.year == firstcohort +3, 2, 
                                                     ifelse(cohort.year == firstcohort +4, 3, 
                                                            ifelse(cohort.year == firstcohort +5, 3,  
                                                                   ifelse(cohort.year == firstcohort +6, 4,
                                                                          ifelse(cohort.year == firstcohort +7, 4, 
                                                                              ifelse(cohort.year == firstcohort +8, 5,
                                                                                        ifelse(cohort.year == firstcohort +9, 5, 
                                                                                               ifelse(cohort.year == firstcohort +10, 6,
                                                                                                      ifelse(cohort.year == firstcohort +11, 6, NA))))))))))))))
#calculate the year for 1st-4th cohort based on the number of offspring ranked (descending)
#males have up to 5 cohorts
Uts_default_sire_cohort_year_rank <-Uts_default_sires_cohort_assignment %>%
  group_by(sire, cohort) %>%
  mutate(cohort.year.rank = ifelse(cohort == "1", dense_rank(desc(n.offspring)), 
                                   ifelse(cohort == "2", dense_rank(desc(n.offspring)), 
                                          ifelse(cohort =="3", dense_rank(desc(n.offspring)),
                                                 ifelse(cohort == "4", dense_rank(desc(n.offspring)), 
                                                        ifelse(cohort == "5", dense_rank(desc(n.offspring)), NA))))))  

#find issues with ties for ranking using dense rank
Uts_default_sire_cohort_year_rank %>% group_by(sire, cohort) %>% filter(cohort == "1") %>% count(cohort.year.rank) %>% filter(n > 1)
Uts_default_sire_cohort_year_rank %>% group_by(sire, cohort) %>% filter(cohort == "2") %>% count(cohort.year.rank) %>% filter(n > 1)
Uts_default_sire_cohort_year_rank %>% group_by(sire, cohort) %>% filter(cohort == "3") %>% count(cohort.year.rank) %>% filter(n > 1)
Uts_default_sire_cohort_year_rank %>% group_by(sire, cohort) %>% filter(cohort == "4") %>% count(cohort.year.rank) %>% filter(n > 1)
Uts_default_sire_cohort_year_rank %>% group_by(sire, cohort) %>% filter(cohort == "5") %>% count(cohort.year.rank) %>% filter(n > 1)

#estimate the 2nd, 3rd, 4th and 5th cohort year
Uts_default_sire_cohort_year <- Uts_default_sire_cohort_year_rank %>%
  mutate(cohort.year.all = ifelse(cohort == "1" & cohort.year.rank == "1", cohort.year, 
                                  ifelse(cohort == "2" & cohort.year.rank == "1", cohort.year, 
                                         ifelse(cohort == "3" & cohort.year.rank == "1", cohort.year, 
                                                ifelse(cohort == "4" & cohort.year.rank == "1", cohort.year, 
                                                    ifelse(cohort == "5" & cohort.year.rank == "1", cohort.year, NA)))))) 

#fill down cohort.year
Uts_default_sire_cohort_year_fill <-Uts_default_sire_cohort_year %>% 
  group_by(sire, cohort) %>%
  fill(cohort.year.all) 

#issue with Uts_11A_34, two rank 1s for 1st cohort, choose 2012 as cohort for individual in 2013
#issue with Uts_12_0y_483, two rank 1s for 1st cohort, chose 2013 as cohort 
#issue with Uts_15A_020, two ranks 1s for 1st cohort, choose 2012 as first cohort
#issue with Uts_16A_083, two rank 1s for 1st cohort, choose 2017 as cohort for individual in 2018
#issue with Uts_17A_049, two rank 1s for 1st cohort, choose 2018 as cohort for individual in 2017
#issue with UTs17A_071, two rank 1s for 1st cohort, choose rank 2 for cohort in 2014

Uts_default_sire_cohort_year_fix <- Uts_default_sire_cohort_year_fill %>%
  mutate(cohort.year.all = ifelse(sire == "Uts_11A_34" & cohort.year.all == 2013, 2012, cohort.year.all)) %>%
  mutate(cohort.year.all = ifelse(sire == "Uts_12_0y_483" & cohort.year.all == 2014, 2013, cohort.year.all)) %>%
  mutate(cohort.year.all = ifelse(sire == "Uts_15A_020" & cohort.year.all == 2014, 2015, cohort.year.all)) %>%
  mutate(cohort.year.all = ifelse(sire == "Uts_16A_083" & cohort.year.all == 2018, 2017, cohort.year.all)) %>%
  mutate(cohort.year.all = ifelse(sire == "Uts_17A_049" & cohort.year.all == 2017, 2018, cohort.year.all)) 

  
#issue with Uts_13A_056 offspring in 2018 should be same cohort as 2017
Uts_default_sire_cohort_year_fix2 <- Uts_default_sire_cohort_year_fix %>%
  mutate(cohort.year.rank = ifelse(sire == "Uts_13A_056" & cohort.year == 2018, 2, cohort.year.rank)) %>%
  mutate(cohort = ifelse(sire == "Uts_13A_056" & cohort == 3, 2, cohort)) 

#fix issue with UTs_18A_046, no cohort year, rank is assigned wrong
Uts_default_sire_cohort_year_fix3 <- Uts_default_sire_cohort_year_fix2 %>%
  mutate(cohort.year.all = ifelse(sire == "Uts_18A_046" & cohort.year == 2017, 2017, cohort.year.all)) %>%
  mutate(cohort.year.rank = ifelse(sire == "Uts_18A_046" & cohort.year == 2017, 1, cohort.year.rank)) %>%
  mutate(cohort.year.rank = ifelse(sire == "Uts_18A_046" & cohort.year == 2019, 2, cohort.year.rank)) 

#reset first cohort based on ranked 1st cohort
Uts_default_sire_cohort_min_fix4 <- Uts_default_sire_cohort_year_fix3 %>%
  group_by(sire) %>%
  mutate(firstcohort = ifelse(cohort == "1" & cohort.year.rank == "1", cohort.year.all, NA)) %>%
  fill(firstcohort) %>%
  mutate(firstcohort = ifelse(sire == "Uts_11A_17", 2010, 
                              ifelse(sire == "Uts_11A_40", 2007, 
                                     ifelse(sire == "Uts_13A_014", 2010, 
                                            ifelse(sire == "Uts_16A_068", 2010, firstcohort))))) %>%
  fill(firstcohort)

#add down rows for classes within cohorts
sire_default_cohort_year_combine <- Uts_default_sire_cohort_min_fix4 %>%
  group_by(sire, cohort, cohort.year.all) %>%
  summarise(across(11:15, sum))

#calculate mean % of classes /sire/cohort
sire_default_cohort_year_combine %>% ungroup() %>% summarise(across(4:8, sum))

#select row info from Uts_default_sire_cohort_AAM
sire_default_cohort_AAM_info <- Uts_default_sire_cohort_AAM %>%
  select(sire, type, class.cor, year, birthyear.int, Scale.smoltage, scale.seaage, InterpAge, Respawner, respawner.info, firstcohort, seaageatmaturity)

#select distinct row info from Uts_default_sire_cohort_AAM
sire_default_cohort_AAM_distinct <- Uts_default_sire_cohort_AAM %>%
  ungroup() %>%
  distinct(sire, type, class.cor, year, birthyear.int, Scale.smoltage, scale.seaage, InterpAge, Respawner, respawner.info, firstcohort, seaageatmaturity)

#join datasets
sire_default_cohort_total <- left_join(sire_default_cohort_AAM_distinct, sire_default_cohort_year_combine, by = "sire")

#check total offspring
sire_default_cohort_total %>% ungroup() %>% tally(n.offspring)

#tally cohorts
sire_default_cohort_total_count <- sire_default_cohort_total %>%
  group_by(sire) %>%
  count() %>%
  rename(cohort.total = n) 

#designate semelparous and iteroparous
sire_default_cohort_total_RESP <- sire_default_cohort_total_count %>%
  mutate(respawner.gen = ifelse(cohort.total > 1, "iteroparous", "semelparous")) %>%
  ungroup()

#combine sire and cohort counts
sire_default_cohort_all <- left_join(sire_default_cohort_total, sire_default_cohort_total_RESP, "sire")

#check total offspring
sire_default_cohort_all %>% ungroup() %>% tally(n.offspring)

#find minimum cohort #
sire_default_cohort_min.cohort <- sire_default_cohort_all %>%
  group_by(sire) %>%
  mutate(min.cohort = min(cohort))

#rank cohorts
sire_default_cohort_total_rank <- sire_default_cohort_min.cohort %>%
  group_by(sire) %>%
  mutate(cohort.rank = ifelse(min.cohort == 1, dense_rank(cohort),
                              ifelse(min.cohort == 2, dense_rank(cohort)+1, NA)))

#check total offspring
sire_default_cohort_total_rank %>% ungroup() %>% tally(n.offspring)

#calculate whether a mature male parr/adult assuming that 3 years is the maximum year for MMP before maturing as an adult
Uts_default_sires_MMP <- sire_default_cohort_total_rank %>%
  rowwise() %>%
  mutate(mature = ifelse(type == "Adult" & (cohort.year.all-birthyear.int) <= Scale.smoltage, "MMP",
                     ifelse(type == "Offspring" & (cohort.year.all-birthyear.int) <= 3, "MMP", "Adult")))

#find cases where possible males bred as a parr and adult
Uts_default_mature <- Uts_default_sires_MMP %>%
  ungroup() %>%
  rowwise(sire) %>% 
  filter(mature == "MMP", class.cor =="Adult")

#UtsSNP just vips
UtsSNP_VIP <- UtsSNP %>%
  select(ID, AKAP11_4_fix, c25_1441_SAC)

#bring in SNP data
sire_default_cohort_SNP <- left_join(Uts_default_sires_MMP, UtsSNP_VIP, c("sire" = "ID"))

####comparison of males and females####
#rename sire
sire_default_cohort_SNP_ID <-sire_default_cohort_SNP %>%
  rename(ID = sire) %>%
  mutate(sex = "sire") %>%
  relocate(sex, .after = ID) 

#rename dam
dam_default_cohort_SNP_ID <-dam_default_cohort_SNP %>%
  rename(ID = dam) %>%
  mutate(sex = "dam") %>%
  relocate(sex, .after = ID) 

#join data for males and females
Uts_default_cohort_SNP_default_1 <-bind_rows(sire_default_cohort_SNP_ID, dam_default_cohort_SNP_ID)  

#caculate cohort max
Uts_default_cohort_SNP_default_2 <- Uts_default_cohort_SNP_default_1 %>%
group_by(ID) %>%
  mutate(cohort.max = max(cohort.total)) %>%
  mutate(cohort.max.year = max(cohort.year.all)) %>%
  ungroup() 

write.csv(Uts_default_cohort_SNP_default_2, "Data/Uts_cohort_SNP_default_11.02.22.csv")

#dams
#check which are not correctly assigned
Uts_default_dams_cohortyear_assignment_check <- Uts_default_dams_cohort_year_fix %>%
  mutate(cohort.year.correct = if_else(cohort.year == cohort.year.all, n.offspring, 0)) %>%
  mutate(cohort.year.incorrect = if_else(cohort.year != cohort.year.all, n.offspring, 0))

#check which are not correctly assigned
Uts_default_sires_cohortyear_assignment_check <- Uts_default_sire_cohort_year_fix %>%
  mutate(cohort.year.correct = if_else(cohort.year == cohort.year.all, n.offspring, 0)) %>%
  mutate(cohort.year.incorrect = if_else(cohort.year != cohort.year.all, n.offspring, 0))

#check correct assignments (dams)
Uts_default_dams_cohortyear_assignment_check %>% ungroup() %>% tally(cohort.year.correct)

# check incorrect assignments (dams)
Uts_default_dams_cohortyear_assignment_check %>% ungroup() %>% tally(cohort.year.incorrect)

#check correct assignments (sires)
Uts_default_sires_cohortyear_assignment_check %>% ungroup() %>% tally(cohort.year.correct)

# check incorrect assignments (sires)
Uts_default_sires_cohortyear_assignment_check %>% ungroup() %>% tally(cohort.year.incorrect)

