#parentage dataset cleanup informed
#default = all age difference priors as estimated by sequoia
#informed = priors for age gap of 0 and 1 for males, and 0, 1, 2 and 3 for females set to 0
#conservative = all priors less than 0.1 set to zero, to exclude all of the most improbable relationships

####packages####
library (tidyverse)

#datasets
UtsSNP <- read.csv("Data/UtsSNP_21.04.13.csv")
Utsadults <- read.csv("Data/UtsadultsALL_21.06.22.csv")
Uts_Birthyear_Calc <- read.csv("Data/Uts_Birthyear_Calc_21_06_22.csv")
Uts_informed_parents_new <- read.csv("Data/uts_informed.prior1.parents.2021-06-18.csv")

#import basic info including birth year (int) and respawner data
Uts_Birthyear.info <-Uts_Birthyear_Calc %>%
  select(ID, sex, type, class.cor, year, BirthYear, birthyear.int, InterpAge, Respawner, respawner.info)

#combine info for all individuals
Uts_parentage_informed_new1 <- left_join(Uts_informed_parents_new, Uts_Birthyear.info, by = c("id" = "ID")) %>%
  rename(ID = id) %>%
  relocate(c(10:17), .after = ID)

#join data for dams
Uts_parentage_informed_new_dams <- left_join(Uts_parentage_informed_new1, Uts_Birthyear.info, by = c("dam" = "ID")) %>%
  relocate(c(18:24), .after = dam) %>%
  rename(sex.dam = sex.y) %>%
  rename(type.dam = type.y) %>%
  rename(class.cor.dam = class.cor.y) %>%
  rename(year.dam = year.y) %>% 
  rename(BirthYear.dam = BirthYear.y) %>%
  rename(birthyear.int.dam = birthyear.int.y) %>%
  rename(Respawner.dam = Respawner.y)
  
#join data for sires
Uts_parentage_informed_new_dams_sires <- left_join(Uts_parentage_informed_new_dams, Uts_Birthyear.info, by = c("sire" = "ID")) %>%
  relocate(c(25:31), .after = sire) %>%
  rename(sex.sire = sex) %>%
  rename(type.sire = type) %>%
  rename(class.cor.sire = class.cor) %>%
  rename(year.sire = year) %>% 
  rename(BirthYear.sire = BirthYear) %>%
  rename(birthyear.int.sire = birthyear.int) %>%
  rename(Respawner.sire = Respawner)
   
#fix names for offspring
Uts_parentage_informed_new_all <- Uts_parentage_informed_new_dams_sires %>%
  rename(sex.off = sex.x) %>%
  rename(type.off = type.x) %>%
  rename(class.cor.off = class.cor.x) %>%
  rename(year.off = year.x) %>% 
  rename(BirthYear.off = BirthYear.x) %>%
  rename(birthyear.int.off = birthyear.int.x) %>%
  rename(Respawner.off = Respawner.x)

#write file
write.csv(Uts_parentage_informed_new_all, "Data/Uts_parentage_informed_21.06.22.csv")

 






