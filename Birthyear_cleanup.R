#Hatch year calculations and cleanup (metadata)
#kenyon mobley

####packages####
library (tidyr)

#files
UtsSNP <- read.csv("/data models/UtsSNP_21.04.13.csv")
Utsadults <- read.csv("/data models/UtsadultsALL_21.06.22.csv")

#add data from SNPdataset
SNPdata <- UtsSNP %>%
  select(ID, sex, type, class.cor, year, BirthYear)

#get info for ID, FW age, Seaage and respawner from adults
Adultsdata <- Utsadults %>%
  select(ID, scale.seaage, Scale.smoltage, InterpAge, Respawner, respawner.info) 

#join datasets
Uts_Birthyear <- left_join(SNPdata, Adultsdata, by ="ID") 

#calculate total age
Uts_Birthyear_TA <- Uts_Birthyear %>%
  mutate(total.age =  ifelse(class.cor == "0+", 0, 
                                 ifelse(class.cor == "1+", 1, 
                                        ifelse(class.cor == "2-3+", 2, 
                                               ifelse(class.cor == "Adult", Scale.smoltage + InterpAge, NA))))) 

#calculate total age assuming the mean smolt age (4) for individuals missing smolt age
Uts_Birthyear_TA2 <- Uts_Birthyear_TA %>%
  mutate(total.age.int =  ifelse(is.na(total.age), 4 + InterpAge, total.age))

#calculate hatch year = year collected - total.age  
Uts_Birthyear_Calc <- Uts_Birthyear_TA2 %>%
  mutate(birthyear.int = year - total.age.int) 

#write file
write.csv(Uts_Birthyear_Calc, "C:/Users/kmo107/OneDrive - UiT Office 365/Documents/projects/Atlantic salmon - Teno River Pedigree/2020 - Utsjoki pedigree data/Utsjoki-salmon-pedigree/data models/Uts_Birthyear_Calc_21_06_22.csv")
