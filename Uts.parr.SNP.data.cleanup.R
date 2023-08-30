#Utsjoki parr SNP data

#libraries
library(tidyverse)

#dataset
juv.location.SNP <- read.csv("C:/Users/kmo107/OneDrive - UiT Office 365/Documents/projects/Atlantic salmon - Teno River Pedigree/2020 - Utsjoki pedigree data/Teno_salmon_pedigree/juv.location.SNP_22.10.21.csv")

#cleanup
#add column class.cor
Uts.parr.SNP <- juv.location.SNP %>%
  mutate(juv.location.SNP, class.cor =  ifelse(class == "2y", "2-3y", 
                                                 ifelse(class == "3y", "2-3y", 
                                                        ifelse(class == "2pp", "2-3y",  
                                                               ifelse(class == "2-3y", "2-3y",  
                                                                      ifelse(class == "1y", "1y",  
                                                                             ifelse(class == "0y", "0y", NA))))))) %>%
  relocate(class.cor, .after = class) %>%
  relocate(year, .after = ID) %>%
  select(-X.1, -year.x, -year.y, -class.x, -class.y, -class.og)

#write file
write.csv(Uts.parr.SNP, "C:/Users/kmo107/OneDrive - UiT Office 365/Documents/projects/Atlantic salmon - Teno River Pedigree/2020 - Utsjoki pedigree data/Teno_salmon_pedigree/Uts.parr.SNP_23.06.09.csv")
