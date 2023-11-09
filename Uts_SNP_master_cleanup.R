#cleanup and combine SNP dataset with parentage, adult info, and sex

#packages
library(tidyverse)

#datasets
UtsSNPraw <- read.csv("Data/UtsSNPMasterDataKM_20.11.24.csv")
Utsadults <- read.csv("Data/UtsadultsALL_21.06.22.csv")
Uts_lifehistory <- read.csv("Data/2021-02-18.uts_lifehist.csv") ##life history data (from Henry)

####fix class####
#convert class to class.cor (e.g. 0+, 1+ and 2-3+) in SNPMasterDataKM file
UtsSNPclass.cor <- UtsSNPraw %>%
  mutate(UtsSNPraw, class.cor =  ifelse(class == "adult", "Adult", 
                                                          ifelse(class == "2y", "2-3+", 
                                                                 ifelse(class == "3y", "2-3+", 
                                                                        ifelse(class == "2pp", "2-3+",  
                                                                               ifelse(class == "2-3y", "2-3+",  
                                                                                      ifelse(class == "1y", "1+",  
                                                                                             ifelse(class == "0y", "0+", NA)))))))) %>%
  relocate(class.cor, .after = class)

#fix sex to M/F
UtsSNPclass.cor <-mutate(UtsSNPclass.cor, sex = ifelse(SDY_ion2 %in% 0, "F",
                                                       ifelse(SDY_ion2 %in% 1, "M", NA))) %>%
  relocate(sex, .after = class.cor)

#create new columns for VIP SNPs and rename them
#UtsadultsALLSNP %>% 
##yearly trends in VIP snp allele frequency, note that for AKAP11 and vgll3mis1 are reversed (1 = LL) all others 1 = EE
##mutate Akap11 and vgll3mis1 into correct order

##first Akap11
UtsSNPgene.cor <- mutate(UtsSNPclass.cor, AKAP11_4_fix = ifelse(AKAP11_4 %in% 2, 2,
                                                               ifelse(AKAP11_4 %in% 1, 3, 1))) %>%
  relocate(AKAP11_4_fix, .after =AKAP11_4) 

#vgll3mis1
UtsSNPgene.cor <-mutate(UtsSNPgene.cor, vgll3mis1_fix = ifelse(UtagF_SS_147a %in% 2, 2,
                                                               ifelse(UtagF_SS_147a %in% 1, 3, 1))) %>%
  relocate(vgll3mis1_fix, .after =UtagF_SS_147a) 

#join datasets
UtsSNP <- left_join(UtsSNPgene.cor, Uts_lifehistory, by = "ID") %>%
  relocate(BirthYear, .after =year) %>%
  select(-Sex)

#write file
write.csv(UtsSNP, "Data/UtsSNP_21.04.13.csv")








