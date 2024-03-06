#R1 revision information
####libraries####
library(tidyverse)
library(cowplot)

#datasets####
Utsadults <- read.csv("Data/UtsadultsALL_21.06.22.csv")
Uts_cohort_SNP_conserved <- read.csv("Data/Uts_cohort_SNP_cons_11.02.22.csv")

####caculate %reproduced
Uts_conserved_mated <- Uts_cohort_SNP_conserved %>%
  filter(firstcohort != 2007) %>%
  filter(firstcohort != 2009) %>%
  filter(firstcohort != 2010) %>%
  filter(firstcohort != 2011) %>% 
  filter(firstcohort != 2018) %>% 
  filter(firstcohort != 2019) 
  
#how many unique females mated?
Uts_cohort_mated %>% filter(sex == "dam") %>% distinct(ID) %>% tally()
#how many unique females)
Utsadults %>% filter(sex == "F") %>% distinct(ID) %>% tally()
#how many females reproduced then captured?
56/93






####total reproductive success for all individuals across all cohorts####
#add down rows for classes within cohorts
Uts_conserved_total_RS <- Uts_cohort_SNP_conserved %>%
  filter(firstcohort != 2007) %>%
  filter(firstcohort != 2009) %>%
  filter(firstcohort != 2010) %>%
  filter(firstcohort != 2011) %>% 
  filter(firstcohort != 2018) %>% 
  filter(firstcohort != 2019) %>%
  group_by(ID, sex, type, class.cor, firstcohort, seaageatmaturity, c25_1441_SAC) %>%
  summarise(across(n.offspring, sum))