#mature male calculations

library(tidyverse)

#datasets
Uts_cohort_SNP_default <- Uts_cohort_SNP_default_11.02.22
Uts_cohort_SNP_informed <- Uts_cohort_SNP_informed_11.02.22
Uts_cohort_SNP_conserved <- Uts_cohort_SNP_cons_11.02.22

#default dataset parentage dataset
#add down rows for classes within cohorts
Uts_cohort_SNP_default_MMP_RS <- Uts_cohort_SNP_default %>%
  filter(sex == "sire") %>%
  group_by(ID, type, mature) %>%
  summarise(across(n.offspring, sum)) 
#pivot wider
Uts_cohort_SNP_default_MMP_RS_wide <- Uts_cohort_SNP_default_MMP_RS %>%
  pivot_wider(
    names_from = c(mature),
    names_sep = "_",
    values_from = n.offspring) %>%
    ungroup()
  
# count adults and offspring as adults
Uts_cohort_SNP_default_MMP_RS_wide %>% group_by(type) %>% summarise(non_na_count = sum(!is.na(Adult)))

# count adults and offspring as MMP
Uts_cohort_SNP_default_MMP_RS_wide %>% group_by(type) %>% summarise(non_na_count = sum(!is.na(MMP))) 

# tally adults and offspring as adults
Uts_cohort_SNP_default_MMP_RS_wide %>% group_by(type) %>% tally(Adult)

# tally adults and offspring as MMP
Uts_cohort_SNP_default_MMP_RS_wide %>% group_by(type) %>% tally(MMP) 

#informed parentage dataset
#add down rows for classes within cohorts
Uts_cohort_SNP_informed_MMP_RS <- Uts_cohort_SNP_informed %>%
  filter(sex == "sire") %>%
  group_by(ID, type, mature) %>%
  summarise(across(n.offspring, sum)) 
#pivot wider
Uts_cohort_SNP_informed_MMP_RS_wide <- Uts_cohort_SNP_informed_MMP_RS %>%
  pivot_wider(
    names_from = c(mature),
    names_sep = "_",
    values_from = n.offspring) %>%
  ungroup()

# count adults and offspring as adults
Uts_cohort_SNP_informed_MMP_RS_wide %>% group_by(type) %>% summarise(non_na_count = sum(!is.na(Adult)))

# count adults and offspring as MMP
Uts_cohort_SNP_informed_MMP_RS_wide %>% group_by(type) %>% summarise(non_na_count = sum(!is.na(MMP))) 

# tally adults and offspring as adults
Uts_cohort_SNP_informed_MMP_RS_wide %>% group_by(type) %>% tally(Adult)

# tally adults and offspring as MMP
Uts_cohort_SNP_informed_MMP_RS_wide %>% group_by(type) %>% tally(MMP) 


#conservative parentage dataset
#add down rows for classes within cohorts
Uts_cohort_SNP_conserved_MMP_RS <- Uts_cohort_SNP_conserved %>%
  filter(sex == "sire") %>%
  group_by(ID, type, mature) %>%
  summarise(across(n.offspring, sum)) 
#pivot wider
Uts_cohort_SNP_conserved_MMP_RS_wide <- Uts_cohort_SNP_conserved_MMP_RS %>%
  pivot_wider(
    names_from = c(mature),
    names_sep = "_",
    values_from = n.offspring) %>%
  ungroup()

# count adults and offspring as adults
Uts_cohort_SNP_conserved_MMP_RS_wide %>% group_by(type) %>% summarise(non_na_count = sum(!is.na(Adult)))

# count adults and offspring as MMP
Uts_cohort_SNP_conserved_MMP_RS_wide %>% group_by(type) %>% summarise(non_na_count = sum(!is.na(MMP))) 

# tally adults and offspring as adults
Uts_cohort_SNP_conserved_MMP_RS_wide %>% group_by(type) %>% tally(Adult)

# tally adults and offspring as MMP
Uts_cohort_SNP_conserved_MMP_RS_wide %>% group_by(type) %>% tally(MMP) 
