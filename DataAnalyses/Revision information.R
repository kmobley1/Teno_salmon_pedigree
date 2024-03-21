#R1 revision information
####libraries####
library(tidyverse)
library(cowplot)
library (DHARMa)

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
Uts_conserved_mated %>% filter(sex == "dam") %>% distinct(ID) %>% tally()
#how many unique females)

Utsadults %>% filter(sex == "F") %>% distinct(ID) %>% tally()
#how many females reproduced then captured?
56/93


#how many adult to adult genotypes do we have in the dataset (all data)
Uts_cohort_SNP_conserved %>% filter(Adult > 0) %>% count()

#what is the year range of adult to adult genotypes (all data)
Uts_cohort_SNP_conserved %>% filter(Adult > 0) %>% group_by(year, c25_1441_SAC) %>% count()

#what is the number of offspring from adult to adult genotypes (all data)
Uts_cohort_SNP_conserved %>% filter(Adult > 0) %>% tally(Adult)

#how many adult to adult to adult genotypes do we have in the restricted dataset
Uts_conserved_mated %>% filter(Adult > 0) %>% count()

#what is the year range of adult to adult genotypes (all data)
Uts_conserved_mated  %>% filter(Adult > 0 & type == "Adult") %>% group_by(year, c25_1441_SAC) %>% count()

#what is the number of offspring from adult to adult genotypes (all data)
Uts_conserved_mated %>% filter(Adult > 0) %>% tally(Adult)

#try running models on sires and dams with adult to adult genotypes
#split into different sexes
#dams
Uts_conserved_mated_RS_dams <- Uts_conserved_mated %>%
  filter(sex == "dam", Adult > 0 & type == "Adult") 
#sires  
Uts_conserved_mated_RS_sires <- Uts_conserved_mated %>%
  filter(sex == "sire", Adult > 0 & type == "Adult") 

#compare negative bionomial, poisson and quasipoisson models for total reproductive success for dams
#vgll3 additive dams negative binomial
mod1damsadults <- glm.nb(n.offspring ~ factor(c25_1441_SAC) + seaageatmaturity, link = log, data=Uts_conserved_mated_RS_dams)
summary(mod1damsadults)
#simulate residuals
resmod1damsadults <- simulateResiduals(mod1damsadults, plot = T)

#vgll3 additive dams negative binomial
mod1siresadults <- glm.nb(n.offspring ~ factor(c25_1441_SAC) + seaageatmaturity, link = log, data=Uts_conserved_mated_RS_sires)
summary(mod1siresadults)
#simulate residuals
resmod1siresadults <- simulateResiduals(mod1siresadults, plot = T)

#how many offspring are in the different size classes 
#0+
Uts_cohort_SNP_conserved %>% tally(X0.) 
#1+
Uts_cohort_SNP_conserved %>% tally(X1.) 
#2-3+
Uts_cohort_SNP_conserved %>% tally(X2.3.) 
#adult
Uts_cohort_SNP_conserved %>% tally(Adult) 

#0+	    8068	78.9%
#1+	    1680	16.4%
#2-3+	   387	 3.8%
#adult	  95	 0.9%
#total 10230 100%





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
