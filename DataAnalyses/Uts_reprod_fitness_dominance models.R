#additive and dominance models of vgll3 and sea age on reproductive success.
#Additive genotypes effects were coded as vgll3*EE = 1, vgll3*EL = 0, vgll3*LL = −1, Additive Dominance genotypes effects were coded as vgll3*EE = 0, vgll3*EL = 1, vgll3*LL = 0.
#binomial GLM models with log link for each sex seperately 

####packages####
library (tidyverse)
library (lme4)
library (lmerTest)
library (MASS)
library (DHARMa)

####database####
Uts_cohort_SNP_conserved <- read.csv("Data/Uts_cohort_SNP_cons_11.02.22.csv")

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

#code additive models
Uts_conserved_total_RS_add <- Uts_conserved_total_RS %>%
  mutate(vgll3_additive = ifelse(c25_1441_SAC == 1, 1,
                                 ifelse(c25_1441_SAC == 2, 0,
                                        ifelse(c25_1441_SAC == 3, -1, NA))))

#code dominance models
Uts_conserved_total_RS_dom <- Uts_conserved_total_RS_add %>%
  mutate(vgll3_dominance = ifelse(c25_1441_SAC == 1, 0,
                                  ifelse(c25_1441_SAC == 2, 1,
                                         ifelse(c25_1441_SAC == 3, 0, NA))))

#split into dams/sires
#dams
Uts_conserved_total_RS_dom_dams <-Uts_conserved_total_RS_dom %>%
  filter(sex == "dam")
#sires
Uts_conserved_total_RS_dom_sires <-Uts_conserved_total_RS_dom %>%
  filter(sex == "sire")

#dams
mod1dams_dom <- glm.nb(n.offspring ~ vgll3_additive + vgll3_dominance + seaageatmaturity, link = log, data=Uts_conserved_total_RS_dom_dams)
summary(mod1dams_dom)
#simulate residuals
resmod1additivedams <- simulateResiduals(mod1dams_dom, plot = T)

#sires
#vgll3 additive sires NB
mod1sires_dom <- glm.nb(n.offspring ~ vgll3_additive + vgll3_dominance + seaageatmaturity, link = log, data=Uts_conserved_total_RS_dom_sires)
summary(mod1sires_dom)
#simulate residuals
resmod1additivesires <- simulateResiduals(mod1sires_dom, plot = T)

####models for first cohort only####
#remove individuals that have cohorts before 2012, and after 2017
Uts_cohort_SNP_conserved_1C <- Uts_cohort_SNP_conserved %>%
  filter(cohort.rank == 1) %>%
  filter(firstcohort != 2007) %>%
  filter(firstcohort != 2009) %>%
  filter(firstcohort != 2010) %>%
  filter(firstcohort != 2011) %>% 
  filter(firstcohort != 2018) %>% 
  filter(firstcohort != 2019) %>% 
  group_by(ID, sex, type, class.cor, firstcohort, seaageatmaturity, c25_1441_SAC) %>%
     summarise(across(n.offspring, sum))

#code additive models
Uts_conserved_total_RS_add1C <- Uts_cohort_SNP_conserved_1C %>%
  mutate(vgll3_additive = ifelse(c25_1441_SAC == 1, 1,
                                 ifelse(c25_1441_SAC == 2, 0,
                                        ifelse(c25_1441_SAC == 3, -1, NA))))

#code dominance models
Uts_conserved_total_RS_dom1C <- Uts_conserved_total_RS_add1C %>%
  mutate(vgll3_dominance = ifelse(c25_1441_SAC == 1, 0,
                                  ifelse(c25_1441_SAC == 2, 1,
                                         ifelse(c25_1441_SAC == 3, 0, NA))))

#split into different sexes
#dams
Uts_conserved_total_RS_dams_1C <- Uts_conserved_total_RS_dom1C %>%
  filter(sex == "dam") 
#sires  
Uts_conserved_total_RS_sires_1C <- Uts_conserved_total_RS_dom1C %>%
  filter(sex == "sire") 

#dams
mod1dams_dom1C <- glm.nb(n.offspring ~ vgll3_additive+ vgll3_dominance+seaageatmaturity, link = log, data=Uts_conserved_total_RS_dams_1C)
summary(mod1dams_dom1C)

#simulate residuals
resmod1additivedams1C <- simulateResiduals(mod1dams_dom1C, plot = T)

#sires
#vgll3 additive sires NB
mod1sires_dom1C <- glm.nb(n.offspring ~ vgll3_additive + vgll3_dominance + seaageatmaturity, link = log, data=Uts_conserved_total_RS_sires_1C)
summary(mod1sires_dom1C)

#simulate residuals
resmod1additivesires1C <- simulateResiduals(mod1sires_dom1C, plot = T)


