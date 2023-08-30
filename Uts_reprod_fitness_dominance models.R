#additive and dominance models

####packages####
library (tidyverse)
library (ggridges)
library (cowplot)
library (lme4)
library (lmerTest)
library (MASS)
library (DHARMa)

####database####
Uts_parentage_conserved <- Uts_parentage_conserved.21.06.18
Uts_cohort_SNP_conserved <- Uts_cohort_SNP_cons_10.11.21
UtsSNP <- UtsSNP_21.04.13
Utsadults <- UtsadultsALL_21.06.22
Uts_Birthyear_Calc <-Uts_Birthyear_Calc_21_06_22

#colorblind palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7","#999999", "#F0E442")

####basic stats ####
#fix dataset, sex.dam = False
Uts_parentage_conserved <- Uts_parentage_conserved %>%
  mutate(sex.dam = replace(sex.dam, sex.dam == "FALSE", "F"))

####total reproductive success for all individuals across all cohorts####
#remove 

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

#vgll3 total offspring additive 
mod1dom<- glm.nb(n.offspring ~ vgll3_additive*sex + vgll3_dominance*sex + seaageatmaturity, link=log, data=Uts_conserved_total_RS_dom)
summary(mod1dom)
#simulate residuals
resmod1mod1dom <- simulateResiduals(mod1dom, plot = T)

#split into different sexes
mod1dams_dom <- glm.nb(n.offspring ~ vgll3_additive+ vgll3_dominance+seaageatmaturity, link = log, data=Uts_conserved_total_RS_dom_dams)
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
  filter(firstcohort != 2019)

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
Uts_conserved_total_RS_dams_1C <- Uts_conserved_total_RS_domIC %>%
  filter(sex == "dam") 
#sires  
Uts_conserved_total_RS_sires_1C <- Uts_conserved_total_RS_domIC %>%
  filter(sex == "sire") 

#vgll3 total offspring additive 
mod1dom1C<- glm.nb(n.offspring ~ vgll3_additive*sex + vgll3_dominance*sex + seaageatmaturity, link=log, data=Uts_conserved_total_RS_dom1C)
summary(mod1dom1C)
#simulate residuals
resmod1mod1dom1C <- simulateResiduals(mod1dom1C, plot = T)

#split into different sexes
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



####test for dominance in seaage v vgll3 models####
#split into different sexes
mod1d_seaagevgll3_dom <- lm(seaageatmaturity ~ vgll3_additive + vgll3_dominance + sex, data=Uts_conserved_total_RS_dom)
summary(mod1d_seaagevgll3_dom)
#simulate residuals
resmodseaagevgll3 <- simulateResiduals(mod1d_seaagevgll3_dom, plot = T)

#sires
#vgll3 additive sires NB
mod1sires_seaagevgll3 <- glm.nb(seaageatmaturity ~ vgll3_additive + vgll3_dominance, link = log, data=Uts_conserved_total_RS_dom_sires)
summary(mod1sires_dom1C)
#simulate residuals
resmod1sires_seaagevgll3 <- simulateResiduals(mod1sires_seaagevgll3, plot = T)


(mod1d_seaagevgll3_dom  <- glmmTMB(seaageatmaturity ~ vgll3_additive*sex + vgll3_dominance*sex,
                                    family=nbinom1, ziformula = ~0, data=Uts_conserved_total_RS_dom))

condition <- lmer(condition ~ VGLL3TOP_add + Sex + (1|Pool) + (1|mother) + (1|father), data=smolt_1row)


####

#vgll3 models 
mod_seaagevgll3 <- glm(seaageatmaturity ~ sex + c25_1441_SAC, data=Uts_conserved_total_RS_dom)
summary(mod_seaagevgll3)
#simulate residuals
resmod_seaagevgll3 <- simulateResiduals(mod_seaagevgll3, plot = T)

#vgll3 additive models NB
mod1d_seaagevgll3_add <- glm(seaageatmaturity ~ vgll3_additive * sex, data=Uts_conserved_total_RS_dom)
summary(mod1d_seaagevgll3_add)
#simulate residuals
resmod1d_seaagevgll3_add <- simulateResiduals(mod1d_seaagevgll3_add, plot = T)

#vgll3 dominance models NB
mod1d_seaagevgll3_dom <- glm(seaageatmaturity ~ vgll3_dominance * sex, data=Uts_conserved_total_RS_dom)
summary(mod1d_seaagevgll3_dom)
#simulate residuals
resmod1d_seaagevgll3_dom <- simulateResiduals(mod1d_seaagevgll3_dom, plot = T)






#vgll3 additive + dominance models NB
mod1d_seaagevgll3_dom0 <- glm(seaageatmaturity ~ vgll3_additive*sex + vgll3_dominance*sex, data=Uts_conserved_total_RS_dom1C)
summary(mod1d_seaagevgll3_dom0)
#simulate residuals
resmod1d_seaagevgll3_dom0 <- simulateResiduals(mod1d_seaagevgll3_dom0, plot = T)

#vgll3 additive models NB
mod1d_seaagevgll3_add <- glm(seaageatmaturity ~ vgll3_additive * sex, data=Uts_conserved_total_RS_dom1C)
summary(mod1d_seaagevgll3_add)
#simulate residuals
resmod1d_seaagevgll3_add <- simulateResiduals(mod1d_seaagevgll3_add, plot = T)

#vgll3 dominance models NB
mod1d_seaagevgll3_dom <- glm(seaageatmaturity ~ vgll3_dominance * sex, data=Uts_conserved_total_RS_dom1C)
summary(mod1d_seaagevgll3_dom)
#simulate residuals
resmod1d_seaagevgll3_dom <- simulateResiduals(mod1d_seaagevgll3_dom, plot = T)

####


#vgll3 additive sires poisson 
mod1d_seaagevgll3_dom1 <- glm(formula = seaageatmaturity ~ vgll3_additive + vgll3_dominance + sex, family = "poisson", data=Uts_conserved_total_RS_dom)
summary(mod1d_seaagevgll3_dom1)
#simulate residuals
resmod1d_seaagevgll3_dom1 <- simulateResiduals(mod1d_seaagevgll3_dom1, plot = T)

#qvgll3 additive sires quasi-poisson 
mod1d_seaagevgll3_dom2 <- glm(formula = seaageatmaturity ~ vgll3_additive + vgll3_dominance + sex, family = "quasipoisson", data=Uts_conserved_total_RS_dom)
summary(mod1d_seaagevgll3_dom2)
#simulate residuals
resmod1d_seaagevgll3_dom2 <- simulateResiduals(mod1d_seaagevgll3_dom2, plot = T)


