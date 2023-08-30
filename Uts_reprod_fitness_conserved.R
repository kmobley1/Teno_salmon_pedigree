#parentage dataset 
#default = all age difference priors as estimated by sequoia
#informed = priors for age gap of 0 and 1 for males, and 0, 1, 2 and 3 for females set to 0
#conservative = all priors less than 0.1 set to zero, to exclude all of the most improbable relationships

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
Uts_cohort_SNP_conserved <- Uts_cohort_SNP_cons_11.02.22
UtsSNP <- UtsSNP_21.04.13
Utsadults <- UtsadultsALL_21.06.22
Uts_Birthyear_Calc <-Uts_Birthyear_Calc_21_06_22

#colorblind palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7","#999999", "#F0E442")

####basic stats ####
#fix dataset, sex.dam = False
Uts_parentage_conserved <- Uts_parentage_conserved %>%
  mutate(sex.dam = replace(sex.dam, sex.dam == "FALSE", "F"))

# adults vs offspring in conservative dataset
Uts_parentage_conserved %>%
  group_by(type.off) %>%
    tally()

#sires in conservative dataset
Uts_parentage_conserved %>%
  group_by(sex.sire) %>%
  tally()

#sires in conservative dataset
Uts_parentage_conserved %>%
  group_by(sex.dam) %>%
  tally() 

#adults in conservative dataset
Uts_parentage_conserved %>%
  group_by(sex.off, type.off) %>%
  tally() 

#find offspring that have both parents assigned
Uts_parentage_conserved_both <- Uts_parentage_conserved %>%
  filter(sex.sire == "M" | sex.dam == "F")

#both in conservative dataset
Uts_parentage_conserved_both %>%
  group_by(type.off) %>%
  tally()
#how many total offspring
Uts_parentage_conserved_both %>%
  tally()

####offspring classes assigned to sires/dams
#dams
Uts_parentage_conserved_classfreqF <- Uts_parentage_conserved %>%
  group_by(sex.dam, class.cor.off) %>%
  filter(!is.na(sex.dam)) %>%
  rename(sex = sex.dam) %>%
  tally() 
  
#sires
Uts_parentage_conserved_classfreqM <- Uts_parentage_conserved %>%
  group_by(sex.sire, class.cor.off) %>%
  filter(!is.na(sex.sire)) %>%
  rename(sex = sex.sire) %>%
  tally() 

#merge datasets
Uts_Parentage_classfreq <- bind_rows(Uts_parentage_conserved_classfreqF, Uts_parentage_conserved_classfreqM )

#count offspring
Uts_Parentage_classfreq %>% tally(n)

#calculate % for each sex
Uts_Parentage_class_percent <- Uts_Parentage_classfreq %>%
  group_by(sex) %>%
  mutate(total = sum(n)) %>%
  mutate(percent = n/total*100)

#create grouped bar plot of #collected individuals/year/class
p.classfreq <-  ggplot(Uts_Parentage_classfreq, aes(y=n, x=class.cor.off, fill = sex)) +geom_bar(stat = "identity", position = "dodge") +
  labs(x="Age class", y="No. individuals") + 
  scale_fill_manual(values=cbPalette, name = "", labels = c("Dam", "Sire")) +
  theme(plot.title=element_text(size=36, hjust=-0.5), legend.title=element_text(size=20), legend.text=element_text(size=20)) +
  theme(axis.title.x = element_text(size=24)) +  theme(axis.text.x = element_text(size=20, color="black")) +
  theme(axis.title.y = element_text(size=24, vjust=3, angle = 90)) +  theme(axis.text.y = element_text(size=20, color="black")) +
  theme(axis.line = element_line(size = 1)) + theme(axis.ticks = element_line(size=1)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.margin=unit(c(1,0,0,1), "cm"))
#show graph
p.classfreq

#reformat table
class.freq.wide <- Uts_Parentage_classfreq %>%
  pivot_wider(
    names_from = c(sex),
    names_sep = "_",
    values_from = n) 

#fix row names 
class.freq.wide2 <- class.freq.wide %>% remove_rownames %>% column_to_rownames(var="class.cor.off")
  
#chi square  
chisq.test(class.freq.wide2) 
  
####total reproductive success for all individuals across all cohorts####
#remove 

#add down rows for classes within cohorts
##remove individuals that have cohorts before 2012, and after 2017
Uts_conserved_total_RS <- Uts_cohort_SNP_conserved %>%
  filter(firstcohort != 2007) %>%
  filter(firstcohort != 2009) %>%
  filter(firstcohort != 2010) %>%
  filter(firstcohort != 2011) %>% 
  filter(firstcohort != 2018) %>% 
  filter(firstcohort != 2019) %>%
  group_by(ID, sex, type, birthyear.int, class.cor, firstcohort, cohort.total, cohort.max, respawner.gen, cohort.max.year, seaageatmaturity, c25_1441_SAC) %>%
  summarise(across(n.offspring, sum))

#split into different sexes
#dams
Uts_conserved_total_RS_dams <- Uts_conserved_total_RS %>%
  filter(sex == "dam") 
#sires  
Uts_conserved_total_RS_sires <- Uts_conserved_total_RS %>%
  filter(sex == "sire") 

#vgll3 additive dams negative binomial
mod1dams <- glm.nb(n.offspring ~ factor(c25_1441_SAC) + seaageatmaturity, link = log, data=Uts_conserved_total_RS_dams)
summary(mod1dams)
#simulate residuals
resmod1dams <- simulateResiduals(mod1dams, plot = T)

#vgll3 additive dams poisson 
mod2dams <- glm(formula = n.offspring ~ factor(c25_1441_SAC) * seaageatmaturity, family = "poisson", data=Uts_conserved_total_RS_dams)
summary(mod2dams)
#simulate residuals
resmod2dams <- simulateResiduals(mod2dams, plot = T)

#qvgll3 additive dams quasi-poisson 
mod3dams <- glm(formula = n.offspring ~ factor(c25_1441_SAC) * seaageatmaturity, family = "quasipoisson", data=Uts_conserved_total_RS_dams)
summary(mod3dams)
#simulate residuals
resmod2dams <- simulateResiduals(mod2dams, plot = T)

#compare AIC
AIC(mod1dams, mod2dams, mod3dams)

#vgll3 additive sires NB
mod1sires<- glm.nb(n.offspring ~ factor(c25_1441_SAC) + seaageatmaturity, link = log, data=Uts_conserved_total_RS_sires)
summary(mod1sires)
#simulate residuals
resmod1sires <- simulateResiduals(mod1sires, plot = T)

#vgll3 additive sires poisson 
mod2sires <- glm(formula = n.offspring ~ factor(c25_1441_SAC) + seaageatmaturity, family = "poisson", data=Uts_conserved_total_RS_sires)
summary(mod2sires)
#simulate residuals
resmod2sires <- simulateResiduals(mod2sires, plot = T)

#qvgll3 additive sires quasi-poisson 
mod3sires <- glm(formula = n.offspring ~ factor(c25_1441_SAC) + seaageatmaturity, family = "quasipoisson", data=Uts_conserved_total_RS_sires)
summary(mod3sires)
#simulate residuals
resmod3sires <- simulateResiduals(mod3sires, plot = T)

#compare AIC
AIC(mod1sires, mod2sires, mod3sires)


#create table of offspring means 
table.vgll3.conserved <- Uts_conserved_total_RS %>%
  group_by(c25_1441_SAC, sex, n.rm=T) %>%
  summarise_if(is.numeric, funs(mean(., na.rm=T), n = sum(!is.na(.)), se = sd(., na.rm=T)/sqrt(sum(!is.na(.)))))

#graph vgll3top conserved offspring vs genotype
pVgll3.conserved <-ggplot(data=table.vgll3.conserved) + geom_point(aes(y=n.offspring_mean, x=c25_1441_SAC, color = sex), size=6, alpha = 3/5, position = position_dodge(width=0.5)) +
  geom_errorbar(aes(x=c25_1441_SAC, ymin=n.offspring_mean-n.offspring_se, ymax=n.offspring_mean+n.offspring_se, color = sex), width=.3, size=1.2, position = position_dodge(width=0.5)) +
  scale_color_manual(values=c("darkorange", "#4271AE"), name = "", labels = c("dam", "sire")) +
  geom_jitter(data=Uts_conserved_total_RS, aes(y=n.offspring, x=c25_1441_SAC, color = sex), position = position_jitterdodge(0.1, 0.1, 0.4), stat="identity", alpha = 2/5) +
  scale_y_log10(breaks = c(1, 11, 101), labels = c("0", "10", "100")) +
  scale_x_continuous(limits = c(), 
                   breaks = c(1,2,3),
                   labels = c("EE", "EL", "LL")) +
  ggtitle("Total")+
  theme(plot.title=element_text(size=24, hjust=0.5), legend.title=element_text(size=20), legend.text=element_text(size=20)) +
  labs(y="No. offspring", x="vgll3 genotype", color = '') + 
  theme(axis.title.x = element_text(size=18)) +  theme(axis.text.x = element_text(size=20, color="black", face = "italic")) +
  theme(axis.title.y = element_text(size=18, vjust=1, angle = 90)) +  theme(axis.text.y = element_text(size=20, color="black")) +
  theme(axis.line = element_line(size = 1)) + theme(axis.ticks = element_line(size=1)) +
  theme(legend.position = "right") + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
#show graph
pVgll3.conserved

#find weird female
Uts_conserved_total_RS %>% 
  group_by(ID) %>%
  filter(sex == "dam" & seaageatmaturity == 1) %>%
  filter(n.offspring==max(n.offspring))

Uts_cohort_SNP_conserved %>% 
  filter(sex == "dam") %>%
  group_by(ID, respawner.info) %>%
    count() %>%
    group_by(respawner.info, n) %>%
    count()
  



#seaageatmaturity and RS, sex
#create table of means 
table.vgll3.conserved.seaage <- Uts_conserved_total_RS %>%
  group_by(seaageatmaturity, sex, n.rm=T) %>%
  summarise_if(is.numeric, funs(mean(., na.rm=T), n = sum(!is.na(.)), se = sd(., na.rm=T)/sqrt(sum(!is.na(.)))))

#graph seageatmaturity and RS
pseaage.RS <-ggplot(data=table.vgll3.conserved.seaage) + geom_point(aes(x=seaageatmaturity, y=n.offspring_mean, colour=sex), size=6, alpha = 3/5, position = position_dodge(width=0.5)) +
  geom_errorbar(aes(x=seaageatmaturity, ymin=n.offspring_mean-n.offspring_se, ymax=n.offspring_mean+n.offspring_se, colour=sex),  width=.3, size=1.2, position = position_dodge(width=0.5)) +
  geom_jitter(data=Uts_conserved_total_RS, aes(y=n.offspring, x=seaageatmaturity, color = sex), position = position_jitterdodge(0.1, 0.1, 0.4), stat="identity", alpha = 2/5) +
  scale_color_manual(values=c("darkorange", "#4271AE")) +
  scale_y_log10(breaks = c(1, 11, 101), labels = c("0", "10", "100")) +
  ggtitle("Total")+
  labs(x="Sea age (SW)", y="No. offspring", color='') + 
  theme(plot.title=element_text(size=24, hjust=0.5), legend.title=element_text(size=20), legend.text=element_text(size=20)) +
  theme(axis.title.x = element_text(size=18)) +  theme(axis.text.x = element_text(size=20, color="black")) +
  theme(axis.title.y = element_text(size=18, vjust=1, angle = 90)) +  theme(axis.text.y = element_text(size=20, color="black")) +
  theme(axis.line = element_line(size = 1)) + theme(axis.ticks = element_line(size=1)) +
  theme(legend.position = "right") + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
#show graph
pseaage.RS



####sea age vs vgll3 genotype ####
#graph vgll3top conserved versus seaage sires/dams
pVgll3.conserved.seaage <-ggplot(data=table.vgll3.conserved) + geom_point(aes(y=seaageatmaturity_mean, x=c25_1441_SAC, color = sex), size=6, alpha = 3/5, position = position_dodge(width=0.2)) +
  geom_errorbar(aes(x=c25_1441_SAC, ymin=seaageatmaturity_mean-seaageatmaturity_se, ymax=seaageatmaturity_mean+seaageatmaturity_se, color = sex), width=.2, size=1.2, position = position_dodge(width=0.2)) +
  scale_color_manual(values=c("darkorange", "#4271AE"), name = "", labels = c("dam", "sire")) +
  geom_jitter(data=Uts_conserved_total_RS, aes(y=seaageatmaturity, x=c25_1441_SAC, color = sex), position = position_jitterdodge(0.1, 0.1, 0.4), stat="identity", alpha = 2/5) +
  scale_y_continuous(limits = c()) +
  scale_x_continuous(limits = c(), 
                     breaks = c(1,2,3),
                     labels = c("EE", "EL", "LL")) +
  ggtitle("Parentage")+
  theme(plot.title=element_text(size=24, hjust=0.5), legend.title=element_text(size=20), legend.text=element_text(size=20)) +
  labs(y="Sea age (SW)", x="vgll3 genotype", color = '') +
  theme(axis.title.x = element_text(size=18)) +  theme(axis.text.x = element_text(size=20, color="black", face = "italic")) +
  theme(axis.title.y = element_text(size=18, vjust=1, angle = 90)) +  theme(axis.text.y = element_text(size=20, color="black")) +
  theme(axis.line = element_line(size = 1)) + theme(axis.ticks = element_line(size=1)) +
  theme(legend.position = "right") + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
#show graph
pVgll3.conserved.seaage

#vgll3 models 
mod_seaagevgll3RS <- glm(seaageatmaturity ~ sex + as.factor(c25_1441_SAC), data=Uts_conserved_total_RS)
summary(mod_seaagevgll3RS)
#simulate residuals
resmod_seaagevgll3RS <- simulateResiduals(mod_seaagevgll3RS, plot = T)



#seaageatmaturity and RS, sex sampled adults only
#working with adult dataset
#combine datasets and make new column for seaage at first maturity
Uts_adults_SNPx <- left_join(Utsadults, UtsSNP, by ="ID") %>%
  filter(year.x != 2019) %>% 
  mutate(recapture = ifelse(is.na(recapture), 0, recapture)) %>%
  mutate(Respawner = ifelse(is.na(Respawner), 0, Respawner)) %>%
  mutate(seaageatmaturity = ifelse(Respawner == 0, InterpAge,
                                   ifelse(Respawner == 1 & respawner.info == "3S1", 3,
                                      ifelse(Respawner == 1 & respawner.info == "2S1", 2, 
                                             ifelse(Respawner == 1 & respawner.info == "1S1", 1, InterpAge))))) %>%
  relocate(seaageatmaturity, .after = respawner.info) %>%
  filter(recapture != "2") 

#create table of offspring means 
#####table means Vgll3top####
tmeansXVgll3top <- Uts_adults_SNPx %>%
  group_by(sex.x, c25_1441_SAC, n.rm=T) %>%
  summarise_if(is.numeric, funs(mean(., na.rm=T), n = sum(!is.na(.)), se = sd(., na.rm=T)/sqrt(sum(!is.na(.)))))

#graph vgll3top conserved versus seaage (no reproduction)
pVgll3topXSW<-ggplot(data=tmeansXVgll3top) + geom_point(aes(x=c25_1441_SAC, y=seaageatmaturity_mean, color=sex.x), size=6, alpha = 3/5, position = position_dodge(width=0.2)) +
  geom_errorbar(aes(x=c25_1441_SAC, ymin=seaageatmaturity_mean-seaageatmaturity_se, ymax=seaageatmaturity_mean+seaageatmaturity_se, color=sex.x),  width=.2, size=1.2, position = position_dodge(width=0.2)) +
  scale_color_manual(values=c("darkorange", "#4271AE"), name = "", labels = c("female", "male"))  +
  geom_jitter(data=Uts_adults_SNPx, aes(y=seaageatmaturity, x=c25_1441_SAC, colour=sex.x), position = position_jitterdodge(0.1, 0.1, 0.4), size=2, stat="identity", alpha = 2/5) +
  scale_y_continuous(limits = c(0,4)) +
  scale_x_continuous(limits = c(), 
                     breaks = c(1,2,3),
                     labels = c("EE", "EL", "LL")) +
  ggtitle("Sampled adults") +
  theme(plot.title=element_text(size=24, hjust=0.5), legend.title=element_text(size=20), legend.text=element_text(size=20)) +
  labs(y="Sea age (SW)", x="vgll3 genotype", color = '') + 
  theme(axis.title.x = element_text(size=18)) +  theme(axis.text.x = element_text(size=20, color="black", face = "italic")) +
  theme(axis.title.y = element_text(size=18, vjust=1, angle = 90)) +  theme(axis.text.y = element_text(size=20, color="black")) +
  theme(axis.line = element_line(size = 1)) + theme(axis.ticks = element_line(size=1)) +
  theme(legend.position = "right") + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
#show graph
pVgll3topXSW

#vgll3 models 
mod_seaagevgll3RSadult <- glm(seaageatmaturity ~ sex.x + as.factor(c25_1441_SAC), data=Uts_adults_SNPx)
summary(mod_seaagevgll3RSadult)
#simulate residuals
resmod_seaagevgll3RSadult <- simulateResiduals(mod_seaagevgll3RSadult, plot = T)



#vgll3 sex 
#dams
mod1sexdams <- glm(seaageatmaturity ~ as.factor(c25_1441_SAC), data=Uts_conserved_total_RS_dams)
summary(mod1sexdams)
#simulate residuals
resmod1sexdams  <- simulateResiduals(mod1sexdams, plot = T)

#sires
mod1sexsires <- glm(seaageatmaturity ~ as.factor(c25_1441_SAC), data=Uts_conserved_total_RS_sires)
summary(mod1sexsires)
anova(mod1sexsires)
#simulate residuals
resmod1sexsires  <- simulateResiduals(mod1sexsires, plot = T)







####vgll3 comparison just with 1st cohort of each sex####
#filter first cohort
#remove individuals that have cohorts before 2012, and after 2017
Uts_cohort_SNP_conserved_1C <- Uts_cohort_SNP_conserved %>%
  filter(cohort.rank == 1) %>%
  filter(firstcohort != 2007) %>%
  filter(firstcohort != 2009) %>%
  filter(firstcohort != 2010) %>%
  filter(firstcohort != 2011) %>% 
  filter(firstcohort != 2018) %>% 
  filter(firstcohort != 2019)

#split into different sexes
#dams
Uts_conserved_total_RS_dams_1C <- Uts_cohort_SNP_conserved_1C %>%
  filter(sex == "dam") 
#sires  
Uts_conserved_total_RS_sires_1C <- Uts_cohort_SNP_conserved_1C %>%
  filter(sex == "sire") 

#model test, neg bionomial 
mod1dams_1C<- glm.nb(n.offspring ~ factor(c25_1441_SAC) + seaageatmaturity, link = log, data=Uts_conserved_total_RS_dams_1C )
summary(mod1dams_1C)
#simulate residuals
resmod1dams_1C <- simulateResiduals(mod1dams_1C, plot = T)

#model test, neg bionomial 
mod1sires_1C<- glm.nb(n.offspring ~ factor(c25_1441_SAC) + seaageatmaturity, link = log, data=Uts_conserved_total_RS_sires_1C )
summary(mod1sires_1C)
#simulate residuals
resmod1sires_1C <- simulateResiduals(mod1sires_1C, plot = T)

#create table of offspring means based on cohorts for vgll3
table.vgll3.conserved_1C <- Uts_cohort_SNP_conserved_1C %>%
  group_by(c25_1441_SAC, sex, n.rm=T) %>%
  summarise_if(is.numeric, funs(mean(., na.rm=T), n = sum(!is.na(.)), se = sd(., na.rm=T)/sqrt(sum(!is.na(.))))) 

#graph vgll3top conserved
pVgll3.conserved_1C <-ggplot(data=table.vgll3.conserved_1C) + geom_point(aes(y=n.offspring_mean, x=c25_1441_SAC, color = sex), size=6, alpha = 3/5, position = position_dodge(width=0.2)) +
  geom_errorbar(aes(x=c25_1441_SAC, ymin=(n.offspring_mean)-(n.offspring_se), ymax=(n.offspring_mean)+(n.offspring_se), color = sex), width=.2, size=1.2, position = position_dodge(width=0.2)) +
  scale_color_manual(values=c("darkorange", "#4271AE")) +
  geom_jitter(data=Uts_cohort_SNP_conserved_1C, aes(y=n.offspring, x=c25_1441_SAC, color = sex), position = position_jitterdodge(0.1, 0.1, 0.4), stat="identity", alpha = 2/5) +
  scale_y_log10(breaks = c(1, 11, 101), labels = c("0", "10", "100")) +
  scale_x_continuous(limits = c(), 
                     breaks = c(1,2,3),
                     labels = c("EE", "EL", "LL")) +
  labs(y="No. offspring", x="vgll3 genotype", color = '') + 
  ggtitle("First reproductive event")+
  theme(plot.title=element_text(size=24, hjust=0.5), legend.title=element_text(size=20), legend.text=element_text(size=20)) +
  theme(axis.title.x = element_text(size=18)) +  theme(axis.text.x = element_text(size=20, color="black" , face = "italic")) +
  theme(axis.title.y = element_text(size=18, vjust=1, angle = 90)) +  theme(axis.text.y = element_text(size=20, color="black")) +
  theme(axis.line = element_line(size = 1)) + theme(axis.ticks = element_line(size=1)) +
  theme(legend.position = "right") + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
#show graph
pVgll3.conserved_1C

#seaageatmaturity and RS, sex
#create table of offspring means 
table.vgll3.conserved_1C.seaage <- Uts_cohort_SNP_conserved_1C %>%
  group_by(seaageatmaturity, sex, n.rm=T) %>%
  summarise_if(is.numeric, funs(mean(., na.rm=T), n = sum(!is.na(.)), se = sd(., na.rm=T)/sqrt(sum(!is.na(.)))))

#graph seageatmaturity and RS
pseaage.RS_1C <-ggplot(data=table.vgll3.conserved_1C.seaage) + geom_point(aes(x=seaageatmaturity, y=n.offspring_mean, colour=sex), size=6, alpha = 3/5, position = position_dodge(width=0.2)) +
  geom_errorbar(aes(x=seaageatmaturity, ymin=n.offspring_mean-n.offspring_se, ymax=n.offspring_mean+n.offspring_se, colour=sex),  width=.2, size=1.2, position = position_dodge(width=0.2)) +
  geom_jitter(data=Uts_cohort_SNP_conserved_1C, aes(y=n.offspring, x=seaageatmaturity, color = sex), position = position_jitterdodge(0.1, 0.1, 0.4), stat="identity", alpha = 2/5) +
  scale_color_manual(values=c("darkorange", "#4271AE")) +
  scale_y_log10(breaks = c(1, 11, 101), labels = c("0", "10", "100")) +
  labs(x="Sea age (SW)", y="No. offspring", color ='') + 
  ggtitle("First reproductive event") +
  theme(plot.title=element_text(size=24, hjust=0.5), legend.title=element_text(size=20), legend.text=element_text(size=20)) +
  theme(axis.title.x = element_text(size=18)) +  theme(axis.text.x = element_text(size=20, color="black")) +
  theme(axis.title.y = element_text(size=18, vjust=1, angle = 90)) +  theme(axis.text.y = element_text(size=20, color="black")) +
  theme(axis.line = element_line(size = 1)) + theme(axis.ticks = element_line(size=1)) +
  theme(legend.position = "right") + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
#show graph
pseaage.RS_1C

####create table of offspring X vgll3 comparing itero/semiparous individuals by sex####
#create table of dams offspring means based on cohorts for vgll3
table.vgll3.conserved_1C_dams <- Uts_cohort_SNP_conserved_1C %>%
  filter(sex == "dam") %>%
  group_by(c25_1441_SAC, respawner.gen, n.rm=T) %>%
  summarise_if(is.numeric, funs(mean(., na.rm=T), n = sum(!is.na(.)), se = sd(., na.rm=T)/sqrt(sum(!is.na(.))))) 

#dams
table.Uts_cohort_SNP_conserved_1C_dams <- Uts_cohort_SNP_conserved_1C %>%
  filter(sex == "dam")

#graph vgll3top conserved dams iteroparous
pVgll3.conserved_1C_dams <-ggplot(data=table.vgll3.conserved_1C_dams) + geom_point(aes(y=n.offspring_mean, x=c25_1441_SAC, color = respawner.gen), size=6, alpha = 3/5, position = position_dodge(width=0.2)) +
  geom_errorbar(aes(x=c25_1441_SAC, ymin=(n.offspring_mean)-(n.offspring_se), ymax=(n.offspring_mean)+(n.offspring_se), color = respawner.gen), width=.2, size=1.2, position = position_dodge(width=0.2)) +
  scale_color_manual(values=c("#009E73", "#CC79A7"), name = "", labels = c("Iteroparous", "Semelparous")) +
  geom_jitter(data=table.Uts_cohort_SNP_conserved_1C_dams, aes(y=n.offspring, x=c25_1441_SAC, color = respawner.gen), position = position_jitterdodge(0.1, 0.1, 0.4), stat="identity", alpha = 2/5) +
  scale_y_log10(breaks = c(1, 11, 101), labels = c("0", "10", "100")) +
    scale_x_continuous(limits = c(), 
                     breaks = c(1,2,3),
                     labels = c("EE", "EL", "LL")) +
  labs(y="No. offspring", x="vgll3 genotype", color = '') +
  ggtitle("Dams")+
  theme(plot.title=element_text(size=24, hjust=0.5), legend.title=element_text(size=20), legend.text=element_text(size=20)) +
  theme(axis.title.x = element_text(size=20)) +  theme(axis.text.x = element_text(size=20, color="black", face = "italic")) +
  theme(axis.title.y = element_text(size=20, vjust=1, angle = 90)) +  theme(axis.text.y = element_text(size=20, color="black")) +
  theme(axis.line = element_line(size = 1)) + theme(axis.ticks = element_line(size=1)) +
  theme(legend.position = "right") + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
#show graph
pVgll3.conserved_1C_dams

#create table of offspring means 
table.vgll3.conserved_1C_dams_seaage <- table.Uts_cohort_SNP_conserved_1C_dams  %>%
  group_by(seaageatmaturity, respawner.gen, n.rm=T) %>%
  summarise_if(is.numeric, funs(mean(., na.rm=T), n = sum(!is.na(.)), se = sd(., na.rm=T)/sqrt(sum(!is.na(.)))))

#graph seageatmaturity and RS
pseaage.RSdams <-ggplot(data=table.vgll3.conserved_1C_dams_seaage) + geom_point(aes(x=seaageatmaturity, y=n.offspring_mean, colour=respawner.gen), size=6, alpha = 3/5, position = position_dodge(width=0.2)) +
  geom_errorbar(aes(x=seaageatmaturity, ymin=n.offspring_mean-n.offspring_se, ymax=n.offspring_mean+n.offspring_se, colour=respawner.gen),  width=.2, size=1.2, position = position_dodge(width=0.2)) +
  geom_jitter(data=table.Uts_cohort_SNP_conserved_1C_dams, aes(y=n.offspring, x=seaageatmaturity, color = respawner.gen), position = position_jitterdodge(0.1, 0.1, 0.4), stat="identity", alpha = 2/5) +
  scale_color_manual(values=c("#009E73", "#CC79A7"), name = "", labels = c("Iteroparous", "Semelparous")) +
  scale_y_log10(breaks = c(1, 11, 101), labels = c("0", "10", "100")) +
  scale_x_continuous(limits = c(0,4)) +
    labs(x="Sea age (SW)", y="No. offspring", color = '') + 
  ggtitle("Dams")+
  theme(plot.title=element_text(size=24, hjust=0.5), legend.title=element_text(size=20), legend.text=element_text(size=20)) +
  theme(axis.title.x = element_text(size=20)) +  theme(axis.text.x = element_text(size=20, color="black")) +
  theme(axis.title.y = element_text(size=20, vjust=1, angle = 90)) +  theme(axis.text.y = element_text(size=20, color="black")) +
  theme(axis.line = element_line(size = 1)) + theme(axis.ticks = element_line(size=1)) +
  theme(legend.position = "right") + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
#show graph
pseaage.RSdams

#model test, neg bionomial 
mod1_dams_RS<- glm.nb(n.offspring ~ respawner.gen + factor(c25_1441_SAC) + seaageatmaturity, link = log, data=table.Uts_cohort_SNP_conserved_1C_dams)
summary(mod1_dams_RS)
#simulate residuals
resmod1_dams_RS <- simulateResiduals(mod1_dams_RS, plot = T)

#sires
#create table of dams offspring means based on cohorts for vgll3
table.vgll3.conserved_1C_sires <- Uts_cohort_SNP_conserved_1C %>%
  filter(sex == "sire") %>%
  group_by(c25_1441_SAC, respawner.gen, n.rm=T) %>%
  summarise_if(is.numeric, funs(mean(., na.rm=T), n = sum(!is.na(.)), se = sd(., na.rm=T)/sqrt(sum(!is.na(.))))) 

#sires
table.Uts_cohort_SNP_conserved_1C_sires <- Uts_cohort_SNP_conserved_1C %>%
  filter(sex == "sire")

#graph vgll3top sires
pVgll3.conserved_1C_sires <-ggplot(data=table.vgll3.conserved_1C_sires) + geom_point(aes(y=n.offspring_mean, x=c25_1441_SAC, color = respawner.gen), size=6, alpha = 3/5, position = position_dodge(width=0.2)) +
  geom_errorbar(aes(x=c25_1441_SAC, ymin=(n.offspring_mean)-(n.offspring_se), ymax=(n.offspring_mean)+(n.offspring_se), color = respawner.gen), width=.2, size=1.2, position = position_dodge(width=0.2)) +
  scale_color_manual(values=c("#009E73", "#CC79A7"), name = "", labels = c("Iteroparous", "Semelparous")) +
  geom_jitter(data=table.Uts_cohort_SNP_conserved_1C_sires, aes(y=n.offspring, x=c25_1441_SAC, color = respawner.gen), position = position_jitterdodge(0.1, 0.1, 0.4), stat="identity", alpha = 2/5) +
  scale_y_log10(breaks = c(1, 11, 101), labels = c("0", "10", "100")) +
  scale_x_continuous(limits = c(), 
                     breaks = c(1,2,3),
                     labels = c("EE", "EL", "LL")) +
  labs(y="No. offspring", x="vgll3 genotype", color = '') +
  ggtitle("Sires")+
  theme(plot.title=element_text(size=24, hjust=0.5), legend.title=element_text(size=20), legend.text=element_text(size=20)) +
  theme(axis.title.x = element_text(size=20)) +  theme(axis.text.x = element_text(size=20, color="black", face = "italic")) +
  theme(axis.title.y = element_text(size=20, vjust=1, angle = 90)) +  theme(axis.text.y = element_text(size=20, color="black")) +
  theme(axis.line = element_line(size = 1)) + theme(axis.ticks = element_line(size=1)) +
  theme(legend.position = "right") + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
#show graph
pVgll3.conserved_1C_sires

#create table of offspring means 
table.vgll3.conserved_1C_sires_seaage <- table.Uts_cohort_SNP_conserved_1C_sires  %>%
  group_by(seaageatmaturity, respawner.gen, n.rm=T) %>%
  summarise_if(is.numeric, funs(mean(., na.rm=T), n = sum(!is.na(.)), se = sd(., na.rm=T)/sqrt(sum(!is.na(.)))))

#graph seageatmaturity and RS
pseaage.conserved_1C_sires_seaage <-ggplot(data=table.vgll3.conserved_1C_sires_seaage) + geom_point(aes(x=seaageatmaturity, y=n.offspring_mean, colour=respawner.gen), size=6, alpha = 3/5, position = position_dodge(width=0.2)) +
  geom_errorbar(aes(x=seaageatmaturity, ymin=n.offspring_mean-n.offspring_se, ymax=n.offspring_mean+n.offspring_se, colour=respawner.gen),  width=.2, size=1.2, position = position_dodge(width=0.2)) +
  geom_jitter(data=table.Uts_cohort_SNP_conserved_1C_sires, aes(y=n.offspring, x=seaageatmaturity, color = respawner.gen), position = position_jitterdodge(0.1, 0.1, 0.4), stat="identity", alpha = 2/5) +
  scale_color_manual(values=c("#009E73", "#CC79A7"), name = "", labels = c("Iteroparous", "Semelparous")) +
  scale_y_log10(breaks = c(1, 11, 101), labels = c("0", "10", "100")) +
  ggtitle("Sires")+
  labs(x="Sea age (SW)", y="No. offspring", color='') + 
  theme(plot.title=element_text(size=24, hjust=0.5), legend.title=element_text(size=20), legend.text=element_text(size=20)) +
  theme(axis.title.x = element_text(size=20)) +  theme(axis.text.x = element_text(size=20, color="black")) +
  theme(axis.title.y = element_text(size=20, vjust=1, angle = 90)) +  theme(axis.text.y = element_text(size=20, color="black")) +
  theme(axis.line = element_line(size = 1)) + theme(axis.ticks = element_line(size=1)) +
  theme(legend.position = "right") + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
#visualize
pseaage.conserved_1C_sires_seaage

#model test, neg bionomial 
mod1_sires_RS<- glm.nb(n.offspring ~ respawner.gen + factor(c25_1441_SAC) + seaageatmaturity, link = log, data=table.Uts_cohort_SNP_conserved_1C_sires)
summary(mod1_sires_RS)
#simulate residuals
resmod1_sires_RS <- simulateResiduals(mod1_sires_RS, plot = T)

####comparision of iteroparous individuals####
#make table for cohort X offspring means
table.cohort.offspring.conserved <- Uts_conserved_total_RS %>%
  group_by(cohort.max, sex, n.rm=T) %>%
  summarise_if(is.numeric, funs(mean(., na.rm=T), n = sum(!is.na(.)), se = sd(., na.rm=T)/sqrt(sum(!is.na(.))))) 

#remove semelparous individuals from main table
table.cohort.SNP.itero.conserved <- Uts_conserved_total_RS 

####graph offspring X cohort for iteroparous individuals
pcohort.offspring_conserved<-ggplot(data=table.cohort.offspring.conserved) + geom_point(aes(y=n.offspring_mean, x=cohort.max, color = sex), size=6, alpha = 3/5, position = position_dodge(width=0.2)) +
  geom_errorbar(aes(x=cohort.max, ymin=n.offspring_mean-n.offspring_se, ymax=n.offspring_mean+n.offspring_se, color =sex),  width=.2, size=1.2, position = position_dodge(width=0.2)) +
  scale_color_manual(values=c("darkorange", "#4271AE")) +
  geom_jitter(data=table.cohort.SNP.itero.conserved, aes(y=n.offspring, x=cohort.max, color = sex), position = position_jitterdodge(0.1, 0.1, 0.4), size=2, stat="identity", alpha = 2/5) +
  labs(y="No. offspring", x="Reproductive event", color ="") + 
  scale_y_log10(breaks = c(1, 11, 101), labels = c("0", "10", "100")) +
  scale_x_continuous(limits = c(), 
                     breaks = c(1,2,3,4),
                     labels = c("1", "2", "3", "4")) +
  theme(plot.title=element_text(size=36, hjust=-0.5), legend.title=element_text(size=20), legend.text=element_text(size=20)) +
  theme(axis.title.x = element_text(size=24)) +  theme(axis.text.x = element_text(size=20, color="black")) +
  theme(axis.title.y = element_text(size=24, vjust=1, angle = 90)) +  theme(axis.text.y = element_text(size=20, color="black")) +
  theme(axis.line = element_line(size = 1)) + theme(axis.ticks = element_line(size=1)) +
  theme(legend.position = "right") + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
  #show graph
pcohort.offspring_conserved

#model test, neg bionomial 
mod1_itero_off<- glm.nb(n.offspring ~ sex + cohort.max + respawner.gen, link = log, data=table.cohort.SNP.itero.conserved)
summary(mod1_itero_off)

#visualize residuals
#simulate residuals
resmod1_mod1_itero_off <- simulateResiduals(mod1_itero_off, plot = T)

####graph cohort X vgll3 signal####
#graph vgll3top x cohort 
pcohort.vgll3.conserved<-ggplot(data=table.cohort.offspring.conserved) + geom_point(aes(y=c25_1441_SAC_mean, x=cohort.max, color = sex), size=6, alpha = 3/5, position = position_dodge(width=0.2)) +
  geom_errorbar(aes(x=cohort.max, ymin=c25_1441_SAC_mean-c25_1441_SAC_se, ymax=c25_1441_SAC_mean+c25_1441_SAC_se, color = sex),  width=.2, size=1.2, position = position_dodge(width=0.2)) +
  scale_color_manual(values=c("darkorange", "#4271AE")) +
  geom_jitter(data=table.cohort.SNP.itero.conserved, aes(y=c25_1441_SAC, x=cohort.max, color = sex), position = position_jitterdodge(0.1, 0.1, 0.4), size=2, stat="identity", alpha = 2/5) +
  labs(y="L allele frequency", x="Reproductive event", color="") + theme(plot.title=element_text(size=36, hjust=1)) +
   scale_y_continuous(limits = c(), 
                     breaks = c(1,2,3),
                     labels = c("0", "0.5", "1.0")) +
  theme(plot.title=element_text(size=36, hjust=-0.5), legend.title=element_text(size=20), legend.text=element_text(size=20)) +
  theme(axis.title.x = element_text(size=24)) +  theme(axis.text.x = element_text(size=20, color="black")) +
  theme(axis.title.y = element_text(size=24, vjust=1, angle = 90)) +  theme(axis.text.y = element_text(size=20, color="black")) +
  theme(axis.line = element_line(size = 1)) + theme(axis.ticks = element_line(size=1)) +
  theme(legend.position = "right") + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
#show graph
pcohort.vgll3.conserved

#model test, neg bionomial 
mod1_itero_vgll3<- glm.nb(n.offspring ~ sex*c25_1441_SAC+cohort.max +respawner.gen, link = log, data=table.cohort.SNP.itero.conserved)
summary(mod1_itero_vgll3)
#simulate residuals
resmod1_itero_vgll3 <- simulateResiduals(mod1_itero_vgll3, plot = T)

#model test, neg bionomial 
mod1_itero_vgll3x<- lm(c25_1441_SAC ~ sex + respawner.gen, data=table.cohort.SNP.itero.conserved)
summary(mod1_itero_vgll3x)
anova(mod1_itero_vgll3x)
#simulate residuals
resmod1_itero_vgll3x <- simulateResiduals(mod1_itero_vgll3x, plot = T)


#visualize RS by sea age and vgll3 genotype/split by sex
#create table of offspring means based on cohorts for vgll3:SIRES
table.vgll3.conserved_1C_siresVGLL3 <- Uts_cohort_SNP_conserved_1C %>%
  filter(sex == "sire") %>%
  group_by(c25_1441_SAC, seaageatmaturity, n.rm=T) %>%
  summarise_if(is.numeric, funs(mean(., na.rm=T), n = sum(!is.na(.)), se = sd(., na.rm=T)/sqrt(sum(!is.na(.))))) 
#table
Uts_cohort_SNP_conserved_1C_sires <- Uts_cohort_SNP_conserved_1C %>%
  filter(sex == "sire")

#graph vgll3top conserved
pVgll3.conserved_1C_siresvgll3 <-ggplot(data=table.vgll3.conserved_1C_siresVGLL3) + geom_point(aes(y=n.offspring_mean, x=seaageatmaturity, color = as.factor(c25_1441_SAC)), size=6, alpha = 3/5, position = position_dodge(width=0.2)) +
  geom_errorbar(aes(x=seaageatmaturity, ymin=n.offspring_mean-n.offspring_se, ymax=n.offspring_mean+n.offspring_se, color = as.factor(c25_1441_SAC)), width=.2, size=1.2, position = position_dodge(width=0.2)) +
  scale_color_manual(values=c("#E69F00", "#56B4E9", "#009E73"), labels = c("EE", "EL", "LL")) +
  geom_jitter(data=Uts_cohort_SNP_conserved_1C_sires, aes(y=n.offspring, x=seaageatmaturity, color = as.factor(c25_1441_SAC)), position = position_jitterdodge(0.1, 0.1, 0.4), stat="identity", alpha = 2/5) +
  scale_y_log10(breaks = c(1, 11, 101), labels = c("0", "10", "100")) +
  labs(y="No. offspring", x="Sea age", color = 'vgll3 genotype', title="Sires") + theme(plot.title=element_text(size=36, hjust=0.5)) +
  scale_x_continuous(limits = c(), 
                     breaks = c(1,2,3, 4),
                     labels = c("1", "2", "3", "4")) +
  theme(plot.title=element_text(size=36, hjust=-0.5), legend.title=element_text(size=20), legend.text=element_text(size=20, face="italic")) +
  theme(axis.title.x = element_text(size=24)) +  theme(axis.text.x = element_text(size=20, color="black")) +
  theme(axis.title.y = element_text(size=24, vjust=1, angle = 90)) +  theme(axis.text.y = element_text(size=20, color="black")) +
  theme(axis.line = element_line(size = 1)) + theme(axis.ticks = element_line(size=1)) +
  theme(legend.position = "right") + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
  
#show graph
pVgll3.conserved_1C_siresvgll3

#create table of offspring means based on cohorts for vgll3: DAMS
table.vgll3.conserved_1C_damsVGLL3 <- Uts_cohort_SNP_conserved_1C %>%
  filter(sex == "dam") %>%
  group_by(c25_1441_SAC, seaageatmaturity, n.rm=T) %>%
  summarise_if(is.numeric, funs(mean(., na.rm=T), n = sum(!is.na(.)), se = sd(., na.rm=T)/sqrt(sum(!is.na(.))))) 

#table
Uts_cohort_SNP_conserved_1C_dams <- Uts_cohort_SNP_conserved_1C %>%
  filter(sex == "dam")

#graph vgll3top conserved
pVgll3.conserved_1C_damssvgll3 <-ggplot(data=table.vgll3.conserved_1C_damsVGLL3) + geom_point(aes(y=n.offspring_mean, x=seaageatmaturity, color = as.factor(c25_1441_SAC)), size=6, alpha = 3/5, position = position_dodge(width=0.2)) +
  geom_errorbar(aes(x=seaageatmaturity, ymin=n.offspring_mean-n.offspring_se, ymax=n.offspring_mean+n.offspring_se, color = as.factor(c25_1441_SAC)), width=.2, size=1.2, position = position_dodge(width=0.2)) +
  scale_color_manual(values=c("#E69F00", "#56B4E9", "#009E73"), labels = c("EE", "EL", "LL")) +
  geom_jitter(data=Uts_cohort_SNP_conserved_1C_dams, aes(y=n.offspring, x=seaageatmaturity, color = as.factor(c25_1441_SAC)), position = position_jitterdodge(0.1, 0.1, 0.4), stat="identity", alpha = 2/5) +
  scale_y_log10(breaks = c(1, 11, 101), labels = c("0", "10", "100")) +
  labs(y="No. offspring", x="Sea age (SW)", color = 'vgll3 genotype', title="Dams") + 
  scale_x_continuous(limits = c(), 
                     breaks = c(1,2,3, 4),
                     labels = c("1", "2", "3", "4")) +
  theme(plot.title=element_text(size=36, hjust=-0.5), legend.title=element_text(size=20), legend.text=element_text(size=20, face="italic")) +
  theme(axis.title.x = element_text(size=24)) +  theme(axis.text.x = element_text(size=20, color="black")) +
  theme(axis.title.y = element_text(size=24, vjust=1, angle = 90)) +  theme(axis.text.y = element_text(size=20, color="black")) +
  theme(axis.line = element_line(size = 1)) + theme(axis.ticks = element_line(size=1)) +
  theme(legend.position = "right") + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
#show graph
pVgll3.conserved_1C_damssvgll3

####make layout of graphs in 2 columns on a page (cowplot), save plot resize 1800X600####
#fig 1 total RS 
fig_1A <- pVgll3.conserved + theme(plot.margin = unit(c(10, 0, 0, 10), units = "pt"))
fig_1B <- pseaage.RS + theme(plot.margin = unit(c(10, 0, 0, 10), units = "pt"))
# Fig plot
Fig1<-plot_grid(fig_1A, fig_1B, 
                label_y = 1,
                labels = c('A', 'B'), label_size = 24, ncol = 2, align = "v")
#show plot
Fig1

#fig 2 total RS of first reproduction
fig_2A <- pVgll3.conserved_1C + theme(plot.margin = unit(c(10, 0, 0, 10), units = "pt"))
fig_2B <- pseaage.RS_1C + theme(plot.margin = unit(c(10, 0, 0, 10), units = "pt"))
# Fig plot
Fig2<-plot_grid(fig_2A, fig_2B, 
                label_y = 1,
                labels = c('C', 'D'), label_size = 24, ncol = 2, align = "v")
#show plot
Fig2

#fig 3 dams/sires first reproduction RS iteroparous v semelparous
fig_3A <- pVgll3.conserved_1C_dams + theme(plot.margin = unit(c(10, 0, 0, 10), units = "pt"))
fig_3B <- pseaage.RSdams + theme(plot.margin = unit(c(10, 0, 0, 10), units = "pt"))
# Fig plot
Fig3<-plot_grid(fig_3A, fig_3B, 
                label_y = 1,
                labels = c('A', 'B'), label_size = 24, ncol = 2, align = "v")
#show plot
Fig3

#fig 4 dams/sires first reproduction iteroparous v semelparous
fig_4A <- pVgll3.conserved_1C_sires + theme(plot.margin = unit(c(10, 0, 0, 10), units = "pt"))
fig_4B <- pseaage.conserved_1C_sires_seaage + theme(plot.margin = unit(c(10, 0, 0, 10), units = "pt"))
# Fig plot
Fig4<-plot_grid(fig_4A, fig_4B, 
                label_y = 1,
                labels = c('C', 'D'), label_size = 24, ncol = 2, align = "v")
#show plot
Fig4

#fig 4
fig_5A <- pcohort.offspring_conserved + theme(plot.margin = unit(c(10, 0, 0, 10), units = "pt"))
fig_5B <- pcohort.vgll3.conserved + theme(plot.margin = unit(c(10, 0, 0, 10), units = "pt"))
# Fig plot
Fig5<-plot_grid(fig_5A, fig_5B, 
                label_y = 1,
                labels = c('A', 'B'), label_size = 24, ncol = 2, align = "v")
#show plot
Fig5
#save plot
save_plot("Fig5x.jpg", Fig1, base_height = 11, base_width = 8.5)

#fig 6 total RS 

Fig6<-plot_grid(fig_1A, fig_1B, fig_2A, fig_2B,
                label_y = 1,
                axis = "tbl",
                labels = c('A', 'B', 'C', 'D'), label_size = 24, ncol = 2, align = "v")
#show plot
Fig6

Fig7<-plot_grid(fig_3A, fig_3B, fig_4A, fig_4B,
                label_y = 1,
                axis = "tbl",
                labels = c('A', 'B', 'C', 'D'), label_size = 24, ncol = 2, align = "v")
#show plot
Fig7

#sea age vs vgll3 genotype
fig_8A <- pVgll3topXSW + theme(plot.margin = unit(c(10, 0, 0, 10), units = "pt"))
fig_8B <- pVgll3.conserved.seaage + theme(plot.margin = unit(c(10, 0, 0, 10), units = "pt"))
# Fig plot
Fig8<-plot_grid(fig_8A, fig_8B, 
                label_y = 1,
                labels = c('A', 'B'), label_size = 24, ncol = 2, align = "v")
#show plot
Fig8



####find individuals that
Uts_cohort_SNP_conserved %>% filter(respawner.gen == "iteroparous" & Respawner == 0 & firstcohort < year)
Uts_cohort_SNP_conserved %>% filter(!is.na(scale.seaage)) %>% filter(sex == "sire") %>% count()
Uts_cohort_SNP_conserved_RS %>% ungroup %>% filter(sex == "sire") %>% filter(type == "Adult") %>% count()

                                    
                                    