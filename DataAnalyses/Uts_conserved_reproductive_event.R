#Reproductive fitness with conservative parentage analysis
#Using the conservative parentage analysis dataset, create glm models and graphs for the effect of sex, respawner and the maximum # of reproductive events

####packages####
library (tidyverse)
library (lme4)
library (lmerTest)
library (MASS)
library (DHARMa)

####database####
Uts_cohort_SNP_conserved <- read.csv("Data/Uts_cohort_SNP_cons_11.02.22.csv")

#colorblind palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7","#999999", "#F0E442")

####total reproductive success for all individuals across all cohorts####
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

####comparision of iteroparous individuals####
#make table for cohort X offspring means
table.cohort.offspring.conserved <- Uts_conserved_total_RS %>%
  group_by(cohort.max, sex, n.rm=T) %>%
  summarise_if(is.numeric, funs(mean(., na.rm=T), n = sum(!is.na(.)), se = sd(., na.rm=T)/sqrt(sum(!is.na(.))))) 

####graph offspring X cohort for iteroparous individuals
pRE.offspring_conserved<-ggplot(data=table.cohort.offspring.conserved) + geom_point(aes(y=n.offspring_mean, x=cohort.max, color = sex), size=6, alpha = 3/5, position = position_dodge(width=0.2)) +
  geom_errorbar(aes(x=cohort.max, ymin=n.offspring_mean-n.offspring_se, ymax=n.offspring_mean+n.offspring_se, color =sex),  width=.2, size=1.2, position = position_dodge(width=0.2)) +
  scale_color_manual(values=c("darkorange", "#4271AE")) +
  geom_jitter(data=Uts_conserved_total_RS, aes(y=n.offspring, x=cohort.max, color = sex), position = position_jitterdodge(0.1, 0.1, 0.4), size=2, stat="identity", alpha = 2/5) +
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
pRE.offspring_conserved

#model test, neg bionomial 
mod1_itero_off<- glm.nb(n.offspring ~ sex + cohort.max + respawner.gen, link = log, data=table.cohort.SNP.itero.conserved)
summary(mod1_itero_off)

#visualize residuals
resmod1_mod1_itero_off <- simulateResiduals(mod1_itero_off, plot = T)


##############################################################################################
####graph cohort X vgll3 signal####
#not used 

#graph vgll3top x cohort 
pcohort.vgll3.conserved<-ggplot(data=table.cohort.offspring.conserved) + geom_point(aes(y=c25_1441_SAC_mean, x=cohort.max, color = sex), size=6, alpha = 3/5, position = position_dodge(width=0.2)) +
  geom_errorbar(aes(x=cohort.max, ymin=c25_1441_SAC_mean-c25_1441_SAC_se, ymax=c25_1441_SAC_mean+c25_1441_SAC_se, color = sex),  width=.2, size=1.2, position = position_dodge(width=0.2)) +
  scale_color_manual(values=c("darkorange", "#4271AE")) +
  geom_jitter(data=Uts_conserved_total_RS, aes(y=c25_1441_SAC, x=cohort.max, color = sex), position = position_jitterdodge(0.1, 0.1, 0.4), size=2, stat="identity", alpha = 2/5) +
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
