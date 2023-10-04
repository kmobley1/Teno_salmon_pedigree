#Reproductive fitness with conservative parentage analysis
#Using the conservative parentage analysis dataset, create glm models and graphs for total reproductive success and reproductive success for the first reproduction for sires and dams separately

####packages####
library (tidyverse)
library (cowplot)
library (lme4)
library (lmerTest)
library (MASS)
library (DHARMa)

####database####
Uts_cohort_SNP_conserved <- read.csv("~/projects/Atlantic salmon - Teno River Pedigree/2020 - Utsjoki pedigree data/Teno_salmon_pedigree/Uts_cohort_SNP_cons_11.02.22.csv")

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

#split into different sexes
#dams
Uts_conserved_total_RS_dams <- Uts_conserved_total_RS %>%
  filter(sex == "dam") 
#sires  
Uts_conserved_total_RS_sires <- Uts_conserved_total_RS %>%
  filter(sex == "sire") 

#compare negative bionomial, poisson and quasipoisson models for total reproductive success for dams
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
mod3dams <- glm(formula = n.offspring ~ factor(c25_1441_SAC) + seaageatmaturity, family = "quasipoisson", data=Uts_conserved_total_RS_dams)
summary(mod3dams)
#simulate residuals
resmod2dams <- simulateResiduals(mod2dams, plot = T)

#compare AIC
AIC(mod1dams, mod2dams)

#vgll3 additive sires NB
mod1sires<- glm.nb(n.offspring ~ factor(c25_1441_SAC) + seaageatmaturity, link = log, data=Uts_conserved_total_RS_sires)
summary(mod1sires)
#simulate residuals
resmod1sires <- simulateResiduals(mod1sires, plot = T)

#vgll3 additive sires poisson 
mod2sires <- glm(formula = n.offspring ~ factor(c25_1441_SAC) * seaageatmaturity, family = "poisson", data=Uts_conserved_total_RS_sires)
summary(mod2sires)
#simulate residuals
resmod2sires <- simulateResiduals(mod2sires, plot = T)

#qvgll3 additive sires quasi-poisson 
mod3sires <- glm(formula = n.offspring ~ factor(c25_1441_SAC) + seaageatmaturity, family = "quasipoisson", data=Uts_conserved_total_RS_sires)
summary(mod3sires)
#simulate residuals
resmod3sires <- simulateResiduals(mod3sires, plot = T)

#compare AIC
AIC(mod1sires, mod2sires)

#create table of offspring means total RS
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

#seaageatmaturity and RS, sex total RS
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

####vgll3 comparison just with 1st reproductive event of each sex####
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

####make layout of graphs in 2 columns on a page (cowplot), save plot resize 1800X600####
#fig 1 total RS 
fig_1A <- pVgll3.conserved + theme(plot.margin = unit(c(10, 0, 0, 10), units = "pt"))
fig_1B <- pseaage.RS + theme(plot.margin = unit(c(10, 0, 0, 10), units = "pt"))
#fig 2 RS of first reproductive event
fig_2A <- pVgll3.conserved_1C + theme(plot.margin = unit(c(10, 0, 0, 10), units = "pt"))
fig_2B <- pseaage.RS_1C + theme(plot.margin = unit(c(10, 0, 0, 10), units = "pt"))

#fig 6 total RS 
TotalRS_firstRS<-plot_grid(fig_1A, fig_1B, fig_2A, fig_2B,
                label_y = 1,
                axis = "tbl",
                labels = c('A', 'B', 'C', 'D'), label_size = 24, ncol = 2, align = "v")
#show plot
TotalRS_firstRS

#find female with lots of offspring that had lots of offspring with seaage at maturity with one SW
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
