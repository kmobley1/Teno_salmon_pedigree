## Data analysis: vgll3 genotype X sea age comparions between sampled adults and parentage datasets
#Using the conservative parentage analysis dataset, create glm models and graphs for vgll3 and sea age for adults and parentage data

####packages####
library (tidyverse)
library (cowplot)
library (lme4)
library (lmerTest)
library (MASS)
library (DHARMa)

####database####
Uts_cohort_SNP_conserved <- read.csv("Data/Uts_cohort_SNP_cons_11.02.22.csv")
UtsSNP <- read.csv("Data/UtsSNP_21.04.13.csv")
Utsadults <- read.csv("Data/UtsadultsALL_21.06.22.csv")

#colorblind palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7","#999999", "#F0E442")

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

#Parentage dataset: vgll3 X sea age models 
mod_seaagevgll3RS <- glm(seaageatmaturity ~ sex + as.factor(c25_1441_SAC), data=Uts_conserved_total_RS)
summary(mod_seaagevgll3RS)
#simulate residuals
resmod_seaagevgll3RS <- simulateResiduals(mod_seaagevgll3RS, plot = T)

#create table of offspring means 
table.vgll3.conserved <- Uts_conserved_total_RS %>%
  group_by(c25_1441_SAC, sex, n.rm=T) %>%
  summarise_if(is.numeric, funs(mean(., na.rm=T), n = sum(!is.na(.)), se = sd(., na.rm=T)/sqrt(sum(!is.na(.)))))

####sea age vs vgll3 genotype ####
#graph vgll3top conserved versus seaage sires/dams
pVgll3.conserved.seaage <-ggplot(data=table.vgll3.conserved) + 
  geom_point(aes(y=seaageatmaturity_mean, x=c25_1441_SAC, color = sex), size=5, alpha = 4/5, position = position_dodge(width=0.2)) +
  geom_errorbar(aes(x=c25_1441_SAC, ymin=seaageatmaturity_mean-seaageatmaturity_se, ymax=seaageatmaturity_mean+seaageatmaturity_se, color = sex), width=.2, size=1.2, position = position_dodge(width=0.2)) +
  scale_color_manual(values=c("darkorange", "#4271AE"), name = "", labels = c("dam", "sire")) +
  geom_jitter(data=Uts_conserved_total_RS, aes(y=seaageatmaturity, x=c25_1441_SAC, color = sex), position = position_jitterdodge(0.1, 0.1, 0.4), stat="identity", alpha = 1/10) +
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
        panel.background = element_blank(),
        legend.key = element_rect(color = "transparent", fill = "transparent")) 
#show graph
pVgll3.conserved.seaage


#count seaage (needed for bubble plot)
Uts_conserved_total_RScount <- Uts_conserved_total_RS %>%
  group_by(sex, c25_1441_SAC, n.rm=T) %>%
  count(seaageatmaturity)

#graph vgll3top conserved versus seaage sires/dams (bubble plot version)
pVgll3.conserved.seaage1 <-ggplot(data=table.vgll3.conserved) + 
  geom_point(aes(y=seaageatmaturity_mean, x=c25_1441_SAC, color = sex), size=5, alpha = 4/5, position = position_dodge(width=0.2)) +
  geom_errorbar(aes(x=c25_1441_SAC, ymin=seaageatmaturity_mean-seaageatmaturity_se, ymax=seaageatmaturity_mean+seaageatmaturity_se, color = sex), width=.2, size=1.2, position = position_dodge(width=0.2)) +
  scale_color_manual(values=c("darkorange", "#4271AE"), name = "", labels = c("dam", "sire")) +
  geom_point(data=Uts_conserved_total_RScount, aes(y=seaageatmaturity, x=c25_1441_SAC, colour=sex, size=n), alpha = 1/5, position = position_dodge(width=0.4)) +
  scale_size(range = c(.1, 20), name="n") +
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
        panel.background = element_blank(),
        legend.key = element_rect(color = "transparent", fill = "transparent"))
#show graph
pVgll3.conserved.seaage1

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

#vgll3 models 
mod_seaagevgll3RSadult <- glm(seaageatmaturity ~ sex.x + as.factor(c25_1441_SAC), data=Uts_adults_SNPx)
summary(mod_seaagevgll3RSadult)
#simulate residuals
resmod_seaagevgll3RSadult <- simulateResiduals(mod_seaagevgll3RSadult, plot = T)

#create table of offspring means 
#####table means Vgll3top####
tmeansXVgll3top <- Uts_adults_SNPx %>%
  group_by(sex.x, c25_1441_SAC, n.rm=T) %>%
  summarise_if(is.numeric, funs(mean(., na.rm=T), n = sum(!is.na(.)), se = sd(., na.rm=T)/sqrt(sum(!is.na(.)))))

#graph vgll3top conserved versus seaage (no reproduction)
pVgll3topXSW<-ggplot(data=tmeansXVgll3top) + 
  geom_point(aes(x=c25_1441_SAC, y=seaageatmaturity_mean, color=sex.x), size=5, alpha = 4/5, position = position_dodge(width=0.2)) +
  geom_errorbar(aes(x=c25_1441_SAC, ymin=seaageatmaturity_mean-seaageatmaturity_se, ymax=seaageatmaturity_mean+seaageatmaturity_se, color=sex.x),  width=.2, size=1.2, position = position_dodge(width=0.2)) +
  scale_color_manual(values=c("darkorange", "#4271AE"), name = "", labels = c("female", "male"))  +
  geom_jitter(data=Uts_adults_SNPx, aes(y=seaageatmaturity, x=c25_1441_SAC, colour=sex.x), position = position_jitterdodge(0.1, 0.1, 0.4), size=2, stat="identity", alpha = 1/10) +
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
        panel.background = element_blank(),
        legend.key = element_rect(color = "transparent", fill = "transparent"))
#show graph
pVgll3topXSW

#count seaage (needed for bubble plot)
tmeansXVgll3topcount <- Uts_adults_SNPx %>%
  group_by(sex.x, c25_1441_SAC, n.rm=T) %>%
  count(seaageatmaturity)

#graph vgll3top conserved versus seaage (no reproduction) bubble plot
pVgll3topXSW1<-ggplot(data=tmeansXVgll3top) + 
  geom_point(aes(y=seaageatmaturity_mean, x=c25_1441_SAC, color=sex.x), size=5, alpha = 4/5, position = position_dodge(width=0.2)) +
  geom_errorbar(aes(x=c25_1441_SAC, ymin=seaageatmaturity_mean-seaageatmaturity_se, ymax=seaageatmaturity_mean+seaageatmaturity_se, color=sex.x),  width=.2, size=1.2, position = position_dodge(width=0.2)) +
  geom_point(data=tmeansXVgll3topcount, aes(y=seaageatmaturity, x=c25_1441_SAC, color=sex.x, size=n), alpha = 1/5, position = position_dodge(width=0.4)) +
  scale_color_manual(values=c("darkorange", "#4271AE"), name = "", labels = c("female", "male"))  +
  scale_size(range = c(.1, 20), name="n") +
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
        panel.background = element_blank(),
        legend.key = element_rect(color = "transparent", fill = "transparent")) 
#show graph
pVgll3topXSW1

#sea age vs vgll3 genotype figure
fig_2A <- pVgll3topXSW + theme(plot.margin = unit(c(10, 0, 0, 10), units = "pt"))
fig_2B <- pVgll3.conserved.seaage + theme(plot.margin = unit(c(10, 0, 0, 10), units = "pt"))
# Fig plot
fig_2<-plot_grid(fig_2A, fig_2B, 
                label_y = 1,
                labels = c('A', 'B'), label_size = 24, ncol = 2, align = "v")
#show plot
fig_2

#sea age vs vgll3 genotype figure
fig_2Aalt <- pVgll3topXSW1 + theme(plot.margin = unit(c(10, 0, 0, 10), units = "pt"))
fig_2Balt <- pVgll3.conserved.seaage1 + theme(plot.margin = unit(c(10, 0, 0, 10), units = "pt"))

# Fig plot
fig_2alt<-plot_grid(fig_2Aalt, fig_2Balt, 
                 label_y = 1,
                 labels = c('A', 'B'), label_size = 24, ncol = 2, align = "v")
#show plot
fig_2alt
