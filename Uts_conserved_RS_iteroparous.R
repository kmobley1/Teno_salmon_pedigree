#Reproductive fitness with conservative parentage analysis
#Using the conservative parentage analysis dataset, create glm models and graphs for total reproductive success and reproductive success for the first reproduction for sires and dams separately
#iteroparous and semelparous individuals

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


####vgll3 comparison just with 1st cohort of each sex####
#filter first cohort (select only 1st reproductive event)
#remove individuals that have cohorts before 2012, and after 2017
Uts_cohort_SNP_conserved_1C <- Uts_cohort_SNP_conserved %>%
  filter(cohort.rank == 1) %>%
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
Uts_conserved_total_RS_dams_1C <- Uts_cohort_SNP_conserved_1C %>%
  filter(sex == "dam") 
#sires  
Uts_conserved_total_RS_sires_1C <- Uts_cohort_SNP_conserved_1C %>%
  filter(sex == "sire") 

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

#fig 3 dams/sires first reproduction RS iteroparous v semelparous
fig_3A <- pVgll3.conserved_1C_dams + theme(plot.margin = unit(c(10, 0, 0, 10), units = "pt"))
fig_3B <- pseaage.RSdams + theme(plot.margin = unit(c(10, 0, 0, 10), units = "pt"))
#fig 4 dams/sires first reproduction iteroparous v semelparous
fig_4A <- pVgll3.conserved_1C_sires + theme(plot.margin = unit(c(10, 0, 0, 10), units = "pt"))
fig_4B <- pseaage.conserved_1C_sires_seaage + theme(plot.margin = unit(c(10, 0, 0, 10), units = "pt"))

#combined figure
RSiteroparous<-plot_grid(fig_3A, fig_3B, fig_4A, fig_4B,
                label_y = 1,
                axis = "tbl",
                labels = c('A', 'B', 'C', 'D'), label_size = 24, ncol = 2, align = "v")
#show plot 
RSiteroparous

