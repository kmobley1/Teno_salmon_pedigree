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



####check # of offspring assigned
Uts_parentage_conserved <- read.csv("Data/Uts_parentage_conserved_21.06.22.csv")

#how many assigned to dams
Uts_parentage_conserved %>% filter(!is.na(dam)) %>% n_distinct("dam")
#make table
dams <- Uts_parentage_conserved %>% filter(!is.na(dam)) %>% group_by(ID) %>% count(dam)

#how many assigned to sires
Uts_parentage_conserved %>% filter(!is.na(sire)) %>% n_distinct("sire")
#make table
sires <- Uts_parentage_conserved %>% filter(!is.na(sire)) %>% group_by(ID) %>% count(sire)
  
#how many families with 1 parent assignment?
oneparent <- full_join(dams, sires) 
#how many
oneparent %>% ungroup() %>% count()

#how many offspring have at least one parent
twoparent <- inner_join(dams, sires) 
#how many
twoparent %>% ungroup() %>% count()

#how many unique families in two parent?
twoparent %>% ungroup() %>% distinct(dam, sire) %>% count()





#check Uts cohorts SNP cons
Uts_cohorts_SNP_cons <- read.csv("Data/Uts_cohort_SNP_cons_11.02.22.csv")

#all
Uts_cohorts_SNP_cons %>% tally(n.offspring)

#dams
Uts_cohorts_SNP_cons %>% filter(sex == "dam") %>% tally(n.offspring)

#sires
Uts_cohorts_SNP_cons %>% filter(sex == "sire") %>% tally(n.offspring)


#look at # individuals / genotype / year


##remove individuals that have cohorts before 2012, and after 2017


####database####
Uts_cohort_SNP_conserved <- read.csv("Data/Uts_cohort_SNP_cons_11.02.22.csv")

#colorblind palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7","#999999", "#F0E442")

uts_conserved_table <- Uts_cohort_SNP_conserved %>%
  filter(firstcohort != 2007) %>%
  filter(firstcohort != 2009) %>%
  filter(firstcohort != 2010) %>%
  filter(firstcohort != 2011) %>% 
  filter(firstcohort != 2018) %>% 
  filter(firstcohort != 2019) %>%
  group_by(sex, firstcohort, c25_1441_SAC) %>%
  tally()

#dams
uts_con_dams <- uts_conserved_table %>%
  filter(sex == "dam")
#sires
uts_con_sires <- uts_conserved_table %>%
  filter(sex == "sire")

#create grouped bar plot of #collected individuals/year/class
p.cons.dams <-  ggplot(uts_con_dams) + aes(x = firstcohort, y = n, fill = factor(c25_1441_SAC)) + geom_bar(stat = "identity", position = "dodge") +
  labs(y="No. individuals", x="Year") + 
  scale_fill_manual(name = "", labels = c("EE", "EL", "LL"), values = c("#E69F00", "#56B4E9", "#009E73")) +
  scale_y_continuous(limits = c(0, 40), expand = c(0, 0)) +
  coord_cartesian(clip = 'off') +
  theme(plot.title=element_text(size=36, hjust=-0.5), legend.title=element_text(size=20), legend.text=element_text(size=16)) +
  theme(axis.title.x = element_text(size=24)) +  theme(axis.text.x = element_text(size=20, color="black")) +
  theme(axis.title.y = element_text(size=24, vjust=3, angle = 90)) +  theme(axis.text.y = element_text(size=20, color="black")) +
  theme(axis.line = element_line(size = 1)) + theme(axis.ticks = element_line(size=1)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.margin=unit(c(1,0,0,1), "cm"))

#show graph
p.cons.dams 

#create grouped bar plot of #collected individuals/year/class
p.cons.sires <-  ggplot(uts_con_sires) + aes(x = firstcohort, y = n, fill = factor(c25_1441_SAC)) + geom_bar(stat = "identity", position = "dodge") +
  labs(y="No. individuals", x="Year") + 
  scale_fill_manual(name = "", labels = c("EE", "EL", "LL"), values = c("#E69F00", "#56B4E9", "#009E73")) +
  scale_y_continuous(limits = c(0, 40), expand = c(0, 0)) +
  coord_cartesian(clip = 'off') +
  theme(plot.title=element_text(size=36, hjust=-0.5), legend.title=element_text(size=20), legend.text=element_text(size=16)) +
  theme(axis.title.x = element_text(size=24)) +  theme(axis.text.x = element_text(size=20, color="black")) +
  theme(axis.title.y = element_text(size=24, vjust=3, angle = 90)) +  theme(axis.text.y = element_text(size=20, color="black")) +
  theme(axis.line = element_line(size = 1)) + theme(axis.ticks = element_line(size=1)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.margin=unit(c(1,0,0,1), "cm"))

#show graph
p.cons.sires 


