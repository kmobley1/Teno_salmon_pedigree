#calculate life history strategies of adults and reconstructed genotypes

library(tidyverse)
library (cowplot)

#datasets
Uts_cohort_SNP_conserved <- read.csv("Data/Uts_cohort_SNP_cons_11.02.22.csv")

#colorblind palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7","#999999", "#F0E442")

#add down rows for classes within cohorts
##remove individuals that have cohorts before 2012, and after 2017
Uts_cohort_SNP_conserved_LHS <- Uts_cohort_SNP_conserved %>%
  filter(firstcohort != 2007) %>%
  filter(firstcohort != 2009) %>%
  filter(firstcohort != 2010) %>%
  filter(firstcohort != 2011) %>% 
  filter(firstcohort != 2018) %>% 
  filter(firstcohort != 2019) 

#How many life history strategies are there in adults?
#with repeat info
Uts_LHS <- Uts_cohort_SNP_conserved_LHS %>%
  filter(type == "Adult", !is.na(respawner.info)) %>%
  unite("LHS", c("Scale.smoltage", "respawner.info"), sep = "-", na.rm = TRUE, remove = FALSE) %>%
  group_by(sex, LHS) %>%
  count()

#now without respawners
Uts_LHS2 <- Uts_cohort_SNP_conserved_LHS %>%
  filter(type == "Adult", is.na(respawner.info)) %>%
  unite("LHS", c("Scale.smoltage", "InterpAge"), sep = "-", na.rm = TRUE, remove = FALSE) %>% 
  group_by(sex, LHS) %>% 
  count()  

#merge datasets
Uts_lifehistory_strategies <- bind_rows(Uts_LHS, Uts_LHS2) 

#fill in columns that are missing
Uts_lifehistory_strategies2 <- Uts_lifehistory_strategies %>%
  spread(LHS, n, fill=0) %>%
  gather(LHS, n, -sex)

#split into Norespawninfo versus repeat spawners
#Norespawninfo
LHS_Norespawninfo <- Uts_lifehistory_strategies2 %>%
  filter(!grepl('S', LHS))

#repeat spawners
LHS_RS <- Uts_lifehistory_strategies2 %>%
  filter(grepl('S', LHS))

#make graph for females and males
#create grouped bar plot of LHS
p.LHSNorespawninfo <-  ggplot(LHS_Norespawninfo, aes(n, LHS, fill = sex)) +geom_bar(stat = "identity", position = "dodge") +
  labs(y="Life history strategy", x="") + 
  scale_fill_manual(values=cbPalette, name = "Sex", labels = c("dam", "sire")) +
  scale_x_continuous(limits = c(0, 100)) +
  theme(plot.title=element_text(size=24, hjust=-0.5)) +
  theme(axis.title.x = element_text(size=20)) +  theme(axis.text.x = element_text(size=16, color="black")) +
  theme(axis.title.y = element_text(size=20, vjust=-10, angle = 90)) +  theme(axis.text.y = element_text(size=16, color="black")) +
  theme(axis.line = element_line(size = 1)) + theme(axis.ticks = element_line(size=1)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.margin=unit(c(1,0,0,1), "cm"))
#show graph
p.LHSNorespawninfo

#make graph for females and males
#create grouped bar plot of LHS for repeat spawners
p.LHSRS <-  ggplot(LHS_RS, aes(n, LHS, fill = sex)) +geom_bar(stat = "identity", position = "dodge") +
  labs(y="", x="") + 
  scale_fill_manual(values=cbPalette, name = "Sex", labels = c("dam", "sire")) +
  scale_x_continuous(limits = c(0, 40)) +
  theme(plot.title=element_text(size=24, hjust=-0.5)) +
  theme(axis.title.x = element_text(size=20)) +  theme(axis.text.x = element_text(size=16, color="black")) +
  theme(axis.title.y = element_text(size=20, vjust=3, angle = 90)) +  theme(axis.text.y = element_text(size=16, color="black")) +
  theme(axis.line = element_line(size = 1)) + theme(axis.ticks = element_line(size=1)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.margin=unit(c(1,0,0,1), "cm"))
#show graph
p.LHSRS


####life history strategies of parentage genotypes####
Uts_cohort_LHS_wide <- Uts_cohort_SNP_conserved_LHS %>% 
  group_by(ID) %>%
  select(ID, sex, type, class.cor, Scale.smoltage, InterpAge, seaageatmaturity, respawner.info, respawner.gen, firstcohort, cohort.total, cohort.rank, cohort.year.all) %>%
  pivot_wider(
        names_from = cohort.rank,
        values_from = cohort.year.all,
        names_prefix = "cohort",
        values_fill = NA, 
        values_fn = sum) 

#calculate time between cohorts
Uts_cohort_LHS_pause <- Uts_cohort_LHS_wide %>%
  mutate(S1 = cohort2-cohort1-1) %>%
  mutate(S2 = cohort3-cohort2-1) %>%
  mutate(S3 = cohort4-cohort3-1)

#unite scale and seaage data (everything)
Uts_cohort_LHS_gen <- Uts_cohort_LHS_pause %>%
    unite("LHS", c("Scale.smoltage", "seaageatmaturity"), sep = "-", na.rm = TRUE, remove = FALSE)  

#unite data
Uts_cohort_LHS_gen_S <- Uts_cohort_LHS_gen %>%
    unite("LHSall", c("LHS", "S1", "S2", "S3"), sep = "S", na.rm = TRUE, remove = FALSE)

#select columns of interest, filter data for adults, count 
Uts_LHS_gen <- Uts_cohort_LHS_gen_S %>%
  filter(type == "Adult") %>%
  select(ID, LHSall, sex) %>%
  group_by(LHSall, sex) %>%
  count()

#fill in columns that are missing
Uts_LHS_gen2 <- Uts_LHS_gen %>%
  spread(LHSall, n, fill=0) %>%
  gather(LHSall, n, -sex)

#split into Norespawninfo? versus repeat spawners
#Norespawninfo
LHS_gen_Norespawninfo <- Uts_LHS_gen2 %>%
  filter(!grepl('S', LHSall))

#repeat spawners
LHS_gen_RS <- Uts_LHS_gen2 %>%
  filter(grepl('S', LHSall))

#make graph for females and males
#create grouped bar plot of Norespawninfo spawners from parentage data
p.LHSgen_Norespawninfo <-  ggplot(LHS_gen_Norespawninfo, aes(n, LHSall, fill = sex)) +geom_bar(stat = "identity", position = "dodge") +
  labs(y="Life history strategy", x="No. individuals") + 
  scale_fill_manual(values=cbPalette, name = "Sex", labels = c("dam", "sire")) +
  scale_x_continuous(limits = c(0, 100)) +
  theme(plot.title=element_text(size=24, hjust=-0.5)) +
  theme(axis.title.x = element_text(size=20)) +  theme(axis.text.x = element_text(size=16, color="black")) +
  theme(axis.title.y = element_text(size=20, vjust=-10, angle = 90)) +  theme(axis.text.y = element_text(size=16, color="black")) +
  theme(axis.line = element_line(size = 1)) + theme(axis.ticks = element_line(size=1)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.margin=unit(c(1,0,0,1), "cm"))
#show graph
p.LHSgen_Norespawninfo     

#create grouped bar plot of Norespawninfo spawners from parentage data
p.LHSgen_RS<-  ggplot(LHS_gen_RS, aes(n, LHSall, fill = sex)) +geom_bar(stat = "identity", position = "dodge") +
  labs(y="", x="No. individuals") + 
  scale_fill_manual(values=cbPalette, name = "Sex", labels = c("dam", "sire")) +
  scale_x_continuous(limits = c(0, 40)) +
  theme(plot.title=element_text(size=24, hjust=-0.5)) +
  theme(axis.title.x = element_text(size=20)) +  theme(axis.text.x = element_text(size=16, color="black")) +
  theme(axis.title.y = element_text(size=20, vjust=1, angle = 90)) +  theme(axis.text.y = element_text(size=16, color="black")) +
  theme(axis.line = element_line(size = 1)) + theme(axis.ticks = element_line(size=1)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.margin=unit(c(1,0,0,1), "cm"))
#show graph
p.LHSgen_RS    

####make layout of graphs in 2 columns on a page (cowplot)####
figLHS_A <- p.LHSNorespawninfo + theme(plot.margin = unit(c(10, 0, 0, 0), units = "pt"))
figLHS_B <- p.LHSRS  + theme(plot.margin = unit(c(10, 0, 0, 0), units = "pt"))
figLHS_C <- p.LHSgen_Norespawninfo  + theme(plot.margin = unit(c(10, 0, 0, 0), units = "pt"))
figLHS_D <- p.LHSgen_RS  + theme(plot.margin = unit(c(10, 0, 0, 0), units = "pt"))
FigLHS<-plot_grid(figLHS_A+ theme(legend.position="none"), figLHS_B + theme(legend.position="none"), figLHS_C + theme(legend.position="none"), figLHS_D + theme(legend.position="none"), NULL, NULL,
                label_y = 1,
                hjust = -1,
                vjust = 1,
                labels = c('A', 'B', 'C', 'D', '', ''), label_size = 24, ncol = 2, nrow = 2, align = "v")
FigLHS



#how many unique LHS if sires and dams are not respawners? (scale data only)
LHS_Norespawninfo %>% group_by(sex) %>% summarise_all(~sum(. != 0))

#how many unique LHS if how many sires and dams are respawners (scale data only)
LHS_RS %>% group_by(sex) %>% summarise_all(~sum(. != 0))

#how many unique LHS if how many sires and dams are not respawners? (scale + parentage analysis)
LHS_gen_Norespawninfo %>% group_by(sex) %>% summarise_all(~sum(. != 0))

#how many unique LHS if how many sires and dams are respawners (scale + parentage analysis)
LHS_gen_RS %>% group_by(sex) %>% summarise_all(~sum(. != 0))
