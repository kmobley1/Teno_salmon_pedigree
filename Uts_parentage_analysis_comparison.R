#parentage dataset comparisons
#default = all age difference priors as estimated by sequoia
#informed = priors for age gap of 0 and 1 for males, and 0, 1, 2 and 3 for females set to 0
#conservative = all priors less than 0.1 set to zero, to exclude all of the most improbable relationships

####packages####
library (tidyverse)
library (ggridges)
library (cowplot)

####data####
Uts_cohort_SNP_default <- read.csv("Data/Uts_cohort_SNP_default_11.02.22.csv")
Uts_cohort_SNP_informed <- read.csv("Data/Uts_cohort_SNP_informed_11.02.22.csv")
Uts_cohort_SNP_conserved <- read.csv("Data/Uts_cohort_SNP_cons_11.02.22.csv")


#colorblind palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7","#999999", "#F0E442")

####count data
#how many unique sires and dams as parents? (default dataset)
Uts_cohort_SNP_default %>%
  select(ID, sex, type, n.offspring) %>%
  group_by(sex, type) %>%
  summarise(count = n_distinct(ID))

#how many unique sires and dams as parents? (informed dataset)
Uts_cohort_SNP_informed %>%
  select(ID, sex, type, n.offspring) %>%
  group_by(sex, type) %>%
  summarise(count = n_distinct(ID))

#how many unique sires and dams as parents? (conserved dataset)
Uts_cohort_SNP_conserved %>%
  select(ID, sex, type, n.offspring) %>%
  group_by(sex, type) %>%
  summarise(count = n_distinct(ID))

#how many offspring assigned to sires and dams? (default dataset)
Uts_cohort_SNP_default %>%
  select(ID, sex, type, n.offspring) %>%
  group_by(sex, type) %>%
  tally(n.offspring)

#how many offspring assigned to sires and dams? (informed dataset)
Uts_cohort_SNP_informed %>%
  select(ID, sex, type, n.offspring) %>%
  group_by(sex, type) %>%
  tally(n.offspring)

#how many offspring assigned to sires and dams? (conserved dataset)
Uts_cohort_SNP_conserved %>%
  select(ID, sex, type, n.offspring) %>%
  group_by(sex, type) %>%
  tally(n.offspring)

####ridgelineplot of age at maturity####
#Uts default 21.06.18
#calculate ageatmaturity
Uts_cohort_default <- Uts_cohort_SNP_default %>%
    mutate(ageatmaturity = firstcohort - birthyear.int) 

#select age at maturity for distinct dams
Uts_cohort_default_dams <- Uts_cohort_default %>%
  filter(sex == "dam") %>%
  distinct(ID, class.cor, ageatmaturity) 

#make ridgelineplot
Histo.default_dams <- ggplot(data=Uts_cohort_default_dams, aes(x=ageatmaturity, y=class.cor)) +
  geom_density_ridges(aes(fill = class.cor), scale = 1, alpha = 0.7) +
  scale_fill_manual(values = cbPalette) +
  labs(x="", y="", fill="") + 
  scale_x_continuous(limits = c(0,10), breaks = c(0, 2, 4, 6, 8, 10)) +
  theme(legend.position = "none") +
  theme(plot.title=element_text(size=24, hjust=-0.5)) +
  theme(axis.title.x = element_text(size=16)) +  theme(axis.text.x = element_text(size=12, color="black")) +
  theme(axis.title.y = element_text(size=16, vjust=1, angle = 90)) +  theme(axis.text.y = element_text(size=12, color="black")) +
  theme(axis.line = element_line(size = 1)) + theme(axis.ticks = element_line(size=1)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

#show historgram
Histo.default_dams

#select age at maturity for distinct males
Uts_cohort_default_sires <- Uts_cohort_default %>%
  filter(sex == "sire") %>%
  distinct(ID, class.cor, ageatmaturity) 

#make ridgelineplot
Histo.default_sires <- ggplot(data=Uts_cohort_default_sires, aes(x=ageatmaturity, y=class.cor)) +
  geom_density_ridges(aes(fill = class.cor), scale = 1, alpha = 0.7) +
  scale_fill_manual(values = cbPalette) +
  labs(x="", y="", fill="") + 
  scale_x_continuous(limits = c(0,10), breaks = c(0, 2, 4, 6, 8, 10)) +
  theme(legend.position = "none") +
  theme(plot.title=element_text(size=24, hjust=-0.5)) +
  theme(axis.title.x = element_text(size=16)) +  theme(axis.text.x = element_text(size=12, color="black")) +
  theme(axis.title.y = element_text(size=16, vjust=1, angle = 90)) +  theme(axis.text.y = element_text(size=12, color="black")) +
  theme(axis.line = element_line(size = 1)) + theme(axis.ticks = element_line(size=1)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

#show histogram
Histo.default_sires 

#Uts informed 21.06.18
#calculate ageatmaturity
Uts_cohort_informed2 <- Uts_cohort_SNP_informed %>%
  mutate(ageatmaturity = firstcohort - birthyear.int) 

#select age at maturity for distinct dams
Uts_cohort_informed2_dams <- Uts_cohort_informed2 %>%
  filter(sex == "dam") %>%
  distinct(ID, class.cor, ageatmaturity) 

#make ridgelineplot
Histo.informed2_dams <- ggplot(data=Uts_cohort_informed2_dams, aes(x=ageatmaturity, y=class.cor)) +
  geom_density_ridges(aes(fill = class.cor), scale = 1, alpha = 0.7) +
  scale_fill_manual(values = cbPalette) +
  labs(x="", y="", fill="") +
  scale_x_continuous(limits = c(0,10), breaks = c(0, 2, 4, 6, 8, 10)) +
  theme(legend.position = "none") +
  theme(plot.title=element_text(size=24, hjust=-0.5)) +
  theme(axis.title.x = element_text(size=16)) +  theme(axis.text.x = element_text(size=12, color="black")) +
  theme(axis.title.y = element_text(size=16, vjust=1, angle = 90)) +  theme(axis.text.y = element_text(size=12, color="black")) +
  theme(axis.line = element_line(size = 1)) + theme(axis.ticks = element_line(size=1)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

#show historgram
Histo.informed2_dams

#select age at maturity for distinct males
Uts_cohort_informed2_sires <- Uts_cohort_informed2 %>%
  filter(sex == "sire") %>%
  distinct(ID, class.cor, ageatmaturity) 

#make ridgelineplot
Histo.informed2_sires <- ggplot(data=Uts_cohort_informed2_sires, aes(x=ageatmaturity, y=class.cor)) +
  geom_density_ridges(aes(fill = class.cor), scale = 1, alpha = 0.7) +
  scale_fill_manual(values = cbPalette) +
  labs(x="", y="", fill="") + 
  scale_x_continuous(limits = c(0,10), breaks = c(0, 2, 4, 6, 8, 10)) +
  theme(legend.position = "none") +
  theme(plot.title=element_text(size=24, hjust=-0.5)) +
  theme(axis.title.x = element_text(size=16)) +  theme(axis.text.x = element_text(size=12, color="black")) +
  theme(axis.title.y = element_text(size=16, vjust=1, angle = 90)) +  theme(axis.text.y = element_text(size=12, color="black")) +
  theme(axis.line = element_line(size = 1)) + theme(axis.ticks = element_line(size=1)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

#show histogram
Histo.informed2_sires

#Uts conserved 21.06.18
#calculate ageatmaturity
Uts_cohort_conserved2 <- Uts_cohort_SNP_conserved %>%
  mutate(ageatmaturity = firstcohort - birthyear.int) 

#select age at maturity for distinct dams
Uts_cohort_conserved2_dams <- Uts_cohort_conserved2 %>%
  filter(sex == "dam") %>%
  distinct(ID, class.cor, ageatmaturity) 

#make ridgelineplot
Histo.conserved2_dams <- ggplot(data=Uts_cohort_conserved2_dams, aes(x=ageatmaturity, y=class.cor)) +
  geom_density_ridges(aes(fill = class.cor), scale = 1, alpha = 0.7) +
  scale_fill_manual(values = cbPalette) +
  labs(x="Age at maturity", y="", fill="") + 
  scale_x_continuous(limits = c(0,10), breaks = c(0, 2, 4, 6, 8, 10)) +
  theme(legend.position = "none") +
  theme(plot.title=element_text(size=24, hjust=-0.5)) +
  theme(axis.title.x = element_text(size=16)) +  theme(axis.text.x = element_text(size=12, color="black")) +
  theme(axis.title.y = element_text(size=16, vjust=1, angle = 90)) +  theme(axis.text.y = element_text(size=12, color="black")) +
  theme(axis.line = element_line(size = 1)) + theme(axis.ticks = element_line(size=1)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

#show histogram
Histo.conserved2_dams

#select age at maturity for distinct males
Uts_cohort_conserved2_sires <- Uts_cohort_conserved2 %>%
  filter(sex == "sire") %>%
  distinct(ID, class.cor, ageatmaturity) 

#make ridgelineplot
Histo.conserved2_sires <- ggplot(data=Uts_cohort_conserved2_sires, aes(x=ageatmaturity, y=class.cor)) +
  geom_density_ridges(aes(fill = class.cor), scale = 1, alpha = 0.7) +
  scale_fill_manual(values = cbPalette) +
  labs(x="Age at maturity", y="", fill="") + 
  scale_x_continuous(limits = c(0,10), breaks = c(0, 2, 4, 6, 8, 10)) +
  theme(legend.position = "none") +
  theme(plot.title=element_text(size=24, hjust=-0.5)) +
  theme(axis.title.x = element_text(size=16)) +  theme(axis.text.x = element_text(size=12, color="black")) +
  theme(axis.title.y = element_text(size=16, vjust=1, angle = 90)) +  theme(axis.text.y = element_text(size=12, color="black")) +
  theme(axis.line = element_line(size = 1)) + theme(axis.ticks = element_line(size=1)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

#show historgram
Histo.conserved2_sires

#make layout of age at maturity distribution graphs in 2 columns on a page (cowplot)
#theme_set(theme_cowplot())
fig_A <- Histo.default_dams + theme(plot.margin = unit(c(10, 0, 0, 0), units = "pt"))
fig_B <- Histo.informed2_dams + theme(plot.margin = unit(c(10, 0, 0, 0), units = "pt"))
fig_C <- Histo.conserved2_dams + theme(plot.margin = unit(c(10, 0, 0, 0), units = "pt"))
fig_D <- Histo.default_sires + theme(plot.margin = unit(c(10, 0, 0, 0), units = "pt"))
fig_E <- Histo.informed2_sires + theme(plot.margin = unit(c(10, 0, 0, 0), units = "pt"))
fig_F <- Histo.conserved2_sires + theme(plot.margin = unit(c(10, 0, 0, 0), units = "pt"))
Fig.ageatmaturity.dist<-plot_grid(fig_A, fig_D, fig_B, fig_E, fig_C, fig_F,
               label_y = 1.0,
               label_x = 0.05,
               labels = c('Dams default', 'Sires default', 'Dams informed', 'Sires informed', 'Dams conserved', 'Sires conserved'), label_size = 12, ncol = 2, align = "v")
#show plot
Fig.ageatmaturity.dist

#saveplot
save_plot("images/Figageatmaturity.dist2.png", Fig.ageatmaturity.dist, base_height = 11.7, base_width = 8.3)









