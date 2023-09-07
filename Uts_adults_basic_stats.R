#Uts_adults basic stats
####libraries####
library(tidyverse)
library(cowplot)

#datasets####
Utsadults <- read.csv("C:/Users/kmo107/OneDrive - UiT Office 365/Documents/projects/Atlantic salmon - Teno River Pedigree/2020 - Utsjoki pedigree data/Teno_salmon_pedigree/UtsadultsALL_21.06.22.csv")

#colorblind palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7","#999999", "#F0E442")

#fill in 0 for NAs in respawner and recapture columms
Utsadults11_18 <- Utsadults %>%
  mutate(Respawner = replace_na(Respawner, 0)) %>%
  mutate(recapture = replace_na(recapture, 0))

#How many adults? 2011 -2018
Utsadults11_18 %>% count()

#how many for each sex?
Utsadults11_18 %>% group_by(sex) %>% count()

#how many are recaptures? (2 = recapture)
Utsadults11_18 %>% group_by(sex, recapture) %>% count()

#How many have scale smolt (i.e. freshwater) age? remove recaptures
Utsadults11_18 %>% filter(recapture <= 1) %>% count(Scale.smoltage)

#mean scale age, remove recaptures
Utsadults11_18 %>% filter(recapture <= 1) %>% select(sex, Scale.smoltage) %>% group_by(sex) %>% dplyr:: summarise_if(is.numeric, funs(mean(., na.rm=T), n = sum(!is.na(.)), se = sd(., na.rm=T)/sqrt(sum(!is.na(.)))))
  
#How many have seaage age? remove recaptures
Utsadults11_18 %>% filter(recapture <= 1) %>% count(scale.seaage)

#mean scale age, seaage remove recaptures
Utsadults11_18 %>% filter(recapture <= 1) %>% select(sex, scale.seaage) %>% group_by(sex) %>% dplyr:: summarise_if(is.numeric, funs(mean(., na.rm=T), n = sum(!is.na(.)), se = sd(., na.rm=T)/sqrt(sum(!is.na(.)))))

####make table of total individuals#### 
uts.adults.ind <- Utsadults11_18 %>%
  count(year, sex) 

#create grouped bar plot of #collected individuals/year/class
p.adults.ind <-  ggplot(uts.adults.ind, aes(factor(year), n, fill = sex)) +geom_bar(stat = "identity", position = "dodge") +
  labs(y="No. individuals", x="Year") + 
  scale_fill_manual(name = "", labels = c("Female", "Male"), values = c("orange", "blue")) +
  scale_y_continuous(limits = c(0, 150), expand = c(0, 0)) +
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
p.adults.ind

#saveplot
#save_plot("C:/Users/kmo107/OneDrive - UiT Office 365/Documents/projects/Atlantic salmon - Teno River Pedigree/2020 - Utsjoki pedigree data/Teno_salmon_pedigree/images/Fig.adultsamples.png", p.adults.ind, base_height = 10.7, base_width = 7.6)
