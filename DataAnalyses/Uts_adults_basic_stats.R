#Uts_adults basic stats
####libraries####
library(tidyverse)
library(cowplot)

#datasets####
Utsadults <- read.csv("Data/UtsadultsALL_21.06.22.csv")

#colorblind palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7","#999999", "#F0E442")

#fill in 0 for NAs in respawner and recapture columms
#calculate age at maturity (1st reproduction)
Utsadults11_18 <- Utsadults %>%
  mutate(Respawner = replace_na(Respawner, 0)) %>%
  mutate(recapture = replace_na(recapture, 0)) %>%
  mutate(ageatmaturity = ifelse(respawner.info == "1S1", 1,
                                       ifelse(respawner.info == "2S1", 2,  
                                              ifelse(respawner.info == "3S1", 3, InterpAge)))) %>%
  relocate(ageatmaturity, .after = respawner.info)

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
  
Utsadults11_18smolt <- Utsadults11_18 %>%
  filter(recapture <= 1) 

smoltage <- lm(Scale.smoltage ~ sex, data = Utsadults11_18smolt)
summary(smoltage)


#How many have seaage age? remove recaptures
Utsadults11_18 %>% filter(recapture <= 1) %>% count(scale.seaage)

#age at maturity range 
Utsadults11_18 %>% filter(recapture <= 1) %>% group_by(sex) %>% count(ageatmaturity)

#age at maturity range 
Utsadults11_18mat <- Utsadults11_18 %>%
    filter(recapture <= 1) 

ageatmat <- lm(ageatmaturity ~ sex, data = Utsadults11_18mat)
summary(ageatmat)

#mean age at maturity remove recaptures
Utsadults11_18 %>% filter(recapture <= 1) %>% select(sex, ageatmaturity) %>% group_by(sex) %>% dplyr:: summarise_if(is.numeric, funs(mean(., na.rm=T), n = sum(!is.na(.)), se = sd(., na.rm=T)/sqrt(sum(!is.na(.)))))

#age at maturity for iteroparous individuals
Utsadults11_18matage <- Utsadults11_18 %>% 
  filter(recapture <= 1) %>%
  filter(respawner.info != "") %>% 
  select(ID, sex, ageatmaturity, respawner.info) 

#respawner info
Utsadults11_18matage %>% group_by(sex) %>% count(respawner.info)

#calculate age ate maturity for iteroparous individuals
Utsadults11_18matage %>% group_by(sex) %>% dplyr:: summarise_if(is.numeric, funs(mean(., na.rm=T), n = sum(!is.na(.)), se = sd(., na.rm=T)/sqrt(sum(!is.na(.)))))

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
save_plot("images/Fig.adult.sample.png", p.adults.ind, base_height = 10.7, base_width = 7.6)
