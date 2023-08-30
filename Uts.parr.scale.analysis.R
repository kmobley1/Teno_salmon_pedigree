#Utsjoki parr scale dataset, analysis for Mature male parr

#basic graphs and analyses
library(tidyverse)
library(DHARMa)
library(ggridges)
library(cowplot)

#dataset
Uts.parr.scale <- Uts.parr.scale_23.06.09.csv
Uts.parr.SNP -> Uts.parr.SNP_23.06.09.csv

#colorblind palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7","#999999", "#F0E442")

#first remove ind that do not have scale.age 
Uts.parr.scale_rmNA_SA <- Uts.parr.scale %>%
  filter(scale.age != "NA")

#count mature male parr (combined data)
Uts.parr.scale_rmNA_SA %>% filter(mature.male.parr == 1) %>% count(mature.male.parr, scale.age)

#make table of sex from Uts.parr.SNP
Uts.parr.SNP.sex <- Uts.parr.SNP %>%
  select("ID", "sex")

#combine SNP data with parr data_combined.rmNA
Uts.parr.scale_sex <- left_join(Uts.parr.scale_rmNA_SA, Uts.parr.SNP.sex, by='ID') 

#new column for males, females and mature male parr
Uts.parr.scale_sex <- Uts.parr.scale_sex %>%
  mutate(Uts.parr.scale_sex, sexy =  ifelse(sex %in% "F", "Female", 
                                                ifelse(mature.male.parr %in% 1, "Mature male",
                                                       ifelse(sex %in% "M", "Immature male",  NA))))

#remove all individuals that were aged <1yr and NAs
Uts.parr.scale_sex <- Uts.parr.scale_sex %>%
  filter(scale.age > 1) %>%
  filter(sexy != "NA")
  
#check if mature male parr are counted correctly (yes)
Uts.parr.scale_sex %>% count(sexy, scale.age) 

#glm of sex category and length
lm.length.sex <- lm(length.cm ~ sexy, data=Uts.parr.scale_sex, type=3)

#show lm results
summary(lm.length.sex)

#simulate residuals
res.lm.length.sex <- simulateResiduals(lm.length.sex, plot = T)

#glm of sex category and length
lm.scale.age.sex <- lm(scale.age ~ sexy, data=Uts.parr.scale_sex, type=3)
#show lm results
summary(lm.scale.age.sex)

#simulate residuals
res.scale.age.sex <- simulateResiduals(lm.scale.age.sex, plot = T)

#means for male 2-3y immature v mature
####make table of means
tmeanslmsex_23y <- Uts.parr.scale_sex %>%
  group_by(sexy, n.rm=T) %>%
  summarise_if(is.numeric, funs(mean(., na.rm=T), n = sum(!is.na(.)), se = sd(., na.rm=T)/sqrt(sum(!is.na(.)))))


#ridgeline plot of length for males, females and mature male parr
pjuv.length.ridge.sex <- ggplot(Uts.parr.scale_sex, aes(x = length.cm, y = sexy)) +
  geom_density_ridges(aes(fill = sexy), scale = 1, alpha = 0.7) +
  scale_fill_manual(values = c("pink", "blue", "purple")) +
  labs(x="Length (cm)", y="", fill="") + 
  theme(legend.position = "none")+
  scale_x_continuous(limits = c(4, 18), expand = c(0, 0)) +
  theme(plot.title=element_text(size=24, hjust=-0.5)) +
  theme(axis.title.x = element_text(size=16)) +  theme(axis.text.x = element_text(size=12, color="black")) +
  theme(axis.title.y = element_text(size=16, vjust=1, angle = 90)) +  theme(axis.text.y = element_text(size=12, color="black")) +
  theme(axis.line = element_line(size = 1)) + theme(axis.ticks = element_line(size=1)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
#visualize
pjuv.length.ridge.sex  

#ridgeline plot of age for males, females and mature male parr
pjuv.scaleage.ridge.sex <- ggplot(Uts.parr.scale_sex, aes(x = scale.age, y = sexy)) +
  geom_density_ridges(aes(fill = sexy), scale = 1, alpha = 0.7) +
  scale_fill_manual(values = c("pink", "blue", "purple")) +
  labs(x="Scale age (years)", y="", fill="") + 
  theme(legend.position = "none")+
  scale_x_continuous(limits = c(0, 4), expand = c(0, 0)) +
  theme(plot.title=element_text(size=24, hjust=-0.5)) +
  theme(axis.title.x = element_text(size=16)) +  theme(axis.text.x = element_text(size=12, color="black")) +
  theme(axis.title.y = element_text(size=16, vjust=1, angle = 90)) +  theme(axis.text.y = element_text(size=12, color="black")) +
  theme(axis.line = element_line(size = 1)) + theme(axis.ticks = element_line(size=1)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
#visualize
pjuv.scaleage.ridge.sex  

#cowplot ridgeline scale and class data
theme_set(theme_cowplot())
fig_1 <- pjuv.scaleage.ridge.sex + theme(plot.margin = unit(c(10, 0, 0, 0), units = "pt"))
fig_2 <- pjuv.length.ridge.sex + theme(plot.margin = unit(c(10, 0, 0, 0), units = "pt"))

#fig for ridgeline scale and class data
SAparrfig1<-plot_grid(fig_1, fig_2, 
                  label_y = 1,
                  labels = c('A','B'), label_size = 12, ncol = 1, nrow =2, align = "v")
#hjust = 1, vjust = 1)
SAparrfig1



