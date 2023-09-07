#juvenile collection for teno salmon pedigree

#libraries
library(tidyverse)

#data 
juv.location.SNP <- read.csv("C:/Users/kmo107/OneDrive - UiT Office 365/Documents/projects/Atlantic salmon - Teno River Pedigree/2020 - Utsjoki pedigree data/Teno_salmon_pedigree/juv.location.SNP_22.10.21.csv")

#colorblind palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7","#999999", "#F0E442")

####total number of individuals collected####
#make table of total individuals from juv.location.snp dataset with dups removed, need to add some rows too
uts.juv.total.ind <- juv.location.SNP %>%
  count(year, class.cor) %>%
  add_row(year = 2012, class.cor = "1+", n = 0) %>%
  add_row(year = 2012, class.cor = "2-3+", n = 0) %>%
  add_row(year =2013, class.cor = "2-3+", n = 0) 

#create grouped bar plot of #collected individuals/year/class
p.num.ind <-  ggplot(uts.juv.total.ind , aes(factor(year), n, fill = class.cor)) +geom_bar(stat = "identity", position = "dodge") +
  labs(y="No. individuals", x="Year") + 
  scale_fill_manual(values=cbPalette, name = "Class", labels = c("0+", "1+", "2-3+")) +
  scale_y_continuous(limits = c(0, 2500), expand = c(0, 0)) +
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
p.num.ind
