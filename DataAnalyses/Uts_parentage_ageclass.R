#parentage dataset conservative

#packages
library (tidyverse)

#database
Uts_parentage_conserved <- read.csv("Data/Uts_parentage_conserved_21.06.22.csv")

#colorblind palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7","#999999", "#F0E442")

#fix dataset, sex.dam = False
Uts_parentage_conserved <- Uts_parentage_conserved %>%
  mutate(sex.dam = replace(sex.dam, sex.dam == "FALSE", "F"))

#offspring classes assigned to sires/dams
#dams
Uts_parentage_conserved_classfreqF <- Uts_parentage_conserved %>%
  group_by(sex.dam, class.cor.off) %>%
  filter(!is.na(sex.dam)) %>%
  rename(sex = sex.dam) %>%
  tally() 

#sires
Uts_parentage_conserved_classfreqM <- Uts_parentage_conserved %>%
  group_by(sex.sire, class.cor.off) %>%
  filter(!is.na(sex.sire)) %>%
  rename(sex = sex.sire) %>%
  tally() 

#merge datasets
Uts_Parentage_classfreq <- bind_rows(Uts_parentage_conserved_classfreqF, Uts_parentage_conserved_classfreqM)

#count offspring
Uts_Parentage_classfreq %>% tally(n)

#create grouped bar plot of #collected individuals/year/class
p.classfreq <-  ggplot(Uts_Parentage_classfreq, aes(y=n, x=class.cor.off, fill = sex)) +geom_bar(stat = "identity", position = "dodge") +
  labs(x="Age class", y="No. individuals") + 
  scale_fill_manual(values=cbPalette, name = "", labels = c("Dam", "Sire")) +
  theme(plot.title=element_text(size=36, hjust=-0.5), legend.title=element_text(size=20), legend.text=element_text(size=20)) +
  theme(axis.title.x = element_text(size=24)) +  theme(axis.text.x = element_text(size=20, color="black")) +
  theme(axis.title.y = element_text(size=24, vjust=3, angle = 90)) +  theme(axis.text.y = element_text(size=20, color="black")) +
  theme(axis.line = element_line(size = 1)) + theme(axis.ticks = element_line(size=1)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.margin=unit(c(1,0,0,1), "cm"))
#show graph
p.classfreq

#reformat table
class.freq.wide <- Uts_Parentage_classfreq %>%
  pivot_wider(
    names_from = c(sex),
    names_sep = "_",
    values_from = n) 

#fix row names 
class.freq.wide2 <- class.freq.wide %>% remove_rownames %>% column_to_rownames(var="class.cor.off")

#chi square  
chisq.test(class.freq.wide2) 

#calculate % for each sex
Uts_Parentage_classfreq %>% group_by(sex) %>% mutate(total = sum(n)) %>% mutate(percent = n/total*100)
