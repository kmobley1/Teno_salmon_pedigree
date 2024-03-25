#make a graph of juvenile vgll3 genotypes

#libary
library(tidyverse)

#datafile
juv.location.SNP <- read.csv("Data/juv.location.SNP_22.10.21.csv")

#colorblind palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7","#999999", "#F0E442")


#make graph of vgll3 genotype 0+ 
table.Juv.vgll3.0 <- juv.location.SNP %>%
  filter(class.cor == "0+") %>%
  group_by(new_site_code, na.rm=T) %>%
  filter(!is.na(sex)) %>%
  summarise_if(is.numeric, funs(mean(., na.rm=T), n = sum(!is.na(.)), se = sd(., na.rm=T)/sqrt(sum(!is.na(.)))))


#0y
pJuv.vgll3.0 <- ggplot(data=table.Juv.vgll3.0) + geom_point(aes(y=new_site_code, x=c25_1441_SAC_mean), size = 3, stroke = 1) +
  geom_errorbarh(data=table.Juv.vgll3.0, aes(y=new_site_code, xmin=c25_1441_SAC_mean-c25_1441_SAC_se, xmax=c25_1441_SAC_mean+c25_1441_SAC_se),  height=0, size=1, alpha = 4/5) +
  labs(title="0+", y="Location", x="") +
  scale_color_manual(values=c("darkorange", "#4271AE")) +
  scale_y_continuous(breaks=seq(1,24,1)) +
  scale_x_continuous(limits=c(0,4)) +
  theme(plot.title=element_text(size=16, hjust=0.5)) +
  theme(axis.title.x = element_text(size=16)) +  theme(axis.text.x = element_text(size=12, color="black")) +
  theme(axis.title.y = element_text(size=16, vjust=1, angle = 90)) +  theme(axis.text.y = element_text(size=12, color="black")) +
  theme(axis.line = element_line(size = 1)) + theme(axis.ticks = element_line(size=1)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
  
#visualize
pJuv.vgll3.0

#1y
gloc_SR_1y <- ggplot(data=tableSR) + geom_point(aes(y=new_site_code, x=sex.ratio_1y_mean), size = 3, stroke = 1, fill = "white") +
  geom_errorbarh(data=tableSR, aes(y=new_site_code, xmin=sex.ratio_1y_mean-sex.ratio_1y_se, xmax=sex.ratio_1y_mean+sex.ratio_1y_se),  height=0, size=1, alpha = 4/5) +
  labs(title="1+", y="", x="") +
  #geom_point(data=juv.location.SNP.sex.ratio.wide, aes(x=sex.ratio_0y, y=new_site_code, colour = factor(year.x)),size = 2, alpha = 0.7) +
  scale_y_continuous(breaks=seq(1,24,1)) +
  scale_x_continuous(limits=c(0,1)) +
  #scale_colour_manual(values=cbPalette, name = "Year") +
  theme(plot.title=element_text(size=16, hjust=0.5)) +
  theme(axis.title.x = element_text(size=16)) +  theme(axis.text.x = element_text(size=12, color="black")) +
  theme(axis.title.y = element_text(size=16, vjust=3, angle = 90)) +  theme(axis.text.y = element_text(size=12, color="black")) +
  theme(axis.line = element_line(size = 1)) + theme(axis.ticks = element_line(size=1)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

#visualize
gloc_SR_1y

#1y
gloc_SR_2_3y <- ggplot(data=tableSR) + geom_point(aes(y=new_site_code, x=sex.ratio_2_3y_mean), size = 3, stroke = 1, fill = "white") +
  geom_errorbarh(data=tableSR, aes(y=new_site_code, xmin=sex.ratio_2_3y_mean-sex.ratio_2_3y_se, xmax=sex.ratio_2_3y_mean+sex.ratio_2_3y_se),  height=0, size=1, alpha = 4/5) +
  labs(title="2-3+", y="", x="") +
  #geom_point(data=juv.location.SNP.sex.ratio.wide, aes(x=sex.ratio_0y, y=new_site_code, colour = factor(year.x)),size = 2, alpha = 0.7) +
  scale_y_continuous(breaks=seq(1,24,1)) +
  scale_x_continuous(limits=c(0,1)) +
  #scale_colour_manual(values=cbPalette, name = "Year") +
  theme(plot.title=element_text(size=16, hjust=0.5)) +
  theme(axis.title.x = element_text(size=16)) +  theme(axis.text.x = element_text(size=12, color="black")) +
  theme(axis.title.y = element_text(size=16, vjust=3, angle = 90)) +  theme(axis.text.y = element_text(size=12, color="black")) +
  theme(axis.line = element_line(size = 1)) + theme(axis.ticks = element_line(size=1)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

#visualize
gloc_SR_2_3y

#cowplot ridgeline scale and class data
theme_set(theme_cowplot())
fig_SR1loc <- gloc_SR_0y + theme(plot.margin = unit(c(10, 0, 0, 0), units = "pt"))
fig_SR2loc <- gloc_SR_1y + theme(plot.margin = unit(c(10, 0, 0, 0), units = "pt"))
fig_SR3loc <- gloc_SR_2_3y + theme(plot.margin = unit(c(10, 10, 0, 0), units = "pt"))

#fig for ridgeline scale and class data
SRlocfig1<-plot_grid(figloc_SR1, figloc_SR2, figloc_SR3, 
                  label_y = 1,
                  labels = c('A','B', 'C'), label_size = 16, ncol = 3, nrow =1, align = "h")
#hjust = 1, vjust = 1)
SRlocfig1
save_plot("SRlocfig1.pdf", SRlocfig1, base_height = 8.5, base_width = 11)



#calculate sex ratio X location
juv.location.SNP.sex.ratio <- juv.location.SNP.sexratiosum %>%
  mutate(sex.ratio = M/(M+F))

#make graph of 0y sex ratio/year
#pivot wider juv.dens.year, rm row with 0
juv.location.SNP.sex.ratio.wide <-juv.location.SNP.sex.ratio %>%
  pivot_wider(c(year.x, new_site_code), names_from = class.cor,
              names_glue = "{.value}_{class.cor}",
              values_from = c(sex.ratio)) %>%
  arrange(new_site_code) %>%
  filter(new_site_code != "0")

#make mean +- SE geometric point
tableSR <- juv.location.SNP.sex.ratio.wide %>%
  group_by(new_site_code) %>%
  summarise_if(is.numeric, funs(mean(., na.rm=T), n = sum(!is.na(.)), se = sd(., na.rm=T)/sqrt(sum(!is.na(.))))) %>%
  rename("sex.ratio_2_3y_mean" = "sex.ratio_2-3+_mean") %>%
  rename("sex.ratio_2_3y_se" = "sex.ratio_2-3+_se") %>%
  rename("sex.ratio_1y_mean" = "sex.ratio_1+_mean") %>%
  rename("sex.ratio_1y_se" = "sex.ratio_1+_se") %>%
  rename("sex.ratio_0y_mean" = "sex.ratio_0+_mean") %>%
  rename("sex.ratio_0y_se" = "sex.ratio_0+_se") 

#horizontal dot plot sex ratio
#0y
gloc_SR_0y <- ggplot(data=tableSR) + geom_point(aes(y=new_site_code, x=sex.ratio_0y_mean), size = 3, stroke = 1, fill = "white") +
  geom_errorbarh(data=tableSR, aes(y=new_site_code, xmin=sex.ratio_0y_mean-sex.ratio_0y_se, xmax=sex.ratio_0y_mean+sex.ratio_0y_se),  height=0, size=1, alpha = 4/5) +
  labs(title="0+", y="Location", x="") +
  #geom_point(data=juv.location.SNP.sex.ratio.wide, aes(x=sex.ratio_0y, y=new_site_code, colour = factor(year.x)),size = 2, alpha = 0.7) +
  scale_y_continuous(breaks=seq(1,24,1)) +
  scale_x_continuous(limits=c(0,1)) +
  #scale_colour_manual(values=cbPalette, name = "Year") +
  theme(plot.title=element_text(size=16, hjust=0.5)) +
  theme(axis.title.x = element_text(size=16)) +  theme(axis.text.x = element_text(size=12, color="black")) +
  theme(axis.title.y = element_text(size=16, vjust=1, angle = 90)) +  theme(axis.text.y = element_text(size=12, color="black")) +
  theme(axis.line = element_line(size = 1)) + theme(axis.ticks = element_line(size=1)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

#visualize
gloc_SR_0y

#1y
gloc_SR_1y <- ggplot(data=tableSR) + geom_point(aes(y=new_site_code, x=sex.ratio_1y_mean), size = 3, stroke = 1, fill = "white") +
  geom_errorbarh(data=tableSR, aes(y=new_site_code, xmin=sex.ratio_1y_mean-sex.ratio_1y_se, xmax=sex.ratio_1y_mean+sex.ratio_1y_se),  height=0, size=1, alpha = 4/5) +
  labs(title="1+", y="", x="") +
  #geom_point(data=juv.location.SNP.sex.ratio.wide, aes(x=sex.ratio_0y, y=new_site_code, colour = factor(year.x)),size = 2, alpha = 0.7) +
  scale_y_continuous(breaks=seq(1,24,1)) +
  scale_x_continuous(limits=c(0,1)) +
  #scale_colour_manual(values=cbPalette, name = "Year") +
  theme(plot.title=element_text(size=16, hjust=0.5)) +
  theme(axis.title.x = element_text(size=16)) +  theme(axis.text.x = element_text(size=12, color="black")) +
  theme(axis.title.y = element_text(size=16, vjust=3, angle = 90)) +  theme(axis.text.y = element_text(size=12, color="black")) +
  theme(axis.line = element_line(size = 1)) + theme(axis.ticks = element_line(size=1)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

#visualize
gloc_SR_1y

#1y
gloc_SR_2_3y <- ggplot(data=tableSR) + geom_point(aes(y=new_site_code, x=sex.ratio_2_3y_mean), size = 3, stroke = 1, fill = "white") +
  geom_errorbarh(data=tableSR, aes(y=new_site_code, xmin=sex.ratio_2_3y_mean-sex.ratio_2_3y_se, xmax=sex.ratio_2_3y_mean+sex.ratio_2_3y_se),  height=0, size=1, alpha = 4/5) +
  labs(title="2-3+", y="", x="") +
  #geom_point(data=juv.location.SNP.sex.ratio.wide, aes(x=sex.ratio_0y, y=new_site_code, colour = factor(year.x)),size = 2, alpha = 0.7) +
  scale_y_continuous(breaks=seq(1,24,1)) +
  scale_x_continuous(limits=c(0,1)) +
  #scale_colour_manual(values=cbPalette, name = "Year") +
  theme(plot.title=element_text(size=16, hjust=0.5)) +
  theme(axis.title.x = element_text(size=16)) +  theme(axis.text.x = element_text(size=12, color="black")) +
  theme(axis.title.y = element_text(size=16, vjust=3, angle = 90)) +  theme(axis.text.y = element_text(size=12, color="black")) +
  theme(axis.line = element_line(size = 1)) + theme(axis.ticks = element_line(size=1)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

#visualize
gloc_SR_2_3y

#cowplot ridgeline scale and class data
theme_set(theme_cowplot())
fig_SR1loc <- gloc_SR_0y + theme(plot.margin = unit(c(10, 0, 0, 0), units = "pt"))
fig_SR2loc <- gloc_SR_1y + theme(plot.margin = unit(c(10, 0, 0, 0), units = "pt"))
fig_SR3loc <- gloc_SR_2_3y + theme(plot.margin = unit(c(10, 10, 0, 0), units = "pt"))

#fig for ridgeline scale and class data
SRlocfig1<-plot_grid(figloc_SR1, figloc_SR2, figloc_SR3, 
                     label_y = 1,
                     labels = c('A','B', 'C'), label_size = 16, ncol = 3, nrow =1, align = "h")
#hjust = 1, vjust = 1)
SRlocfig1
save_plot("SRlocfig1.pdf", SRlocfig1, base_height = 8.5, base_width = 11)