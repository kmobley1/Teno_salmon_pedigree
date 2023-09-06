#Uts_adults basic stats
####libraries####
library(tidyverse)

#import datasets####

## import adults dataset of phenotypic data (desktop)
Utsadults <- UtsadultsALL_21.06.22

#remove 2019 adults
Utsadults11_18 <- Utsadults %>%
  filter(year != "2019") %>%
  mutate(Respawner = replace_na(Respawner, 0)) %>%
  mutate(recapture = replace_na(recapture, 0))

#How many adults? 2011 -2018
Utsadults11_18 %>% count()

#how many for each sex?
Utsadults11_18 %>% group_by(sex) %>% count()

#how many unique?
Utsadults11_18 %>% filter(recapture <= 1) %>% count()

#How many have scale smolt age? remove recaptures
Utsadults11_18 %>% filter(recapture <= 1) %>% count(Scale.smoltage)

#How many have seaage age? remove recaptures
Utsadults11_18 %>% filter(recapture <= 1) %>% count(scale.seaage)

#means, remove recaptures
Utsadults11_18 %>% filter(recapture <= 1) %>% group_by(sex) %>% dplyr:: summarise_if(is.numeric, funs(mean(., na.rm=T), n = sum(!is.na(.)), se = sd(., na.rm=T)/sqrt(sum(!is.na(.)))))
  
