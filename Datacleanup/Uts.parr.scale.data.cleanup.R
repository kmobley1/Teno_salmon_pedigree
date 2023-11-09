#Utsjoki parr scale data

#libraries
library(tidyverse)

#dataset
parr_data_combined <- read.csv("Data/parr_data_combined_22.10.21.csv")

#add column class.cor
Uts.parr.scale <- parr_data_combined %>%
  mutate(parr_data_combined, date = ifelse(date == "", sample.date, 
                                             ifelse(date != "", date, NA))) %>%
  mutate(parr_data_combined, class.cor =  ifelse(class == "2y", "2-3y", 
                                                        ifelse(class == "3y", "2-3y", 
                                                               ifelse(class == "2pp", "2-3y",  
                                                                      ifelse(class == "2-3y", "2-3y",  
                                                                             ifelse(class == "1y", "1y",  
                                                                                    ifelse(class == "0y", "0y", NA))))))) %>%
  relocate(class.cor, .after = class) %>%
  relocate(year, .after = ID) %>%
  relocate(sample.date, .after = date) %>%
  rename(scale.reading = class.1) 

#remove columns
Uts.parr.scale <- Uts.parr.scale %>%
  select(-X.2, -X.1, -X, -lat, -long, -site_description) 



#write file
write.csv(Uts.parr.scale, "Data/Uts.parr.scale_23.06.09.csv")
