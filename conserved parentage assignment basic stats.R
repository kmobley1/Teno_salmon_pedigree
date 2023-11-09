#Parentage assignment with conservative datset, how many individuals are assigned?

####packages####
library (tidyverse)

####database####
Uts_parentage_conserved <- read.csv("Data/Uts_parentage_conserved_21.06.22.csv")

####basic stats ####
#fix dataset, sex.dam = False
Uts_parentage_conserved <- Uts_parentage_conserved %>%
  mutate(sex.dam = replace(sex.dam, sex.dam == "FALSE", "F"))

# adults vs offspring in conservative dataset
Uts_parentage_conserved %>%
  group_by(type.off) %>%
  tally()

#sires in conservative dataset
Uts_parentage_conserved %>%
  group_by(sex.sire) %>%
  tally()

#dams in conservative dataset
Uts_parentage_conserved %>%
  group_by(sex.dam) %>%
  tally() 

#adults in conservative dataset
Uts_parentage_conserved %>%
  group_by(sex.off, type.off) %>%
  tally() 

#find offspring that have both parents assigned
Uts_parentage_conserved_both <- Uts_parentage_conserved %>%
  filter(sex.sire == "M" | sex.dam == "F")

#both in conservative dataset
Uts_parentage_conserved_both %>%
  group_by(type.off) %>%
  tally()

#how many total offspring
Uts_parentage_conserved_both %>%
  tally()

