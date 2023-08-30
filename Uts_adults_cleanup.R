####cleanup adult dataset. Calculate seaage for samples missing weight, calculate residuals for condition factor

library(tidyverse)
library(modelr)

####datasets####

#adult information
Utsadults <- read_csv("metadata/Uts_Adult_2011-2019.csv")

####cleanup Utsadults data####
#remove 2019 adults
Utsadults <- Utsadults %>%
  filter(year != "2019") 

#### Calculate seaage for samples missing from weight distribution (from Mobley et al. 2019)####
# Assume a normal distribution within each seaage
# and get most likely age class for each value
# Each sex in turn - different distrib of weights between age classes
# Males first
## And split further
utsfem<-Utsadults[which(Utsadults$sex=="F"), ]
utsmal<-Utsadults[which(Utsadults$sex=="M"), ]
# Make a new variable to take the interpolated ages, based on the known age data
utsmal$InterpAge<-utsmal$scale.seaage
# Find the indivs lacking age
noage<-which(is.na(utsmal$scale.seaage))
means<-tapply(utsmal$wt.kg, utsmal$scale.seaage, function (x) mean(x, na.rm=T))
sds<-tapply(utsmal$wt.kg, utsmal$scale.seaage, function (x) sd(x, na.rm=T))
# New after removing dead fish - no sd for 4 yrs **km, did not remove dead or repeated data
# Replace with something reasonable to work out probs
sds[which(is.na(sds))]<-max(sds, na.rm=T)
#next bit
utsmal$InterpAge[noage] <- unlist(lapply(utsmal$wt.kg[noage], function (x) {
  probs<- unlist(lapply(unique(utsmal$InterpAge[which(!is.na(utsmal$InterpAge))]), function (i) {
    dnorm(x, mean=means[[i]], sd=sds[[i]]) } ))
  names(probs)<-unique(utsmal$InterpAge[which(!is.na(utsmal$InterpAge))])
  return(names(probs)[which(probs==max(probs))])
}))
#
utsmal$InterpAge<-as.numeric(utsmal$InterpAge)
# Include these interpolated ages in the categorical assignments
##utsadultsclean$InterpCatAge<-ifelse(utsadultsclean$InterpAge>1, "MSW", ifelse(Utsadults2012.2018$InterpAge==1, "1SW", NA))
##utsadultsclean$InterpCatAge<-as.factor(utsadultsclean$InterpCatAge)
rm(noage, means, sds)
# Now females
utsfem$InterpAge<-utsfem$scale.seaage
noage<-which(is.na(utsfem$scale.seaage))
means<-tapply(utsfem$wt.kg, utsfem$scale.seaage, mean)
sds<-tapply(utsfem$wt.kg, utsfem$scale.seaage, sd)
utsfem$InterpAge[noage] <- unlist(lapply(utsfem$wt.kg[noage], function (x) {
  probs<- unlist(lapply(unique(utsfem$InterpAge[which(!is.na(utsfem$InterpAge))]), function (i) {
    dnorm(x, mean=means[[i]], sd=sds[[i]]) } ))
  names(probs)<-unique(utsfem$InterpAge[which(!is.na(utsfem$InterpAge))])
  return(names(probs)[which(probs==max(probs))])
}
))
utsfem$InterpAge<-as.numeric(utsfem$InterpAge)
#remove values 
rm(noage, means, sds)

#recombine male and female datasets
utsALL<-bind_rows(utsfem, utsmal) %>%
  relocate(InterpAge, .after = scale.seaage)

##calculate residuals of length ~ weight - this may change residuals of 2012 and 2013 due to the missing length weight data previously for the 3 individuals, need to check
##dplyr, purr solution - issue with not adding residual column to dataset - this is fixed by printing without model column
## calculate residuals
##!!!!!!residuals calculated in Mobley 2019/2020 are not based on log!, simple linear regression of weight and length
models <- utsALL %>% 
  group_by(sex) %>% #include year here if we want condition per year
  nest() %>%
  mutate(model = map(data, ~lm(wt.kg ~ length.cm, data = .x)),
         residuals = map2(data, model, add_residuals))
#unnest residuals, call to new dataframe
utsALLres<-models %>% unnest(residuals)
#print dataframe, there is a problem with printing with the data and model column, if you remove them then you can print out as a .csv file
utsALLnew<-utsALLres %>%
  select(c(-data, -model)) %>%
  relocate(resid, .after = length.cm) %>%
  rename(condition = resid)

#write file
write.csv(utsALLnew, "~/projects/Atlantic salmon - Teno River Pedigree/2020 - Utsjoki pedigree data/reprofitness/Utsjoki-salmon-pedigree/data models/UtsadultsALL_21.06.22.csv")

#how many are recaptures? 
utsALLnew %>% count(recapture)

#how many are missing scale samples for seawater age
utsALLnew %>% count(scale.seaage)

#how many are missing scale samples for Freshwater age
utsALLnew %>% count(Scale.smoltage)






