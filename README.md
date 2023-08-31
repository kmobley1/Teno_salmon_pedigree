---
title: Teno_salmon_pedigree
author: Kenyon Mobley
date: 30/08/2023
output: html_document
---

#Analysis of teno salmon pedigree (2011-2019) Utsjoki, Finland

Data cleanup: adult collection data
#Calculate seaage for samples missing weight, calculate residuals for condition factor

```
R code: Uts_adults_cleanup.R

Dependent datafiles:
Uts_Adult_2011-2019.csv #collection data for adults including scale data 

Output data file: UtsadultsALL_21.06.22
```

Data cleanup: SNP dataset (adults and juveniles, i.e. 'parr')
#cleanup and combine SNP dataset with parentage, adult info, and sex
```
R code: Uts_SNP_master_cleanup.R

Dependent datafiles:
UtsSNPMasterDataKM_20.11.24.csv #Data from raw sequencing files, corrected and concatenated 
UtsadultsALL_21.06.22.csv #Adult collection data cleaned
2021-02-18.uts_lifehist.csv #Data from @henryjuho of rough estimate of birth (hatch) year 

Output data file: UtsSNP_21.04.13.csv
```
Data cleanup: birthyear (i.e. hatch year) calculations 
#Hatch year calculations and cleanup 

```
R code: Birthyear_cleanup.R

Dependent datafiles: 
UtsSNP_21.04.13.csv #SNP data cleaned
UtsadultsALL_21.06.22.csv #Adult collection data cleaned

Output data file: Uts_Birthyear_Calc_21_06_22.csv
```

Data cleanup: Parr scale data #Utsjoki parr scale data from collections 
```
R code: Uts.parr.scale.data.cleanup.R #Utsjoki parr scale dataset, analysis for Mature male parr

Dependent datafiles:
parr_data_combined_22.10.21 #parr scale data combined over 2012-2019

Output data file: Uts.parr.scale_23.06.09
```

Data cleanup: Parr collection and SNP data #collection data combined with SNP data 
```
R code: Uts.parr.SNP.data.cleanup.R #Utsjoki parr SNP data 

Dependent datafiles:
juv.location.SNP_22.10.21 #collection data for parr combined with SNP data 

Output data file: Uts.parr.SNP_23.06.09.csv
```

Data cleanup: default parentage analysis results #original dataset from @henryjuho and renamed
#default = all age difference priors as estimated by sequoia
```
R code: Uts_parentage_default_cleanup.R #parentage dataset cleanup default

Dependentt data files:
UtsSNP_21.04.13.csv #SNP data cleaned
UtsadultsALL_21.06.22.csv #Adult collection data cleaned
Output data file: Uts_Birthyear_Calc_21_06_22.csv #hatch year calculations
uts_default.prior0.parents.2021-06-18.csv #from @henryjuho (original file renamed 2021-06-18.uts_default.prior0.parents.csv)

OUtput data file: Uts_parentage_default_21.06.22.csv
```

Data cleanup: informed parentage analysis results #original dataset from @henryjuho and renamed
#informed = priors for age gap of 0 and 1 for males, and 0, 1, 2 and 3 for females set to 0
```
R code: Uts_parentage_informed_new_cleanup.R #parentage dataset cleanup informed

Dependentt data files:
UtsSNP_21.04.13.csv #SNP data cleaned
UtsadultsALL_21.06.22.csv #Adult collection data cleaned
Uts_Birthyear_Calc_21_06_22.csv #hatch year calculations
uts_informed.prior1.parents.2021-06-18.csv #from @henryjuho (original file renamed 2021-06-18.uts_informed.prior1.parents.csv)

OUtput data file: Uts_parentage_informed_21.06.22.csv
```

Data cleanup: Conservative parentage analysis results #original dataset from @henryjuho and renamed
#conservative = all priors less than 0.1 set to zero, to exclude all of the most improbable relationships
```
R code: Uts_parentage_conserved.21.06.18_cleanup_new.R #parentage dataset cleanup conservative

Dependentt data files:
UtsSNP_21.04.13.csv #SNP data cleaned
UtsadultsALL_21.06.22.csv #Adult collection data cleaned
Uts_Birthyear_Calc_21_06_22.csv #hatch year calculations
uts_conservative.prior2.parents_2021-06-18.csv #from @henryjuho (original file renamed 2021-06-18.uts_conservative.prior2.parents.csv)

OUtput data file: Uts_parentage_conserved_21.06.22.csv 
```
Data cleanup: Calculate the number of reproductive events (cohorts) and offspring for each reproductive event for individual sires and dams using the conservative parentage analysis dataset
```
R code: Uts_cohort_SNP_conserved_cleanup_MMPtweak.R #parentage dataset cleanup conservative

Dependentt data files:
UtsSNP_21.04.13.csv #SNP data cleaned
Uts_Birthyear_Calc_21_06_22.csv #hatch year calculations
Uts_parentage_conserved_21.06.22.csv #conservative parentage data cleaned up

OUtput data file: Uts_cohort_SNP_cons_11.02.22.csv 
```



