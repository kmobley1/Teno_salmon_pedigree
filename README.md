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
2021-02-18.uts_lifehist.csv #Data from @henryjuho of rough estimate of hatch year 

Output data file: UtsSNP_21.04.13.csv
```
Data cleanup: birthyear (i.e. hatch year) calculations 
#Hatch year calculations and cleanup (metadata)

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

Data cleanup: Parr collection and SNP data
```
R code: Uts.parr.SNP.data.cleanup.R #Utsjoki parr SNP data 

Dependent datafiles:
juv.location.SNP_22.10.21 #collection data for parr combined with SNP data 

Output data file: Uts.parr.SNP_23.06.09.csv
```
Data cleanup: 

