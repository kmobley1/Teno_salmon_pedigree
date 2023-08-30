---
title: Teno_salmon_pedigree
author: Kenyon Mobley
date: 30/08/2023
output: html_document
---


# Teno_salmon_pedigree
#Analysis of teno salmon pedigree (2011-2019) Utsjoki, Finland

##Data cleanup for files used in analyses

Data cleanup: SNP dataset (adults and juveniles, i.e. 'parr')

```shell script
metadata/Uts_SNP_master_cleanup.R``` 

Dependent datafiles:
Data from raw sequencing files, corrected and concatenated: ```UtsSNPMasterDataKM_20.11.24.csv```
Adult collection data: ```UtsadultsALL_21.06.22.csv```
Data from Henry Barton of rough estimate of hatch year: ```2021-02-18.uts_lifehist.csv```

Output data file: ```UtsSNP_21.04.13.csv```

Data cleanup: birthyear (i.e. hatch year) calculations

life history strategies.R
mature_male_parr_RS_11_02_22.R
