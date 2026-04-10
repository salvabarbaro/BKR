# BKR
Replication file to Barbaro, Kurella, Roth: Electoral outcomes versus voters' preferences: On the different tales the data can tell. Journal of Elections, Public Opinion &amp; Parties

## Scripts
### BKR01_Data2021.R
Creates rankings from Skalometer values
### BKR01_BordaPlurality2021.R
Uses the rankings generated with BKR01_Data2021.R and calculates Borda and plurality seat shares. The results are displayed in Fig.2 (and in some variations used in presentations)
### BKR01_Boxplots2021.R
Uses the rankings generated with BKR01_Data2021.R and visualises the data in boxplots (not part of the paper, but used in some previous versions)
### BKR02_AFD.R
Script to mirror plurality vote shares (polling data) and avg. Skalometer-based backing for the radical right - various figures.
### BKR02_LongTerm.R 
Generates Fig. 1 by using data provided by Döring, Holger; Manow, Philip, 2018, "ParlGov 2018 Release",https://doi.org/10.7910/DVN/F0YGNC, Harvard Dataverse, V1.

# Data
Data are not freely but easily available. An account with gesis is required, thus we cannot make the data available here. 

# Notes on reproduction
The replication files are tested on a Linux OS. In case you do not use Linux, adjust all libraries and codes associated with paralell computing. In particular, DO NOT RUN any line with 
```
mclapply()
'''