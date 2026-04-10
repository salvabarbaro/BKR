## Barbaro, Kurella, Roth
## Short-Term (2021) Analysis
######################################################
library(gesisdata)                        # Remote   #
#library(wdman)                            # Data Load#
#delete the LICENSE.chromedriver file                #
#wdman::selenium(retcommand=T)                        #
######################################################
library(haven)  # loading the file non-remotedly
library(ggplot2)
library(tidyr)
library(dplyr)
library(hutils)
library(ggpubr)
library(reshape2)
library(forcats)
library(gtsummary)
library(readxl)
library(parallel)
#library(foreach)
#library(doParallel)
nbcores <- detectCores() - 2
#################################
options(
  gesis_email    = "sbarbaro@uni-mainz.de",
  gesis_password = "N6sMP8zMzs6KhDh%",
  gesis_use      = 5
)
gesis_download_fixed(
  file_id = "ZA7856",
  download_dir = getwd()
)
############################################################################
## Get data using the gesisdata library 
gesis_download(file_id = "ZA7856", 
               email = "sbarbaro@uni-mainz.de",  # gesis access e-mail
               password = "N6sMP8zMzs6KhDh%", # gesis access password
               download_dir = getwd(),
               use = 5, reset = T) 
# Alternative: download the dta file and read it with the help of the haven library.
za7856 <- read_dta("DATA/ZA7856_v1-0-0.dta") %>%
  mutate(id = 1:nrow(.)) %>% relocate(id, .before = V1) %>% # we add a variable "id" and place it as the first column
  mutate(V50.UNI = ifelse(V6 == 9, V51, V50)) %>% # add new variable: merge CDU and CSU 
  mutate(V50.UNI = labelled(V50.UNI), levels=V52) %>%
  relocate(V50.UNI, .after = V51) %>%
  mutate(across(c("V50.UNI", "V52", "V53", "V54", "V55", "V56") , ~na_if(., 99))) %>%
  mutate(across(c("V60", "V61", "V62") , ~na_if(., 99  ))) %>%
  mutate(across(c("V60", "V61", "V62") , ~na_if(., 98  ))) 
### Consider the observation weiths:
za.w.df <- za7856 %>%  weight2rows(., "V342", rows.out = 2*nrow(.))
###############################################################################
ranking.df <- za7856 %>% 
  select(., c("id",, "V50.UNI", "V52", "V53", "V54", "V55", "V56")) %>% 
  rename(UNION = "V50.UNI", SPD = "V52", AFD = "V53", 
         FDP = "V54", LINKE = "V55", GREEN = "V56") %>%
  melt(., id.vars = "id", value.name = "value")  %>%
  arrange(id, desc(value))  %>%
  group_by(id) %>%
  mutate(ranking = rank(-value, ties.method = "min")) %>%
  select(-value)  %>%
  spread(id, ranking, convert = T) %>% t() %>% as.data.frame() %>%
  setNames(c("UNION", "SPD",   "AFD",   "FDP",   "LINKE", "GREEN")) %>%
  slice(-c(1)) %>%
  mutate(id = 1:nrow(.), .before = "UNION") %>%
  left_join(x = ., y = za7856 %>% select(., c("id", "V342")), by = "id")
#
## Approach A: use the Skalometer data only
ranking.w <- ranking.df %>% 
  left_join(x = ., y = za7856 %>% select(., c("id", "V5")), by = "id") %>%
  weight2rows(., "V342", rows.out = 2*nrow(.)) %>% select(., -c("id"))


## Approach B: Use the ranking variables and round-out the orderings by the help of the Skalometer data
### V60ff - approach
########################################################################################################
####### for the first three ranking numbers, we use V 61 - V 63  #######################################
########################################################################################################
prank.fun <- function(V6xnb, area){
  r1 <- ifelse(area$V60 == V6xnb, 1, 0)
  r2 <- ifelse(area$V61 == V6xnb, 2, 0)
  r3 <- ifelse(area$V62 == V6xnb, 3, 0)
  rdf <- as.data.frame(cbind(r1, r2, r3))
  rdf[is.na(rdf)] <- 0
  p.temp <- apply(rdf, 1, max)
  prank <- ifelse(p.temp == 0, 4 , p.temp)
  return(prank)
}

dnoby <- za7856 %>% filter(., V6 != 9) %>% select(., c("id", "V6", "V60", "V61", "V62")) %>% 
  left_join(x = ., y = ranking.df, by = "id")
dby   <- za7856 %>% filter(., V6 == 9) %>% select(., c("id", "V6", "V60", "V61", "V62")) %>% 
  left_join(x = ., y = ranking.df, by = "id")


dnoby$UNI.r <- prank.fun(V6xnb = 1, area = dnoby)
dnoby$SPD.r <- prank.fun(V6xnb = 3, area = dnoby)
dnoby$LIN.r <- prank.fun(V6xnb = 6, area = dnoby)
dnoby$GRE.r <- prank.fun(V6xnb = 7, area = dnoby)
dnoby$FDP.r <- prank.fun(V6xnb = 5, area = dnoby)
dnoby$AFD.r <- prank.fun(V6xnb = 4, area = dnoby)
##
dby$UNI.r <- prank.fun(V6xnb = 2, area = dby)
dby$SPD.r <- prank.fun(V6xnb = 3, area = dby)
dby$LIN.r <- prank.fun(V6xnb = 6, area = dby)
dby$GRE.r <- prank.fun(V6xnb = 7, area = dby)
dby$FDP.r <- prank.fun(V6xnb = 5, area = dby)
dby$AFD.r <- prank.fun(V6xnb = 4, area = dby)

ranking.df.V6X <- rbind(dnoby, dby) %>% arrange(id) %>%
  mutate(UNION.V6X = ifelse(UNI.r < 4, UNI.r, UNION),
         SPD.V6X   = ifelse(SPD.r < 4, SPD.r, SPD), 
         AFD.V6X   = ifelse(AFD.r < 4, AFD.r, AFD),
         FDP.V6X   = ifelse(FDP.r < 4, FDP.r, FDP),
         LINKE.V6X = ifelse(LIN.r < 4, LIN.r, LINKE),
         GREEN.V6X   = ifelse(GRE.r < 4, GRE.r, GREEN))  %>%
  select(., c("id", "UNION.V6X", "SPD.V6X", "AFD.V6X", "FDP.V6X", "LINKE.V6X", "GREEN.V6X", "expand.w"))  %>%
  setNames(., c("id", "UNION", "SPD", "AFD", "FDP", "LINKE", "GREEN", "expand.w") )

ranking.df <- ranking.df.V6X


