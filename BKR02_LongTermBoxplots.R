######################################################
library(gesisdata)                        # Remote   #
library(wdman)                            # Data Load#
library(RSelenium)
#delete the LICENSE.chromedriver file                #
selenium(retcommand=T)                               #
######################################################
library(haven)
library(ggplot2)
library(tidyr)
library(dplyr)
library(hutils)
library(ggpubr)
library(ggsignif)
library(reshape2)
library(forcats)
library(gtsummary)
library(foreach)
library(doParallel)
library(lubridate)
library(ggdist)
nbcores <- detectCores() - 4 
setwd("~/Documents/Research/Elections/PR/")
#################################################################
## Get data using the gesisdata library 
gesis_download(file_id = "ZA2391", 
               email = "",  # gesis access e-mail
               password = "", # gesis access password
               download_dir = "") # enter the download directory
# Alternative: download the dta file and read it with the help of the haven library.

ZA2391_v14_0_0 <- read_dta("DATA/ZA2391_v14-0-0.dta") %>% 
  mutate(id = 1:nrow(.)) %>% relocate(id, .before = v1) %>%
  mutate(v9.UNI = ifelse(v75 == 9, v10, v9)) %>% 
  mutate(v9.UNI = labelled(v9.UNI), levels=v8) %>%
  relocate(v9.UNI, .after = v10) %>%
  mutate(across(c("v8", "v9.UNI", "v11", "v12", "v13", "v14") , ~na_if(., 99)))  %>%
  mutate(v78corr = ifelse(v4 == 2020, v78 * 1000, v78) ) %>%
  mutate(Year = ymd(v4, truncated = 2L), .before = v4)

za.w.df <- weight2rows(ZA2391_v14_0_0, "v78corr", rows.out = nrow(ZA2391_v14_0_0))
party.list = list("UNION", "SPD", "GREEN", "FDP", "LINKE",  "AFD")

plot.data <- za.w.df %>%
  select(., c("Year", "v8", "v9.UNI", "v11", "v12", "v13", "v14", "v4")) %>%
  rename(UNION = "v9.UNI", SPD = "v8", AFD = "v13", FDP = "v11", LINKE = "v14", GREEN = "v12") %>%
  melt(., id.vars = c("Year", "v4")) %>%
  left_join(x = ., 
            y = data.frame(variable = unique(.$variable), 
                           colfill = c("red", "black", "yellow2", 
                                       "forestgreen", "chocolate", "violet" )), 
            by = "variable")
#


plot.fun <- function(p){
  ggplot(data = plot.data %>% filter(., variable == p), 
         aes(y = value, x = Year, group = Year)) +
    geom_boxplot(aes(colour = Year), 
                 outlier.size = 0, 
                 col = as.character(plot.data %>% 
                                      filter(., variable %in% p) %>% 
                                      select("colfill") %>% slice(1)), 
                 alpha = 0.3,
                 fill = as.character(plot.data %>% 
                                       filter(., variable %in% p) %>% 
                                       select("colfill") %>% slice(1)) )  + 
    theme_gray(base_size = 20) + theme(legend.position = "none") +
    labs(subtitle = p, y = "", x = "Year")
}

plots <- mclapply(party.list, plot.fun, mc.cores = nbcores)
Fig5 <- ggarrange(plotlist = plots, ncol = 2, nrow = 3)
Fig5
ggsave("pics/LongTermBoxplots.pdf", 
       width = 16, height = 9, plot = Fig5)  


