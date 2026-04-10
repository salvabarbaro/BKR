library(dplyr)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(ggpubr)
library(lubridate)
#
## we use the electoral results provided by 
## Döring, Holger; Manow, Philip, 2018, "ParlGov 2018 Release", 
## https://doi.org/10.7910/DVN/F0YGNC, Harvard Dataverse, V1, UNF:6:NQUZONiGgflT9PBAyCVu+Q== [fileUNF] 
# deu.elections is a subset from the above data set, restricted to German federal results.
# Data for 2021 are added manually
deu.elections <- read.csv2(url("https://gitlab.rlp.net/sbarbaro/aggregationrule/-/raw/main/elections.csv?inline=false"),
                header = T, sep = ",")
#
cols2 <- c("AFD" = "chocolate", "FDP" = "yellow1", "GREEN" = "forestgreen", "LINKE" = "violet", 
           "SPD" = "red", "UNION" = "black")

Fig1 <- ggplot(data = deu.elections ,
       aes(x = lubridate::year(Year), y = (vote_share/100), colour = party, group = party)) +
  geom_line(linewidth = 1) + geom_point() +
  scale_y_continuous(labels = scales::percent) +
  scale_colour_manual(values = cols2, 
                      labels = c("UNION", "SPD", "GREEN", "FDP", "LINKE", "AFD"),
                      breaks = c("UNION", "SPD", "GREEN", "FDP", "LINKE", "AFD")) +
  theme_gray(base_size = 22) +
  theme(legend.position = "bottom") + 
  labs(col = "Party", y = "Vote Shares", x = "Year")
ggsave("electoralresults2.pdf", width = 16, height = 9, plot = Fig1)  


