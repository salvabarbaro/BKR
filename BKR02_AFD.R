## Barbaro, Kurella, Roth: Electoral outcomes versus voters' preferences: on the different tales the data can tell. JoEPOP 2026
### Assessment of the electoral success of the AFD between 2014 - 2016
######################################################
######################################################
library(haven)
library(ggplot2)
library(tidyr)
library(dplyr)
library(hutils)
library(ggpubr)
library(ggsignif)
library(ggpp)
library(reshape2)
library(forcats)
library(gtsummary)
library(foreach)
library(doParallel)
library(lubridate)
library(ggdist)
nbcores <- detectCores() - 2 
#################################################################
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
## load data from the Forschungsgruppe Wahlen 
btwpolls <- read.csv(url("https://gitlab.rlp.net/sbarbaro/btw21/-/raw/main/btwpolls.csv?inline=false")) %>%
  mutate(Datum = dmy(Datum)) %>% rename(Date = Datum) %>% 
  select(-c("ErhebZeitraum", "Participants"))  %>%
  melt(., id.vars = c("Date"))
##
### AFD between 2014 and 2016
## Literature: Second, the shift to the right coincides with the 2015 suspension of the Dublin regulation. 
afd.df <- btwpolls %>% filter(., variable %in% c("AFD")) %>% 
  filter(., (Date > "2014-01-01" & Date < "2014-12-31") | (Date > "2016-01-01" & Date < "2016-12-31")) %>%
  mutate(year = lubridate::year(Date))
afd.df %>% group_by(year) %>% summarise(avg = mean(value))

afd.df <- btwpolls %>% filter(., variable %in% c("AFD")) %>% 
  filter(., Date > "2014-01-01" & Date < "2016-12-31") %>%
  mutate(year = lubridate::year(Date))
avgpolls <- afd.df %>% group_by(year) %>% summarise(avg = mean(value), avg = scales::percent(avg, accuracy = 0.1L))  #%>%

afdpolls.pic <- ggplot(data = afd.df,
                       aes(x = Date, y = value)) +
  geom_line(col = "chocolate") +
  theme_bw(base_size = 20) +
  scale_y_continuous(labels = scales::percent) +
  annotate(geom = "table", x = as.Date("2014-08-01"), y = 0.115, label = list(avgpolls), 
           vjust = 1, hjust = 0, size = 6) +
  labs(y = "Vote Share (Polling Data)", x = "Year")

afd2.df <- plot.data %>% 
  filter(., variable == "AFD", v4 %in% 2014:2016) %>% 
  mutate(year = lubridate::year(Year)) %>%
  mutate(grade = value - 6)

#
avggrade <- afd2.df %>% group_by(year) %>% summarise(avg = round(mean(grade, na.rm = T),1))
#
afdgrade.pic <- ggplot(data = afd2.df, 
                       aes(x = Year, y = grade, group = year )) +
  geom_boxplot(fill = "orange", col = "chocolate", alpha = 0.8) +
  labs(x = "Date") +
  annotate(geom = "table", x = as.Date("2015-04-01"), y = 3.5, label = list(avggrade), 
           vjust = 1, hjust = 0, size = 6) +
  theme_bw(base_size = 20) +
  labs(y = "Skalometer value", x = "Year")

afdpics <- ggarrange(afdpolls.pic, afdgrade.pic, ncol = 2)
afdpics
ggsave("afd1416.pdf", plot = afdpics, width = 16, height = 9)



#### both panels in one figures with two axis
# First step: subset za.w.df 2014 - 2016, group by v3 (Erhebungsmonat) and calculate the mean values for v13
dates <- format(seq(as.Date("2014-01-01"), as.Date("2016-12-01"), by = "month"), "%Y-%m")
newdf <- za.w.df %>% filter(., v4 %in% 2014:2016) %>%
  group_by(v4, v3) %>% reframe(skalo.afd = mean(v13, na.rm=T)) %>% 
  mutate(dates = dates)
## same with polling data:
newdf2 <- afd.df %>%
  mutate(dates = format(floor_date(Date, "month"), "%Y-%m"))
newdf3 <- left_join(x = newdf2, y = newdf, by = "dates") %>% 
  select(., c("dates", "value", "skalo.afd")) %>% group_by(dates) %>% 
  mutate(polls = mean(value, na.rm = T), perception = mean(skalo.afd, na.rm = T)) %>%
  mutate(dates = ymd(dates, truncated = 2L))
rm(newdf, newdf2)

sc.par <- .025

ggplot(newdf3, aes(x = dates)) +
  scale_x_date(date_breaks = "3 month", labels = scales::date_format("%m-%Y")) +
  geom_line(aes(y = polls, color = "Polls") ) +
  geom_line(aes(y = (perception) * sc.par, color = "Skalometer") ) +  
  scale_y_continuous(
    name = "Vote share (polling data)", 
    labels = scales::percent, 
#    limits = c(0, .15),
    sec.axis = sec_axis(~./sc.par -6, name = "Avg. skalometer value") )+
  theme_gray(base_size = 22) + 
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_viridis_d(begin = 0.2, end = 0.8, option = "inferno") +
  labs(x = " ", color = "Variables")
ggsave("afdnewplot.pdf", width = 16, height = 9)

### for presentations with gray background
ggplot(newdf3, aes(x = dates)) +
  scale_x_date(date_breaks = "3 month", labels = scales::date_format("%m-%Y")) +
  geom_line(aes(y = polls, color = "Polls") ) +
  geom_line(aes(y = (perception) * sc.par, color = "Skalometer") ) +  
  scale_y_continuous(
    name = "Vote share (polling data)", 
    labels = scales::percent, 
    sec.axis = sec_axis(~./sc.par -6, name = "Avg. skalometer value") )+
  theme_bw(base_size = 26) +
  theme(legend.position = "none",
    axis.title.y.left = element_text(color = "#C1002B"),  
    axis.title.y.right = element_text(color = "#310CC1"),
    axis.text.x = element_text(angle = 45, hjust = 1, color = "#C1002B"),  
    axis.text.y.right = element_text(color = "#310CC1"),
    axis.text.y.left = element_text(color = "#C1002B"),
    legend.title = element_text(color = "#C1002B"),                        # Color legend title #C1002B
    legend.text = element_text(color = "#C1002B"),                         # Color legend text #C1002B
    legend.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),      # Transparent plot background
    panel.background = element_rect(fill = "transparent", color = NA),     # Transparent panel background                                # Remove minor gridlines
    panel.grid.major = element_line(color = "white"),                      # White major gridlines
    panel.grid.minor = element_line(color = "white")  
  ) +
  scale_color_manual(values = c("#C1002B", "#310CC1")) +
#  scale_color_viridis_d(begin = 0.2, end = 0.8, option = "inferno") +
# #C1002B  
  labs(x = " ", color = "Variables")
ggsave("afdtrans.pdf", width = 16, height = 9)

