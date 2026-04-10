#
#
# Politbarometer kumulation -  delta votes ~ delta skalo
#
#########################################################

library(haven)
library(stats)
library(dplyr)
#

setwd("~/Documents/Research/Elections/AnnaProjects/BKR/")

d <- read_dta("DATA/ZA2391_v14-0-0.dta") #%>% as.data.frame(.)



# mean skalo pds in 1990
mspds <- mean(d$v14[d$v4==1990 & d$v14!=0 & d$v14!=99 & d$v14!=11])

# Reduce to election years and Aug-Sep

d <- d[(d$v4==1980 & (d$v3==9 | d$v3==10) )|
         (d$v4==1983  & (d$v3==2 | d$v3==3) )|
         (d$v4==1986  & (d$v3==12 ) )|
         (d$v4==1987  & (d$v3==1 ) )|
         (d$v4==1990  & (d$v3==11 | d$v3==12) )|
         (d$v4==1994  & (d$v3==9 | d$v3==10) )|
         (d$v4==1998  & (d$v3==9 | d$v3==8) )|
         (d$v4==2002  & (d$v3==9 | d$v3==8) )|
         (d$v4==2005  & (d$v3==9 | d$v3==8) )|
         (d$v4==2009  & (d$v3==9 | d$v3==8) )|
         (d$v4==2013  & (d$v3==9 | d$v3==8) )|
         (d$v4==2017  & (d$v3==9 | d$v3==8) )|
         (d$v4==2021  & (d$v3==9 | d$v3==8) ),]



d$t <- 1
d$t[d$v4==1983] <- 2
d$t[d$v4==1986] <- 3
d$t[d$v4==1987] <- 3
d$t[d$v4==1990] <- 4
d$t[d$v4==1994] <- 5
d$t[d$v4==1998] <- 6
d$t[d$v4==2002] <- 7
d$t[d$v4==2005] <- 8
d$t[d$v4==2009] <- 9
d$t[d$v4==2013] <- 10
d$t[d$v4==2017] <- 11
d$t[d$v4==2021] <- 12

# replace CDU in Bayern with CSU
d$v9[d$v75==9] <- d$v10[d$v75==9]

# set missings in skalo
d$v9[d$v9==0 | d$v9==99] <- NA
d$v8[d$v8==0 | d$v8==99] <- NA
d$v11[d$v11==0 | d$v11==99] <- NA
d$v12[d$v12==0 | d$v12==99] <- NA
d$v13[d$v13==0 | d$v13==99] <- NA
d$v14[d$v14==0 | d$v14==99] <- NA




# assign intended vote choice maximum party rating to avoid ties at top

d$rating_cdu <- d$v9
d$rating_spd <- d$v8
d$rating_fdp <- d$v11
d$rating_gru <- d$v12
d$rating_lin <- d$v14
d$rating_afd <- d$v13

# impute ratings for PDS 1990, since no Skalometer ratings: vote intention=12, 
#                              all else=mean rating of other waves in 1990
d$rating_lin[d$t==4 & d$v6!=6] <- mspds

# reduce to respondents who intend to vote for one of the parliamentary parties
d <- d[d$v6==1 |d$v6==2 |d$v6==3 |
         d$v6==4 |d$v6==6 |d$v6==49 ,]


for(i in d$id){
  if(d$v6[i]==1){d$rating_cdu[i] <- 12}
  if(d$v6[i]==2){d$rating_spd[i] <- 12}
  if(d$v6[i]==3){d$rating_fdp[i] <- 12}
  if(d$v6[i]==4){d$rating_gru[i] <- 12}
  if(d$v6[i]==6){d$rating_lin[i] <- 12}
  if(d$v6[i]==49){d$rating_afd[i] <- 12}
}




# reduce to respondents with complete preference profiles (Skalometer)
d <- d[!is.na(d$rating_cdu) & !is.na(d$rating_spd) & !is.na(d$rating_fdp) & !is.na(d$rating_gru) ,]
d <- d[(!is.na(d$rating_lin) & d$t>3) | (is.na(d$rating_lin) & d$t<4),]
d <- d[(!is.na(d$rating_afd) & d$t>9) | (is.na(d$rating_afd) & d$t<10) ,]


d$id <- c(1:length(d[,1]))


# ranking matrix for calculating plurality and borda seats
trank <- matrix(NA, nrow=length(d[,1]), ncol=6)

for(i in d$id){
  tmp <- d[i,c(86:91)]
  tr <- rank(-1*tmp, na.last=TRUE, ties.method="max")
  tr[is.na(tmp)] <- NA
  trank[i,] <- tr
   d$cdu_first[i] <- ifelse(tr[1]==1 , 1, 0) 
   d$spd_first[i] <- ifelse(tr[2]==1 , 1, 0) 
   d$fdp_first[i] <- ifelse(tr[3]==1 , 1, 0) 
   d$gru_first[i] <- ifelse(tr[4]==1 , 1, 0) 
   d$lin_first[i] <- ifelse(tr[5]==1 , 1, 0) 
   d$afd_first[i] <- ifelse(tr[6]==1 , 1, 0) 
   
}

# Borda scores

bs <- trank
for(j in 1:12){
  if(j>9){ bs[d$t==j,] <- 6-trank[d$t==j,] }
  if(j<10&j>3){ bs[d$t==j,] <- 5-trank[d$t==j,] }
  if(j<4){ bs[d$t==j,] <- 4-trank[d$t==j,] }
}




# Define aggregate data matrix, one row per election
dd <- as.data.frame(matrix(1:12, ncol=1))
names(dd) <- "t"


for(i in 1:12){
  dd$skalo_cdu[i] <-  weighted.mean(d$rating_cdu[d$t==i], w=d$v78[d$t==i],  na.rm=TRUE) 
  dd$skalo_spd[i] <-  weighted.mean(d$rating_spd[d$t==i], w=d$v78[d$t==i],  na.rm=TRUE) 
  dd$skalo_fdp[i] <-  weighted.mean(d$rating_fdp[d$t==i], w=d$v78[d$t==i],  na.rm=TRUE) 
  dd$skalo_gru[i] <-  weighted.mean(d$rating_gru[d$t==i], w=d$v78[d$t==i],  na.rm=TRUE) 
  dd$skalo_lin[i] <-  weighted.mean(d$rating_lin[d$t==i], w=d$v78[d$t==i],  na.rm=TRUE) 
  dd$skalo_afd[i] <-  weighted.mean(d$rating_afd[d$t==i], w=d$v78[d$t==i],  na.rm=TRUE) 
  
  dd$plur_seats_cdu[i] <- weighted.mean(d$cdu_first[d$t==i], w=d$v78[d$t==i],  na.rm=TRUE) 
  dd$plur_seats_spd[i] <- weighted.mean(d$spd_first[d$t==i], w=d$v78[d$t==i],  na.rm=TRUE) 
  dd$plur_seats_fdp[i] <- weighted.mean(d$fdp_first[d$t==i], w=d$v78[d$t==i],  na.rm=TRUE) 
  dd$plur_seats_gru[i] <- weighted.mean(d$gru_first[d$t==i], w=d$v78[d$t==i],  na.rm=TRUE) 
  dd$plur_seats_lin[i] <- weighted.mean(d$lin_first[d$t==i], w=d$v78[d$t==i],  na.rm=TRUE) 
  dd$plur_seats_afd[i] <- weighted.mean(d$afd_first[d$t==i], w=d$v78[d$t==i],  na.rm=TRUE) 
  
  crossum <-  sum(colSums( cbind(c(bs[d$t==i,1]*d$v78[d$t==i]),
                             c(bs[d$t==i,2]*d$v78[d$t==i]),
                             c(bs[d$t==i,3]*d$v78[d$t==i]),
                             c(bs[d$t==i,4]*d$v78[d$t==i]),
                             c(bs[d$t==i,5]*d$v78[d$t==i]),
                             c(bs[d$t==i,6]*d$v78[d$t==i])), na.rm=TRUE))
  dd$borda_seats_cdu[i] <- sum(c(bs[d$t==i,1]*d$v78[d$t==i]) , na.rm=TRUE) / crossum
  dd$borda_seats_spd[i] <- sum(c(bs[d$t==i,2]*d$v78[d$t==i]) , na.rm=TRUE) / crossum
  dd$borda_seats_fdp[i] <- sum(c(bs[d$t==i,3]*d$v78[d$t==i]) , na.rm=TRUE) / crossum
  dd$borda_seats_gru[i] <- sum(c(bs[d$t==i,4]*d$v78[d$t==i]) , na.rm=TRUE) / crossum
  dd$borda_seats_lin[i] <- sum(c(bs[d$t==i,5]*d$v78[d$t==i]) , na.rm=TRUE) / crossum
  dd$borda_seats_afd[i] <- sum(c(bs[d$t==i,6]*d$v78[d$t==i]) , na.rm=TRUE) / crossum
  
  dd$year[i] <- d$v4[d$t==i][1]
}
dd$year[dd$year==1986] <- 1987
dd$borda_seats_afd[ dd$borda_seats_afd==0] <- NA
dd$borda_seats_lin[ dd$borda_seats_lin==0] <- NA






##### delta variables ###

for(i in 2:12){
dd$delta_skalo_cdu[i] <- dd$skalo_cdu[i] - dd$skalo_cdu[i-1]
dd$delta_skalo_spd[i] <- dd$skalo_spd[i] - dd$skalo_spd[i-1]
dd$delta_skalo_fdp[i] <- dd$skalo_fdp[i] - dd$skalo_fdp[i-1]
dd$delta_skalo_gru[i] <- dd$skalo_gru[i] - dd$skalo_gru[i-1]
dd$delta_skalo_lin[i] <- dd$skalo_lin[i] - dd$skalo_lin[i-1]
dd$delta_skalo_afd[i] <- dd$skalo_afd[i] - dd$skalo_afd[i-1]

dd$delta_seats_cdu[i] <- dd$seats_cdu[i] - dd$seats_cdu[i-1]
dd$delta_seats_spd[i] <- dd$seats_spd[i] - dd$seats_spd[i-1]
dd$delta_seats_fdp[i] <- dd$seats_fdp[i] - dd$seats_fdp[i-1]
dd$delta_seats_gru[i] <- dd$seats_gru[i] - dd$seats_gru[i-1]
dd$delta_seats_lin[i] <- dd$seats_lin[i] - dd$seats_lin[i-1]
dd$delta_seats_afd[i] <- dd$seats_afd[i] - dd$seats_afd[i-1]

dd$delta_pl_seats_cdu[i] <- dd$plur_seats_cdu[i] - dd$plur_seats_cdu[i-1]
dd$delta_pl_seats_spd[i] <- dd$plur_seats_spd[i] - dd$plur_seats_spd[i-1]
dd$delta_pl_seats_fdp[i] <- dd$plur_seats_fdp[i] - dd$plur_seats_fdp[i-1]
dd$delta_pl_seats_gru[i] <- dd$plur_seats_gru[i] - dd$plur_seats_gru[i-1]
dd$delta_pl_seats_lin[i] <- dd$plur_seats_lin[i] - dd$plur_seats_lin[i-1]
dd$delta_pl_seats_afd[i] <- dd$plur_seats_afd[i] - dd$plur_seats_afd[i-1]


dd$delta_borda_seats_cdu[i] <- dd$borda_seats_cdu[i] - dd$borda_seats_cdu[i-1]
dd$delta_borda_seats_spd[i] <- dd$borda_seats_spd[i] - dd$borda_seats_spd[i-1]
dd$delta_borda_seats_fdp[i] <- dd$borda_seats_fdp[i] - dd$borda_seats_fdp[i-1]
dd$delta_borda_seats_gru[i] <- dd$borda_seats_gru[i] - dd$borda_seats_gru[i-1]
dd$delta_borda_seats_lin[i] <- dd$borda_seats_lin[i] - dd$borda_seats_lin[i-1]
dd$delta_borda_seats_afd[i] <- dd$borda_seats_afd[i] - dd$borda_seats_afd[i-1]
}






# generate stacked data set: one row per election and party

library(survival)
dlong <- reshape(dd, direction="long", idvar="t", timevar="party", 
        times=c("cdu", "spd", "fdp", "gru", "lin", "afd"),                         
        varying=list( 
          c("delta_skalo_cdu", "delta_skalo_spd", "delta_skalo_fdp", "delta_skalo_gru", 
            "delta_skalo_lin", "delta_skalo_afd"),
          c("delta_pl_seats_cdu", "delta_pl_seats_spd", "delta_pl_seats_fdp", "delta_pl_seats_gru", 
            "delta_pl_seats_lin", "delta_pl_seats_afd"),
          c("delta_borda_seats_cdu", "delta_borda_seats_spd", "delta_borda_seats_fdp", "delta_borda_seats_gru", 
            "delta_borda_seats_lin", "delta_borda_seats_afd")))


# OLS Regression: swing in seats ~ swing in skalo rating, plurality
mpl <- lm(delta_pl_seats_cdu  ~ delta_skalo_cdu , data=dlong)
npl <- as.data.frame(seq(-3,3,by = 0.5))
names(npl) <- "delta_skalo_cdu"
cpl <- predict(mpl, newdata=npl, interval="confidence",level = 0.95)


# OLS Regression: swing in seats ~ swing in skalo rating, borda
mborda <- lm(delta_borda_seats_cdu  ~ delta_skalo_cdu , data=dlong)
nborda <- as.data.frame(seq(-3,3,by = 0.5))
names(nborda) <- "delta_skalo_cdu"
cborda <- predict(mborda, newdata=nborda, interval="confidence",level = 0.95)


# Table 2: OLS Regressions 
library(stargazer)
stargazer(mpl,mborda,
          header=F, 
          type= "latex", dep.var.labels = "Party seat share",
          title= "Niche party vote", notes = "standard errors in parentheses")




# Frequency of cases of monotonicity failure
length(dlong[!is.na(dlong$delta_pl_seats_cdu) & 
               ((dlong$delta_pl_seats_cdu>0 & dlong$delta_skalo_cdu<0) |
                (dlong$delta_pl_seats_cdu<0 & dlong$delta_skalo_cdu>0)),1])/
  length(dlong[!is.na(dlong$delta_pl_seats_cdu),1])

length(dlong[!is.na(dlong$delta_borda_seats_cdu) & 
               ((dlong$delta_borda_seats_cdu>0 & dlong$delta_skalo_cdu<0) |
                  (dlong$delta_borda_seats_cdu<0 & dlong$delta_skalo_cdu>0)),1])/
  length(dlong[!is.na(dlong$delta_borda_seats_cdu),1])





# Figure 6: delta seats ~ delta skalo

par(mfrow=c(1,2))
plot(dd$delta_pl_seats_spd ~ dd$delta_skalo_spd, pch=19, col="red",
     xlim=c(-2.5,2.5), ylim=c(-0.15,0.15), cex=1.3, main="Plurality",
     xlab="Swing in party support", ylab="Swing in seat share")
rect(par("usr")[1], par("usr")[3],
     par("usr")[2], par("usr")[4],
     col = "#ebebeb")
abline(mpl)
#lines(seq(-3,3,0.5), cpl[,2], lty=2)
#lines(seq(-3,3,0.5), cpl[,3],  lty=2)
abline(h=seq(-0.15,0.15, by=0.025),v=seq(-2.5,2.5,by=0.5), col="white")
abline(h=0,v=0,  lwd=2.5, col="white")
points(dd$delta_skalo_spd, dd$delta_pl_seats_spd, pch=19, cex=1.3, col="red")
points(dd$delta_skalo_cdu, dd$delta_pl_seats_cdu, pch=19, cex=1.3)
points(dd$delta_skalo_fdp, dd$delta_pl_seats_fdp, pch=19, cex=1.3, col="gold")
points(dd$delta_skalo_gru, dd$delta_pl_seats_gru, pch=19, cex=1.3, col="forestgreen")
points(dd$delta_skalo_lin, dd$delta_pl_seats_lin, pch=19, cex=1.3, col="magenta")
points(dd$delta_skalo_afd, dd$delta_pl_seats_afd, pch=19, cex=1.3, col="brown")
legend("topleft",  pch=19, col=c("black", "red", "gold", "forestgreen", "magenta", "brown"),
       legend=c("UNION", "SPD", "FDP", "GREEN", "LINKE", "AFD"),  bty="n")



plot(dd$delta_borda_seats_spd ~ dd$delta_skalo_spd, pch=19, col="red",
     xlim=c(-2.5,2.5), ylim=c(-0.15,0.15), cex=1.3,main="Borda",
     xlab="Swing in party support", ylab="Swing in seat share")
rect(par("usr")[1], par("usr")[3],
     par("usr")[2], par("usr")[4],
     col = "#ebebeb")
abline(mborda)

abline(h=seq(-0.15,0.15, by=0.025),v=seq(-2.5,2.5,by=0.5), col="white")
abline(h=0,v=0,  lwd=2.5, col="white")
points(dd$delta_skalo_spd, dd$delta_borda_seats_spd, pch=19, cex=1.3, col="red")
points(dd$delta_skalo_cdu, dd$delta_borda_seats_cdu, pch=19, cex=1.3)
points(dd$delta_skalo_fdp, dd$delta_borda_seats_fdp, pch=19, cex=1.3, col="gold")
points(dd$delta_skalo_gru, dd$delta_borda_seats_gru, pch=19, cex=1.3, col="forestgreen")
points(dd$delta_skalo_lin, dd$delta_borda_seats_lin, pch=19, cex=1.3, col="magenta")
points(dd$delta_skalo_afd, dd$delta_borda_seats_afd, pch=19, cex=1.3, col="brown")
legend("topleft",  pch=19, col=c("black", "red", "gold", "forestgreen", "magenta", "brown"),
       legend=c("UNION", "SPD", "FDP", "GREEN", "LINKE", "AFD"),  bty="n")

