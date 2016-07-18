# Nunavut 6-9-Aug-16

library(FSA)      # for filterD(), hist(), Summarize()
library(dplyr)    # for mutate(), select()

# Set your working directory to where your external data files (and scripts) are located.
setwd("C:/aaaWork/Web/GitHub/RcourseNunavut2016/Handouts")
dSC <- read.csv("SawyerCo_reduced.csv")
dSC <- mutate(dSC,loglen=log(len),logwt=log(weight))
Sturg <- filterD(dSC,waterbody %in% c("CHIPPEWA RIVER","HUNTER LAKE"),
                 species=="Lake Sturgeon",!is.na(len),!is.na(weight))

clr1 <- c("black","blue")
clr2 <- col2rgbt(clr1,1/3)
plot(weight~len,data=Sturg,pch=19,col=clr2[waterbody],
     xlab="Total Length (mm)",ylab="Weight (g)")           # Left
plot(logwt~loglen,data=Sturg,pch=19,col=clr2[waterbody],
     xlab="log Total Length (mm)",ylab="log Weight (g)")   # Right

dvr1 <- lm(logwt~loglen*waterbody,data=Sturg)
residPlot(dvr1,legend=FALSE)
anova(dvr1)
dvr2 <- lm(logwt~loglen+waterbody,data=Sturg)
anova(dvr2)
summary(dvr2)
round(cbind(ests=coef(dvr2),confint(dvr2)),3)

tmp <- data.frame(loglen=log(c(1000,1000)),waterbody=c("CHIPPEWA RIVER","HUNTER LAKE"))
( p1 <- predict(dvr2,tmp,interval="confidence") )
exp(p1)

plot(weight~len,data=Sturg,pch=19,col=clr2[waterbody],
     xlab="Total Length (mm)",ylab="Weight (g)")
Summarize(loglen~waterbody,data=Sturg,digits=1)
logL <- seq(6.15,7.35,length.out=199)
logW <- predict(dvr2,data.frame(loglen=logL,waterbody="CHIPPEWA RIVER"),interval="confidence")
cL <- exp(logL)
cW <- exp(logW)
lines(cL,cW[,"fit"],lwd=2,col=clr1[1])
lines(cL,cW[,"lwr"],lwd=2,lty=2,col=clr1[1])
lines(cL,cW[,"upr"],lwd=2,lty=2,col=clr1[1])
logL <- seq(6.95,7.55,length.out=199)
logW <- predict(dvr2,data.frame(loglen=logL,waterbody="HUNTER LAKE"),interval="confidence")
hL <- exp(logL)
hW <- exp(logW)
lines(hL,hW[,"fit"],lwd=2,col=clr1[2])
lines(hL,hW[,"lwr"],lwd=2,lty=2,col=clr1[2])
lines(hL,hW[,"upr"],lwd=2,lty=2,col=clr1[2])
legend("topleft",legend=c("Chippewa R.","Hunter L."),lwd=2,col=clr1,pch=19,bty="n",cex=0.8)

lwCompPreds(dvr2,qlens.dec=0)
lwCompPreds(dvr2,lens=c(700,900,1100,1300,1500,1700),show.preds=TRUE)


# Script created at 2016-07-17 23:13:37
