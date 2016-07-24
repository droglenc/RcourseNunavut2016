# Nunavut 6-9-Aug-16

library(FSA)
library(dplyr)

dWE <- read.csv("WalleyeErie2.csv")
dWE <- mutate(dWE,floc=factor(loc),fyear=factor(year),logw=log(w),logtl=log(tl))
dWE_13 <- filterD(dWE,loc==1,year==2013)

clr1 <- c("black","blue")
clr2 <- col2rgbt(clr1,1/6)
plot(w~tl,data=dWE_13,pch=19,col=clr2[sex],xlab="Total Length (mm)",ylab="Weight (g)")
plot(logw~logtl,data=dWE_13,pch=19,col=clr2[sex],xlab="log Total Length (mm)",ylab="log Weight (g)")

dvr1 <- lm(logw~logtl*sex,data=dWE_13)
residPlot(dvr1,legend=FALSE)

anova(dvr1)

dvr2 <- lm(logw~logtl+sex,data=dWE_13)

cbind(ests=coef(dvr2),confint(dvr2))

tmp <- data.frame(logtl=log(rep(c(350,450,550,650),2)),sex=rep(c("female","male"),each=4))
predW <- predict(dvr1,tmp,interval="prediction")
cbind(tmp,exp(predW))

Summarize(logtl~sex,data=dWE_13,digits=1)
logL1 <- seq(5.65,6.55,length.out=199)
logW1 <- predict(dvr2,data.frame(logtl=logL1,sex="female"),interval="prediction")
plot(logw~logtl,data=dWE_13,pch=19,col=clr2[sex],
     xlab="log Length (mm)",ylab="log Weight (g)")
lines(logL1,logW1[,"fit"],lwd=2)
lines(logL1,logW1[,"lwr"],lwd=2,lty=2)
lines(logL1,logW1[,"upr"],lwd=2,lty=2)
logL2 <- seq(5.65,6.45,length.out=199)
logW2 <- predict(dvr2,data.frame(logtl=logL2,sex="male"),interval="prediction")
lines(logL2,logW2[,"fit"],lwd=2,col=clr1[2])
lines(logL2,logW2[,"lwr"],lwd=2,lty=2,col=clr1[2])
lines(logL2,logW2[,"upr"],lwd=2,lty=2,col=clr1[2])
L1 <- exp(logL1)
W1 <- exp(logW1)
plot(w~tl,data=dWE_13,pch=19,col=clr2[sex],xlab="Length (mm)",ylab="Weight (g)")
lines(L1,W1[,"fit"],lwd=2)
lines(L1,W1[,"lwr"],lwd=2,lty=2)
lines(L1,W1[,"upr"],lwd=2,lty=2)
L2 <- exp(logL2)
W2 <- exp(logW2)
lines(L2,W2[,"fit"],lwd=2,col=clr1[2])
lines(L2,W2[,"lwr"],lwd=2,lty=2,col=clr1[2])
lines(L2,W2[,"upr"],lwd=2,lty=2,col=clr1[2])

dWE_F08 <- filterD(dWE,sex=="female",year==2008)
dvr3 <- lm(logw~logtl*floc,data=dWE_F08)
anova(dvr3)
cbind(ests=coef(dvr3),confint(dvr3))

clr1 <- c("blue","red","black")
clr2 <- col2rgbt(clr1,1/20)
Summarize(logtl~floc,data=dWE_F08,digits=1)
logL1 <- seq(5.65,6.55,length.out=199)
logW1 <- predict(dvr3,data.frame(logtl=logL1,floc="1"),interval="prediction")
plot(logw~logtl,data=dWE_13,pch=19,col=clr2[loc],cex=0.7,
     xlab="log Length (mm)",ylab="log Weight (g)")
lines(logL1,logW1[,"fit"],lwd=2,col=clr1[1])
lines(logL1,logW1[,"lwr"],lwd=2,lty=2,col=clr1[1])
lines(logL1,logW1[,"upr"],lwd=2,lty=2,col=clr1[1])
logL2 <- seq(5.65,6.65,length.out=199)
logW2 <- predict(dvr3,data.frame(logtl=logL2,floc="2"),interval="prediction")
lines(logL2,logW2[,"fit"],lwd=2,col=clr1[2])
lines(logL2,logW2[,"lwr"],lwd=2,lty=2,col=clr1[2])
lines(logL2,logW2[,"upr"],lwd=2,lty=2,col=clr1[2])
logL3 <- seq(5.85,6.65,length.out=199)
logW3 <- predict(dvr3,data.frame(logtl=logL3,floc="3"),interval="prediction")
lines(logL3,logW3[,"fit"],lwd=2,col=clr1[3])
lines(logL3,logW3[,"lwr"],lwd=2,lty=2,col=clr1[3])
lines(logL3,logW3[,"upr"],lwd=2,lty=2,col=clr1[3])

lwCompPreds(dvr3,qlens.dec=0)


# Script created at 2016-07-24 11:53:02
