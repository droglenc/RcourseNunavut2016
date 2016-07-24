# Nunavut 6-9-Aug-16

library(FSA)
library(dplyr)

dWE <- read.csv("WalleyeErie2.csv")
dWE <- mutate(dWE,floc=factor(loc),fyear=factor(year),logw=log(w),logtl=log(tl))
dWE_13 <- filterD(dWE,loc==1,year==2013)

slr2 <- lm(logw~logtl,data=dWE_13)
residPlot(slr2)

( sum <- summary(slr2) )
sum$r.squared

cbind(ests=coef(slr2),confint(slr2))

tmp <- data.frame(logtl=log(c(100,200,300,400,500)))
predW <- predict(slr2,tmp,interval="prediction")
exp(predW)

clr <- col2rgbt("black",1/10)
Summarize(~logtl,data=dWE_13,digits=1)
logL <- seq(5.65,6.55,length.out=199)
logW <- predict(slr2,data.frame(logtl=logL),interval="prediction")
plot(logw~logtl,data=dWE_13,pch=19,col=clr,xlab="log Total Length (mm)",ylab="log Weight (g)")
lines(logL,logW[,"fit"],lwd=2)
lines(logL,logW[,"lwr"],lwd=2,lty=2)
lines(logL,logW[,"upr"],lwd=2,lty=2)
L <- exp(logL)
W <- exp(logW)
plot(w~tl,data=dWE_13,pch=19,col=clr,xlab="Total Length (mm)",ylab="Weight (g)")
lines(L,W[,"fit"],lwd=2)
lines(L,W[,"lwr"],lwd=2,lty=2)
lines(L,W[,"upr"],lwd=2,lty=2)


# Script created at 2016-07-24 10:57:56
