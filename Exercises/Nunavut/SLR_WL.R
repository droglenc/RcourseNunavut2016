# Nunavut 6-9-Aug-16

library(FSA)
library(dplyr)
library(readxl)

source("PG008_readdata.R")
dNU_FW10 <- filterD(dNU,year==2010,water.type %in% c("Freshwater","freshwater"))
dNU_FW10 <- mutate(dNU_FW10,logwt=log(wt),logfl=log(FL))
dNU_FW10 <- filterD(dNU_FW10,logfl>4.7)

slr1 <- lm(logwt~logfl,data=dNU_FW10)
residPlot(slr1)

( sum <- summary(slr1) )
sum$r.squared

cbind(ests=coef(slr1),confint(slr1))

tmp <- data.frame(logfl=log(c(200,400,600,800)))
predW <- predict(slr1,tmp,interval="prediction")
exp(predW)

Summarize(~logfl,data=dNU_FW10,digits=1)
logL <- seq(4.85,6.85,length.out=199)
logW <- predict(slr1,data.frame(logfl=logL),interval="prediction")
plot(logwt~logfl,data=dNU_FW10,pch=19,col=col2rgbt("black",1/10),
     xlab="log Length (mm)",ylab="log Weight (g)")
lines(logL,logW[,"fit"],lwd=2)
lines(logL,logW[,"lwr"],lwd=2,lty=2)
lines(logL,logW[,"upr"],lwd=2,lty=2)
L <- exp(logL)
W <- exp(logW)
plot(wt~FL,data=dNU_FW10,pch=19,col=col2rgbt("black",1/10),xlab="Length (mm)",ylab="Weight (g)")
lines(L,W[,"fit"],lwd=2)
lines(L,W[,"lwr"],lwd=2,lty=2)
lines(L,W[,"upr"],lwd=2,lty=2)


# Script created at 2016-07-17 22:38:47
