# Nunavut 6-9-Aug-16

library(FSA)
library(dplyr)
library(readxl)

source("PG008_readdata.R")
dNU_FW <- mutate(dNU,logwt=log(wt),logfl=log(fl))
dNU_FW <- filterD(dNU_FW,!is.na(wt),water.type %in% c("freshwater","Freshwater"))
dNU_FW10 <- filterD(dNU_FW,year==2010,logfl>4.7,sex!="U")

clr1 <- c("black","blue")
clr2 <- col2rgbt(clr1,1/3)
plot(wt~fl,data=dNU_FW10,pch=19,col=clr2[sex],xlab="Fork Length (mm)",ylab="Weight (g)")
plot(logwt~logfl,data=dNU_FW10,pch=19,col=clr2[sex],xlab="log Fork Length (mm)",ylab="log Weight (g)")

dvr1 <- lm(logwt~logfl*sex,data=dNU_FW10)
residPlot(dvr1,legend=FALSE)

anova(dvr1)

dvr2 <- lm(logwt~logfl,data=dNU_FW10)

cbind(ests=coef(dvr2),confint(dvr2))

tmp <- data.frame(logfl=log(c(200,400,600,800)))
predW <- predict(dvr2,tmp,interval="prediction")
cbind(tmp,exp(predW))

dNU_FW1314 <- filterD(dNU_FW,year>=2013)
dvr3 <- lm(logwt~logfl*fyear,data=dNU_FW1314)
anova(dvr3)
cbind(ests=coef(dvr3),confint(dvr3))

clr1 <- c("red","black")
clr2 <- col2rgbt(clr1,1/6)
Summarize(logfl~fyear,data=dNU_FW1314,digits=1)
logL1 <- seq(4.65,6.75,length.out=199)
logW1 <- predict(dvr3,data.frame(logfl=logL1,fyear="2013"),interval="prediction")
plot(logwt~logfl,data=dNU_FW1314,pch=19,col=clr2[fyear],cex=0.7,
     xlab="log Fork Length (mm)",ylab="log Weight (g)")
lines(logL1,logW1[,"fit"],lwd=2,col=clr1[1])
lines(logL1,logW1[,"lwr"],lwd=2,lty=2,col=clr1[1])
lines(logL1,logW1[,"upr"],lwd=2,lty=2,col=clr1[1])
logL2 <- seq(5.15,6.75,length.out=199)
logW2 <- predict(dvr3,data.frame(logfl=logL2,fyear="2014"),interval="prediction")
lines(logL2,logW2[,"fit"],lwd=2,col=clr1[2])
lines(logL2,logW2[,"lwr"],lwd=2,lty=2,col=clr1[2])
lines(logL2,logW2[,"upr"],lwd=2,lty=2,col=clr1[2])

lwCompPreds(dvr3,qlens.dec=0)


# Script created at 2016-08-08 14:15:19
