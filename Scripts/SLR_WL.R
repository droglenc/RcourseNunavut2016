# Nunavut 6-9-Aug-16

library(FSA)      # for filterD(), hist(), Summarize()
library(dplyr)    # for mutate(), select()

# Set your working directory to where your external data files (and scripts) are located.
setwd("C:/aaaWork/Web/GitHub/RcourseNunavut2016/Handouts")
dSC <- read.csv("SawyerCo_reduced.csv")
dSC <- mutate(dSC,loglen=log(len),logwt=log(weight))
Sturg <- filterD(dSC,waterbody=="CHIPPEWA RIVER",species=="Lake Sturgeon",!is.na(len),!is.na(weight))

plot(weight~len,data=Sturg,pch=19,col=col2rgbt("black",1/3),
     xlab="Length (mm)",ylab="Weight (g)")           # Left
plot(logwt~loglen,data=Sturg,pch=19,col=col2rgbt("black",1/3),
     xlab="log Length (mm)",ylab="log Weight (g)")   # Right

slr1 <- lm(weight~len,data=Sturg)
residPlot(slr1)
slr2 <- lm(logwt~loglen,data=Sturg)
residPlot(slr2)
summary(slr2)
cbind(ests=coef(slr2),confint(slr2))

predict(slr2,data.frame(loglen=log(500)),interval="confidence")
predict(slr2,data.frame(loglen=log(c(500,800))),interval="prediction")

Summarize(~loglen,data=Sturg,digits=1)
logL <- seq(6.2,7.4,length.out=199)
logW <- predict(slr2,data.frame(loglen=logL),interval="prediction")
L <- exp(logL)
W <- exp(logW)
headtail(W)
plot(weight~len,data=Sturg,pch=19,col=col2rgbt("black",1/3),xlab="Length (mm)",ylab="Weight (g)")
lines(L,W[,"fit"],lwd=2)
lines(L,W[,"lwr"],lwd=2,lty=2)
lines(L,W[,"upr"],lwd=2,lty=2)


# Script created at 2016-07-17 19:58:39