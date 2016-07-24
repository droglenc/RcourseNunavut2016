# Nunavut 6-9-Aug-16

library(FSA)      # for filterD(), hist(), Summarize()
library(dplyr)    # for mutate(), select()

# Set your working directory to where your external data files (and scripts) are located.
setwd("C:/aaaWork/Web/GitHub/RcourseNunavut2016/Handouts")
dSC <- read.csv("SawyerCo_reduced.csv")
dSC <- mutate(dSC,loglen=log(len),logwt=log(weight))
Sturg <- filterD(dSC,waterbody=="CHIPPEWA RIVER",species=="Lake Sturgeon",!is.na(len),!is.na(weight))

clr <- col2rgbt("black",1/3)
plot(weight~len,data=Sturg,pch=19,col=clr,xlab="Total Length (mm)",ylab="Weight (g)")           # Left
plot(logwt~loglen,data=Sturg,pch=19,col=clr,xlab="log Total Length (mm)",ylab="log Weight (g)") # Right

slr1 <- lm(weight~len,data=Sturg)
residPlot(slr1)
slr2 <- lm(logwt~loglen,data=Sturg)
residPlot(slr2)

summary(slr2)
cbind(ests=coef(slr2),confint(slr2))

( p1 <- predict(slr2,data.frame(loglen=log(500)),interval="confidence") )
exp(p1)
( p2 <- predict(slr2,data.frame(loglen=log(c(500,800))),interval="prediction") )
exp(p2)

Summarize(~len,data=Sturg,digits=1)
L <- seq(495,1494,length.out=199)
W <- exp(predict(slr2,data.frame(loglen=log(L)),interval="prediction"))
headtail(W)
plot(weight~len,data=Sturg,pch=19,col=clr,xlab="Total Length (mm)",ylab="Weight (g)")
lines(L,W[,"fit"],lwd=2)
lines(L,W[,"lwr"],lwd=2,lty=2)
lines(L,W[,"upr"],lwd=2,lty=2)


# Script created at 2016-07-24 10:52:28
