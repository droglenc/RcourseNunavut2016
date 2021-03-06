# Nunavut 6-9-Aug-16

library(car)
library(multcomp)
library(FSA)
library(dplyr)
library(plotrix)

dWE <- read.csv("WalleyeErie2.csv")
dWE <- mutate(dWE,floc=factor(loc),fyear=factor(year),logw=log(w))
dWE_13 <- filterD(dWE,year==2013)

aov1 <- lm(w~floc,data=dWE_13)
residPlot(aov1)
leveneTest(aov1)

aov2 <- lm(logw~floc,data=dWE_13)
residPlot(aov2)
leveneTest(aov2)

mc2 <- glht(aov2,mcp(floc="Tukey"))
summary(mc2)
cld(mc2)
sum <- Summarize(logw~floc,data=dWE_13)
sum <- select(sum,floc,n,mean,sd)
sum <- mutate(sum,loc=fact2num(floc),se=sd/sqrt(n),
                   LCI=mean-qt(0.975,df=n-1)*se,UCI=mean+qt(0.975,df=n-1)*se)
with(sum,plotCI(loc,mean,li=LCI,ui=UCI,pch=19,ylim=c(6.4,7.4),xlim=c(0.8,3.2),
                     xlab="Location Code",ylab="Mean Log Weight (g)"))
with(sum,text(loc,UCI,c("a","b","c"),pos=3))

sum2 <- mutate(sum,bmean=exp(mean),bLCI=exp(LCI),bUCI=exp(UCI))
sum2 <- select(sum2,-(floc:sd),-(se:UCI))
with(sum2,plotCI(loc,bmean,li=bLCI,ui=bUCI,pch=19,ylim=c(600,1600),xlim=c(0.8,3.2),
                     xlab="Location Code",ylab="Mean Weight (g)"))
with(sum2,text(loc,bUCI,c("a","b","c"),pos=3))

dWE_1 <- filterD(dWE,loc==1,age==1)

aov1 <- lm(tl~fyear,data=dWE_1)
residPlot(aov1)
leveneTest(aov1)

mc1 <- glht(aov1,mcp(fyear="Tukey"))
summary(mc1)
cld(mc1)
sum <- Summarize(tl~fyear,data=dWE_1)
sum <- select(sum,fyear,n,mean,sd)
sum <- mutate(sum,year=fact2num(fyear),se=sd/sqrt(n),
                  LCI=mean-qt(0.975,df=n-1)*se,UCI=mean+qt(0.975,df=n-1)*se)
with(sum,plotCI(year,mean,li=LCI,ui=UCI,pch=19,ylim=c(290,390),xlim=c(2003.8,2014.2),
                     xlab="Capture Year",ylab="Mean Total Length (mm)"))
with(sum,text(year,UCI,c("a","b","b","g","d","c","c","d","f","de","ef"),pos=3))


# Script created at 2016-07-23 22:32:04
