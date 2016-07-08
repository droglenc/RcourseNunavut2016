# Nunavut 6-9-Aug-16

library(car)
library(multcomp)
library(FSA)
library(dplyr)
library(readxl)
library(plotrix)

source("NU_readdata.R")
dNU_FW <- filterD(dNU,water.type=="Freshwater",year>2000)

hist(FL~fyear,data=dNU_FW,w=25,xlim=c(100,800))
dNU_FW <- filterD(dNU_FW,FL<1500)

aov1 <- lm(FL~fyear,data=dNU_FW)
residPlot(aov1)
leveneTest(aov1)
anova(aov1)
mc1 <- glht(aov1,mcp(fyear="Tukey"))
summary(mc1)
cld(mc1)

sum <- Summarize(FL~fyear,data=dNU_FW)
sum <- select(sum,fyear,n,mean,sd)
sum <- mutate(sum,year=fact2num(fyear),se=sd/sqrt(n),
                   LCI=mean-qt(0.975,df=n-1)*se,UCI=mean+qt(0.975,df=n-1)*se)
with(sum,plotCI(year,mean,li=LCI,ui=UCI,pch=19,xlim=c(2009.8,2014.2),ylim=c(475,600),
                     xlab="Year",ylab="Mean Fork Length (mm)"))
with(sum,text(year,UCI,c("c","ab","bc","a","c"),pos=3))


# Script created at 2016-07-07 21:54:24
