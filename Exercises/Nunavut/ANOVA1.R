# Nunavut 6-9-Aug-16

library(car)
library(multcomp)
library(FSA)
library(dplyr)
library(readxl)
library(plotrix)

source("PG008_readdata.R")
dNU_SW <- filterD(dNU,water.type %in% c("Seawater","seawater"))

aov1 <- lm(FL~fyear,data=dNU_SW)
residPlot(aov1)
leveneTest(aov1)
anova(aov1)
mc1 <- glht(aov1,mcp(fyear="Tukey"))
summary(mc1)
cld(mc1)

sum <- Summarize(FL~fyear,data=dNU_SW)
sum <- select(sum,fyear,n,mean,sd)
sum <- mutate(sum,year=fact2num(fyear),se=sd/sqrt(n),
                   LCI=mean-qt(0.975,df=n-1)*se,UCI=mean+qt(0.975,df=n-1)*se)
with(sum,plotCI(year,mean,li=LCI,ui=UCI,pch=19,xlim=c(2005.8,2012.2),ylim=c(590,790),
                     xlab="Year",ylab="Mean Fork Length (mm)"))
with(sum,text(year,UCI,c("a","b","b","b","b"),pos=3))

dNU_FW <- filterD(dNU,water.type %in% c("Freshwater","freshwater"),age>=15,age<=20)

aov2 <- lm(FL~fyear,data=dNU_FW)
residPlot(aov2)
leveneTest(aov2)
anova(aov2)
mc2 <- glht(aov2,mcp(fyear="Tukey"))
summary(mc2)
cld(mc2)

sum <- Summarize(FL~fyear,data=dNU_FW)
sum <- select(sum,fyear,n,mean,sd)
sum <- mutate(sum,year=fact2num(fyear),se=sd/sqrt(n),
                   LCI=mean-qt(0.975,df=n-1)*se,UCI=mean+qt(0.975,df=n-1)*se)
with(sum,plotCI(year,mean,li=LCI,ui=UCI,pch=19,xlim=c(2001.8,2010.2),ylim=c(640,760),
                     xlab="Year",ylab="Mean Fork Length (mm)"))
with(sum,text(year,UCI,c("a","b","b"),pos=3))


# Script created at 2016-07-17 19:33:57
