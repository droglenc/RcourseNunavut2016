# Nunavut 6-9-Aug-16

library(car)      # leveneTest()
library(multcomp) # for glht(), mcp()  DO BEFORE dplyr!!
library(FSA)      # for filterD(), hist(), Summarize()
library(dplyr)    # for mutate(), select()
library(plotrix)  # for plotCI()

# Set your working directory to where your external data files (and scripts) are located.
setwd("C:/aaaWork/Web/GitHub/RcourseNunavut2016/Handouts")
dSC <- read.csv("SawyerCo_reduced.csv")
dSC <- mutate(dSC,sex=mapvalues(sex,from="",to="ND"),fyear=factor(year))
LChip_LMB <- filterD(dSC,waterbody=="LAKE CHIPPEWA",species=="Largemouth Bass")

Summarize(len~fyear,data=LChip_LMB)
boxplot(len~fyear,data=LChip_LMB,xlab="Year",ylab="Total Length (mm)")

hist(len~fyear,data=LChip_LMB,same.ylim=FALSE)

aov1 <- lm(len~fyear,data=LChip_LMB)
residPlot(aov1)
leveneTest(aov1)

LChip_LMB2 <- filterD(LChip_LMB,len>150)
aov2 <- lm(len~fyear,data=LChip_LMB2)
residPlot(aov2)
leveneTest(aov2)
anova(aov2)
mc2 <- glht(aov2,mcp(fyear="Tukey"))
summary(mc2)
cld(mc2)

sum_LMB2 <- Summarize(len~fyear,data=LChip_LMB2)
sum_LMB2 <- select(sum_LMB2,fyear,n,mean,sd)
sum_LMB2 <- mutate(sum_LMB2,year=fact2num(fyear),se=sd/sqrt(n),
                   LCI=mean-qt(0.975,df=n-1)*se,UCI=mean+qt(0.975,df=n-1)*se)
sum_LMB2
with(sum_LMB2,plotCI(year,mean,li=LCI,ui=UCI,pch=19,ylim=c(300,400),xlim=c(2009.8,2014.2),
                     xlab="Year",ylab="Mean Total Length (mm)"))
with(sum_LMB2,text(year,UCI,c("a","b","a","ab","ab"),pos=3))
axis(1,c("2011","2013"))

kruskal.test(len~fyear,data=LChip_LMB2)
dunnTest(len~fyear,data=LChip_LMB2)


# Script created at 2016-07-07 09:44:22
