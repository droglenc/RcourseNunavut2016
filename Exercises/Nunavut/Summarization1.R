# Nunavut 6-9-Aug-16

library(FSA)
library(dplyr)
library(readxl)

source("PG008_readdata.R")
str(dNU)

( n_yr <- xtabs(~fyear,data=dNU) )

p_yr <- prop.table(n_yr)*100
round(p_yr,1)
barplot(p_yr,xlab="Year",ylab="Percent of All Fish Sampled")

n_sexyr <- xtabs(~sex+fyear,data=dNU)
round(prop.table(n_sexyr,margin=2)*100,1)

( n_matsexyr <- xtabs(~mat+year+sex,data=dNU) )

dNU_13 <- filterD(dNU,year==2013)

hist(~fl,data=dNU_13,w=20,xlab="Fork Length (mm)")

Summarize(~fl,data=dNU_13,digits=1)

dNU_13 <- mutate(dNU_13,lcat50=lencat(fl,w=50))
tp_lcat50 <- prop.table(xtabs(~lcat50,dNU_13))*100
round(rcumsum(tp_lcat50),1)
round(rcumsum(tp_lcat50),1)["450"]

round(cumsum(tp_lcat50),1)
round(cumsum(tp_lcat50),1)["700"]

hist(~wt,data=dNU_13,w=100,xlab="Weight (grams)")
Summarize(~wt,data=dNU_13,digits=1)

plot(wt~fl,data=dNU_13,pch=19,col=col2rgbt("black",1/10))

dNU_13 <- mutate(dNU_13,logfl=log(fl),logwt=log(wt))
plot(logwt~logfl,data=dNU_13,pch=19,col=col2rgbt("black",1/10))
with(dNU_13,cor(logwt,logfl))

xtabs(~age+fyear,data=dNU)
dNU_10 <- filterD(dNU,year==2010)

hist(~age,data=dNU_10,w=1,xlab="Age (years)")
Summarize(~age,data=dNU_10,digits=1)

tp_age <- prop.table(xtabs(~age,dNU_10))*100
round(rcumsum(tp_age),1)
round(rcumsum(tp_age),1)["8"]

round(cumsum(tp_age),1)
round(cumsum(tp_age),1)["15"]

plot(fl~age,data=dNU_10,pch=19,col=col2rgbt("black",1/10))


# Script created at 2016-07-23 21:53:47
