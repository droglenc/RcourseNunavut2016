# Nunavut 6-9-Aug-16

library(FSA)
library(dplyr)
library(readxl)

source("NU_readdata.R")

( n_yr <- xtabs(~fyear,data=dNU) )

p_yr <- prop.table(n_yr)*100
round(p_yr,1)
barplot(p_yr,xlab="Year",ylab="Percent of All Fish Sampled")

n_sexyr <- xtabs(~sex+fyear,data=dNU)
round(prop.table(n_sexyr,margin=2)*100,1)

( n_matsexyr <- xtabs(~mat+year+sex,data=dNU) )

dNU_14 <- filterD(dNU,year==2014)

hist(~FL,data=dNU_14,w=10,xlab="Fork Length (mm)")

Summarize(~FL,data=dNU_14,digits=1)

hist(~wt,data=dNU_14,w=100,xlab="Weight (grams)")
Summarize(~wt,data=dNU_14,digits=1)

plot(wt~FL,data=dNU,pch=19,col=col2rgbt("black",1/10))

dNU_14 <- mutate(dNU_14,logfl=log(FL),logwt=log(wt))
plot(logwt~logfl,data=dNU_14,pch=19,col=col2rgbt("black",1/10))
with(dNU_14,cor(logwt,logfl))


# Script created at 2016-07-07 15:43:00
