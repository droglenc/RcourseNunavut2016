# Nunavut 6-9-Aug-16

library(FSA)      # for filterD(), fact2num(), catchCurve()
library(dplyr)    # for mutate(), group_by(), summarize()
library(magrittr)

dWE1 <- read.csv("WalleyeErie2.csv") %>%
  filterD(loc==1)
dWE1_13 <- filterD(dWE1,year==2013) %>%
  group_by(age) %>%
  summarize(Freq=n()) %>%
  mutate(logfreq=log(Freq)) %>%
  as.data.frame()

( tmp <- xtabs(~age+year,data=dWE1) )

d03 <- diags(tmp,which=0,incl.labels="row",val.name="Freq",label.name="age") %>%
  mutate(Freq=mapvalues(Freq,from=0,to=NA)) %>%
  mutate(logfreq=log(Freq))
plot(logfreq~age,data=d03,xlab="Age",ylab="Log Frequency",pch=19)
cc03a <- catchCurve(Freq~age,data=d03,ages2use=2:9)
coef(cc03a)
confint(cc03a)
plot(cc03a)
cc03b <- catchCurve(Freq~age,data=d03,ages2use=4:11)
coef(cc03b)
confint(cc03b)
plot(cc03b)

d01 <- diags(tmp,which=2,incl.labels="row",val.name="Freq",label.name="age") %>%
  mutate(Freq=mapvalues(Freq,from=0,to=NA)) %>%
  mutate(logfreq=log(Freq))
plot(logfreq~age,data=d01,xlab="Age",ylab="Log Frequency",pch=19)
cc01a <- catchCurve(Freq~age,data=d01,ages2use=2:11)
coef(cc01a)
confint(cc01a)
plot(cc01a)


# Script created at 2016-07-24 20:47:20
