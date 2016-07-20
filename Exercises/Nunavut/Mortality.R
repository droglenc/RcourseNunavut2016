# Nunavut 6-9-Aug-16

library(FSA)      # for filterD(), fact2num(), catchCurve()
library(dplyr)    # for mutate(), group_by(), summarize()
library(magrittr)
library(readxl)

source("PG008_readdata.R")
dNU_FW07 <- filterD(dNU,water.type %in% c("Freshwater","freshwater"),year==2007,!is.na(age)) %>%
  group_by(age) %>%
  summarize(Freq=n()) %>%
  mutate(logfreq=log(Freq)) %>%
  as.data.frame()
plot(logfreq~age,data=dNU_FW07)
cc07 <- catchCurve(Freq~age,data=dNU_FW07,ages2use=16:30)
plot(cc07)
coef(cc07)
confint(cc07)

dNU_FW10 <- filterD(dNU,water.type %in% c("Freshwater","freshwater"),year==2010,!is.na(age)) %>%
  group_by(age) %>%
  summarize(Freq=n()) %>%
  mutate(logfreq=log(Freq)) %>%
  as.data.frame()
plot(logfreq~age,data=dNU_FW10)
cc10 <- catchCurve(Freq~age,data=dNU_FW10,ages2use=20:30)
plot(cc10)
coef(cc10)
confint(cc10)

dNU_FW07 <- mutate(dNU_FW07,year=2007)
dNU_FW10 <- mutate(dNU_FW10,year=2010)
dNU_FW0710 <- rbind(filterD(dNU_FW07,age>=16),filterD(dNU_FW10,age>=20)) %>%
  mutate(fyear=factor(year))

lm1 <- lm(logfreq~age*fyear,data=dNU_FW0710)
anova(lm1)
fitPlot(lm1)


# Script created at 2016-07-20 16:19:40
