# Nunavut 6-9-Aug-16

library(FSA)
library(dplyr)
library(readxl)

source("PG008_readdata.R")
str(dNU)

dNU <- select(dNU,-locAKA,-gonad.prsrvd,-(finclip:gillarch.frozen),-(lat:sky),-remarks,-dbedits)
headtail(dNU)

dNU <- mutate(dNU,fl.in=fl/25.4,w.lbs=wt/454)
headtail(dNU)

dNU <- mutate(dNU,sex2=mapvalues(sex,from=c("M","F","U"),to=c("male","female","unknown")))
headtail(dNU)

levels(dNU$fyear)

tmp <- filterD(dNU,sex2=="male")
levels(tmp$sex2)
tmp <- filterD(dNU,!sex2 %in% c("female","unknown"))
levels(tmp$sex2)

tmp <- filterD(dNU,year %in% c(2006,2010,2013))
levels(tmp$fyear)

tmp <- filterD(dNU,sex2=="female",year==2010)
headtail(tmp)

( tmp <- filterD(dNU,sex2=="female",fl>750) )

( tmp <- filterD(dNU,sex2=="male",year==2002,is.na(age)) )

( tmp <- filterD(dNU,sex2=="male",!is.na(age)) )


# Script created at 2016-08-08 14:15:39
