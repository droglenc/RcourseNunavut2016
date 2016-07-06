# Nunavut 6-9-Aug-16

library(FSA)
library(dplyr)
dWE <- read.csv("WalleyeErie2.csv")
str(dWE)

dWE <- select(dWE,-setID,-grid)
headtail(dWE)

dWE <- mutate(dWE,tl.in=tl/25.4,w.lbs=w/454)
headtail(dWE)

dWE <- mutate(dWE,sex2=mapvalues(sex,from=c("male","female"),to=c("M","F")))
headtail(dWE)

dWE <- mutate(dWE,fyear=factor(year))
headtail(dWE)
levels(dWE$fyear)

tmp <- filterD(dWE,sex=="male")
levels(tmp$sex)

tmp <- filterD(dWE,year %in% c(2005,2009,2013))
levels(tmp$fyear)

tmp <- filterD(dWE,sex=="female",year==2010)
headtail(tmp)

( tmp <- filterD(dWE,sex=="female",tl>750) )

( tmp <- filterD(dWE,sex=="male",year==2013,is.na(w)) )

( tmp <- filterD(dWE,sex=="male",is.na(age)) )

library(readxl)
library(dplyr)
meta <- read.csv("NU_metadata.csv",stringsAsFactors=FALSE)
dNU <- read_excel("PG027.SA.Data.xlsx",na="nd",skip=1,
                  col_names=meta$new_names,col_types=meta$new_types)
dNU <- mutate(dNU,netset.time=format(netset.time,"%T"),netlift.time=format(netlift.time,"%T"),
              fyear=factor(year),loc=factor(loc),locAKA=factor(locAKA),water.type=factor(water.type),
              spec=factor(spec),sex=factor(sex),mat=factor(mat),life.hist=factor(life.hist))
dNU <- as.data.frame(dNU)


# Script created at 2016-07-06 17:14:54
