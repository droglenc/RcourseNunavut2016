# Nunavut 6-9-Aug-16
library(FSA)

dWE <- read.csv("WalleyeErie2.csv")
str(dWE)

ncol(dWE)

nrow(dWE)

names(dWE)[5]
class(dWE[,5])

names(dWE)[2]
class(dWE[,2])

library(readxl)
library(dplyr)
( meta <- read.csv("NU_metadata.csv",stringsAsFactors=FALSE) )
dNU <- read_excel("PG027.SA.Data.xlsx",na="nd",skip=1,
                  col_names=meta$new_names,col_types=meta$new_types)
dNU <- mutate(dNU,netset.time=format(netset.time,"%T"),netlift.time=format(netlift.time,"%T"),
              fyear=factor(year),loc=factor(loc),locAKA=factor(locAKA),water.type=factor(water.type),
              spec=factor(spec),sex=factor(sex),mat=factor(mat),life.hist=factor(life.hist))
dNU <- as.data.frame(dNU)
str(dNU)


# Script created at 2016-07-06 15:53:13
