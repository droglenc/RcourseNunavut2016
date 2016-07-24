# Nunavut 6-9-Aug-16

library(FSA)
library(dplyr)
library(readxl)

source("PG008_readdata.R")
str(dNU)

ncol(dNU)

nrow(dNU)

names(dNU)[5]
class(dNU[,5])

names(dNU)[2]
class(dNU[,2])

dNU$fl

dNU$fl[5]

dNU$fl[dNU$year==2002]


# Script created at 2016-07-23 21:07:09
