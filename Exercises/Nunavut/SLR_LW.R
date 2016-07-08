# Nunavut 6-9-Aug-16

library(FSA)
library(dplyr)
library(readxl)

source("NU_readdata.R")
dNU_FW <- filterD(dNU,water.type=="Freshwater",year>2000)


# Script created at 2016-07-07 21:54:09
