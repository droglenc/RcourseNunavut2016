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

dWE$tl

dWE$tl[5]

dWE$tl[dWE$year==2003]


# Script created at 2016-07-23 21:08:09
