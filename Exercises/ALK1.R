# Nunavut 6-9-Aug-16

library(FSA)
library(FSAdata)
library(dplyr)
library(nnet)

data(RockBassLO2)
rb.len <- filterD(RockBassLO2,is.na(age))
nrow(rb.len)
rb.age <- filterD(RockBassLO2,!is.na(age))
nrow(rb.age)

rb.age.mod <- mutate(rb.age,lcat10=lencat(tl,w=10))

freq <- xtabs(~lcat10+age,rb.age.mod)

sum(freq["180",])

sum(freq[,"7"])

freq["210","5"]/sum(freq["210",])

freq["180","5"]/sum(freq["180",])

ALK1 <- prop.table(freq,margin=1)

ALK1["210","5"]

30*ALK1["180","5"]

alkPlot(ALK1)

mlr <- multinom(age~lcat10,data=rb.age.mod,maxit=500)
lens <- seq(110,270,10)
ALK2 <- predict(mlr,data.frame(lcat10=lens),type="probs")
row.names(ALK2) <- lens

ALK2["210","5"]

30*ALK2["180","5"]


# Script created at 2016-07-25 10:19:58
