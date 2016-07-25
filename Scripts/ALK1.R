# Nunavut 6-9-Aug-16

library(FSA)      # for filterD(), hist(), Summarize(), dunnTest()
library(FSAdata)  # for SpotVA2 data
library(dplyr)    # for mutate()
library(nnet)     # for multinom()

data(SpotVA2)
headtail(SpotVA2)
sp.len <- filter(SpotVA2,is.na(age))
headtail(sp.len)
sp.age <- filter(SpotVA2,!is.na(age))
headtail(sp.age)
sp.age.mod <- mutate(sp.age,lcat=lencat(tl,w=1))
headtail(sp.age.mod)

( raw <- xtabs(~lcat+age,data=sp.age.mod) )
( ALK.obs <- prop.table(raw,margin=1) )

mlr <- multinom(age~lcat,data=sp.age.mod,maxit=500)
lens <- seq(6,13,1)
ALK.sm <- predict(mlr,data.frame(lcat=lens),type="probs")
row.names(ALK.sm) <- lens
round(ALK.sm,3)

lblTL <- "Total Length (cm)"
alkPlot(ALK.obs,xlab=lblTL)
alkPlot(ALK.sm,xlab=lblTL)

alkPlot(ALK.sm,pal="gray",showLegend=TRUE,xlab=lblTL)
alkPlot(ALK.sm,type="area",pal="gray",showLegend=TRUE,xlab=lblTL)

alkPlot(ALK.sm,type="lines",pal="gray",xlab=lblTL)
alkPlot(ALK.sm,type="bubble",xlab=lblTL)


# Script created at 2016-07-25 09:29:43
