# Nunavut 6-9-Aug-16

library(FSA)
library(dplyr)

dWE <- read.csv("WalleyeErie2.csv")
dWE <- mutate(dWE,floc=factor(loc),fyear=factor(year))
str(dWE)

( n_loc <- xtabs(~floc,data=dWE) )

p_loc <- prop.table(n_loc)*100
round(p_loc,1)
barplot(p_loc,xlab="Location Code",ylab="Percent of All Fish Sampled")

n_sexyr <- xtabs(~sex+year,data=dWE)
round(prop.table(n_sexyr,margin=2)*100,1)

( n_sexyrloc <- xtabs(~sex+year+loc,data=dWE) )

dWE_14_1 <- filterD(dWE,year==2014,loc==1)

hist(~tl,data=dWE_14_1,w=10,xlab="Total Length (mm)",ylim=c(0,100))

Summarize(~tl,data=dWE_14_1,digits=1)

hist(~age,data=dWE_14_1,w=1,xlab="Age")

Summarize(~age,data=dWE_14_1,digits=1)

tp_age <- prop.table(xtabs(~age,dWE_14_1))*100
round(rcumsum(tp_age),1)
round(rcumsum(tp_age),1)["8"]

round(cumsum(tp_age),1)
round(cumsum(tp_age),1)["5"]

plot(tl~age,data=dWE_14_1,pch=19,col=col2rgbt("black",1/10))

dWE_14_1 <- mutate(dWE_14_1,logtl=log(tl),logw=log(w))
plot(logw~logtl,data=dWE_14_1,pch=19,col=col2rgbt("black",1/10))
with(dWE_14_1,cor(logw,logtl))


# Script created at 2016-07-23 21:56:17
