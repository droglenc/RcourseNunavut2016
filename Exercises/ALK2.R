# Nunavut 6-9-Aug-16

source("ALK1.R")

rb.len.mod <- alkIndivAge(ALK1,age~tl,data=rb.len)
rb.comb <- rbind(rb.age,rb.len.mod)
rb.comb <- mutate(rb.comb,lcat10=lencat(tl,w=10))
agefreq <- xtabs(~age,data=rb.comb)
lenfreq <- xtabs(~lcat10,data=rb.comb)
lensum <- Summarize(tl~age,data=rb.comb)

agefreq[["5"]]

agefreq[["11"]]

hist(~age,data=rb.comb,w=1,xlab="Age")

lenfreq[["150"]]

lensum$mean[lensum$age=="5"]

plot(tl~age,data=rb.comb,ylab="Total Length (mm)",xlab="Age (yrs)",
     pch=19,col=col2rgbt("black",1/20))
lines(mean~fact2num(age),data=lensum,col="blue",lwd=2)


# Script created at 2016-07-25 10:30:54
