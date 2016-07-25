# Nunavut 6-9-Aug-16

# Appropriately set the working directory before this
# This also ran library(FSA) which also provides alkIndivAge(), Summarize(), hist()
source("../Scripts/ALK1.R")

ls()
headtail(sp.len)

sp.len.mod <- alkIndivAge(ALK.obs,age~tl,data=sp.len)
headtail(sp.len.mod)
sp.comb <- rbind(sp.age,sp.len.mod)
str(sp.comb)

( agefreq <- xtabs(~age,data=sp.comb) )
round(prop.table(agefreq)*100,1)

hist(~age,data=sp.comb,w=1,xlab="Age (yrs)")

( sp.sum <- Summarize(tl~age,data=sp.comb,digits=2) )
plot(tl~age,data=sp.comb,ylab=lblTL,xlab="Age (yrs)",pch=19,col=col2rgbt("black",0.1))
lines(mean~fact2num(age),data=sp.sum,col="blue",lwd=2)


# Script created at 2016-07-25 09:35:42
