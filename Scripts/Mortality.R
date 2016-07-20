# Nunavut 6-9-Aug-16

library(FSA)      # for filterD(), fact2num(), catchCurve()
library(dplyr)    # for mutate(), group_by(), summarize()

# Set your working directory to where your external data files (and scripts) are located.
setwd("C:/aaaWork/Web/GitHub/RcourseNunavut2016/Handouts")
dSC <- read.csv("SawyerCo_reduced.csv")
wae <- filterD(dSC,waterbody=="NELSON LAKE",species=="Walleye",!is.na(age),year==2014)

d <- data.frame( xtabs(~age,data=wae))
str(d)
d <- mutate(d,age=fact2num(age))
str(d)
d

wae <- group_by(wae,age)
d <- summarize(wae,Freq=n())
str(d)
d <- as.data.frame(d)
str(d)
d

d <- mutate(d,logfreq=log(Freq))

plot(logfreq~age,data=d,xlab="Age",ylab="Log Frequency",pch=19)

cc1 <- lm(logfreq~age,data=filterD(d,age>=5))
anova(cc1)
( cf <- coef(cc1) )
( Z <- -cf["age"] )
( A <- 1-exp(-Z) )
plot(logfreq~age,data=d,xlab="Age",ylab="Log Frequency",pch=19)
abline(cc1)

cc2 <- catchCurve(Freq~age,data=d,ages2use=5:9)
summary(cc2)
confint(cc2)
plot(cc2)

wae2 <- filterD(dSC,waterbody %in% c("LAKE CHIPPEWA","NELSON LAKE"),
                species=="Walleye",!is.na(age),year==2014)
wae2 <- group_by(wae2,waterbody,age)
d3 <- summarize(wae2,Freq=n())
d3 <- mutate(d3,logfreq=log(Freq))
( d3 <- as.data.frame(d3) )

clr1 <- c("black","blue")
plot(logfreq~age,data=d3,col=clr1[waterbody],pch=19)
( d4 <- rbind(filterD(d3,waterbody=="LAKE CHIPPEWA",age>=3),
              filterD(d3,waterbody=="NELSON LAKE",age>=5)) )
cc2 <- lm(logfreq~age*waterbody,data=d4)
anova(cc2)

ccC <- catchCurve(Freq~age,data=filterD(d3,waterbody=="LAKE CHIPPEWA"),ages2use=3:9)
coef(ccC)
confint(ccC)
ccN <- catchCurve(Freq~age,data=filterD(d3,waterbody=="NELSON LAKE"),ages2use=5:9)
coef(ccN)
confint(ccN)

plot(logfreq~age,data=d3,col=clr1[waterbody],xlab="Age",ylab="Log Frequency")
points(logfreq~age,data=filterD(d3,waterbody=="LAKE CHIPPEWA",age>=3),pch=19,col=clr1[1])
points(logfreq~age,data=filterD(d3,waterbody=="NELSON LAKE",age>=5),pch=19,col=clr1[2])
abline(ccC$lm,col=clr1[1],lwd=2)
abline(ccN$lm,col=clr1[2],lwd=2)
legend("bottomleft",levels(d3$waterbody),col=clr1,pch=19,lwd=1,bty="n",cex=0.7)


# Script created at 2016-07-20 13:33:21
