# Nunavut 6-9-Aug-16

library(FSA)        # for filterD(), headtail(), col2rgbt(), fact2num(), vbFuns(), vbStart(), confint()
library(dplyr)      # for mutate()
library(nlstools)   # for nlsBoot()
library(AICcmodavg) # for aictab()

# Set your working directory to where your external data files (and scripts) are located.
setwd("C:/aaaWork/Web/GitHub/RcourseNunavut2016/Handouts")
dSC <- read.csv("SawyerCo_reduced.csv")
wae <- filterD(dSC,waterbody %in% c("LAKE CHIPPEWA","SAND LAKE"), 
               species=="Walleye",!is.na(len),!is.na(age))

xlbl <- "Age (yrs)"
ylbl <- "Total Length (in)"
clr1 <- c("black","blue")
clr2 <- col2rgbt(clr1,1/5)

( sum <- Summarize(len~age+waterbody,data=wae,digits=1) )

plot(len~age,data=wae,pch=19,col=clr2[waterbody],xlab=xlbl,ylab=ylbl)
lines(mean~fact2num(age),data=filterD(sum,waterbody=="LAKE CHIPPEWA"),col=clr1[1],lwd=2)
lines(mean~fact2num(age),data=filterD(sum,waterbody=="SAND LAKE"),col=clr1[2],lwd=2)

( svOm <- vbStarts(len~age,data=wae,plot=TRUE) )
( svLKt <- Map(rep,svOm,c(2,2,2)) )

vbLKt <- len~Linf[waterbody]*(1-exp(-K[waterbody]*(age-t0[waterbody])))
fitLKt <- nls(vbLKt,data=wae,start=svLKt)
residPlot(fitLKt)

vbOm <- len~Linf*(1-exp(-K*(age-t0)))
fitOm <- nls(vbOm,data=wae,start=svOm)

extraSS(fitOm,com=fitLKt,sim.name="{Omega}",com.name="{Linf,K,t0}")
lrt(fitOm,com=fitLKt,sim.name="{Omega}",com.name="{Linf,K,t0}")

vbLK <- len~Linf[waterbody]*(1-exp(-K[waterbody]*(age-t0)))
( svLK <- Map(rep,svOm,c(2,2,1)) )
fitLK <- nls(vbLK,data=wae,start=svLK)
vbLt <- len~Linf[waterbody]*(1-exp(-K*(age-t0[waterbody])))
svLt <- Map(rep,svOm,c(2,1,2))
fitLt <- nls(vbLt,data=wae,start=svLt)
vbKt <- len~Linf*(1-exp(-K[waterbody]*(age-t0[waterbody])))
svKt <- Map(rep,svOm,c(1,2,2))
fitKt <- nls(vbKt,data=wae,start=svKt)
extraSS(fitLK,fitLt,fitKt,com=fitLKt,com.name="{Linf,K,t0}",
        sim.names=c("{Linf,K}","{Linf,t0}","{K,t0}"))

vbL <- len~Linf[waterbody]*(1-exp(-K*(age-t0)))
( svL <- Map(rep,svOm,c(2,1,1)) )
fitL <- nls(vbL,data=wae,start=svL)
vbK <- len~Linf*(1-exp(-K[waterbody]*(age-t0)))
svK <- Map(rep,svOm,c(1,2,1))
fitK <- nls(vbK,data=wae,start=svK)
fitK <- nls(vbK,data=wae,start=svK,control=list(minFactor=1e-15,maxiter=500))

extraSS(fitL,fitK,com=fitLK,com.name="{Linf,K}",sim.names=c("{Linf}","{K}"))

summary(fitK,correlation=TRUE)
round(cbind(coef(fitK),confint(fitK)),3)

vb <- vbFuns("typical")
# Lake Chippewa
crLC <- filterD(wae,waterbody=="LAKE CHIPPEWA")
svLC <- list(Linf=492,K=0.37,t0=0)
fitLC <- nls(len~vb(age,Linf,K,t0),data=crLC,start=svLC)
# Sand Lake
crSL <- filterD(wae,waterbody=="SAND LAKE")
svSL <- list(Linf=492,K=0.26,t0=0)
fitSL <- nls(len~vb(age,Linf,K,t0),data=crSL,start=svSL)

offset <- 0.08
# Lake Chippewa
plot(len~I(age-offset),data=crLC,pch=19,col=clr2[1],ylim=c(200,550),xlab=xlbl,ylab=ylbl)
curve(vb(x-offset,coef(fitLC)),from=2,to=10,col=clr1[1],lwd=2,add=TRUE)
# Sand Lake
points(len~I(age+offset),data=crSL,pch=19,col=clr2[2])
curve(vb(x+offset,coef(fitSL)),from=4,to=9,col=clr1[2],lwd=2,add=TRUE)
legend("bottomright",levels(wae$waterbody),pch=19,col=clr1,bty="n",cex=0.8)

vbt <- len~Linf*(1-exp(-K*(age-t0[waterbody])))
svt <- Map(rep,svOm,c(1,1,2))
fitt <- nls(vbt,data=wae,start=svt)

ms <- list(fitOm,fitL,fitK,fitt,fitLK,fitLt,fitKt,fitLKt)
mnames <- c("{Omega}","{Linf}","{K}","{t0}","{Linf,K}","{Linf,t0}","{K,t0}","{Linf,K,t0}")
aictab(ms,mnames)


# Script created at 2016-07-24 20:41:20
