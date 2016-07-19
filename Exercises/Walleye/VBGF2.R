# Nunavut 6-9-Aug-16

library(FSA)      # for filterD(), headtail(), col2rgbt(), vbFuns(), vbStart()
library(dplyr)    # for mutate(), select()
library(nlstools) # for nlsBoot()

dWE <- read.csv("WalleyeErie2.csv")
dWE_13 <- filterD(dWE,loc==1,year==2013)

xlbl <- "Age (yrs)"
ylbl <- "Total Length (in)"
clr1 <- c("black","blue")
clr2 <- col2rgbt(clr1,1/5)

plot(tl~age,data=dWE_13,pch=19,col=clr2[sex],xlab=xlbl,ylab=ylbl)

( svOm <- vbStarts(tl~age,data=dWE_13) )
( svLKt <- Map(rep,svOm,c(2,2,2)) )
vbLKt <- tl~Linf[sex]*(1-exp(-K[sex]*(age-t0[sex])))
fitLKt <- nls(vbLKt,data=dWE_13,start=svLKt)
residPlot(fitLKt)

bootLKt <- nlsBoot(fitLKt,niter=100)
cbind(Ests=coef(fitLKt),confint(fitLKt))

vbOm <- tl~Linf*(1-exp(-K*(age-t0)))
fitOm <- nls(vbOm,data=dWE_13,start=svOm)

vbLK <- tl~Linf[sex]*(1-exp(-K[sex]*(age-t0)))
svLK <- Map(rep,svOm,c(2,2,1))
fitLK <- nls(vbLK,data=dWE_13,start=svLK)
vbLt <- tl~Linf[sex]*(1-exp(-K*(age-t0[sex])))
svLt <- Map(rep,svOm,c(2,1,2))
fitLt <- nls(vbLt,data=dWE_13,start=svLt)
vbKt <- tl~Linf*(1-exp(-K[sex]*(age-t0[sex])))
svKt <- Map(rep,svOm,c(1,2,2))
fitKt <- nls(vbKt,data=dWE_13,start=svKt)

vbL <- tl~Linf[sex]*(1-exp(-K*(age-t0)))
svL <- Map(rep,svOm,c(2,1,1))
fitL <- nls(vbL,data=dWE_13,start=svL)
vbK <- tl~Linf*(1-exp(-K[sex]*(age-t0)))
svK <- Map(rep,svOm,c(1,2,1))
fitK <- nls(vbK,data=dWE_13,start=svK)
vbt <- tl~Linf*(1-exp(-K*(age-t0[sex])))
svt <- Map(rep,svOm,c(1,1,2))
fitt <- nls(vbt,data=dWE_13,start=svt)

extraSS(fitOm,com=fitLKt,sim.name="{Omega}",com.name="{Linf,K,t0}")
extraSS(fitLK,fitLt,fitKt,com=fitLKt,com.name="{Linf,K,t0}",
        sim.names=c("{Linf,K}","{Linf,t0}","{K,t0}"))
extraSS(fitL,fitt,com=fitLt,com.name="{Linf,t0}",
        sim.names=c("{Linf}","{t0}"))

lrt(fitOm,com=fitLKt,sim.name="{Omega}",com.name="{Linf,K,t0}")
lrt(fitLK,fitLt,fitKt,com=fitLKt,com.name="{Linf,K,t0}",
    sim.names=c("{Linf,K}","{Linf,t0}","{K,t0}"))
lrt(fitL,fitt,com=fitLt,com.name="{Linf,t0}",
    sim.names=c("{Linf}","{t0}"))

summary(fitLt)
cbind(Ests=coef(fitLt),confint(fitLt))

vb <- vbFuns("typical")

# females
fem <- filterD(dWE_13,sex=="female")
svf <- list(Linf=643,K=0.34,t0=-1.4)
fitf <- nls(tl~vb(age,Linf,K,t0),data=fem,start=svf)
# males
mal <- filterD(dWE_13,sex=="male")
svm <- list(Linf=567,K=0.34,t0=-1.9)
fitm <- nls(tl~vb(age,Linf,K,t0),data=mal,start=svm)

cbind(EstF=coef(fitf),confint(fitf),EstM=coef(fitm),confint(fitm))
cbind(Est=coef(fitLKt),confint(fitLKt))

offset <- 0.08
# females
plot(tl~I(age-offset),data=fem,pch=19,col=clr2[1],ylim=c(0,660),xlim=c(-2,10),xlab=xlbl,ylab=ylbl)
curve(vb(x-offset,coef(fitf)),from=-2,to=10,col=clr1[1],lwd=2,add=TRUE)
# males
points(tl~I(age+offset),data=mal,pch=19,col=clr2[2])
curve(vb(x+offset,coef(fitm)),from=-2,to=10,col=clr1[2],lwd=2,add=TRUE)
legend("bottomright",levels(dWE_13$sex),pch=19,col=clr1,bty="n",cex=0.8)

dWE_1013 <- filterD(dWE,loc==1,year %in% c(2010,2013))
dWE_1013 <- mutate(dWE_1013,fyear=factor(year))

vbOm <- tl~Linf*(1-exp(-K*(age-t0)))
svOm <- vbStarts(tl~age,data=dWE_1013)
fitOm <- nls(vbOm,data=dWE_1013,start=svOm)

svLKt <- Map(rep,svOm,c(2,2,2))
vbLKt <- tl~Linf[fyear]*(1-exp(-K[fyear]*(age-t0[fyear])))
fitLKt <- nls(vbLKt,data=dWE_1013,start=svLKt)

vbLK <- tl~Linf[fyear]*(1-exp(-K[fyear]*(age-t0)))
svLK <- Map(rep,svOm,c(2,2,1))
fitLK <- nls(vbLK,data=dWE_1013,start=svLK)
vbLt <- tl~Linf[fyear]*(1-exp(-K*(age-t0[fyear])))
svLt <- Map(rep,svOm,c(2,1,2))
fitLt <- nls(vbLt,data=dWE_1013,start=svLt)
vbKt <- tl~Linf*(1-exp(-K[fyear]*(age-t0[fyear])))
svKt <- Map(rep,svOm,c(1,2,2))
fitKt <- nls(vbKt,data=dWE_1013,start=svKt)

vbL <- tl~Linf[fyear]*(1-exp(-K*(age-t0)))
svL <- Map(rep,svOm,c(2,1,1))
fitL <- nls(vbL,data=dWE_1013,start=svL)
vbK <- tl~Linf*(1-exp(-K[fyear]*(age-t0)))
svK <- Map(rep,svOm,c(1,2,1))
fitK <- nls(vbK,data=dWE_1013,start=svK)
vbt <- tl~Linf*(1-exp(-K*(age-t0[fyear])))
svt <- Map(rep,svOm,c(1,1,2))
fitt <- nls(vbt,data=dWE_1013,start=svt)

extraSS(fitOm,com=fitLKt,sim.name="{Omega}",com.name="{Linf,K,t0}")
extraSS(fitLK,fitLt,fitKt,com=fitLKt,com.name="{Linf,K,t0}",
        sim.names=c("{Linf,K}","{Linf,t0}","{K,t0}"))
extraSS(fitK,fitt,com=fitLt,com.name="{K,t0}",
        sim.names=c("{K}","{t0}"))

# 2010
plot(tl~I(age-offset),data=filterD(dWE_1013,year==2010),pch=19,col=clr2[1],
     ylim=c(0,700),xlim=c(-2,16),xlab=xlbl,ylab=ylbl)
curve(vb(x-offset,coef(fitLKt)[c(1,3,5)]),from=-2,to=16,col=clr1[1],lwd=2,add=TRUE)
# males
points(tl~I(age+offset),data=filterD(dWE_1013,year==2013),pch=19,col=clr2[2])
curve(vb(x+offset,coef(fitLKt)[c(2,4,6)]),from=-2,to=16,col=clr1[2],lwd=2,add=TRUE)
legend("bottomright",levels(dWE_1013$fyear),pch=19,col=clr1,bty="n",cex=0.8)


# Script created at 2016-07-19 12:34:25
