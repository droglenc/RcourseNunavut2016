# Nunavut 6-9-Aug-16

library(FSA)      # for filterD(), headtail(), col2rgbt(), vbFuns(), vbStart()
library(dplyr)    # for mutate(), select()
library(nlstools) # for nlsBoot()
library(readxl)

source("PG008_readdata.R")
dNU_FW <- filterD(dNU,!is.na(FL),!is.na(age),water.type %in% c("freshwater","Freshwater"),year==2007)
# Removed some obvious outliers
dNU_FW <- filterD(dNU_FW,!(age>10 & FL<200))

xlbl <- "Age (yrs)"
ylbl <- "Fork Length (in)"
clr1 <- c("black","blue")
clr2 <- col2rgbt(clr1,1/5)

plot(FL~age,data=dNU_FW,pch=19,col=clr2[sex],xlab=xlbl,ylab=ylbl)

( svOm <- vbStarts(FL~age,data=dNU_FW,plot=TRUE) )
( svLKt <- Map(rep,svOm,c(2,2,2)) )
vbLKt <- FL~Linf[sex]*(1-exp(-K[sex]*(age-t0[sex])))
fitLKt <- nls(vbLKt,data=dNU_FW,start=svLKt)
residPlot(fitLKt)

bootLKt <- nlsBoot(fitLKt,niter=100)
cbind(Ests=coef(fitLKt),confint(fitLKt))

vbOm <- FL~Linf*(1-exp(-K*(age-t0)))
fitOm <- nls(vbOm,data=dNU_FW,start=svOm)

vbLK <- FL~Linf[sex]*(1-exp(-K[sex]*(age-t0)))
svLK <- Map(rep,svOm,c(2,2,1))
fitLK <- nls(vbLK,data=dNU_FW,start=svLK)
vbLt <- FL~Linf[sex]*(1-exp(-K*(age-t0[sex])))
svLt <- Map(rep,svOm,c(2,1,2))
fitLt <- nls(vbLt,data=dNU_FW,start=svLt)
vbKt <- FL~Linf*(1-exp(-K[sex]*(age-t0[sex])))
svKt <- Map(rep,svOm,c(1,2,2))
fitKt <- nls(vbKt,data=dNU_FW,start=svKt)

vbL <- FL~Linf[sex]*(1-exp(-K*(age-t0)))
svL <- Map(rep,svOm,c(2,1,1))
fitL <- nls(vbL,data=dNU_FW,start=svL)
vbK <- FL~Linf*(1-exp(-K[sex]*(age-t0)))
svK <- Map(rep,svOm,c(1,2,1))
fitK <- nls(vbK,data=dNU_FW,start=svK)
vbt <- FL~Linf*(1-exp(-K*(age-t0[sex])))
svt <- Map(rep,svOm,c(1,1,2))
fitt <- nls(vbt,data=dNU_FW,start=svt)

extraSS(fitOm,com=fitLKt,sim.name="{Omega}",com.name="{Linf,K,t0}")
extraSS(fitLK,fitLt,fitKt,com=fitLKt,com.name="{Linf,K,t0}",
        sim.names=c("{Linf,K}","{Linf,t0}","{K,t0}"))
extraSS(fitL,fitK,com=fitLK,com.name="{Linf,K}",
        sim.names=c("{Linf}","{K}"))

lrt(fitOm,com=fitLKt,sim.name="{Omega}",com.name="{Linf,K,t0}")
lrt(fitLK,fitLt,fitKt,com=fitLKt,com.name="{Linf,K,t0}",
    sim.names=c("{Linf,K}","{Linf,t0}","{K,t0}"))
lrt(fitL,fitK,com=fitLK,com.name="{Linf,K}",
    sim.names=c("{Linf}","{K}"))

summary(fitLK)
cbind(Ests=coef(fitLK),confint(fitLK))

vb <- vbFuns("typical")

# females
fem <- filterD(dNU_FW,sex=="F")
svf <- list(Linf=733,K=0.23,t0=5.6)
fitf <- nls(FL~vb(age,Linf,K,t0),data=fem,start=svf)
# males
mal <- filterD(dNU_FW,sex=="M")
svm <- list(Linf=920,K=0.15,t0=5.6)
fitm <- nls(FL~vb(age,Linf,K,t0),data=mal,start=svm)

cbind(EstF=coef(fitf),confint(fitf),EstM=coef(fitm),confint(fitm))
cbind(Est=coef(fitLKt),confint(fitLKt))

offset <- 0.08
# females
plot(FL~I(age-offset),data=fem,pch=19,col=clr2[1],ylim=c(0,1000),xlim=c(3,30),xlab=xlbl,ylab=ylbl)
curve(vb(x-offset,coef(fitf)),from=5,to=30,col=clr1[1],lwd=2,add=TRUE)
# males
points(FL~I(age+offset),data=mal,pch=19,col=clr2[2])
curve(vb(x+offset,coef(fitm)),from=5,to=26,col=clr1[2],lwd=2,add=TRUE)
legend("bottomright",levels(dNU_FW$sex),pch=19,col=clr1,bty="n",cex=0.8)


# Script created at 2016-07-18 21:54:17
