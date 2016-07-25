# Nunavut 6-9-Aug-16

library(FSA)
library(dplyr)
library(nlstools)
library(readxl)
library(AICcmodavg)

source("PG008_readdata.R")
dNU_FW <- filterD(dNU,!is.na(fl),!is.na(age),water.type %in% c("freshwater","Freshwater"),year==2010)
# Removed some obvious outliers
dNU_FW <- filterD(dNU_FW,!(age>10 & fl<200))

xlbl <- "Age (yrs)"
ylbl <- "Fork Length (mm)"
clr1 <- "black"
clr2 <- col2rgbt(clr1,1/9)

plot(fl~age,data=dNU_FW,pch=19,col=clr2,xlab=xlbl,ylab=ylbl)

vb <- vbFuns("Typical")
svb <- vbStarts(fl~age,data=dNU_FW,type="Typical",plot=TRUE)
fit1 <- nls(fl~vb(age,Linf,K,t0),data=dNU_FW,start=svb)
cf <- coef(fit1)
plot(fl~age,data=dNU_FW,xlab=xlbl,ylab=ylbl,pch=19,col=clr2)
curve(vb(x,cf),from=6,to=29,n=500,lwd=2,col=clr1,add=TRUE)

residPlot(fit1)

summary(fit1,correlation=TRUE)

cf

confint(fit1)
boot1 <- nlsBoot(fit1,niter=1000)
confint(boot1)

ageX <- 18
predict(fit1,data.frame(age=ageX))
pv <- apply(boot1$coefboot,MARGIN=1,FUN=vb,t=ageX)
quantile(pv,c(0.025,0.975))

vbgq <- vbFuns("GallucciQuinn")
svbgq <- vbStarts(fl~age,data=dNU_FW,type="GallucciQuinn")
fitgq <- nls(fl~vbgq(age,omega,K,t0),data=dNU_FW,start=svbgq)
cfgq <- coef(fitgq)
plot(fl~age,data=dNU_FW,xlab=xlbl,ylab=ylbl,pch=19,col=clr2)
curve(vb(x,cf),from=6,to=35,n=500,lwd=5,col=clr1,add=TRUE)
curve(vbgq(x,cfgq),from=6,to=35,n=500,lwd=2,col="red",add=TRUE)
cf
cfgq

lgf <- logisticFuns()
plot(fl~age,data=dNU_FW,xlab=xlbl,ylab=ylbl,pch=19,col=clr2)
curve(lgf(x,Linf=850,gninf=0.2,ti=10),from=5,to=29,n=500,lwd=2,col=clr1,add=TRUE)
svlgf <- list(Linf=850,gninf=0.2,ti=10)
fitlgf <- nls(fl~lgf(age,Linf,gninf,ti),data=dNU_FW,start=svlgf)
clgf <- coef(fitlgf)

ggf <- GompertzFuns()
plot(fl~age,data=dNU_FW,xlab=xlbl,ylab=ylbl,pch=19,col=clr2)
curve(ggf(x,Linf=850,gi=0.2,ti=8),from=5,to=29,n=500,lwd=2,col=clr1,add=TRUE)
svggf <- list(Linf=850,gi=0.2,ti=8)
fitggf <- nls(fl~ggf(age,Linf,gi,ti),data=dNU_FW,start=svggf)
cggf <- coef(fitggf)

plot(fl~age,data=dNU_FW,xlab=xlbl,ylab=ylbl,pch=19,col=clr2)
curve(vb(x,cf),from=5,to=29,n=500,lwd=6,col=clr1,add=TRUE)
curve(lgf(x,clgf),from=5,to=29,n=500,lwd=4,col="blue",add=TRUE)
curve(ggf(x,cggf),from=5,to=29,n=500,lwd=2,col="red",add=TRUE)
cf
clgf
cggf
aictab(list(fit1,fitlgf,fitggf),modnames=c("von Bertalanffy","logistic","Gompertz"))


# Script created at 2016-07-24 19:49:46
