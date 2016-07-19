# Nunavut 6-9-Aug-16

library(FSA)      # for filterD(), headtail(), col2rgbt(), vbFuns(), vbStart()
library(dplyr)    # for mutate(), select()
library(nlstools) # for nlsBoot()
library(AICcmodavg)

dWE <- read.csv("WalleyeErie2.csv")
dWE_13 <- filterD(dWE,loc==1,year==2013)

xlbl <- "Age (yrs)"
ylbl <- "Total Length (in)"
clr1 <- "black"
clr2 <- col2rgbt(clr1,1/9)

plot(tl~age,data=dWE_13,pch=19,col=clr2,xlab=xlbl,ylab=ylbl)

vb <- vbFuns("Typical")
svb <- vbStarts(tl~age,data=dWE_13,type="Typical",plot=TRUE)
fit1 <- nls(tl~vb(age,Linf,K,t0),data=dWE_13,start=svb)
cf <- coef(fit1)
plot(tl~age,data=dWE_13,xlab=xlbl,ylab=ylbl,pch=19,col=clr2)
curve(vb(x,cf),from=1,to=14,n=500,lwd=2,col=clr1,add=TRUE)

residPlot(fit1)

summary(fit1,correlation=TRUE)

cf

confint(fit1)
boot1 <- nlsBoot(fit1,niter=1000)
confint(boot1)

ageX <- 3
predict(fit1,data.frame(age=ageX))
pv <- apply(boot1$coefboot,MARGIN=1,FUN=vb,t=ageX)
quantile(pv,c(0.025,0.975))

vbgq <- vbFuns("GallucciQuinn")
svbgq <- vbStarts(tl~age,data=dWE_13,type="GallucciQuinn")
fitgq <- nls(tl~vbgq(age,omega,K,t0),data=dWE_13,start=svbgq)
cfgq <- coef(fitgq)
plot(tl~age,data=dWE_13,xlab=xlbl,ylab=ylbl,pch=19,col=clr2)
curve(vb(x,cf),from=1,to=14,n=500,lwd=5,col=clr1,add=TRUE)
curve(vbgq(x,cfgq),from=1,to=14,n=500,lwd=2,col="red",add=TRUE)
cf
cfgq

lgf <- logisticFuns()
plot(tl~age,data=dWE_13,xlab=xlbl,ylab=ylbl,pch=19,col=clr2)
curve(lgf(x,Linf=625,gninf=0.4,ti=0),from=1,to=14,n=500,lwd=2,col=clr1,add=TRUE)
svlgf <- list(Linf=625,gninf=0.4,ti=0)
fitlgf <- nls(tl~lgf(age,Linf,gninf,ti),data=dWE_13,start=svlgf)
clgf <- coef(fitlgf)

ggf <- GompertzFuns()
plot(tl~age,data=dWE_13,xlab=xlbl,ylab=ylbl,pch=19,col=clr2)
curve(ggf(x,Linf=625,gi=0.4,ti=0),from=1,to=14,n=500,lwd=2,col=clr1,add=TRUE)
svggf <- list(Linf=625,gi=0.4,ti=0)
fitggf <- nls(tl~ggf(age,Linf,gi,ti),data=dWE_13,start=svggf)
cggf <- coef(fitggf)

plot(tl~age,data=dWE_13,xlab=xlbl,ylab=ylbl,pch=19,col=clr2)
curve(vb(x,cf),from=1,to=14,n=500,lwd=6,col=clr1,add=TRUE)
curve(lgf(x,clgf),from=1,to=14,n=500,lwd=4,col="blue",add=TRUE)
curve(ggf(x,cggf),from=1,to=14,n=500,lwd=2,col="red",add=TRUE)
cf
clgf
cggf
aictab(list(fit1,fitlgf,fitggf),modnames=c("von Bertalanffy","logistic","Gompertz"))


# Script created at 2016-07-19 09:57:05
