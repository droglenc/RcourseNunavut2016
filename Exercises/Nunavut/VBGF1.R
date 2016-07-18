# Nunavut 6-9-Aug-16

library(FSA)      # for filterD(), headtail(), col2rgbt(), vbFuns(), vbStart()
library(dplyr)    # for mutate(), select()
library(nlstools) # for nlsBoot()
library(readxl)

source("PG008_readdata.R")
dNU_FW <- filterD(dNU,!is.na(wt),!is.na(age),water.type %in% c("freshwater","Freshwater"))
# Removed some obvious outliers
dNU_FW <- filterD(dNU_FW,!(age>10 & FL<200))

xlbl <- "Age (yrs)"
ylbl <- "Fork Length (mm)"
clr1 <- "black"
clr2 <- col2rgbt(clr1,1/9)

plot(FL~age,data=dNU_FW,pch=19,col=clr2,xlab=xlbl,ylab=ylbl)

vb <- vbFuns("Typical")
svb <- vbStarts(FL~age,data=dNU_FW,type="Typical",plot=TRUE)
fit1 <- nls(FL~vb(age,Linf,K,t0),data=dNU_FW,start=svb)
cf <- coef(fit1)
plot(FL~age,data=dNU_FW,xlab=xlbl,ylab=ylbl,pch=19,col=clr2)
curve(vb(x,cf),from=6,to=35,n=500,lwd=2,col=clr1,add=TRUE)

residPlot(fit1)

summary(fit1,correlation=TRUE)

boot1 <- nlsBoot(fit1,niter=1000)
cbind(Ests=cf,confint(boot1))

ageX <- 18
predict(fit1,data.frame(age=ageX))
pv <- apply(boot1$coefboot,MARGIN=1,FUN=vb,t=ageX)
quantile(pv,c(0.025,0.975))

vbgq <- vbFuns("GallucciQuinn")
svbgq <- vbStarts(FL~age,data=dNU_FW,type="GallucciQuinn")
fitgq <- nls(FL~vbgq(age,omega,K,t0),data=dNU_FW,start=svbgq)
cfgq <- coef(fitgq)
plot(FL~age,data=dNU_FW,xlab=xlbl,ylab=ylbl,pch=19,col=clr2)
curve(vb(x,cf),from=6,to=35,n=500,lwd=5,col=clr1,add=TRUE)
curve(vbgq(x,cfgq),from=6,to=35,n=500,lwd=2,col="red",add=TRUE)
cf
cfgq


# Script created at 2016-07-18 15:13:09
