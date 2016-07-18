# Nunavut 6-9-Aug-16

library(FSA)      # for filterD(), headtail(), col2rgbt(), vbFuns(), vbStart()
library(dplyr)    # for mutate(), select()
library(nlstools) # for nlsBoot()

# Set your working directory to where your external data files (and scripts) are located.
setwd("C:/aaaWork/Web/GitHub/RcourseNunavut2016/Handouts")
dSC <- read.csv("SawyerCo_reduced.csv")
wae <- filterD(dSC,waterbody=="NELSON LAKE",species=="Walleye",!is.na(len),!is.na(age))

xlbl <- "Age (yrs)"
ylbl <- "Total Length (in)"
clr1 <- "black"
clr2 <- col2rgbt(clr1,1/5)

plot(len~age,data=wae,pch=19,col=clr2,xlab=xlbl,ylab=ylbl)
Summarize(len~age,data=wae,digits=1)

vb <- vbFuns("Typical",msg=TRUE)
vb

# Demos manual generation with plot ... LEFT plot
svb <- vbStarts(len~age,data=wae,type="Typical",plot=TRUE,fixed=list(Linf=525,K=0.3,t0=0))
# Demos automatic generation ... RIGHT plot
svb <- vbStarts(len~age,data=wae,type="Typical",plot=TRUE)

fit1 <- nls(len~vb(age,Linf,K,t0),data=wae,start=svb)
residPlot(fit1)

summary(fit1,correlation=TRUE)
( cf <- coef(fit1) )
confint(fit1)

boot1 <- nlsBoot(fit1,niter=1000)
str(boot1)
headtail(boot1$coefboot)
confint(boot1,plot=TRUE,rows=1,cols=3)

ageX <- 5
predict(fit1,data.frame(age=ageX))
pv <- apply(boot1$coefboot,MARGIN=1,FUN=vb,t=ageX)
quantile(pv,c(0.025,0.975))

plot(len~age,data=wae,xlab=xlbl,ylab=ylbl,pch=19,col=clr2)
curve(vb(x,cf),from=2,to=9,n=500,lwd=2,col=clr1,add=TRUE)

gomp <- GompertzFuns(msg=TRUE)
plot(len~age,data=wae,pch=19,col=clr2,xlab=xlbl,ylab=ylbl)
curve(gomp(x,Linf=520,gi=0.3,ti=3),from=2,to=9,n=500,lwd=2,add=TRUE,col="red")
curve(gomp(x,Linf=520,gi=0.6,ti=2),from=2,to=9,n=500,lwd=2,add=TRUE,col="blue")
curve(gomp(x,Linf=520,gi=0.7,ti=2),from=2,to=9,n=500,lwd=2,add=TRUE,col=clr1)

fit2 <- nls(len~gomp(age,Linf,gi,ti),data=wae,start=list(Linf=520,gi=0.7,ti=2))
AIC(fit1,fit2)

plot(len~age,data=wae,xlab=xlbl,ylab=ylbl,pch=19,col=clr2,xlim=c(0,10),ylim=c(0,550))
curve(vb(x,cf),from=0,to=10,n=500,lwd=4,col=clr1,add=TRUE)
curve(gomp(x,coef(fit2)),from=0,to=10,n=500,lwd=2,col="red",add=TRUE)
legend("bottomright",c("von Bertalanffy","Gompertz"),col=c("black","red"),lwd=2,bty="n",cex=0.8)


# Script created at 2016-07-18 12:38:57
