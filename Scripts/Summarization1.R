# Nunavut 6-9-Aug-16

library(FSA)     # for filterD(), rcumsum(), hist(), Summarize()
library(dplyr)   # for mutate()

# Set your working directory to where your external data files (and scripts) are located.
setwd("C:/aaaWork/Web/GitHub/RcourseNunavut2016/Handouts")
dSC <- read.csv("SawyerCo_reduced.csv")
names(dSC)
levels(dSC$sex)
dSC <- mutate(dSC,sex=mapvalues(sex,from="",to="ND"),fyear=factor(year),lcat25=lencat(len,w=25))
levels(dSC$waterbody)
levels(dSC$species)
LChip_WAE <- filterD(dSC,waterbody=="LAKE CHIPPEWA",species=="Walleye")
LChip_WAE11 <- filterD(LChip_WAE,year==2011)

( t_sex <- xtabs(~sex,data=LChip_WAE11) )
( t_sex1 <- t_sex[-1] )
( tp_sex1 <- prop.table(t_sex1)*100 )

barplot(t_sex1)  # Left
barplot(tp_sex1,xlab="Sex",ylab="Percent of Sample",ylim=c(0,60),col="black")  # Right

( t_len25 <- xtabs(~lcat25,data=LChip_WAE11) )
tp_len25 <- prop.table(t_len25)*100
round(tp_len25,1)
round(cumsum(tp_len25),1)
round(rcumsum(tp_len25),1)

hist(~len,data=LChip_WAE11,xlab="Total Length (mm)",ylim=c(0,1000),w=10)  # Left
hist(~len,data=LChip_WAE11,xlab="Total Length (mm)",ylim=c(0,2500),breaks=seq(100,800,25))  # Right

Summarize(~len,data=LChip_WAE11,digits=1)

( t_seas <- xtabs(~mon+fyear,data=LChip_WAE) )
round(prop.table(t_seas)*100,1)
round(prop.table(t_seas,margin=2)*100,1)
round(prop.table(t_seas,margin=1)*100,1)

Sturg <- filterD(dSC,species=="Lake Sturgeon",waterbody %in% c("CHIPPEWA RIVER","HUNTER LAKE"))

plot(weight~len,data=Sturg) # Left
plot(weight~len,data=Sturg,pch=19,col=col2rgbt("black",1/3),
     ylab="Weight (g)",xlab="Total Length (mm)")  # Right

with(Sturg,cor(weight,len))
with(Sturg,cor(weight,len,use="pairwise.complete.obs"))
with(Sturg,cor(weight,len,use="pairwise.complete.obs",method="spearman"))


# Script created at 2016-07-23 21:37:24
