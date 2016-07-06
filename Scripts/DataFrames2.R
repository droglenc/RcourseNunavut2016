# Nunavut 6-9-Aug-16

library(FSA)    # for headtail(), filterD()
library(dplyr)  # for select(), mutate(), filter()

# Set your working directory to where your external data files (and scripts) are located.
setwd("C:/aaaWork/Web/GitHub/RcourseNunavut2016/Handouts")
dSC <- read.csv("SawyerCo_reduced.csv")
str(dSC)
headtail(dSC,n=6)

dSC <- select(dSC,-age_strux,-lennote)
headtail(dSC)

dSC <- mutate(dSC,len.in=len/25.4)
headtail(dSC)

dSC <- mutate(dSC,lcat10=lencat(len,w=10))
headtail(dSC)

dSC <- mutate(dSC,newspec=mapvalues(species,from=c("Walleye","Muskellunge"),to=c("WAE","MUE")))
headtail(dSC)

dSC <- select(dSC,-(len.in:newspec))
headtail(dSC,n=2)

levels(dSC$waterbody)
dSC_chip <- filter(dSC,waterbody=="CHIPPEWA RIVER")
levels(dSC_chip$waterbody)
dSC_chip <- filterD(dSC,waterbody=="CHIPPEWA RIVER")
levels(dSC_chip$waterbody)
dSC_rivers <- filterD(dSC,waterbody %in% c("CHIPPEWA RIVER","NAMEKAGON RIVER"))
levels(dSC_rivers$waterbody)
levels(dSC_rivers$gear)
dSC_rivers <- filterD(dSC_rivers,gear!="BOTTOM GILL NET")
levels(dSC_rivers$gear)
levels(dSC_rivers$species)
dSC_rivers_trout <- filterD(dSC_rivers,species %in% c("Brook Trout","Brown Trot"))
levels(dSC_rivers_trout$species)
dSC_rivers_trout <- filterD(dSC_rivers,species %in% c("Brook Trout","Brown Trout"))
levels(dSC_rivers_trout$species)
tmp <- filterD(dSC_rivers_trout,len>=100 & len<200)
headtail(tmp)
tmp <- filterD(dSC_rivers_trout,len<100 | len>200)
headtail(tmp)
( bigBrowns <- filterD(dSC_rivers_trout,species=="Brown Trout",mon %in% c("Jul","Aug"),len>=500) )
( bigBrowns_nowt <- filterD(bigBrowns,is.na(weight)) )
( bigBrowns_wghd <- filterD(bigBrowns,!is.na(weight)) )

which(bigBrowns$len==536)
bigBrowns[4,]
( bigBrowns2 <- bigBrowns[-4,] )


# Script created at 2016-07-06 09:41:16
