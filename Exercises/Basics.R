# Nunavut 6-9-Aug-16

library(FSA)

87/3

( res <- 87/1400/2*1000 )

res/1000*300

ct <- c(87,54,12,98,45,5,78) 

ft <- c(3,3,2,5,2,2,4)

dow <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")

( cpe <- ct/ft )

mean(cpe)

cpe[3]

cpe[c(3,5)]

cpe[-7]

cpe[ft==2]

cpe.gt3 <- cpe[ft>=3]
mean(cpe.gt3)

cpe.wd <- cpe[!dow %in% c("Sat","Sun")]
mean(cpe.wd)
sd(cpe.wd)


# Script created at 2016-07-23 21:07:31
