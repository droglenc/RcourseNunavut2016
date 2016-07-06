# Nunavut 6-9-Aug-16

library(FSA)  # for mrClosed()

3+4*2
res <- 3+4*2
res
( res <- (2+3)*(7+2) )
res*2

sqrt(17)
( res <- sqrt(17) )
dat <- c(3,6,8,3,5,6,2,7,6,8,2,10)
mean(dat)
mean(dat,trim=0.1)

mr1 <- mrClosed(M=346,n=184,m=49,method="Chapman")
mr1
summary(mr1)
summary(mr1,verbose=TRUE)
confint(mr1,verbose=TRUE)

( lake <- c("Deep","Long","Star","Twin") )
( numSpec <- c(4,8,7,3) )
( maxDepth <- c(6.5,7.8,3.8,25.6) )
( springFed <- c(TRUE,FALSE,FALSE,TRUE) )

lake[1]
lake[2]
lake[-1]
lake[2,3,4]
lake[c(2,3,4)]

lake[c(TRUE,FALSE,FALSE,TRUE)]
lake=="Star"
maxDepth[lake=="Star"]
numSpec[maxDepth<7]


# Script created at 2016-07-06 08:18:08
