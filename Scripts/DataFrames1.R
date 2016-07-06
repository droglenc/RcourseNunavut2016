# Nunavut 6-9-Aug-16

library(FSA)    # for headtail()
library(readxl) # for read_excel()
library(dplyr)  # for mutate()

# Set your working directory to where your external data files (and scripts) are located.
setwd("C:/aaaWork/Web/GitHub/RcourseNunavut2016/Handouts")

dSC <- read.csv("SawyerCo_reduced.csv")
str(dSC)
headtail(dSC)
dSC$len
dSC$len[1]
dSC$len[c(1,3,5)]

################################################################################
# This is used to demonstrate problems with loading the Nunavut data as given
# to Derek Ogle.  See the next chunk for a better method.
tmp <- read_excel("PG027.SA.Data.xlsx")
str(tmp,list.len=10)          # list.len only used to save space
tmp$Index[1:10]               # positions used simply to limit output length
tmp$Location - Name[1:10]
tmp$'Location - Name'[1:10]
################################################################################

# Get new names and defined data types
( meta <- read.csv("NU_metadata.csv",stringsAsFactors=FALSE) )
# Now read the data
dNU <- read_excel("PG027.SA.Data.xlsx",na="nd",skip=1,
                  col_names=meta$new_names,col_types=meta$new_types)
str(dNU)

# Adjust types of some variables
dNU <- mutate(dNU,netset.time=format(netset.time,"%T"),netlift.time=format(netlift.time,"%T"),
              fyear=factor(year),loc=factor(loc),locAKA=factor(locAKA),water.type=factor(water.type),
              spec=factor(spec),sex=factor(sex),mat=factor(mat),life.hist=factor(life.hist))
dNU <- as.data.frame(dNU)
str(dNU)

dNU$FL


# Script created at 2016-07-06 08:24:19
