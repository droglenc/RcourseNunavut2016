meta <- read.csv("NU_metadata.csv",stringsAsFactors=FALSE)
dNU <- read_excel("PG027.SA.Data.xlsx",na="nd",skip=1,
                  col_names=meta$new_names,col_types=meta$new_types)
dNU <- mutate(dNU,netset.time=format(netset.time,"%T"),netlift.time=format(netlift.time,"%T"),
              fyear=factor(year),loc=factor(loc),locAKA=factor(locAKA),water.type=factor(water.type),
              spec=factor(spec),sex=factor(sex),mat=factor(mat),life.hist=factor(life.hist))
dNU <- as.data.frame(dNU)
