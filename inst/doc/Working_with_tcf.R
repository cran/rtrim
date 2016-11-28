## ------------------------------------------------------------------------
library(rtrim)
tmp <- "FILE skylark.dat
TITLE  skylark-1d
NTIMES 8
NCOVARS 2
LABELS
Habitat
Cov2
END
MISSING 999
WEIGHT Absent
COMMENT Example 1; using linear trend model
WEIGHTING off
OVERDISP on
SERIALCOR on
MODEL 2
"
write(tmp,file="skylark.tcf")
data(skylark)
skylark[is.na(skylark)] <- 999
write.table(skylark,file="skylark.dat",col.names=FALSE,row.names=FALSE)

## ------------------------------------------------------------------------
tc <- read_tcf("skylark.tcf")
m <- trim(tc)

## ------------------------------------------------------------------------
wald(m)

## ------------------------------------------------------------------------
tc

