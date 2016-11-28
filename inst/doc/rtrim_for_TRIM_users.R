## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(
  fig.width  = 7,
  fig.height = 5
)

## ------------------------------------------------------------------------
library(rtrim)
data(skylark)
# inspect the dataset
head(skylark,3)

## ------------------------------------------------------------------------
m1 <- trim(count ~ site + time, data=skylark,model=2)

## ------------------------------------------------------------------------
m1 <- trim(skylark, count.id="count", site.id="site", time.id="time", model=2)

## ------------------------------------------------------------------------
# summarize the model
summary(m1)

## ------------------------------------------------------------------------
totals(m1)

## ------------------------------------------------------------------------
gof(m1)

## ------------------------------------------------------------------------
coefficients(m1)

## ------------------------------------------------------------------------
plot(overall(m1))

## ------------------------------------------------------------------------
m2 <- trim(count ~ site + time + Habitat, data=skylark, model=2)

## ------------------------------------------------------------------------
m3 <- trim(count ~ site + time + Habitat, data=skylark, model=2
     , overdisp = TRUE, serialcor = TRUE, changepoints=1:7, autodelete=TRUE)
m3$changepoints

## ------------------------------------------------------------------------
m4 <- trim(count ~ site + time + Habitat, data=skylark, model=2
     , overdisp = TRUE, serialcor = TRUE, changepoints=1:7, stepwise = TRUE)
m4$changepoints

## ------------------------------------------------------------------------
data(skylark)
count_summary(skylark)


## ------------------------------------------------------------------------
check_observations(skylark, model=2, changepoints=c(1,4))

