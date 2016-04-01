library(devtools)
library(roxygen2)

#install.packages("R.rsp")

setwd("~/R/LineChart-package/")

devtools::build(path="packaged")

install.packages("packaged/LineChart_0.1.tar.gz", repos=NULL)



library(LineChart)

data = ChickWeight
data$Diet = LETTERS[data$Diet]
lineChart(weight ~ Diet, data)

plotDf = createPlottingDf(weight ~ Diet, data)
plotDf$x

