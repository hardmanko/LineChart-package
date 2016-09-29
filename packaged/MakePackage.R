library(devtools)
library(roxygen2)

install.packages("R.rsp")

setwd("~/R/LineChart-package/")

devtools::document()

devtools::build(path="packaged")

devtools::check()

#Testing
install.packages("packaged/LineChart_0.2.tar.gz", repos=NULL)



library(LineChart)

settings = buildGroupSettings(0, symbol=16, col="blue")

data = ChickWeight

data$Diet = LETTERS[data$Diet]
lineChart(weight ~ Diet, data, settings=settings, legendPosition = "bottomright")



lineChart(weight ~ Time * Diet, ChickWeight)




