library(devtools)
library(roxygen2)

install.packages("R.rsp")



setwd("~/../Programming/R/LineChart/")

devtools::use_package("BayesFactor", type="Suggests")

devtools::document()

devtools::install()



devtools::check()

devtools::build(path="packaged")

install.packages("packaged/LineChart_0.3.tar.gz", repos=NULL)



library(LineChart)

?lineChart

settings = buildGroupSettings(0, symbol=16, col="blue")
data = ChickWeight



data$Diet = LETTERS[data$Diet]
lineChart(weight ~ Diet, data, settings=settings)


plotDf = createPlottingDf(weight ~ Time * Diet, data)
lineChartDf(plotDf)

lineChart(weight ~ Time * Diet, data)




quartiles = function(x) {
  qs = as.numeric(quantile(x, c(0.25, 0.75)))
  list(eb=qs, includesCenter=TRUE)
}


lineChart(weight ~ Time * Diet, data, errBarType = quartiles, centralTendencyType = "median")
