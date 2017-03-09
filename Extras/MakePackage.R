library(devtools)
library(roxygen2)

install.packages("R.rsp")



pkgVer = "0.3.0"

setwd("~/../Programming/R/LineChart/")

devtools::use_package("BayesFactor", type="Suggests")


desc = read.dcf("DESCRIPTION")
desc[,"Version"] = pkgVer
desc[,"Date"] = format(Sys.Date(), "%Y")
write.dcf(desc, "DESCRIPTION")

lic = read.dcf("LICENSE")
lic[,"YEAR"] = format(Sys.Date(), "%Y")
write.dcf(lic, "LICENSE")

file.remove("NAMESPACE")
devtools::document()


devtools::install(build_vignettes = TRUE)





devtools::check()

devtools::build(path="packaged")

install.packages("packaged/LineChart_0.3.tar.gz", repos=NULL)




remove.packages("LineChart")
devtools::install_github("hardmanko/LineChart-package", build_vignettes = TRUE)




library(LineChart)

?lineChart

vignette("introduction", "LineChart")

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
