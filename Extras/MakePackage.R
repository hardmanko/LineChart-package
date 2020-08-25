install.packages("R.rsp")



library(devtools)
library(roxygen2)



pkgVer = "0.3.2"

setwd("D:/Programming/R/LineChart/")


usethis::use_package("BayesFactor", type="Suggests")


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




remove.packages("LineChart")
devtools::install_github("hardmanko/LineChart-package", build_vignettes = TRUE)

devtools::install_github("hardmanko/LineChart-package@v0.3.2", build_vignettes = TRUE)





library(LineChart)

?lineChart

vignette("introduction", "LineChart")


settings = buildGroupSettings(0, symbol=16, col="blue")

dd = ChickWeight

dd$Diet = LETTERS[dd$Diet]

dd = dd[ sample(1:nrow(dd), nrow(dd), replace=FALSE), ]
unique(dd$Diet)



lineChart(weight ~ Diet, dd)


plotDf = createPlottingDf(weight ~ Diet, dd)

lineChartDf(plotDf)

lineChart(weight ~ Time * Diet, dd)


###
# Custom error bar

quartiles = function(x) {
  qs = as.numeric(quantile(x, c(0.25, 0.75)))
  list(eb=qs, includesCenter=TRUE)
}


lineChart(weight ~ Time * Diet, data, errBarType = quartiles, centralTendencyType = "median")


###
# Replicates

pdf1 = lineChart(weight ~ Diet, dd)
pdf2 = lineChart(weight ~ Diet, dd, replicate = "Chick", repFun = mean)

add = aggregate(weight ~ Diet * Chick, dd, mean)
pdf3 = lineChart(weight ~ Diet, add)





