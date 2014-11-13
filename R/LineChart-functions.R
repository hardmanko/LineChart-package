
#' Open the introductory manual for the LineChart package.
#' 
#' This function open the introductory manual/vignettes.
#' 
#' @export
lineChartManual = function() {
  vignette("introduction", package="LineChart")
}

#' Plot data with a line chart
#' 
#' This function is the primary interface to the LineChart package.
#' 
#' @inheritParams createPlottingDf
#' @inheritParams lineChartDf
#' 
#' @param legendPosition The position at which the legend should be placed. If this is not NULL, it is passed through to legend directly.
#' @param legendTitle The title to be used in the legend box. If "GROUP_NAME" (default), the name of the source of data for the grouping variable will be used. If no legend title is desired, use NULL.
#' @param ... Additional parameters, currently passed on to the legend creating functions.
#' 
#' @return The plotting data frame that was used is returned invisibly.
#' 
#' @seealso \code{\link{createPlottingDf}}, \code{\link{lineChartDf}}, \code{\link{buildGroupSettings}}, \code{\link{extractGroupSettings}}, \code{\link{applySettingsToPlottingDf}}, \code{\link{legendFromPlottingDf}}, \code{\link{legendFromSettings}}
#' 
#' @export
#' 
#' @examples
#' #Basic use case: Plotting data frame with default appearance.
#' data(ChickWeight)
#' lineChart(weight ~ Time * Diet, ChickWeight, legendPosition="topleft")
#' 
#' #You can modify the appearance of groups by providing group settings:
#' settings = buildGroupSettings(group=1:4, symbol=21:24, lty=1:4)
#' lineChart(weight ~ Time * Diet, ChickWeight, legendPosition="topleft", settings=settings)
#' 
#' #Let's ignore group 3
#' settings[ settings$group == 3, "include" ] = FALSE 
#' lineChart(weight ~ Time * Diet, ChickWeight, legendPosition="topleft", settings=settings)
#' 
#' #If for whatever reason you have data in two different data frames that you 
#' #want in the same plot, use add=TRUE to add to the existing plot.
#' cw12 = ChickWeight[ ChickWeight$Diet %in% 1:2, ]
#' cw34 = ChickWeight[ ChickWeight$Diet %in% 3:4, ]
#' 
#' #Distinguish between the groups by fill color.
#' settings$fillColor = c("white", "white", "black", "black") 
#' settings$include = TRUE #Include all groups
#' 
#' lineChart(weight ~ Time * Diet, cw12, legendPosition="topleft", 
#'   settings=settings, ylim=c(40,300))
#' lineChart(weight ~ Time * Diet, cw34, legendPosition="bottomright", 
#'   settings=settings, add=TRUE)
lineChart = function(formula, data, legendPosition="topright", settings=NULL,
                     errBarType = "SE",
                     title="", xlab=NULL, ylab=NULL, legendTitle="GROUP_NAME", 
                     xlim=NULL, ylim=NULL,
                     plotXAxis=TRUE, plotYAxis=TRUE,
                     lwd.axes=par()$lwd, add=FALSE,
                     ...) 
{
  plotDf = createPlottingDf(formula, data, settings=settings, errBarType=errBarType)
  
  lineChartDf(plotDf, title=title, xlab=xlab, ylab=ylab, ylim=ylim, xlim=xlim,
                plotXAxis=plotXAxis, plotYAxis=plotYAxis, lwd.axes=lwd.axes, add=add)
  
  if (!is.null(legendPosition)) {
    if (!is.null(legendTitle) && (legendTitle == "GROUP_NAME")) {
      legendTitle = attr(plotDf, "originalNames", exact=TRUE)$group
    }
    legendFromPlottingDf(legendPosition, plotDf, title=legendTitle, ...=...)
  }
  invisible(plotDf)
}


#' Creates a plottable data frame.
#' 
#' This function creates a data frame that can be used for plotting
#' from a user-supplied data frame. The user data will be aggregated
#' based on the formula. The resulting plotting data frame can be used 
#' with `lineChartDf`.
#' 
#' Note that if no grouping variables are supplied by the formula, a `group` column 
#' will still be made in the resulting data frame and the group name will be the number 0.
#' 
#' @param formula The formula should be of the form `y ~ x` or `y ~ x * g1 * g2 * ... * gK`, where the `gN` are any number of grouping variables and 'y', 'x', and 'gi' are the names of columns in `data`. If grouping variables are used, `x` must come before them.
#' @param data A data frame containing the data to be used in creation of the plotting data frame.
#' @param settings Plotting settings for the different groups in the data, such as the symbol to use.
#' @param errBarType The type of error bar to use. Can be "SE" for standard error, "SD" for standard deviation, or "CI95" for a 95\% confidence interval. If NULL, no error bars are created. If a function is supplied, the function should take one argument, which will be the data used to plot a single data point, and return two error bar amounts. These amounts should be the signed difference between the end of the error bar and the mean of the data for that data point.
#' @return A data frame which can be plotted with lineChartDf. See the documentation for lineChartDf for examples.
#' @export
#' 
#' @examples
#' 
#' data(ChickWeight)
#' 
#' #Example of a user-defined error bar function:
#' quartiles = function(x) {
#'  as.numeric(quantile(x - mean(x), c(0.25, 0.75)))
#' }
#' 
#' plotDf = createPlottingDf(weight ~ Time * Diet, ChickWeight, errBarType=quartiles)
#' lineChartDf(plotDf)
#' 
createPlottingDf = function(formula, data, settings = NULL, errBarType = "SE") {
  
  mf = model.frame(formula=formula, data=data)
  
  plotDf = aggregate(formula, mf, mean)
  
  
  terms = terms.formula(formula)
  
  groups = as.character(attr(terms, "variables"))
  
  groupCount = length(groups) - 3
  
  hasGroups = groupCount > 0
  
  if (hasGroups) {
    groups = groups[ 4:length(groups) ]
    
    levels = NULL
    if (groupCount > 1) {
      levels = apply(plotDf[ , groups ], 1, function(x) { paste(x, sep="", collapse=":") })
    } else {
      levels = plotDf[ , groups ]
    }
    
    plotDf = plotDf[ , !(names(plotDf) %in% groups)]
    
    plotDf[ , paste(groups, collapse=":") ] = levels
    
  } else {
    plotDf[ , "indicator_group" ] = 0
  }
  
  attr(plotDf, "originalNames") = list(y = names(plotDf)[2],
                                       x = names(plotDf)[1],
                                       group = names(plotDf)[3]
  )
  if (!hasGroups) {
    attr(plotDf, "originalNames")$group = NULL
  }
  names(plotDf) = c("x", "y", "group")
  
  
  errorBarFunction = NULL
  if (is.function(errBarType)) {
    errorBarFunction = errBarType
  } else {
    if (!is.null(errBarType)) {
      if (errBarType == "SE") {
        errorBarFunction = function(x) { y = sqrt(var(x)/length(x)); c(-y, y) }
      } else if (errBarType == "CI95") {
        errorBarFunction = function(x) {
          t = t.test(x, conf.level=.95)
          y = as.numeric(t$conf.int - t$estimate)
        }
      } else if (errBarType == "SD") {
        errorBarFunction = function(x) { y = sqrt(var(x)); c(-y, y) }
      }
    }
  }
  
  if (is.null(errorBarFunction)) {
    plotDf$errBar = NA
    plotDf$errBarLower = NA
  } else {
    #plotDf$errBar = aggregate(formula, mf, errorBarFunction)[,names(mf)[1]]
    
    errBars = aggregate(formula, mf, errorBarFunction)[,names(mf)[1]]
    errBars[is.na(errBars)] = 0
    
    #If the ordering of the error bars is wrong, swap lower and upper
    errBars[ errBars[,1] > errBars[,2], c(1,2) ] = errBars[ errBars[,1] > errBars[,2], c(2,1) ]
    
    plotDf$errBarLower = errBars[,1]
    plotDf$errBar = errBars[,2]
  }
  
  if (is.null(settings)) {
    settings = buildGroupSettings(plotDf$group, suppressWarnings=TRUE)
  }
  plotDf = applySettingsToPlottingDf(settings, plotDf)
  
  plotDf
}


#' Plot data using a plotting data frame
#' 
#' This function uses a data frame with the same format as one created by createPlottingDf.
#' This is mainly an internal function that is called by lineChart, but it can be called
#' directly if more control of plotDf is desired.
#' 
#' 
#' @details
#' If xlab and/or ylab are NULL, the original names of the source of the data are used for plotting, if available. The original names are stored in attributes and sometimes attributes are lost or, in the case of a manually created plotting data frame, not present. The original names are stored in an attribute called "originalNames" which is a list with "y", "x", and "group" components.
#' 
#' @param plotDf The data frame to plotted.
#'
#' @param title The title to place at the top of the plot.
#' @param xlab The label to place on the x-axis. If NULL (default), the name of the source of data for the x-variable will be used, if available.
#' @param ylab The label to place on the y-axis. If NULL (default), the name of the source of data for the y-variable will be used if available.
#' @param xlim The plotting range of the x-axis.
#' @param ylim The plotting range of the y-axis.
#' @param plotXAxis Should the x-axis be plotted?
#' @param plotYAxis Should the y-axis be plotted?
#' @param lwd.axes The line width of the axes and box around the plotting area.
#' @param add If FALSE, a new plot will be started and the data plotted in it. If TRUE, the data will be plotted in the current plotting device, if any.
#' 
#' @export
#' @examples
#' data(ChickWeight)
#' #Use just a subset of the data
#' ChickWeight = ChickWeight[ ChickWeight$Diet %in% c(1, 3), ]
#' 
#' plotDf = createPlottingDf(weight ~ Time * Diet, ChickWeight, errBarType="SD")
#' 
#' #Make single-sided error bars, using the standard deviation on just one side
#' plotDf[plotDf$group == 1, ]$errBar = 0
#' plotDf[plotDf$group == 3, ]$errBarLower = 0
#' 
#' lineChartDf(plotDf)
#' 
#' #Make assymetrical error bars based on quantiles in the data.
#' quants = aggregate(weight ~ Time * Diet, ChickWeight, function(x) { quantile(x, c(.25, .75)) })
#' 
#' #The values for errBar and errBarLower must be relative to the y value of the plotting data frame.
#' plotDf$errBarLower = quants[,3][,1] - plotDf$y
#' plotDf$errBar = quants[,3][,2] - plotDf$y
#' 
#' lineChartDf(plotDf)
lineChartDf = function(plotDf,
                       title="", xlab=NULL, ylab=NULL, 
                       ylim=NULL, xlim=NULL,
                       plotXAxis=TRUE, plotYAxis=TRUE,
                       lwd.axes=par()$lwd, add=FALSE)
{
  
  originalNames = attr(plotDf, "originalNames", exact=TRUE)
  
  if (is.null(xlab)) {
    if (is.null(originalNames)) {
      xlab = ""
    } else {
      xlab = originalNames$x
    }
  }
  
  if (is.null(ylab)) {
    if (is.null(originalNames)) {
      ylab = ""
    } else {
      ylab = originalNames$y
    }
  }
  
  if (is.null(xlim)) {
    xlim = range(plotDf$x)
  }
  if (is.null(ylim) || is.na(ylim)) {
    
    if (is.null(plotDf$errBar) || is.na(plotDf$errBar)) {
      ylim = c(min(plotDf$y), max(plotDf$y))
    } else {
      if (is.null(plotDf$errBarLower) || is.na(plotDf$errBarLower)) {
        ylim = c(min(plotDf$y - plotDf$errBar), max(plotDf$y + plotDf$errBar))
      } else {
        ylim = c(min(plotDf$y + plotDf$errBarLower), max(plotDf$y + plotDf$errBar))
      }
    }
    
  }
  
  if (!add) { #Start a new plot
    plot.default(x=plotDf$x, y=plotDf$y, ylim=ylim, xlim=xlim, xlab="", 
                 ylab="", type='n', main=title, axes=FALSE )
    
    if (plotXAxis) {
      axis(1, lwd=lwd.axes, labels=unique(plotDf$x), at=unique(plotDf$x), 
           cex.axis=par()$cex.axis)
      mtext(xlab, side=1, line=2, cex=par()$cex.lab * par()$cex)
    }
    
    if (plotYAxis) {
      axis(2, lwd=lwd.axes, las=1, cex.axis=par()$cex.axis)
      mtext(ylab, side=2, line=3, cex=par()$cex.lab * par()$cex)
    }
    
    box(lwd=lwd.axes)
  }
  
  for (g in unique(plotDf$group)) {
    groupIndices = (1:length(plotDf$group))[plotDf$group == g]
    for (i in groupIndices) {
      
      pl = plotDf[i,]
      
      if (pl$include) {
        if (!is.null(pl$errBar) && !is.na(pl$errBar)) {
          
          if (!is.null(pl$errBarLower) && !is.na(pl$errBarLower)) {
            drawErrorBars( x=pl$x, CI=c(pl$y + pl$errBarLower, pl$y + pl$errBar), mean=pl$y, 
                           color=pl$color, lwd=pl$lwd, width=pl$width.errBar)
          } else {
            drawErrorBars( x=pl$x, ciAmount=pl$errBar, mean=pl$y, 
                           color=pl$color, lwd=pl$lwd, width=pl$width.errBar)
          }
          
        }
      }
    }
  }
  
  drawConnectedPointsDf(plotDf)
}


#' Create a data frame with appearance settings for plotting
#' 
#' This function is the primary way to provide settings for the groups used in plotting.
#' You need to provide the indentifiers for each group and the various settings to be used
#' for that group. All settings are optional and will be set with reasonable defaults if
#' not specified.
#' 
#' @param group The indentifiers for the groups.
#' @param altName An alternative name to use in legends, instead of 'group'. If provided and some values are NA, those values will be replaced with the group name.
#' @param color The color to be used for the symbols and lines.
#' @param fillColor The fill color to be used in case the symbols are of a fillable type (e.g. 21:25).
#' @param symbol The plotting character to use.
#' @param cex.symbol The character expansion for the plotting symbols.
#' @param width.errBar The width of the heads of the error bars (if present).
#' @param lty The line type.
#' @param lwd The line width.
#' @param include Boolean. Should this group be included in plots?
#' @param suppressWarnings Boolean. By default you are warned about which settings are left at default values. Set to TRUE to suppress these warnings.
#' 
#' @return A data frame containing appearance setting data that can be used with lineChart for the settings argument.
#' 
#' @export
#' 
#' @examples
#' library(LineChart)
#' 
#' data(ChickWeight)
#' settings = buildGroupSettings(group=1:4, symbol=21:24, color=c("red", "green", "orange", "blue"))
#' lineChart( weight ~ Time * Diet, ChickWeight, legendPosition="topleft" )
buildGroupSettings = function(group, altName=NULL, color=NULL, fillColor=NULL, symbol=NULL,
                              cex.symbol=NULL, width.errBar=NULL, lty=NULL, lwd=NULL,
                              include=NULL, suppressWarnings=FALSE) 
{
  group = unique(group)
  ng = length(group)
  
  usedDefaults = NULL
  
  if (is.null(altName)) {
    altName = group
  } else {
    altName[ is.na(altName) ] = group[ is.na(altName) ]
  }
  
  if (is.null(symbol)) {
    usedDefaults = append(usedDefaults, "symbol")
    symbol = toupper(as.character(substr(group, 1, 1)))
  }
  
  if (is.null(cex.symbol)) {
    usedDefaults = append(usedDefaults, "cex.symbol")
    cex.symbol = rep(par()$cex, ng)
  }
  
  if (is.null(width.errBar)) {
    usedDefaults = append(usedDefaults, "width.errBar")
    width.errBar = cex.symbol * .07
  }
  
  if (is.null(color)) {
    usedDefaults = append(usedDefaults, "color")
    color = rainbow(ng)
  }
  
  if (is.null(fillColor)) {
    usedDefaults = append(usedDefaults, "fillColor")
    fillColor = color
  }
  
  if (is.null(lty)) {
    usedDefaults = append(usedDefaults, "lty")
    lty = rep(par()$lty, ng)
  }
  
  if (is.null(lwd)) {
    usedDefaults = append(usedDefaults, "lwd")
    lwd = rep(par()$lwd, ng)
  }
  
  if (is.null(include)) {
    usedDefaults = append(usedDefaults, "include")
    include = rep(TRUE, ng)
  }
  
  df = data.frame(group = group,
                  altName = altName,
                  color = color,
                  fillColor = fillColor,
                  symbol = symbol,
                  cex.symbol = cex.symbol,
                  width.errBar = width.errBar,
                  lty = lty,
                  lwd = lwd,
                  include = include,
                  stringsAsFactors=FALSE
  )
  
  if (!is.null(usedDefaults) && !suppressWarnings) {
    warning( paste("Defaults used for:", paste(usedDefaults, collapse=", ")), call.=FALSE)
  }
  
  df
}

#' Extract the appearance settings from a plotting data frame
#' 
#' @param plotDf The plotting data frame to extract settings from.
#' @return The extracted settings.
#' 
#' @export
#' 
#' @examples
#' data(ChickWeight)
#' plotDf = lineChart(weight ~ Time * Diet, ChickWeight, legendPosition="topleft")
#' 
#' #From a data frame used for plotting, extract the settings
#' settings = extractGroupSettings(plotDf)
#' 
#' settings$symbol = 21:24 #Make a small adjustment to the settings
#' 
#' #Re-plot the data with new settings
#' lineChart(weight ~ Time * Diet, ChickWeight, legendPosition="topleft", settings=settings)
extractGroupSettings = function(plotDf) {
  agg = aggregate(plotDf, list(plotDf$group), function(x) {x[1]})
  settings = subset(agg, select=getSettingsDfCOlumnNames())
  replaceMissingSettings(settings)
}

#' Apply appearance settings to a plotting data frame.
#' 
#' @param settings The appearance settings to apply.
#' @param plotDf The plotting data frame to apply the settings to.
#' @return A new plotting data frame with settings applied.
#' 
#' @export
applySettingsToPlottingDf = function (settings, plotDf) {
  groupColumn = attr(plotDf, "varNames")$group
  
  groupNames = plotDf$group
  if (is.factor(groupNames)) {
    groupNames = levels(groupNames)[groupNames]
  }
  
  for (g in unique(groupNames)) {
    plotRows = (groupNames == g)
    setRows = (settings$group == g)
    
    for (col in getSettingsDfCOlumnNames()) {
      plotDf[ plotRows, col ] = settings[ setRows, col ]
    }
  }
  plotDf
}

#' Create a legend based on a plotting data frame with group appearance settings
#' 
#' @param position The position of the legend. This is passed to the `x` argument of legend().
#' @param plotDf The plotting data frame from which settings will be extracted and used to create the legend.
#' @param ... A few variable arguments are passed through to legend(): y, cex, inset, title, box.lwd, and horiz.
#' @export
legendFromPlottingDf = function(position, plotDf, ...) {
  legendFromSettings(position, settings=extractGroupSettings(plotDf), ...=...)
}

#' Create a legend based on group appearance settings
#' 
#' @param position The position of the legend. This is passed to the `x` argument of legend().
#' @param settings The settings data frame that will be used to create the legend.
#' @param ... A few variable arguments are passed through to legend(): y, cex, inset, title, box.lwd, and horiz.
#' @export
legendFromSettings = function(position, settings, ...) {
  vargs = list(...)
  
  settings = settings[ settings$include, ]

  legend(x=position, y=vargs$y, legend=settings$altName, col=settings$color, 
         pt.bg=settings$fillColor, 
         pch=settings$symbol, pt.cex=settings$cex.symbol, 
         lwd = settings$lwd, lty = settings$lty,
         inset = ifelse(is.null(vargs$inset), 0, vargs$inset),
         cex = ifelse(is.null(vargs$cex), 1, vargs$cex),
         box.lwd = ifelse(is.null(vargs$box.lwd), par()$lwd, vargs$box.lwd), 
         title = vargs$title, 
         horiz = ifelse(is.null(vargs$horiz), FALSE, vargs$horiz)  
  )
}



getSettingsDfCOlumnNames = function() {
  c("group", "altName", "lty", "lwd", "symbol", "cex.symbol", "width.errBar", "color", "fillColor", "include")
}

replaceMissingSettings = function(settings) {
  gsn = getSettingsDfCOlumnNames()
  currentNames = names(settings)
  
  missing = gsn[!(gsn %in% currentNames)]
  
  if (length(missing) > 0) {
    defaults = buildGroupSettings(settings$group, suppressWarnings=TRUE)
    
    for (m in missing) {
      settings[,m] = defaults[,m]
    }
  }
  
  settings
}

extendSettings = function(settings, group) {
  allGroups = unique(c(settings$group, group))
  newSettings = buildGroupSettings(allGroups, suppressWarnings=TRUE)
  
  for (g in allGroups) {
    if (g %in% settings$group) {
      newSettings[g == newSettings$group,] = settings[g == settings$group,]
    }
  }
  newSettings
}


#This can take a confidence interval (CI; two values around a central mean) or a mean and the amount
#to go in either direction. Single-sided error bars can be made by providing one side of the CI and the mean. For asymmetrical or single-sided error bars, both CI and mean must be provided.
drawErrorBars = function(x, CI=c(mean-ciAmount, mean+ciAmount), width=.1*par()$cex, ciAmount=NULL, mean=NULL, color="black", lty=1, lwd=1, vertical=TRUE) {
  
  x0 = rep(x, 2)
  x1 = x0
  y0 = rep(ifelse(is.null(mean), mean(CI), mean), 2)
  y1 = CI[!is.null(CI)]
  
  if (!vertical) {
    x0 = y0
    x1 = y1
    y0 = x0
    y1 = x1
  }
  
  dSquared = (x0 - x1)^2 + (y0 - y1)^2
  
  for (i in 1:length(x0)) {
    #Only draw arrows with nonzero length so as to avoid warnings
    if (dSquared[i] != 0) {
      arrows(x0=x0[i], x1=x1[i], y0=y0[i], y1=y1[i], angle=90, length=width, 
             lty=lty, lwd=lwd, col=color, code=2)
    }
  }
}

#' Draw points with connect lines.
#' 
#' Draw points specified by a plotting data frame with connect lines. This function is primarily for internal use within the LineChart package. If you want to add connected points to a plot, it will probably be easier to use lineChart with add = TRUE.
#'
#' @param plotDf The data frame containing the data to be plotted. it must have several columns with specific names. See the return value of `createPlottingDf` for those column names.
drawConnectedPointsDf = function(plotDf) {
  for (g in unique(plotDf$group)) {
    pl = plotDf[plotDf$group == g,]
    
    if (pl$include[1]) {
      lines(pl$x, pl$y, col=pl$color, lwd=pl$lwd, lty=pl$lty)
      points(pl$x, pl$y, pch=pl$symbol, 
             bg=pl$fillColor, col=pl$color,
             cex=pl$cex.symbol, lwd=pl$lwd)
    }
  }
}
