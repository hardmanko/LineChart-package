
#' Open the introductory manual for the LineChart package.
#' 
#' This function opens the introductory manual/vignettes.
#' 
#' @export
#' @importFrom grDevices rainbow
#' @importFrom graphics arrows axis box legend lines mtext par plot.default points
#' @importFrom stats aggregate model.frame t.test terms.formula var
#' @importFrom utils vignette
#' 
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
#' @param legendPosition The position at which the legend should be placed. If NULL, no legend is plotted. If "CHOOSE_BEST", the best legend position is selected. Otherise, it is passed through to legend() directly.
#' @param legendTitle The title to be used in the legend box. If "GROUP_NAME" (default), the name of the source of data for the grouping variable will be used. If no legend title is desired, use NULL.
#' @param ... Additional parameters, currently passed on to the legend creating functions.
#' 
#' @return The plotting data frame that was used is returned invisibly. It contains all of the information used to create the plot.
#' 
#' @seealso [`createPlottingDf`], [`lineChartDf`], [`buildGroupSettings`], [`extractGroupSettings`], [`applySettingsToPlottingDf`], [`legendFromPlottingDf`], [`legendFromSettings`]
#' 
#' @md
#' @export
#' 
#' @examples
#' #Basic use case: Plotting data frame with default appearance.
#' data(ChickWeight)
#' lineChart(weight ~ Time * Diet, ChickWeight)
#' 
#' #You can modify the appearance of groups by providing group settings:
#' settings = buildGroupSettings(group=1:4, symbol=21:24, lty=1:4, 
#'   fillColor=gray(seq(0, 1, length.out = 4)))
#' lineChart(weight ~ Time * Diet, ChickWeight, settings=settings)
#' 
#' #Let's ignore group 3
#' settings[ settings$group == 3, "include" ] = FALSE 
#' lineChart(weight ~ Time * Diet, ChickWeight, settings=settings)
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
#' #The legend doesn't update, but another legend gets added.
lineChart = function(formula, data, settings=NULL, legendPosition="CHOOSE_BEST",
										 centralTendencyType = "mean", errBarType = "SE",
                     title="", xlab=NULL, ylab=NULL, legendTitle="GROUP_NAME", 
                     xlim=NULL, ylim=NULL,
                     plotXAxis=TRUE, plotYAxis=TRUE,
                     lwd.axes=par()$lwd, add=FALSE, xOrder = NULL,
										 replicate = NULL, repFun = mean,
                     ...) 
{
  plotDf = createPlottingDf(formula, data, settings=settings, 
                            centralTendencyType=centralTendencyType, errBarType=errBarType, 
                            xOrder = xOrder, replicate = replicate, repFun = repFun)
  
  lineChartDf(plotDf, title=title, xlab=xlab, ylab=ylab, ylim=ylim, xlim=xlim,
                plotXAxis=plotXAxis, plotYAxis=plotYAxis, lwd.axes=lwd.axes, add=add)
  
  if (!is.null(legendPosition)) {
    if (!is.null(legendTitle) && (legendTitle == "GROUP_NAME")) {
      legendTitle = attr(plotDf, "originalNames", exact=TRUE)$group
    }
  	# Only plot legend if there is more than one group
  	if (length(unique(plotDf$group)) > 1) {
    	legendFromPlottingDf(legendPosition, plotDf, title=legendTitle, ...=...)
  	}
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
#' @param formula The formula should be of the form `y ~ x` or `y ~ x * g1 * g2 * ... * gK`, where the `gN` are any number of grouping variables and `y`, `x`, and `gi` are the names of columns in `data`. If grouping variables are used, `x` must come before them.
#' @param data A data frame containing the data to be used in creation of the plotting data frame.
#' @param settings Plotting settings for the different groups in the data, such as the symbol to use.
#' @param centralTendencyType Character or function. The type of central tendency measure to use. Can be "mean" or "median". If a function, should be a function of one vector argument that returns a scalar.
#' @param errBarType The type of error bar to use. Can be "SE" for standard error, "SD" for standard deviation, "CI95" for a 95\% confidence interval, or "Cred95" for 95% credible interval. If `NULL`, no error bars are created. If a function is supplied, the function should take one vector argument, which is the data used to plot a single data point. It should return either 1) a length-2 vector or 2) a list with two elements. If returning 1), the values in the vector should be distances from the central tendency measure that the error bars should be drawn (i.e. they should be more-or-less centered on 0). If returning 2), the list should contain \code{eb}, which is either a) error bar distances OR b) error bar endpoints, and \code{includesCenter}, which indicates whether `eb` is distances or endpoints. If `eb` is distances, then it does not include the center, so `includesCenter` should be `FALSE`. If `eb` is endpoints, then it does include the center, so `includesCenter` should be `TRUE`.
#' @param xOrder The order in which to plot the x variable. A character vector containing all of the levels of the x variable in the order in which they should be plotted. If the `x` variable is numeric or can be coerced to numeric, this argument does nothing.
#' @param replicate The name of a column in `data` providing indices for replicates (e.g. a participant number). The data will be aggregated for each replicate with `repFun` before being plotted. Do not include the replicate column in `formula`.
#' @param repFun Used to aggregate values for each replicate.
#' 
#' @return A data frame which can be plotted with [`lineChartDf`]. See the documentation for lineChartDf for examples.
#' @md
#' @export
#' 
#' @examples
#' 
#' data(ChickWeight)
#' 
#' #Example of a user-defined central tendency function:
#' trimmedMedian = function(x) {
#'   #trim highest and lowest values
#'   x = sort(x)[ 2:(length(x) - 1) ]
#'   median(x)
#' }
#' 
#' #Example of a user-defined error bar function:
#' quartiles = function(x) {
#'   qs = as.numeric(quantile(x, c(0.25, 0.75)))
#'   list(eb=qs, includesCenter=TRUE)
#' }
#' 
#' plotDf = createPlottingDf(weight ~ Time * Diet, ChickWeight, 
#'   centralTendencyType=trimmedMedian, errBarType=quartiles)
#' lineChartDf(plotDf)
#' 
createPlottingDf = function(formula, data, 
                            settings = NULL, 
                            centralTendencyType = "mean", 
                            errBarType = "SE", 
                            xOrder = NULL,
                            replicate = NULL, repFun = mean) 
{
  
	errorBarFunction = NULL
	if (is.function(errBarType)) {
		errorBarFunction = errBarType
	} else {
		if (!is.null(errBarType)) {
			errorBarFunction = getErrorBarFunctionFromName(errBarType)
		} else {
		  errorBarFunction = function(x) {
		    list(eb=c(0, 0), includesCenter=FALSE)
		  }
		}
	}
	
	if (is.function(centralTendencyType)) {
		centralTendencyFunction = centralTendencyType
	} else {
		centralTendencyFunction = getCentralTendencyFunctionFromName(centralTendencyType)
	}
	
	if (!is.null(replicate)) {
	  f2 = update.formula(formula, paste0(". ~ . * ", replicate))
	  data = aggregate(f2, data, centralTendencyFunction)
	}
	
  mf = model.frame(formula=formula, data=data)
  plotDf = aggregate(formula, mf, centralTendencyFunction)
  

  terms = terms.formula(formula)
  groups = attr(terms, "term.labels")[ attr(terms, "order") == 1 ]
  
  

  
  groupCount = length(groups) - 1
  hasGroups = groupCount > 0
  
  if (hasGroups) {
    groups = groups[ 2:length(groups) ]
    
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


  variableNames = list(y = names(plotDf)[2],
  										 x = names(plotDf)[1],
  										 group = names(plotDf)[3])
  attr(plotDf, "originalNames") = variableNames
  if (!hasGroups) {
    attr(plotDf, "originalNames")$group = NULL
  }
  names(plotDf) = c("x", "y", "group")
  
  # Add number of observations
  yVariable = all.vars(formula)[1]
  plotDf$NObs = aggregate(formula, mf, length)[ , yVariable ]
  
  #### Make error bars
  if (is.null(errorBarFunction)) {
  	plotDf$ebUpper = NA
  	plotDf$ebLower = NA
  } else {
  	
  	ebfWrapper = function(x) {
  		temp = errorBarFunction(x)
  		if (is.list(temp)) {
  			if (temp$includesCenter) {
  				temp = temp$eb - centralTendencyFunction(x)
  			} else {
  				temp = temp$eb
  			}
  		}
  		temp
  	}
  	
  	errBars = aggregate(formula, mf, ebfWrapper)[,names(mf)[1]]
  	errBars[is.na(errBars)] = 0
  	
  	#If the ordering of the error bars is wrong, swap lower and upper
  	luSwapped = errBars[,1] > errBars[,2]
  	if (any(luSwapped)) {
  		warning("Some of the error bars appear to be in the wrong order (lower end > upper end). They have been swapped.")
  		errBars[ luSwapped, c(1,2) ] = errBars[ luSwapped, c(2,1) ]
  	}
  	
  	plotDf$ebLower = errBars[,1]
  	plotDf$ebUpper = errBars[,2]
  }
  
  
  plotDf$xLabel = as.character(plotDf$x)
  
  if (is.factor(plotDf$x)) {
  	plotDf$x = levels(plotDf$x)[plotDf$x]
  }
  
  convertToNumeric = function(x) {
  	if (is.factor(x)) {
  		x = levels(x)[x]
  	}
  	suppressWarnings( as.numeric(x) )
  }
  
  canBeNumeric = function(x) {
		nx = convertToNumeric(x)
  	all(is.na(nx) == FALSE)
  }
  
  xCanBeNumeric = canBeNumeric(plotDf$x)
  
  #reorder X based on xOrder
  xOrderProblem = !all(xOrder %in% plotDf$x) || !all(plotDf$x %in% xOrder)
  if (is.null(xOrder) || xOrderProblem) {
  	if (xOrderProblem && !is.null(xOrder)) {
  		warning("xOrder contains values not found in the x-variable, or vice versa. It has been ignored.")
  	}
  	#Use the order of the variable in the data
  	xOrder = unique(data[ , variableNames$x ])
  	if (xCanBeNumeric) {
  		xOrder = sort(convertToNumeric(xOrder))
  	}
  }
  tempDf = NULL
  for (i in 1:length(xOrder)) {
  	rows = xOrder[i] == plotDf$xLabel
  	tempDf = rbind(tempDf, plotDf[ rows, ])
  }
  plotDf = tempDf

  #Provide x values for non-numeric x
  if (xCanBeNumeric) {
  	plotDf$x = convertToNumeric(plotDf$x)
  } else {
  	uniqueXLabels = unique(plotDf$xLabel)
  	plotDf$x = rep(0, nrow(plotDf))
  	for (i in 1:length(uniqueXLabels)) {
  		plotDf$x[ plotDf$xLabel == uniqueXLabels[i] ] = i
  	}
  }

  if (is.null(settings)) {
    settings = buildGroupSettings(plotDf$group, suppressWarnings=TRUE)
  }
  
  pdfSUG = sort(unique(plotDf$group))
  setSUG = sort(unique(settings$group))
  
  if (!all(setSUG == setSUG)) {
    
    pdfGrpStr = paste0("(", paste(pdfSUG, collapse=", "), ")")
    setGrpStr = paste0("(", paste(setSUG, collapse=", "), ")")
    
    msg = paste0("Group settings don't match the groups being plotted. Plotted groups are ",
                 pdfGrpStr, " while the group names in settings are ", setGrpStr, ". Using defaults.")
    warning(msg)
    
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
#' If `xlab` and/or `ylab` are `NULL`, the original names of the source of the data are used for plotting, if available. The original names are stored in attributes and sometimes attributes are lost or, in the case of a manually created plotting data frame, not present. The original names are stored in an attribute called "originalNames" which is a list with "y", "x", and "group" components.
#' 
#' @param plotDf The data frame to plotted.
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
#' @md
#' @export
#' @examples
#' data(ChickWeight)
#' #Use just a subset of the data
#' ChickWeight = ChickWeight[ ChickWeight$Diet %in% c(1, 3), ]
#' 
#' plotDf = createPlottingDf(weight ~ Time * Diet, ChickWeight, errBarType="SD")
#' 
#' #Make single-sided error bars, using the standard deviation on just one side
#' plotDf[plotDf$group == 1, ]$ebUpper = 0 # Upper error bar, if ebLower is also provided
#' plotDf[plotDf$group == 3, ]$ebLower = 0
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
  
  xlim = valueIfNull(xlim, range(plotDf$x))
  
  if (is.null.or.na(ylim)) {
    
    if (is.null.or.na(plotDf$ebUpper)) {
      ylim = c(min(plotDf$y), max(plotDf$y))
    } else {
      if (is.null.or.na(plotDf$ebLower)) {
        ylim = c(min(plotDf$y - plotDf$ebUpper), max(plotDf$y + plotDf$ebUpper))
      } else {
        ylim = c(min(plotDf$y + plotDf$ebLower), max(plotDf$y + plotDf$ebUpper))
      }
    }
    
  }
  
  if (!add) { #Start a new plot
    plot.default(x=plotDf$x, y=plotDf$y, ylim=ylim, xlim=xlim, xlab="", 
                 ylab="", type='n', main=title, axes=FALSE )
    
    if (plotXAxis) {
    	uniqueXs = unique(plotDf$x)
    	matchingLabels = rep("", length(uniqueXs))
    	for (i in 1:length(uniqueXs)) {
    		matchingLabels[i] = plotDf$xLabel[ plotDf$x == uniqueXs[i] ][1]
    	}
    	
    	axis(1, lwd=lwd.axes, labels=matchingLabels, at=uniqueXs, 
           cex.axis=par()$cex.axis)
      mtext(xlab, side=1, line=3, cex=par()$cex.lab * par()$cex)
    }
    
    if (plotYAxis) {
      axis(2, lwd=lwd.axes, las=1, cex.axis=par()$cex.axis)
      mtext(ylab, side=2, line=3, cex=par()$cex.lab * par()$cex)
    }
    
    box(lwd=lwd.axes)
  }
  
  # Draw error bars
  for (g in unique(plotDf$group)) {
    groupIndices = (1:length(plotDf$group))[plotDf$group == g]
    for (i in groupIndices) {
      
      pl = plotDf[i,]
      
      if (pl$include) {
      	if (!is.null.or.na(pl$ebUpper) || !is.null.or.na(pl$ebLower)) {
      		
      		CI = c(pl$y + pl$ebLower, pl$y + pl$ebUpper)
      		
      		drawErrorBars(x = pl$x, mean = pl$y, CI = CI,
      									color = pl$color, lwd=pl$lwd, width=pl$width.errBar)
          
        }
      }
    }
  }
  
  # Draw points after error bars so the point is on top of error bar center.
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
#' @param groupLabel An alternative name to use in legends, instead of 'group'. If provided and some values are NA, those values will be replaced with the group name.
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
#' lineChart( weight ~ Time * Diet, ChickWeight, settings = settings, legendPosition="topleft" )
buildGroupSettings = function(group, groupLabel=NULL, color=NULL, fillColor=NULL, symbol=NULL,
                              cex.symbol=NULL, width.errBar=NULL, lty=NULL, lwd=NULL,
                              include=NULL, suppressWarnings=FALSE) 
{
	if (!suppressWarnings && length(unique(group)) < length(group)) {
		warning("There are duplicate elements in group.")
	}
	group = unique(group)

  ng = length(group)
  #if (ng == 1) {
  #	group = 0
  #}
  
  usedDefaults = NULL
  
  if (is.null(groupLabel)) {
    groupLabel = group
  } else {
    groupLabel[ is.na(groupLabel) ] = group[ is.na(groupLabel) ]
  }
  
  if (is.null(symbol)) {
    usedDefaults = append(usedDefaults, "symbol")
    bestSymbols = c(21:25, 0:20)

    while (length(group) > length(bestSymbols)) {
    	bestSymbols = c(bestSymbols, bestSymbols)
    }

    symbol = bestSymbols[1:length(group)]
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
    if (ng == 1) {
    	color = "black"
    }
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
                  groupLabel = groupLabel,
                  color = color,
                  fillColor = fillColor,
                  symbol = symbol,
                  cex.symbol = cex.symbol,
                  lty = lty,
                  lwd = lwd,
  								width.errBar = width.errBar,
                  include = include,
                  stringsAsFactors=FALSE
  )
  
  if (!is.null(usedDefaults) && !suppressWarnings) {
    cat( paste("Note: Plotting defaults used for:", paste(usedDefaults, collapse=", "), "\n") )
  }
  
  df
}

getSettingsDfColumnNames = function() {
	names(buildGroupSettings(0, suppressWarnings = TRUE))
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
  settings = subset(agg, select=getSettingsDfColumnNames())
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
    
    for (col in getSettingsDfColumnNames()) {
      plotDf[ plotRows, col ] = settings[ setRows, col ]
    }
  }
  plotDf
}

replaceMissingSettings = function(settings) {
  gsn = getSettingsDfColumnNames()
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


# This takes scalar x and mean and two element CI
drawErrorBars = function(x, mean, CI, width=.1*par()$cex, color="black", lty=1, lwd=1, vertical=TRUE) {
	
	x0 = rep(x, 2)
	x1 = x0
	y0 = rep(mean, 2)
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

    	if (nrow(pl) >= 2) {
	    	for (i in 1:(nrow(pl) - 1)) {
	    	
	      	lines(pl$x[c(i, i+1)], pl$y[c(i, i+1)], col=pl$color[i], lwd=pl$lwd[i], lty=pl$lty[i])
	    	}
    	}
	
			points(pl$x, pl$y, pch=pl$symbol, 
				bg=pl$fillColor, col=pl$color,
				cex=pl$cex.symbol, lwd=pl$lwd)

    }
  }
}


#' Create a legend based on a plotting data frame with group appearance settings
#' 
#' @param position The position of the legend. If "CHOOSE_BEST", the best legend position is selected. Otherwise, this is passed to the `x` argument of legend().
#' @param plotDf The plotting data frame from which settings will be extracted and used to create the legend.
#' @param ... A few variable arguments are passed through to legend(): y, cex, inset, title, box.lwd, and horiz.
#' @export
legendFromPlottingDf = function(position, plotDf, ...) {
	
	vargs = list(...)
	
	settings = extractGroupSettings(plotDf)
	
	if (position == "CHOOSE_BEST") {
		positionLegendFunction = function(pos) {
			coreLegendFunction(position = pos, settings = settings, plot=FALSE, vargs=vargs)
		}
		
		points = cbind(plotDf$x, plotDf$y)
		
		points = rbind(points, cbind(plotDf$x, plotDf$y + plotDf$ebLower))
		points = rbind(points, cbind(plotDf$x, plotDf$y + plotDf$ebUpper))
		
		yrange = range(points[,2])
		yDist = yrange[2] - yrange[1]
		
		bestPos = NULL
		for (padding in seq(yDist / 10, 0, length.out=4)) {
			bestPos = getBestLegendPositions(points, positionLegendFunction, padding)
			if (min(bestPos$numPointsIn) == 0) {
				break
			}
		}
		
		position = bestPos[ bestPos$isBest, "pos" ][1]
	}
	
	coreLegendFunction(position = position, settings = settings, plot=TRUE, vargs=vargs)
}

#' Create a legend based on group appearance settings
#' 
#' @param position The position of the legend. This is passed to the `x` argument of legend().
#' @param settings The settings data frame that will be used to create the legend.
#' @param ... A few variable arguments are passed through to legend(): y, cex, inset, title, box.lwd, and horiz.
#' @export
legendFromSettings = function(position, settings, ...) {
	vargs = list(...)
	
	coreLegendFunction(position = position, settings = settings, plot=TRUE, vargs=vargs)
}


coreLegendFunction = function(position, settings, plot, vargs) {
	
	settings = settings[ settings$include, ]
	
	legend(x=position, y=vargs$y, 
				 legend=settings$groupLabel, 
				 col=settings$color, 
				 pt.bg=settings$fillColor, 
				 pch=settings$symbol, 
				 pt.cex=settings$cex.symbol, 
				 lwd = settings$lwd, 
				 lty = settings$lty,
				 inset = valueIfNull(vargs$inset, 0),
				 cex = valueIfNull(vargs$cex, 1),
				 box.lwd = valueIfNull(vargs$box.lwd, par()$lwd),
				 horiz = valueIfNull(vargs$horiz, FALSE),
				 title = vargs$title, 
				 plot = plot
	)
	
}




getBestLegendPositions = function(points, legendFunction, padding = 0) {
	
	allPositions = c("center", "left", "right", "top", "bottom", "topleft", "topright", "bottomleft", "bottomright")
	positionPreference = c(3, rep(2, 4), rep(1, 4))
	
	res = data.frame(pos=allPositions, positionPreference = positionPreference, 
									 numPointsIn=NA, isBest = FALSE, stringsAsFactors = FALSE)
	
	for (i in 1:length(allPositions)) {
		pointsIn = testLegendPosition(allPositions[i], points, legendFunction, padding)
		res$numPointsIn[i] = sum(pointsIn)
	}

	res$isBest[ res$numPointsIn == min(res$numPointsIn) ] = TRUE
	
	#Deselect those with less that best preference of the remaining ones
	bestPreference = min(res$positionPreference[ res$isBest ])
	res$isBest[ res$isBest == TRUE & res$positionPreference > bestPreference ] = FALSE

	res
}

testLegendPosition = function(position, points, legendFunction, padding) {
	
	rect = legendFunction(position)$rect
	
	rect = addPadding(position, rect, padding)
	
	rect$right = rect$left + rect$w
	rect$bottom = rect$top - rect$h
	
	pointIn = rep(FALSE, nrow(points))
	
	for (i in 1:nrow(points)) {
		x = points[i,1]
		y = points[i,2]
		
		inLR = x > rect$left && x < rect$right
		inTB = y > rect$bottom && y < rect$top
		
		pointIn[i] = inLR && inTB
	}
	
	pointIn
}

addPadding = function(position, rect, padding) {
	
	rect$w = rect$w + padding
	rect$h = rect$h + padding
	
	if (grepl("left", position, fixed=TRUE)) {
		#do nothing
	} else if (grepl("right", position, fixed=TRUE)) {
		rect$left = rect$left - padding
	} else {
		rect$left = rect$left - padding / 2
	}
	
	if (grepl("top", position, fixed=TRUE)) {
		#do nothing
	} else if (grepl("bottom", position, fixed=TRUE)) {
		rect$top = rect$top + padding
	} else {
		rect$top = rect$top + padding / 2
	}
	
	rect
}



is.null.or.na = function(x) {
	is.null(x) || all(is.na(x))
}

valueIfNull = function(x, value) {
	if (is.null(x)) {
		x = value
	}
	x
}



getErrorBarFunctionFromName = function(errBarType) {
	
	if (errBarType == "SE") {
		
		errorBarFunction = function(x) { 
			y = stats::sd(x) / sqrt(length(x))
			list(eb=c(-y, y), includesCenter=FALSE)
		}
		
	}	else if (errBarType == "SD") {
		
		errorBarFunction = function(x) { 
			y = stats::sd(x)
			list(eb=c(-y, y), includesCenter=FALSE)
		}
		
	} else if (errBarType == "CI95") {
		
		errorBarFunction = function(x) {
			
			stdErr = stats::sd(x) / sqrt(length(x))
			tq = stats::qt(c(0.025, 0.975), df=length(x) - 1)
			confInt = mean(x) + stdErr * tq
			
			list(eb=confInt, includesCenter=TRUE)
		}
		
	} else if (errBarType == "Cred95") {
		
		errorBarFunction = function(x) {
			
			if (var(x) == 0) {
				return(list(eb=c(0,0), includesCenter=FALSE))
			}
			
			if (!requireNamespace("BayesFactor", quietly = TRUE)) {
				stop("The package BayesFactor is needed for credible intervals. Please install it.", call. = FALSE)
			}
			
			tp = BayesFactor::ttestBF(x, iterations=10000, posterior=TRUE, progress=FALSE)
			mu = tp[,"mu"]
			
			qs = as.numeric(stats::quantile(mu, c(0.025, 0.975)))
			
			list(eb=qs, includesCenter=TRUE)
		}
		
	} else {
		stop("Unknown errBarType.")
	}
	
	errorBarFunction
}


getCentralTendencyFunctionFromName = function(centralTendencyType) {
	
	if (centralTendencyType == "mean") {
		centralTendencyFun = mean
	} else if (centralTendencyType == "median") {
		centralTendencyFun = stats::median
	}	else {
		stop("Unknown centralTendencyType.")
	}
	
	centralTendencyFun
}
