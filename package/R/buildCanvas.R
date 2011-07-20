is.categorical <- function(x) {
	inherits(x, c("character", "factor"))
}

notYetImplemented <- function(...) {
	require(grid)
	grid.newpage()
	grid.text("This feature has not yet been implemented.")
}



plotProportionBars <-
plotPoints <-
calcMean <-
calcMedian <-
calcCI <-
plotTriangleDist <-
plotCIStack <-
calcDiffMean <-
calcDiffMedian <-
plotArrow <-
splitViewports <-
plotTriangleDist <-
yAxisViewports <- function() {
	notYetImplemented()
	stop("method does not exist yet.")
}
		
		
		
	e$buildCanvas <- function() {
		if (is.null(e$xData)) {
			grid.newpage()
			grid.text("Please select Variable 1")
			return()
		}
		if (is.categorical(e$xData) & !is.categorical(e$yData) & !is.null(e$yData))
			reverseVariables()
	
		method <- svalue(e$stat)
		e$c1 <- canvas$new(x = e$xData, y = e$yData)
	
		if (is.categorical(e$xData)) {
			e$c1$plotData <- plotProportionBars
			e$c1$viewports <- makeViewports(e$xData, proportion = TRUE)
		} else {
			e$c1$plotData <- plotPoints
			e$c1$viewports <- makeViewports(e$xData)
		}
	
		if (is.null(e$yData)) {
			e$c1$calcStatistic <- list(mean = calcMean, median = calcMedian, 
				"Confidence Interval" = calcCI)[method]
			e$c1$plotStatDist <- list(mean = plotTriangleDist, 
				median = plotTriangleDist, 
				"Confidence Interval" = plotCIStack)[method]
		} else if (is.categorical(e$yData)) {
			e$c1$calcStatistic <- list(mean = calcDiffMean, 
				median = calcDiffMedian)[method]
			e$c1$plotStatistic <- plotArrow
			e$c1$viewports <- splitViewports(c1$viewports, 
				levels = nlevel(e$yData))
			e$c1$plotStatDist <- plotTriangleDist
		} else {
			e$c1$viewports <- yAxisViewports(c1$viewports, e$yData)
			e$c1$calcStatistic <- notYetImplemented
			e$c1$plotStatistic <- notYetImplemented
			e$c1$plotData <- notYetImplemented
		}
		
	e$c1$drawCanvas()
	
	}
