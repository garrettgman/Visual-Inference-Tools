# The high level actions performed by the VIT Canvas. The details of how these methods accomplish their goals is set by loadDetails, which chooses the appropriate method based on the type of sata and the statistic being examined

PLOT_DATA <- function(canvas) 
	warning("PLOT_DATA details must be set by loadDetails()")
	
CALC_STAT <- function(canvas) 
	warning("CALC_STAT details must be set by loadDetails()")
	
CALC_STAT_DIST <- function(canvas)
	warning("CALC_STAT_DIST details must be set by loadDetails()")
	
PLOT_STAT <- function(canvas)
	warning("PLOT_STAT details must be set by loadDetails()")

PLOT_STAT_DIST <- function(canvas)
	warning("PLOT_STAT_DIST details must be set by loadDetails()")

DISPLAY_RESULT <- function(canvas)
	warning("DISPLAY_RESULT details must be set by loadDetails()")
	

# Fills in the correct details for PLOT_DATA, CALC_STAT, CALC_STAT_DIST, PLOT_STAT, PLOT_STAT_DIST, DISPLAY_RESULT based on x, y, and method. Method should be the selected value of the vit GUI e$stat combobox.	
loadDetails <- function(x, y, method) {
	confidenceCheck(x, y, method)
	if (is.null(y)) {
		if (is.categorical(x)) {
			PLOT_DATA <<- plotProportionBars
		} else {
			PLOT_DATA <<- plotPoints
		}
			CALC_STAT <<- list(mean = calcMean, median = calcMedian, 
				"confidence interval" = calcCI)[[method]]
			PLOT_STAT_DIST <<- list(mean = plotTriangleDist, 
				median = plotTriangleDist, 
				"confidence interval" = plotCIStack)[[method]]		
	} else if (is.categorical(y)) {
		CALC_STAT <<- list(mean = calcDiffMean, 
			median = calcDiffMedian)[[method]]
		PLOT_STAT <<- plotArrow
		PLOT_STAT_DIST <<- plotTriangleDist	
			
		if (is.categorical(x)) {
			PLOT_DATA <<- plotProportionGroups
		} else {
			PLOT_DATA <<- plotPointGroups
		}		
	} else {
		CALC_STAT <<- notYetImplemented
		PLOT_STAT <<- notYetImplemented
		PLOT_DATA <<- notYetImplemented			
	}
}

is.categorical <- function(x) {
	inherits(x, c("character", "factor"))
}

notYetImplemented <- function(...) {
	require(grid)
	grid.newpage()
	grid.text("This feature has not yet been implemented.")
}
