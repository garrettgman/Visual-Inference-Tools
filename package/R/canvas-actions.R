# The high level actions performed by the VIT Canvas. The details of how these methods accomplish their goals is set by loadActions, which chooses the appropriate method based on the type of sata and the statistic being examined

PLOT_DATA <- function() 
	warning("PLOT_DATA details must be set by canvas$loadMethods()")
	
CALC_STAT <- function() 
	warning("CALC_STAT details must be set by canvas$loadMethods()")
	
CALC_STAT_DIST <- function()
	warning("CALC_STAT_DIST details must be set by canvas$loadMethods()")
	
PLOT_STAT <- function()
	warning("PLOT_STAT details must be set by canvas$loadMethods()")

PLOT_STAT_DIST <- function()
	warning("PLOT_STAT_DIST details must be set by canvas$loadMethods()")

DISPLAY_RESULT <- function()
	warning("DISPLAY_RESULT details must be set by canvas$loadMethods()")
	

# Fills in the correct details for PLOT_DATA, CALC_STAT, CALC_STAT_DIST, PLOT_STAT, PLOT_STAT_DIST, DISPLAY_RESULT based on x, y, and method. Method should be the selected value of the vit GUI e$stat combobox.	
loadActions <- function(x, y, method) {
		confidenceCheck(x, y, method)
		if (is.categorical(x)) {
			PLOT_DATA <- plotProportionBars
		} else {
			PLOT_DATA <- plotPoints
		}
	
		if (is.null(y)) {
			CALC_STAT <- list(mean = calcMean, median = calcMedian, 
				"confidence interval" = calcCI)[[method]]
			PLOT_STAT_DIST <- list(mean = plotTriangleDist, 
				median = plotTriangleDist, 
				"confidence interval" = plotCIStack)[[method]]		
		} else if (is.categorical(y)) {
			CALC_STAT <- list(mean = calcDiffMean, 
				median = calcDiffMedian)[[method]]
			PLOT_STAT <- plotArrow
			PLOT_STAT_DIST <- plotTriangleDist			
		} else {
			CALC_STAT <- notYetImplemented
			PLOT_STAT <- notYetImplemented
			PLOT_DATA <- notYetImplemented			
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
