# Fills in the correct details for PLOT_DATA, CALC_STAT, CALC_STAT_DIST, PLOT_STAT, PLOT_STAT_DIST, DISPLAY_RESULT based on x, y, and method. Method should be the selected value of the vit GUI e$stat combobox.
loadPlotDetails <- function(x, y) {
	if (is.null(y)) {
		if (is.categorical(x)) {
			PLOT_DATA <<- plotProportionBars
			PLOT_SAMPLE <<- plotSampleProportionBars
		} else {
			PLOT_DATA <<- plotPointsAndBoxplot
			PLOT_SAMPLE <<- plotSamplePointsAndBoxplot
		}
	} else if (is.categorical(y)) {
		PLOT_STAT <<- plotArrow

		if (is.categorical(x)) {
			PLOT_DATA <<- plotProportionGroups
			PLOT_SAMPLE <<- plotSampleProportionGroups
		} else {
			PLOT_DATA <<- plotPointGroups
			PLOT_SAMPLE <<- plotSamplePointGroups
		}
	} else {
		PLOT_DATA <<- notYetImplemented
		PLOT_SAMPLE <<- notYetImplemented
	}
}

loadPlotStat <- function(stat.method) {
	PLOT_STAT <<- c("mean" = plotTriangle, "median" = plotTriangle, 
		"confidence interval - mean" = plotCI,
		"confidence interval - median" = plotCI)[stat.method]
}

loadStat <- function(stat, method){
    if (substr(stat, 1, 10) == "confidence"){
        CALC_STAT <<- c(calcCIWald, calcCIBootPerc, calcCIBootSE, 
        	calcCIBootTSE)[[which(method == c("normal", "percentile bootstrap", 
        	"normal bootstrap", "t bootstrap"))]]
    } else {
        CALC_STAT <<- c(mean, median)[[which(stat == c("mean", "median"))]]
    }
}