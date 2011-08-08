# Fills in the correct details for PLOT_DATA, CALC_STAT, CALC_STAT_DIST, PLOT_STAT, PLOT_STAT_DIST, DISPLAY_RESULT based on x, y, and method. Method should be the selected value of the vit GUI e$stat combobox.
loadPlotDetails <- function(x, y) {
	if (is.null(y)) {
		if (is.categorical(x)) {
			PLOT_DATA <<- plotProportionBars
			PLOT_SAMPLE <<- plotSampleProportionBars
			ANIMATE_SAMPLE <<- notYetImplemented
		} else {
			PLOT_DATA <<- plotPointsAndBoxplot
			PLOT_SAMPLE <<- plotSamplePointsAndBoxplot
			ANIMATE_SAMPLE <<- dropPoints
		}
	} else if (is.categorical(y)) {
		PLOT_STAT <<- plotArrow

		if (is.categorical(x)) {
			PLOT_DATA <<- plotProportionGroups
			PLOT_SAMPLE <<- plotSampleProportionGroups
			ANIMATE_SAMPLE <<- notYetImplemented
		} else {
			PLOT_DATA <<- plotPointGroups
			PLOT_SAMPLE <<- plotSamplePointGroups
			ANIMATE_SAMPLE <<- notYetImplemented
		}
	} else {
		PLOT_DATA <<- notYetImplemented
		PLOT_SAMPLE <<- notYetImplemented
		ANIMATE_SAMPLE <<- notYetImplemented
	}
}

loadStatDetails<- function(e) {
	stat.method <- svalue(e$stat)
	PLOT_STAT <<- c("mean" = plotTriangle, "median" = plotTriangle, 
		"confidence interval - mean" = plotCI,
		"confidence interval - median" = plotCI)[[stat.method]]
	PLOT_STAT_DIST <<- notYetImplemented
	ANIMATE_STAT <<- notYetImplemented
	DISPLAY_RESULTS <<- notYetImplemented
		 
	if (stat.method %in% c("confidence interval - mean", 
		"confidence interval - median")) {
			ci.method <- svalue(e$cimeth)
			CALC_STAT <<- c("normal" = calcCIWald, 
				"percentile bootstrap" = calcCIBootPerc, 
				"normal bootstrap" = calcCIBootSE, 
				"t bootstrap" = calcCIBootTSE)[[ci.method]]
	} else {
		CALC_STAT <<- c("mean" = mean, "median" = median)[[stat.method]]
	}
}