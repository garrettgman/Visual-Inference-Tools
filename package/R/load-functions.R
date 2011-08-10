# Fills in the correct details for PLOT_DATA, CALC_STAT, CALC_STAT_DIST, PLOT_STAT, PLOT_STAT_DIST, DISPLAY_RESULT based on x, y, and method. Method should be the selected value of the vit GUI e$stat combobox.
loadPlotDetails <- function(x, y) {
	if (is.null(y)) {
		if (is.categorical(x)) {
			PLOT_DATA <<- plotProportionBars
			PLOT_SAMPLE <<- plotSampleProportionBars
			ANIMATE_SAMPLE <<- notYetImplemented
		} else {
			PLOT_DATA <<- plotPointsAndBoxplot
			PLOT_SAMPLE <<- plotPointsAndBoxplot
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

		 
	if (stat.method == "confidence interval - mean") { 
		ci.method <- svalue(e$cimeth)
		CALC_STAT <<- c("normal" = calcCIWald, 
			"percentile bootstrap" = calcCIBootPercMean, 
			"normal bootstrap" = calcCIBootSEMean, 
			"t bootstrap" = calcCIBootTSEMean)[[ci.method]]
		addLine(e$c1, mean) 
		ANIMATE_STAT <<- dropCI
		PLOT_STAT_DIST <<- plotCIDistMean
		HANDLE_1000 <<- ci1000
		DISPLAY_RESULT <<- CIcounter
		MISCELLANEOUS <<- ci_miscellaneous
	} else if (stat.method == "confidence interval - median") {
		ci.method <- svalue(e$cimeth)
		CALC_STAT <<- c("percentile bootstrap" = calcCIBootPercMedian, 
			"normal bootstrap" = calcCIBootSEMedian, 
			"t bootstrap" = calcCIBootTSEMedian)[[ci.method]]
		addLine(e$c1, median)
		ANIMATE_STAT <<- dropCI
		PLOT_STAT_DIST <<- plotCIDistMedian
		HANDLE_1000 <<- ci1000
		DISPLAY_RESULT <<- CIcounter
		MISCELLANEOUS <<- ci_miscellaneous
	} else {
		CALC_STAT <<- c("mean" = mean, "median" = median)[[stat.method]]
		ANIMATE_STAT <<- dropTriangle
		PLOT_STAT_DIST <<- notYetImplemented
		HANDLE_1000 <<- notYetImplemented
		DISPLAY_RESULT <<- notYetImplemented
		MISCELLANEOUS <<- notYetImplemented
	}
}