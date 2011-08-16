# Fills in the correct details for PLOT_DATA once a variable has been loaded. You can make a certain statistical method override these details by having loadStatDetails reset PLOT_DATA for that method.
loadPlotDetails <- function(x, y) {
	if (is.null(y)) {
		if (is.categorical(x)) {
			load_categorical_1d()
		} else {
			load_numeric_1d()
		}
	} else if (is.categorical(y)) {

		if (is.categorical(x)) {
			load_categorical_2d()
		} else {
			load_mixed_2d()
		}
	} else {
		load_numeric_2d()
	}
}

# calls the load function for the selected statistic method. The load functions are stored together with the details they load in the methods file for each method
loadStatDetails <- function(e) {
	stat.method <- svalue(e$stat)
	e$c1$n <- as.numeric(svalue(e$ssize))
	enabled(e$show.ci) <- FALSE
	list("mean" = load_bootstrapping_mean,
		"median" = load_bootstrapping_median,
		"confidence interval - mean" = load_CI_mean,
		"confidence interval - median" = load_CI_median)[[stat.method]](e)
	e$loaded <- TRUE
}
