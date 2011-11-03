# Fills in the correct details for PLOT_DATA once a variable has been
# loaded. You can make a certain statistical method override these
# details by having loadStatDetails reset PLOT_DATA for that method.
loadPlotDetails <- function(e, canvas) {
    x <- e$xData
    y <- e$yData
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
    if (e$data.boxes) GRAPHPATH <<- graphPathBoxes else GRAPHPATH <<- graphPathNoBoxes
}

# calls the load function for the selected statistic method. The load
# functions are stored together with the details they load in the
# methods file for each method
loadStatDetails <- function(e) {
	stat.method <- paste(e$method, svalue(e$stat), sep = ".")
	list("ci.mean" = load_CI_mean, "ci.median" = load_CI_median,
             "bootstrap.mean" = load_bootstrap_mean,
             "bootstrap.median" = load_bootstrap_median,
             "permutation.mean" = load_permutation_mean,
             "permutation.median" = load_permutation_median)[[stat.method]](e)
	e$loaded <- TRUE
}

