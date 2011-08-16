# The high level actions performed by the VIT Canvas. The details of how these methods accomplish their goals is set by loadDetails, which chooses the appropriate method based on the type of sata and the statistic being examined

#' Creates the initial plot of the data 
PLOT_DATA <- function(canvas)
	warning("PLOT_DATA details must be set by loadPlotDetails()")

#' Creates the initial plot of the sample
PLOT_SAMPLE <- function(canvas)
	warning("PLOT_SAMPLE details must be set by loadDetails()")
	
#' Animates the construction of the sample from the data
ANIMATE_SAMPLE <- function(canvas)
	warning("ANIMATE_SAMPLE details must be set by loadDetails()")

#' Method used to calculate a single statistic from a single group of observsations
CALC_STAT <- function(canvas)
	warning("CALC_STAT details must be set by loadDetails()")

#' Creates a plot of the data parameter
PLOT_DATA_STAT <- function(canvas)
	warning("PLOT_STAT details must be set by loadDetails()")
	
#' Creates a plot of the sample statistic
PLOT_SAMPLE_STAT <- function(canvas)
	warning("PLOT_STAT details must be set by loadDetails()")

#' Creates a plot of the distribution of sample statistics
PLOT_STAT_DIST <- function(canvas)
	warning("PLOT_STAT_DIST details must be set by loadDetails()")
	
#' Animates the addition of the current sample statistic to the statistic distribution
ANIMATE_STAT <- function(canvas)
	warning("ANIMATE_STAT details must be set by loadDetails()")

#' Displays any final conlusions or information required by the method whenever a statistic is added to the statistic distribution
DISPLAY_RESULT <- function(canvas)
	warning("DISPLAY_RESULT details must be set by loadDetails()")

#' Manages the methods' performance of 1000 complete runs. At the moment this has its own method because running 1000 methods the usual way would be time prohibitive.
HANDLE_1000 <- function(e)
	warning("HANDLE_1000 details must be set by loadDetails()")
	
test_function <- function(canvas)
	warning("test_function details must be set by loadDetails()")
	
#' returns all actions to their original empty values, with the exception of PLOT_DETAILS. Since clear_actions() is only called when a new variable is added,  it will be followed by e$build_canvas, which will change PLOT_DATA if necessary
clear_actions <- function(e) {
	PLOT_SAMPLE <<- function(canvas)
		warning("PLOT_SAMPLE details must be set by loadDetails()")
	
	ANIMATE_SAMPLE <<- function(canvas)
		warning("ANIMATE_SAMPLE details must be set by loadDetails()")

	CALC_STAT <<- function(canvas)
		warning("CALC_STAT details must be set by loadDetails()")

	PLOT_DATA_STAT <<- function(canvas)
		warning("PLOT_STAT details must be set by loadDetails()")
	
	PLOT_SAMPLE_STAT <<- function(canvas)
		warning("PLOT_STAT details must be set by loadDetails()")

	PLOT_STAT_DIST <<- function(canvas)
		warning("PLOT_STAT_DIST details must be set by loadDetails()")
	
	ANIMATE_STAT <<- function(canvas)
		warning("ANIMATE_STAT details must be set by loadDetails()")

	DISPLAY_RESULT <<- function(canvas)
		warning("DISPLAY_RESULT details must be set by loadDetails()")

	HANDLE_1000 <<- function(e)
		warning("HANDLE_1000 details must be set by loadDetails()")

	MISCELLANEOUS <<- function(env)
		warning("MISCELLANEOUS details must be set by loadDetails()")
	
	e$loaded <- FALSE
}
	