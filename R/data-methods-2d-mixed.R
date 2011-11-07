# plotting methods for displaying two dimensional data that has one numeric dimension and one categorical dimension. These methods rely on the methods for 1d numeric data.
load_mixed_2d <- function()
	PLOT_DATA <<- plotPointGroups

# plotPoints and plotBoxplot are defined in the data-methods-numeric-1d.R file
plotPointGroups <- function(canvas) {
	canvas$y <- stackPoints(canvas$x, canvas$levels, vp = canvas$graphPath("data"), y.min = 0.3, y.max = 1)
	n <- 1
	for (i in unique(canvas$levels)) {
		plotPoints(canvas, canvas$x[canvas$levels == i],
			canvas$y[canvas$levels == i],
			vp = canvas$graphPath("data", as.character(n)),
			name = "dataPlot")
		plotBoxplot(canvas, canvas$x[canvas$levels == i],
			stat = NULL, stat.color = NULL, vp = canvas$graphPath("data", as.character(n)),
			name = "dataPlot")
		n <- n + 1
	}
}
