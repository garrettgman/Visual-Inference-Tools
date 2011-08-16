# plotting methods for displaying two dimensional data that has one numeric dimension and one categorical dimension. These methods rely on the methods for 1d numeric data.
load_mixed_2d <- function()
	PLOT_DATA <<- plotPointGroups
	
# plotPoints and plotBoxplot are defined in the data-methods-numeric-1d.R file
plotPointGroups <- function(canvas) {
	canvas$y <- stackPoints(canvas$x, canvas$levels, vp = graphPath("data"))
	n <- 1
	for (i in unique(canvas$levels)) {
		plotPoints(canvas, canvas$x[canvas$levels == i], 
			canvas$y[canvas$levels == i],
			vp = graphPath("data", as.character(n)),
			name = "dataPlot")
		plotBoxplot(canvas, canvas$x[canvas$levels == i], 
			vp = graphPath("data", as.character(n)),
			name = "dataPlot")
		n <- n + 1
	}
}