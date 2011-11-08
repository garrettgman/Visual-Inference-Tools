# plotting methods for displaying two dimensional data that has one numeric dimension and one categorical dimension. These methods rely on the methods for 1d numeric data.
load_mixed_2d <- function()
	PLOT_DATA <<- plotPointGroupsAndBoxplot

# plotPoints and plotBoxplot are defined in the data-methods-numeric-1d.R file
plotPointGroupsAndBoxplot <- function(canvas) {
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

plotPointGroups <- function(canvas, x, y, levels, graph, cols = NULL, name){
    if (is.null(cols)) cols = "grey60"
    cols <- rep(cols, length = length(unique(levels)))
    n <- 1
    for (i in unique(levels)) {
        plotPoints(canvas, x[levels == i],
                   y[canvas$levels == i], col = cols[n],
                   vp = canvas$graphPath(graph, as.character(n)),
                   name = name)
        n <- n + 1
    }
}
