# plotting methods for displaying two dimensional data that has one numeric dimension and one categorical dimension. These methods rely on the methods for 1d numeric data.

# PLOT_DATA method for mixed 2d data
plotPointGroups <- function(canvas, x, vp, name) {
	n <- 1
	for (i in unique(canvas$y)) {
		plotPointsAndBoxplot(canvas, x[canvas$y == i],
			vp = appendPath(vp, n),
			name = paste(name, n, sep = "."))
		n <- n + 1
	}
}

# PLOT_SAMPLE method for mixed 2d data
plotSamplePointGroups <- function(canvas, x, vp, name) {
	n <- 1
	for (i in unique(canvas$y)) {
		plotSamplePointsAndBoxplot(canvas, x[canvas$y == i],
			vp = appendPath(vp, n),
			name = paste(name, n, sep = "."))
		n <- n + 1
	}
}