# plotting methods for displaying one dimensional, numeric data

load_numeric_1d <- function() {
	PLOT_DATA <<- plotPointsAndBoxplot
}



#' Used for displaying numeric data. Plots a group of points with a boxplot in
#' the specified viewport. The points will be stacked on top of each other in an
#' intelligent manner when necessary.
#'
#' @param canvas a canvasClass object made with the generator object canvas
# NOTE: should plotPoints be replaced with a points GROB that calculates stacking in drawDetails? So stacking changes as the plot is resized? ANSWER: Because we would have to repeat all of these calculations for every step of the animations, which would really slow things down.
#' PLOT_DATA method for numeric, 1d data. Also used for 2d data when one varible is numeric
plotPointsAndBoxplot <- function(canvas) {
    canvas$y <- stackPoints(canvas$x, vp = canvas$graphPath("data"))
    if (length(canvas$x) >= 1000)
        plotHist(canvas, canvas$x, canvas$graphPath("data"), "dataPlot")
    else {
        plotPoints(canvas, canvas$x, canvas$y, canvas$graphPath("data"), "dataPlot")
        plotBoxplot(canvas, canvas$x, stat = NULL, stat.color = NULL, canvas$graphPath("data"), "dataPlot")
    }
}


#' helper function for plotting numeric data
plotPoints <- function(canvas, x, y, vp, name, black = FALSE, alpha = 1, col = "grey60") {
    if (black){
        pch = 19
        col = "black"
    } else {
        pch = 1
        col = col
    }
    points.name <- paste(name, "points", vpNumber(vp), sep = ".")
    canvas$image <- addGrob(canvas$image,
                            pointsGrob(x = x, y = y, vp = vp, name = points.name,
                                       gp = gpar(col = col, lwd = 2, alpha = alpha), pch = pch))
}

#' helper function for plotting numeric data
plotBoxplot <- function(canvas, x, stat, stat.color, vp, name) {
	bp.name <- paste(name, "boxplot", vpNumber(vp), sep = ".")
	box.plot <- boxplotGrob(data = x, stat = stat, stat.color = stat.color, name = bp.name, vp = vp)
	canvas$image <- addGrob(canvas$image, box.plot)
}

#' helper function for plotting numeric data
plotHist <- function(canvas, x, vp, name) {
	boxes <- length(hist(x, plot = FALSE)$mids)
	canvas$image <- addGrob(canvas$image, histGrob(x, breaks = seq(min(x),
		max(x), length.out = boxes), freq = 0.8, name = paste(name, "hist",
                                                         sep = "."), vp = vp))
}
