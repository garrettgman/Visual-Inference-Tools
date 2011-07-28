#' Used for displaying numeric data. Plots a group of points with a boxplot in
#' the specified viewport. The points will be stacked on top of each other in an
#' intelligent manner when necessary.
#'
#' @param canvas a canvasClass object made with the generator object canvas
# NOTE: should plotPoints be replaced with a points GROB that calculates stacking in drawDetails? So stacking changes as the plot is resized? ANSWER: Because we would have to repeat all of these calculations for every step of the animations, which would really slow things down.
plotPoints <- function(canvas, x, vp, name) {
	if (length(x) > 100) plotHist(canvas, x, vp, name)
	else {
		# drawing points
		if (length(x) <= 1) df <- data.frame(x = x, y = 0)
		else {
			seekViewport(vp)

			pheight <- convertY(unit(1, "char"), "native",
				valueOnly = TRUE) * 0.8
			binwidth <- pheight * diff(range(x)) * 0.5
  			nbins <- ceiling(diff(range(x) / binwidth))
			breaks <- min(x) + c(0:(nbins)) * binwidth
			group <- cut(x, breaks, include.lowest = TRUE)
			max.stack <- max(table(group))
			ydist <- min(1/max.stack, as.numeric(pheight))
			df <- data.frame(x = x, group = group)
			df$y <- NA
			for (i in levels(group)) {
				igroup <- which(df$group == i)
				j <- nrow(df[igroup, ])
				if (j > 0) df[igroup, ]$y <- seq_len(j)
			}

			df$y <- (df$y - 1) * ydist
		}

		canvas$image <- addGrob(canvas$image, pointsGrob(x = df$x, y = df$y,
			vp = vp, name = paste(name, "points", sep = "."),
			gp = gpar(col = "grey50")))
		canvas$image <- addGrob(canvas$image, boxplotGrob(data = x,
			name = paste(name, "boxplot", sep = "."), vp = vp))

	}
}

plotHist <- function(canvas, x, vp, name)
    canvas$image <- addGrob(canvas$image, histGrob(x, breaks = seq(min(x), max(x), length.out = 11),
                                                   name = paste(name, "hist", sep = "."), vp = vp))

plotPointGroups <- function(canvas, x, vp, name) {
	n <- 1
	for (i in unique(canvas$y)) {
		plotPoints(canvas, x[canvas$y == i],
			vp = appendPath(vp, n),
			name = paste(name, n, sep = "."))
		n <- n + 1
	}
}

plotProportionGroups <-
plotProportionBars <-
plotTriangleDist <-
plotCIStack <-
plotArrow <-
splitViewports <-
plotTriangleDist <-
yAxisViewports <- function() {
	notYetImplemented()
	stop("method does not exist yet.")
}
