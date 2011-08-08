# plotting methods for displaying one dimensional, numeric data

#' Used for displaying numeric data. Plots a group of points with a boxplot in
#' the specified viewport. The points will be stacked on top of each other in an
#' intelligent manner when necessary.
#'
#' @param canvas a canvasClass object made with the generator object canvas
# NOTE: should plotPoints be replaced with a points GROB that calculates stacking in drawDetails? So stacking changes as the plot is resized? ANSWER: Because we would have to repeat all of these calculations for every step of the animations, which would really slow things down.
#' PLOT_DATA method for numeric, 1d data. Also used for 2d data when one varible is numeric
plotPointsAndBoxplot <- function(canvas, x, vp, name) {
	plotPoints(canvas, x, vp, name)
	plotBoxplot(canvas, x, vp, name)
}

	
#' helper function for plotting numeric data
plotPoints <- function(canvas, x, vp, name) {
	y <- stackPoints(x, vp)
	
	canvas$image <- addGrob(canvas$image, pointsGrob(x = x, y = y, vp = vp, 
		name = paste(name, "points", sep = "."), gp = gpar(col = "grey50", 
		lwd = 2)))
}

#' helper function for plotting numeric data
plotBoxplot <- function(canvas, x, vp, name) {
	bp.name <- paste(name, "boxplot", vpNumber(vp), sep = ".")
	box.plot <- boxplotGrob(data = x, name = bp.name, vp = vp)
	canvas$image <- addGrob(canvas$image, box.plot)
}

#' helper function for plotting numeric data
plotHist <- function(canvas, x, vp, name) {
	boxes <- length(hist(x, plot = FALSE)$mids)
	canvas$image <- addGrob(canvas$image, histGrob(x, breaks = seq(min(x), 
		max(x), length.out = boxes), freq = 0.8, name = paste(name, "hist", 
		sep = "."), vp = vp))
}

#' PLOT_SAMPLE method for numeric, 1d data. Also used for 2d data when one varible is numeric
plotSamplePointsAndBoxplot <- function(canvas, x, vp, name) {
	plotPoints(canvas, x, vp, name)
	plotSampleBoxplot(canvas, x, vp, name)
}

#' helper function for plotting numeric samples. Unlike plotBoxplot, plotSampleBoxplot manages the creation and maitenance of ghost boxplots
plotSampleBoxplot <- function(canvas, x, vp, name) {
	bp.name <- paste(name, "boxplot", vpNumber(vp), sep = ".")
	ghosts.name <- paste(name, "ghosts", vpNumber(vp), sep = ".")

	if (bp.name %in% childNames(canvas$image)) {
		old.bp <- getGrob(canvas$image, gPath(bp.name))

		if (ghosts.name %in% childNames(canvas$image)) {
			ghosts <- getGrob(canvas$image, gPath(ghosts.name))
			ghosts <- updateGhosts(ghosts, old.bp)
		} else {
			ghosts <- makeGhosts(old.bp, vp = vp, name = ghosts.name)
		}
		canvas$image <- addGrob(canvas$image, ghosts)
	}
	box.plot <- boxplotGrob(data = x, name = bp.name, vp = vp)
	canvas$image <- addGrob(canvas$image, box.plot)
}


#' Animates a sample of points dropping down from the collection of points in the data window. The ANIMATE_SAMPLE method for numeric, one dimensional data.
dropPoints <- function(canvas, n.steps) {
	index <- canvas$samples[[canvas$which.sample]]
	x <- canvas$x[index]
	y.start <- canvas$y[index] + 2 # to place in data vp
	y.end <- stackPoints(x, graphPath("sample")) + 1
	
	n <- length(x):1
	m <- length(x)
	
	step <- (max(y.start) - 1.5) / n.steps
	
	if ("samplePlot.points" %in% childNames(canvas$image))
		canvas$image <- removeGrob(canvas$image, gPath(c("samplePlot.points")))
	
	for (i in 1:(length(x) + n.steps)) {
		
		o <- pmax(n - m, 0) * step
		canvas$image <- addGrob(canvas$image, pointsGrob(x, 
			y = pmax(y.start - o, y.end), vp = vpPath("canvas.frame", 
			"animation.field"), gp = gpar(lwd = 2, col = "grey50"), 
			name = "temp"))
		if (i < length(x)) {
			canvas$image <- addGrob(canvas$image, pointsGrob(x[i], 
				y = y.start[i], vp = vpPath("canvas.frame", 
				"animation.field"), pch = 19, 
				name = "highlight"))	
		} else {
			canvas$image <- addGrob(canvas$image, pointsGrob(NA, 
				y = NA, vp = vpPath("canvas.frame", 
				"animation.field"), gp = gpar(fill = "black"), 
				name = "highlight"))	
		}
		canvas$drawImage()
		m <- m - 1
	}
	canvas$image <- removeGrob(canvas$image, gPath(c("temp")))
	canvas$image <- removeGrob(canvas$image, gPath(c("highlight")))
}

dropCI <- function(canvas, nsteps) {

}

#' Calculates y values for a vector of overlapping x values. Y values are designed to prevent overlapping by creating the appearance of stacked data. A helper function for numeric 1d Data. Also used for 2d data where one dimension is numeric, and one is categorical.
stackPoints <- function(x, vp, y.min = 0.5, y.max = 1) {
		if (length(x) <= 1) df <- data.frame(x = x, y = y.min)
		else {
			seekViewport(vp)
			pheight <- convertHeight(unit(1, "char"), "native",
				valueOnly = TRUE) * 0.8
			binwidth <- convertWidth(unit(1, "char"), "native", 
				valueOnly = TRUE)
			upViewport(0)
			
  			nbins <- ceiling(diff(range(x)) / binwidth)
			breaks <- min(x) + c(0:(nbins)) * binwidth
			group <- cut(x, breaks, include.lowest = TRUE)
			max.stack <- max(table(group))
			ydist <- min(diff(c(y.min, y.max))/max.stack, as.numeric(pheight))
			df <- data.frame(x = x, group = group)
			df$y <- NA
			for (i in levels(group)) {
				igroup <- which(df$group == i)
				j <- nrow(df[igroup, ])
				if (j > 0) df[igroup, ]$y <- seq_len(j)
			}

			df$y <- (df$y - 1) * ydist + y.min

		}
		
		df$y
		
}