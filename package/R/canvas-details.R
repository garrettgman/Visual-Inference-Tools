#' Used for displaying numeric data. Plots a group of points with a boxplot in
#' the specified viewport. The points will be stacked on top of each other in an
#' intelligent manner when necessary.
#'
#' @param canvas a canvasClass object made with the generator object canvas
# NOTE: should plotPoints be replaced with a points GROB that calculates stacking in drawDetails? So stacking changes as the plot is resized? ANSWER: Because we would have to repeat all of these calculations for every step of the animations, which would really slow things down.
plotPointsAndBoxplot <- function(canvas, x, vp, name) {
	plotPoints(canvas, x, vp, name)
	plotBoxplot(canvas, x, vp, name)
}

plotSamplePointsAndBoxplot <- function(canvas, x, vp, name) {
	plotPoints(canvas, x, vp, name)
	plotSampleBoxplot(canvas, x, vp, name)
}


plotPoints <- function(canvas, x, vp, name) {
	if (length(x) > 100) plotHist(canvas, x, vp, name)
	else {
		# drawing points
		if (length(x) <= 1) df <- data.frame(x = x, y = 0)
		else {
			seekViewport(vp)
			pheight <- convertHeight(unit(1, "char"), "native",
				valueOnly = TRUE) * 0.8
			binwidth <- convertWidth(unit(1, "char"), "native", 
				valueOnly = TRUE)
  			nbins <- ceiling(diff(range(x)) / binwidth)
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

			df$y <- (df$y - 1) * ydist + 1
		}
		canvas$image <- addGrob(canvas$image, pointsGrob(x = df$x, y = df$y,
			vp = vp, name = paste(name, "points", sep = "."),
			gp = gpar(col = "grey50", lwd = 2)))

	}
}

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

plotBoxplot <- function(canvas, x, vp, name) {
	bp.name <- paste(name, "boxplot", vpNumber(vp), sep = ".")
	box.plot <- boxplotGrob(data = x, name = bp.name, vp = vp)
	canvas$image <- addGrob(canvas$image, box.plot)
}


plotHist <- function(canvas, x, vp, name){
    boxes <- length(hist(x, plot = FALSE)$mids)
    canvas$image <- addGrob(canvas$image, histGrob(x, breaks = seq(min(x), max(x), length.out = boxes),
                                                   freq = 0.8, name = paste(name, "hist", sep = "."),
                                                   vp = vp))
}

plotCI <- function(canvas, vp, name){
    ci = canvas$calcStat()
    canvas$image <- addGrob(canvas$image, ciGrob(ci = ci, vp = vp,
                                                 name = paste(name, "ci", sep = ".")))
}

plotPointGroups <- function(canvas, x, vp, name) {
	n <- 1
	for (i in unique(canvas$y)) {
		plotPointsAndBoxplot(canvas, x[canvas$y == i],
			vp = appendPath(vp, n),
			name = paste(name, n, sep = "."))
		n <- n + 1
	}
}

plotSamplePointGroups <- function(canvas, x, vp, name) {
	n <- 1
	for (i in unique(canvas$y)) {
		plotSamplePointsAndBoxplot(canvas, x[canvas$y == i],
			vp = appendPath(vp, n),
			name = paste(name, n, sep = "."))
		n <- n + 1
	}
}


plotProportionGroups <-
plotProportionBars <-
plotSampleProportionBars <-
plotSampleProportionGroups <-
plotTriangleDist <-
plotCIStack <-
plotArrow <-
splitViewports <-
plotTriangleDist <-
yAxisViewports <- function() {
	notYetImplemented()
	stop("method does not exist yet.")
}
