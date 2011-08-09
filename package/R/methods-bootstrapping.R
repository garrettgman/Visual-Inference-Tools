#' methods for exploring the sampling distributions of bootstrapped statistics

#' bootstrapping method for PLOT_STAT
plotTriangle <- function(canvas, vp, name) {
	x <- canvas$getStat()
	canvas$image <- addGrob(canvas$image, pointsGrob(x = x, y = 0, pch = 17, 
		gp = gpar(col = "blue"), vp = vp, name = "sample.stat"))
}

plot_ghosts <- function(canvas, name, vp = graphPath("sample")) {
	i <- canvas$which.sample - 1
}