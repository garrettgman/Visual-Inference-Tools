#' methods for exploring the sampling distributions of bootstrapped statistics

#' bootstrapping method for PLOT_STAT
plotTriangle <- function(canvas, vp, name) {
	x <- canvas$getStat()
	canvas$image <- addGrob(canvas$image, pointsGrob(x = x, y = 0.1, pch = 17, 
		gp = gpar(col = "blue"), vp = vp, name = paste(name, "stat", sep = ".")))
	canvas$drawImage()
}