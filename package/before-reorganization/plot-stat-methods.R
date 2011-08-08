plotCI <- function(canvas, vp, name) {
	bounds <- canvas$getStat()
	x <- mean(bounds)
	canvas$image <- addGrob(canvas$image, rectGrob(x = unit(x, "native"), 
		y = unit(0.5, "native"), width = diff(bounds), 
		height = unit(0.125, "native"), gp = gpar(col = "grey50", 
		fill = "grey50"), vp = vp, name = paste(name, "CI", sep = ".")))
	canvas$drawImage()
}

plotTriangle <- function(canvas, vp, name) {
	x <- canvas$getStat()
	canvas$image <- addGrob(canvas$image, pointsGrob(x = x, y = 0.1, pch = 17, 
		gp = gpar(col = "blue"), vp = vp, name = paste(name, "stat", sep = ".")))
	canvas$drawImage()
}