#' Animates a sample of points dropping down from the collection of points in the data window.

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
				"animation.field"), gp = gpar(pch = 19), 
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
		