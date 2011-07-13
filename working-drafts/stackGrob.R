#' Displays a vector of numbers as a neatly stacked histogram of points. The vertical 
#' overlap of points and binwidths of the histogram are determined by the relationship of 
#' point size to the plot size.
stackGrob <- function(x, pch = 1, gp = gpar(), vp = NULL, name = NULL){
	grob(x = x, pch = pch, gp = gp, vp = vp, name = name, cl = "stackGrob")
}

grid.stack <- function(...){
	grid.draw(stackGrob(...))
}

drawDetails.stackGrob <- function(grob, recording){
	x <- grob$x
	pheight <- convertY(unit(1, "char"), "native", valueOnly = TRUE) * 0.8
	binwidth <- pheight * diff(range(x))
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
		df[igroup, ]$y <- seq_len(j)
	}
	
	df$y <- (df$y - 1) * ydist
	
	grid.points(x = df$x, y = df$y, gp = grob$gp, vp = grob$vp)
}


editDetails.stackGrob <- function(x, spec){
	x <- stackGrob(x = x$x, pch = x$pch, gp = x$gp, vp = x$vp, name = x$name)
	x
}

validDetails.stackGrob = function(x){
  x
}
