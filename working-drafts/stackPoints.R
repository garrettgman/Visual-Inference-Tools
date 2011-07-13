#' Calculate y positions for stacking points along an x axis
#'
#' @param x a numeric vector of values to be stacked along the x axis
#' @param xstart lowest value on the xaxis
#' @param xend highest value on the xaxis
#' @param pheight height of the point grob in "npc" units. Used to prevent points from floating
#' @param binwidth The width of bins to stack points into.
#' @param nbins The number of bins to stack points into. nbins will be ignored if binwidth is specified
#'
#' @return a vector of y values. When x is plotted against stackPoints(x)
stackPoints <- function(x, y = c(0,1), pheight = unit(1, "char"), xmin = min(x), xmax = max(x), ymin = min(y), ymax = max(y), vp.path = current.vpPath(), 
	binwidth = NULL, nbins = 30){
		
		
		currentVP <- current.vpPath()
		seekViewport(vp.path)
		pheight <- convertY(pheight, "native", valueOnly = TRUE) * 0.8
		
		binwidth <- pheight * diff(range(x))
		
		nbins <- ceiling(diff(range(x) / binwidth))
		breaks <- min(x) + c(0:(nbins)) * binwidth
		group <- cut(x, breaks, include.lowest = TRUE)
		max.stack <- max(table(group))
		ydist <- min(1/max.stack, as.numeric(pheight))
		
		span <- xmax - xmin
	
		if (is.null(binwidth)) {
			binwidth <- span / nbins
		} else {
			nbins <- ceiling(span / binwidth)
		}
		
		breaks <- xmin + c(0:(nbins)) * binwidth
		group <- cut(x, breaks, include.lowest = TRUE)
		max.stack <- max(table(group))
		ydist <- min(1/max.stack, as.numeric(pheight)) * (ymax - ymin)
	
		df <- data.frame(x = x, group = group)
		df$y <- NA
		for (i in levels(group)) {
			df[df$group == i, ]$y <- seq_len(nrow(df[df$group == i, ]))
		}

		(df$y - 1) * ydist + ymin
}

