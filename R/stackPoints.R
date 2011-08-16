#' Calculates y values for a vector of overlapping x values. Y values are designed to prevent overlapping by creating the appearance of stacked data. A helper function for numeric 1d Data. Also used for 2d data where one dimension is numeric, and one is categorical.
stackPoints <- function(x, levels = rep(1, length(x)), vp, y.min = 0.5, 
	y.max = 1) {
		if (length(x) <= 1) y.min
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
			
			stack.y <- rep(NA, length(x))
				
			for (level in unique(levels)) {
				which.x <- which(levels == level)
				max.stack <- max(table(group[which.x]))
				ydist <- min(diff(c(y.min, y.max))/max.stack, 
					as.numeric(pheight))
				df <- data.frame(x = x[which.x], group = group[which.x])
				df$y <- NA
				for (i in levels(group)) {
					igroup <- which(df$group == i)
					j <- nrow(df[igroup, ])
					if (j > 0) df[igroup, ]$y <- seq_len(j)
				}
				stack.y[which.x] <- (df$y - 1) * ydist + y.min
			}
		stack.y	
	}
}