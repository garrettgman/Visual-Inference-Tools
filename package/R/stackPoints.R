#' calculates y values for a vector of overlapping x values. Y values are designed 
#' to prevent overlapping by creating the appearance of stacked data
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