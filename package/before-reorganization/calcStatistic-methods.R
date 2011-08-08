calcMean <- function(x, ...) mean(x)
	
calcMedian <- function(x, ...) median(x)

calcCI <- function(x, ...) quantile(x, prob = c(0.025, 0.975))

calcDiffMean <- function(x, y, ...) { # what do we actually need?
	if (!is.factor(y)) y <- as.factor(y)
	
	n <- nlevels(y)
	group.means <- vector(length = n) 
	for (i in 1:n) {
		group.means[i] <- mean(x[y == levels(y)[i]])
	}
	arrow.spans <- data.frame(start = group.means[-n], end = group.means[-1])
	arrow.spans
}
	
calcDiffMedian <- function(x, y, ...) { # what do we actually need?
	if (!is.factor(y)) y <- as.factor(y)
	
	n <- nlevels(y)
	group.medians <- vector(length = n) 
	for (i in 1:n) {
		group.medians[i] <- median(x[y == levels(y)[i]])
	}
	arrow.spans <- data.frame(start = group.medians[-n], end = group.medians[-1])
	arrow.spans
}