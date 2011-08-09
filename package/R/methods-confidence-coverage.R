#' methods for exploring the coverage of confidence intervals calculated in varying ways from the data.

#' confidence coverage method for PLOT_STAT
plotCI <- function(canvas, vp) {
	bounds <- canvas$getStat()
	x <- mean(bounds)
	canvas$image <- addGrob(canvas$image, rectGrob(x = unit(x, "native"), 
		y = unit(0.25, "native"), width = unit(diff(bounds), "native"), 
		height = unit(0.015, "native"), gp = gpar(col = "blue", 
		fill = "blue"), vp = vp, name = "sample.stat"))
}


#' confidence coverage method for ANIMATE_STAT
dropCI <- function(canvas, n.steps) {
	stat.grob <- getGrob(canvas$image, gPath(c("sample.stat")))
	grob.width <- stat.grob$width
	grob.x <- stat.grob$x
	canvas$image <- removeGrob(canvas$image, gPath(c("sample.stat")))
	
	y.start <- 1.25	
	y.end <- .02 * min(canvas$which.sample, 41)
	
	step <- (y.start - y.end)/n.steps
	
	for (i in 1:n.steps) {
		canvas$image <- addGrob(canvas$image, rectGrob(x = grob.x, 
			y = unit(y.start - i * step, "native"), width = grob.width, 
			height = unit(0.015, "native"), gp = gpar(col = "blue", 
			fill = "blue"), vp = vpPath("canvas.frame", "animation.field"), 
			name = "sample.stat"))
	
		canvas$drawImage()	
	}
	canvas$image <- removeGrob(canvas$image, gPath(c("sample.stat")))
}

addLine <- function(canvas, fun) {
	x <- fun(canvas$x)
	canvas$image <- addGrob(canvas$image, segmentsGrob(x0 = x, x1 = x, y0 = 0, 
		y1 = 3, default.units = "native", gp = gpar(col = "grey60"), 
		vp = vpPath("canvas.frame", "animation.field"), name = "hline"))
}


#' the various confidence coverage methods for CALC_STAT
#' note that the percentile bootstrap methods do not perform very well. Here's the speed of makeStatDistribution when using different version of them on an original data of size 80
# colMean(samps)
#   user  system elapsed 
# 40.614   2.822  43.669 
 
# apply(samps, 1, mean)
#   user  system elapsed 
# 40.958   2.851  44.828 
 
# apply(samps, 1, median)
#    user  system elapsed 
#129.160   3.507 133.886 
calcCIWald <- function(x){
    n <- length(x)
    se <- sd(x)/sqrt(n)
    mean(x) + c(-1, 1)*qt(0.975, n - 1)*se
}

calcCIBootPercMean <- function(x){
    n <- length(x)
    nboots <- 999
    samps <- matrix(sample(x, size = nboots*n, replace = TRUE), nrow = nboots,
    	ncol = n)
    means <- colMeans(samps)
    quantile(means, prob = c(0.025, 0.975))
}

calcCIBootPercMedian <- function(x){
    n <- length(x)
    nboots <- 999
    samps <- matrix(sample(x, size = nboots*n, replace = TRUE), nrow = nboots,
                    ncol = n)
    medians <- apply(samps, 1, median)
    quantile(medians, prob = c(0.025, 0.975))
}

calcCIBootSEMean <- function(x){
    n <- length(x)
    nboots <- 1000
    samps <- matrix(sample(x, size = nboots*n, replace = TRUE), nrow = nboots, 
    	ncol = n)
    means <- colMeans(samps)
    se <- sd(means)
    mean(x) + c(-1, 1) * 2 * se
}

calcCIBootSEMedian <- function(x){
    n <- length(x)
    nboots <- 1000
    samps <- matrix(sample(x, size = nboots*n, replace = TRUE), nrow = nboots,
                 ncol = n)
    medians <- apply(samps, 1, median)
    se <- sd(medians)
    median(x) + c(-1, 1) * 2 * se
}

calcCIBootTSEMean <- function(x){
    n <- length(x)
    nboots <- 1000
    samps <- matrix(sample(x, size = nboots*n, replace = TRUE), nrow = nboots,
                    ncol = n)
    means <- apply(samps, 1, mean)
    se <- sd(means)
    mean(x) + c(-1, 1) * qt(0.975, n - 1) * se
}

calcCIBootTSEMedian <- function(x){
    n <- length(x)
    nboots <- 1000
    samps <- matrix(sample(x, size = nboots*n, replace = TRUE), nrow = nboots,
                    ncol = n)
    medians <- apply(samps, 1, median)
    se <- sd(medians)
    median(x) + c(-1, 1) * qt(0.975, n - 1) * se
}