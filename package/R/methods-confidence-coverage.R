#' methods for exploring the coverage of confidence intervals calculated in varying ways from the data.

#' confidence coverage method for PLOT_STAT
plotCI <- function(canvas, vp, name) {
	bounds <- canvas$getStat()
	x <- mean(bounds)
	canvas$image <- addGrob(canvas$image, rectGrob(x = unit(x, "native"), 
		y = unit(0.5, "native"), width = diff(bounds), 
		height = unit(0.125, "native"), gp = gpar(col = "grey50", 
		fill = "grey50"), vp = vp, name = paste(name, "CI", sep = ".")))
	canvas$drawImage()
}

#' the various confidence coverage methods for CALC_STAT
calcCIWald <- function(x){
    n <- length(x)
    se <- sd(x)/sqrt(n)
    mean(x) + c(-1, 1)*qt(0.975, n - 1)*se
}

calcCIBootPerc <- function(x){
    statfun <- eval(parse(text = e$cistat))
    statfun <- mean
    n <- length(x)
    nboots <- 9999
    samps <- matrix(sample(x, size = nboots*n, replace = TRUE), nrow = nboots,
                    ncol = n)
    means <- apply(samps, 1, statfun)
    quantile(means, prob = c(0.025, 0.975))
}

calcCIBootSE <- function(x){
    statfun <- eval(parse(text = e$cistat))
    n <- length(x)
    nboots <- 10000
    samps <- matrix(sample(x, size = nboots*n, replace = TRUE), nrow = nboots,
                    ncol = n)
    means <- apply(samps, 1, statfun)
    se <- sd(means)
    mean(x) + c(-1, 1)*2*se
}

calcCIBootTSE <- function(x){
    statfun <- eval(parse(text = e$cistat))
    n <- length(x)
    nboots <- 10000
    samps <- matrix(sample(x, size = nboots*n, replace = TRUE), nrow = nboots,
                    ncol = n)
    means <- apply(samps, 1, statfun)
    se <- sd(means)
    mean(x) + c(-1, 1)*qt(0.975, n - 1)*se
}