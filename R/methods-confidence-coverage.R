#' methods for exploring the coverage of confidence intervals calculated in varying ways from the data.

#' confidence coverage method for PLOT_STAT
plotCI <- function(canvas, vp) {
	bounds <- canvas$getStat()
	x <- mean(bounds)
	canvas$image <- addGrob(canvas$image, rectGrob(x = unit(x, "native"), 
		y = unit(0.2, "native"), width = unit(diff(bounds), "native"), 
		height = unit(0.015, "native"), gp = gpar(col = "blue", 
		fill = "blue"), vp = vp, name = "sample.stat"))
}


#' confidence coverage method for ANIMATE_STAT
dropCI <- function(canvas, n.steps) {
	stat.grob <- getGrob(canvas$image, gPath(c("sample.stat")))
	grob.width <- stat.grob$width
	grob.x <- stat.grob$x
	canvas$image <- removeGrob(canvas$image, gPath(c("sample.stat")))
	
	y.start <- 1.2	
	y.end <- .02 * min(canvas$which.sample - 1, 41)
	
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

#' confidence convergence methods for PLOT_STAT_DIST
plotCIDistMean <- function(canvas) {
	i <- canvas$which.sample - 1
	bounds <- canvas$getStat(i)
	x <- mean(bounds) 
	X <- mean(canvas$x)
	if (X >= bounds[1] & X <= bounds[2]) color <- "green"
	else color <- "red"
	current <- data.frame(x = x, width = diff(c(bounds)), color = color)
	
	if ("stat.dist" %in% childNames(canvas$image)) {
		dist.grob <- getGrob(canvas$image, gPath(c("stat.dist")))
		dist.df <- dist.grob$data
		if (nrow(dist.df) >= 40) dist.df <- dist.df[-1,]
		dist.df <- rbind(dist.df[, -4], current)
	} else dist.df <- current
	
	dist.df$y <- 0.02 * 1:nrow(dist.df)
	green <- dist.df[dist.df$color == "green",]
	red <- dist.df[dist.df$color == "red",]
	
	if (nrow(green) > 0) {
		greenRects <- rectGrob(x = unit(green$x, "native"), 
			y = unit(green$y, "native"), width = unit(green$width, "native"), 
			height = unit(0.015, "native"), vp = graphPath("stat"), 
			gp = gpar(col = NA, fill = "green"))
	} else greenRects <- NULL
	
	if (nrow(red) > 0) {	
		redRects <- rectGrob(x = unit(red$x, "native"), 
			y = unit(red$y, "native"), width = unit(red$width, "native"),
			height = unit(0.015, "native"), vp = graphPath("stat"), 
			gp = gpar(col = NA, fill = "red"))
	} else redRects <- NULL
	
	new.dist <- gTree(data = dist.df, name = "stat.dist", 
		childrenvp = canvas$viewports, children = gList(greenRects, redRects))
		
	canvas$image <- addGrob(canvas$image, new.dist)
}

plotCIDistMedian <- function(canvas) {
	i <- canvas$which.sample - 1
	bounds <- canvas$getStat(i)
	x <- mean(bounds) 
	X <- median(canvas$x)
	if (X >= bounds[1] & X <= bounds[2]) color <- "green"
	else color <- "red"
	current <- data.frame(x = x, width = diff(c(bounds)), color = color)
	
	if ("stat.dist" %in% childNames(canvas$image)) {
		dist.grob <- getGrob(canvas$image, gPath(c("stat.dist")))
		dist.df <- dist.grob$data
		if (nrow(dist.df) >= 40) dist.df <- dist.df[-1,]
		dist.df <- rbind(dist.df[, -4], current)
	} else dist.df <- current
	
	dist.df$y <- 0.02 * 1:nrow(dist.df)
	green <- dist.df[dist.df$color == "green",]
	red <- dist.df[dist.df$color == "red",]
	
	if (nrow(green) > 0) {
		greenRects <- rectGrob(x = unit(green$x, "native"), 
			y = unit(green$y, "native"), width = unit(green$width, "native"), 
			height = unit(0.015, "native"), vp = graphPath("stat"), 
			gp = gpar(col = NA, fill = "green"))
	} else greenRects <- NULL
	
	if (nrow(red) > 0) {	
		redRects <- rectGrob(x = unit(red$x, "native"), 
			y = unit(red$y, "native"), width = unit(red$width, "native"),
			height = unit(0.015, "native"), vp = graphPath("stat"), 
			gp = gpar(col = NA, fill = "red"))
	} else redRects <- NULL
	
	new.dist <- gTree(data = dist.df, name = "stat.dist", 
		childrenvp = canvas$viewports, children = gList(greenRects, redRects))
		
	canvas$image <- addGrob(canvas$image, new.dist)
}


#' confidence coverage method for DISPLAY_RESULT
CIcounter <- function(canvas, env) {
	if (is.null(env$results)) {
		bounds <- do.call("rbind", canvas$stat.dist)
		X <- mean(CALC_STAT(canvas$x))
		env$results <- X >= bounds[,1] & X <= bounds[,2]
	}
	
	total <- canvas$which.sample - 1
	success <- sum(env$results[1:total])
	
	svalue(env$ci.counter) <- paste(success, "of", total, "contain true value:", 
		round(success/total*100, 1), "%")
	
}

#' prepares way for CIcounter
ci_miscellaneous <- function(env) {
	if (!is.null(env$ci.counter)) delete(env$controls.vit, env$ci.counter)
	env$ci.counter <- glabel()
	add(env$controls.vit, env$ci.counter)
	env$results <- NULL
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
    means <- rowMeans(samps)
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

#' confidence coverage method for HANDLE_1000: how to display the results of 1000 bootstrap samples
ci1000 <- function(e){
	for (j in seq(1 , 1000, by = 10)) {
			e$c1$plotStat(vp = graphPath("sample"))
			e$c1$which.sample <- j + 1
			e$c1$plotStatDist()
			e$c1$drawImage()
	}
	
	if (is.null(e$results)) {
		bounds <- do.call("rbind", e$c1$stat.dist)
		X <- mean(CALC_STAT(e$c1$x))
		e$results <- X >= bounds[,1] & X <= bounds[,2]
	}
	
	success <- sum(e$results)
	
	svalue(e$ci.counter) <- paste(success, "of", 1000, "contain true value:", 
		success/10, "%")
}
