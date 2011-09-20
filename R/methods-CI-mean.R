#' methods for exploring the coverage of confidence intervals for the mean calculated in varying ways from the data.
load_CI_mean <- function(e) {
	confidence_check(e)

	PLOT_DATA <<- PLOT_DATA
	PLOT_SAMPLE <<- plotSamplePointsAndBoxplot
	CALC_STAT <<- c("normal" = calcCIWald, "percentile bootstrap" =
		calcCIBootPercMean, "normal bootstrap" = calcCIBootSEMean,
		"t bootstrap" = calcCIBootTSEMean)[[svalue(e$cimeth)]]
	PLOT_DATA_STAT <<- addMeanLine
	PLOT_SAMPLE_STAT <<- plotCI
	PLOT_STAT_DIST <<- plotCIDistMean
	ANIMATE_SAMPLE <<- dropPoints1d
	ANIMATE_STAT <<- dropCI
	DISPLAY_RESULT <<- CIcounter
	HANDLE_1000 <<- ci1000

	# getting things ready for a confidence interval counter
	if (!is.null(e$ci.counter)) delete(e$controls.vit, e$ci.counter)
	e$ci.counter <- glabel()
	add(e$controls.vit, e$ci.counter)
	e$results <- NULL
}


plotSamplePointsAndBoxplot <- function(canvas, i) {
	x <- canvas$samples[[i]]
	if (length(x) >= 100)
		plotHist(canvas, x, graphPath("data"), "dataPlot")
	else {
		y <- stackPoints(x, vp = graphPath("sample"))
		plotPoints(canvas, x, y, graphPath("sample"), "samplePlot")
		plotBoxplot(canvas, x, graphPath("sample"), "samplePlot")
	}
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

calcCIBootSEMean <- function(x){
    n <- length(x)
    nboots <- 1000
    samps <- matrix(sample(x, size = nboots*n, replace = TRUE), nrow = nboots,
    	ncol = n)
    means <- colMeans(samps)
    se <- sd(means)
    mean(x) + c(-1, 1) * 2 * se
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

addMeanLine <- function(canvas) {
	x <- mean(canvas$x)
	canvas$image <- addGrob(canvas$image, segmentsGrob(x0 = x, x1 = x, y0 = 0,
		y1 = 3, default.units = "native", gp = gpar(col = "grey60"),
		vp = vpPath("canvas.frame", "animation.field"), name = "hline"))
}

plotCI <- function(canvas, i) {
	bounds <- canvas$stat.dist[[i]]
	x <- mean(bounds)
	canvas$image <- addGrob(canvas$image, rectGrob(x = unit(x, "native"),
		y = unit(0.2, "native"), width = unit(diff(bounds), "native"),
		height = unit(0.015, "native"), gp = gpar(col = "blue",
		fill = "blue"), vp = graphPath("sample"), name = "sample.stat"))
}

plotCIDistMean <- function(canvas) {
	i <- canvas$which.sample
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

#' Animates a sample of points dropping down from the collection of points in the data window. The ANIMATE_SAMPLE method for numeric, one dimensional data.
dropPoints1d <- function(canvas, n.steps) {
	if ("samplePlot.points.1" %in% childNames(canvas$image))
		canvas$image <- removeGrob(canvas$image, gPath("samplePlot.points.1"))

	index <- canvas$indexes[[canvas$which.sample]]
	x <- canvas$x[index]
	y.start <- canvas$y[index] + 2 # to place in data vp
	y.end <- stackPoints(x, vp = graphPath("sample")) + 1

	n <- length(x):1
	m <- length(x)


        n1 <- c((3:1)*n.steps, rep(0, m - 3))
        m1 <- 3*n.steps
	step <- (max(y.start) - 1.5) / n.steps
        n[1:3] <- (y.start[1:3] - y.end[1:3])/step + m
	if ("samplePlot.points" %in% childNames(canvas$image))
		canvas$image <- removeGrob(canvas$image, gPath(c("samplePlot.points")))
        for (i in 1:(3*n.steps)){
            o <- pmax(n1 - m1, 0)*step
            canvas$image <- addGrob(canvas$image, pointsGrob
                                    (x, y = pmax(y.start - o, y.end),
                                     vp = vpPath("canvas.frame",
                                     "animation.field"), gp = gpar(lwd = 2, col = "grey50"),
                                     name = "temp"))
            if ((i - 1) %% n.steps == 0){
                canvas$image <- addGrob(canvas$image, pointsGrob
                                        (x[i %/% n.steps + 1], y = y.start[i %/% n.steps + 1],
                                         vp = vpPath("canvas.frame", "animation.field"),
                                         pch = 19, name = "highlight"))
            }
            m1 <- m1 - 1
            canvas$drawImage()
        }

	for (i in 1:(length(x) + n.steps)) {

		o <- pmax(n - m, 0) * step
		canvas$image <- addGrob(canvas$image, pointsGrob(x,
			y = pmax(y.start - o, y.end), vp = vpPath("canvas.frame",
			"animation.field"), gp = gpar(lwd = 2, col = "grey50"),
			name = "temp"))
		if (i < length(x)) {
			canvas$image <- addGrob(canvas$image, pointsGrob(x[i],
				y = y.start[i], vp = vpPath("canvas.frame",
				"animation.field"), pch = 19,
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





#' confidence coverage method for DISPLAY_RESULT
CIcounter <- function(canvas, env) {
	if (is.null(env$results)) {
		bounds <- do.call("rbind", canvas$stat.dist)
		X <- mean(CALC_STAT(canvas$x))
		env$results <- X >= bounds[,1] & X <= bounds[,2]
	}

	total <- canvas$which.sample
	success <- sum(env$results[1:total])

	svalue(env$ci.counter) <- paste(success, "of", total, "contain true value:",
		round(success/total*100, 1), "%")

}


#' confidence coverage method for HANDLE_1000: how to display the results of 1000 bootstrap samples
ci1000 <- function(canvas, e){
	if ("samplePlot.points.1" %in% childNames(canvas$image))
		canvas$image <- removeGrob(canvas$image, gPath("samplePlot.points.1"))
	if ("samplePlot.boxplot.1" %in% childNames(e$c1$image))
		canvas$image <- removeGrob(canvas$image, gPath("samplePlot.boxplot.1"))

	if (is.null(e$results)) {
		bounds <- do.call("rbind", canvas$stat.dist)
		X <- mean(CALC_STAT(canvas$x))
		e$results <- X >= bounds[,1] & X <= bounds[,2]
	}

	svalue(e$ci.counter) <- c("                                              ")

	for (j in c(seq(1 , 1000, by = 10), 1000)) {
			canvas$plotSampleStat()
			canvas$plotStatDist()
			canvas$drawImage()
			canvas$advanceWhichSample()

			success <- sum(e$results[1:j])
			svalue(e$ci.counter) <- paste(success, "of", j,
				"contain true value:", round(success/j * 100, 2), "%")

	}
	canvas$image <- removeGrob(canvas$image, gPath("sample.stat"))
	canvas$drawImage()
}



