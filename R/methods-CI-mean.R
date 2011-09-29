#' methods for exploring the coverage of confidence intervals for the mean calculated in varying ways from the data.
load_CI_mean <- function(e) {
	confidence_check(e)

	PLOT_DATA <<- PLOT_DATA
	PLOT_SAMPLE <<- plotSamplePointsAndBoxplot
        SHOW_LABELS <<- ciLabels
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
        e$replace <- FALSE
	e$results <- NULL
}


plotSamplePointsAndBoxplot <- function(canvas, i) {
    x <- canvas$samples[[i]]
    if (length(x) >= 100)
        plotHist(canvas, x, graphPath("data"), "dataPlot")
    else {
        y <- stackPoints(x, vp = graphPath("sample"))
        plotPoints(canvas, x, y, graphPath("sample"), "samplePlot", black = TRUE)
        plotBoxplot(canvas, x, graphPath("sample"), "samplePlot")
    }
}

ciLabels <- function(canvas){
    canvas$image <- addGrob(canvas$image, textGrob("Population", x = 0, y = 0.9,
                                                   just = c("left", "top"),
                                                   vp = graphPath("data")))
    canvas$image <- addGrob(canvas$image, textGrob("Sample", x = 0, y = 0.8,
                                                   just = c("left", "top"),
                                                   vp = graphPath("sample")))
    canvas$image <- addGrob(canvas$image, textGrob("CI history", x = 0, y = 0.8,
                                                   just = c("left", "top"),
                                                   vp = graphPath("stat")))
    canvas$drawImage()
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
    means <- rowMeans(samps)
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
dropPoints1d <- function(canvas, n.steps, n.slow, move = TRUE) {
    if ("samplePlot.points.1" %in% childNames(canvas$image))
        canvas$image <- removeGrob(canvas$image, gPath("samplePlot.points.1"))
    if ("samplePlot.points" %in% childNames(canvas$image))
        canvas$image <- removeGrob(canvas$image, gPath(c("samplePlot.points")))
    index <- canvas$indexes[[canvas$which.sample]]
    x <- canvas$x[index]
    y.start <- y.pos <- canvas$y[index] + 2 # to place in data vp
    y.end <- stackPoints(x, vp = graphPath("sample")) + 1
    y.step <- (y.start - y.end)/n.steps
    n.slow <- min(n.slow, length(x))
    ## Lighting up of sampled points.
    if (move){
        for (i in 1:length(x)){
            canvas$image <- addGrob(canvas$image,
                                    pointsGrob(x[1:i], y = (canvas$y[index])[1:i],
                                               vp = graphPath("data"),
                                               pch = 19,
                                               name = "data.samp"))
            if (i <= n.slow) speed = 10 else speed = 1
            for (j in 1:speed) canvas$drawImage()
        }
        ## Force pause before points drop.
        for (i in 1:20) canvas$drawImage()
    }
    canvas$image <- addGrob(canvas$image,
                            pointsGrob(x, y = canvas$y[index], vp = graphPath("data"),
                                       pch = 19,
                                       name = "data.samp"))
    ## Dropping of points.
    if (move){
        for (i in 1:n.steps){
            y.pos <- y.pos - y.step
            canvas$image <- addGrob(canvas$image,
                                    pointsGrob(x, y.pos, vp = vpPath("canvas.frame",
                                                         "animation.field"), pch = 19,
                                               name = "temp"))
            canvas$drawImage()
        }
        canvas$image <- removeGrob(canvas$image, gPath(c("temp")))
    }
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
    if ("data.samp" %in% childNames(e$c1$image))
        canvas$image <- removeGrob(canvas$image, gPath("data.samp"))
    if (canvas$which.sample >= 900) canvas$which.sample <- round(runif(1, 0, 100))
    bounds <- do.call("rbind", canvas$stat.dist)
    X <- mean(CALC_STAT(canvas$x))
    e$results <- X >= bounds[,1] & X <= bounds[,2]
    ## Overall coverage percentage
    totperc <- mean(!e$results)
    ## Required 'red' CIs for final display of 40 CIs.
    noreq <- ceiling(40*totperc)
    if (noreq > 0){
        plotted.index <- (canvas$which.sample + 61):(canvas$which.sample + 100)
        plotted.samples <- canvas$samples[plotted.index]
        diff <- sum(!e$results[plotted.index]) - noreq
        if (diff != 0) {
            if (diff > 0) {
                index.changeout <- sample(which(!e$results[plotted.index]),
                                          size = diff) + canvas$which.sample + 60
                index.changein <- sample(which(e$results), size = diff)
            } else {
                index.changeout <- sample(which(e$results[plotted.index]),
                                          size = abs(diff)) + canvas$which.sample + 60
                index.changein <- sample(which(!e$results), size = abs(diff))
            }
            for (i in 1:abs(diff)){
                samples.changeout <- canvas$samples[[index.changeout[i]]]
                canvas$samples[[index.changeout[i]]] <- canvas$samples[[index.changein[i]]]
                canvas$samples[[index.changein[i]]] <- samples.changeout
                statdist.changeout <- canvas$stat.dist[[index.changeout[i]]]
                canvas$stat.dist[[index.changeout[i]]] <- canvas$stat.dist[[index.changein[i]]]
                canvas$stat.dist[[index.changein[i]]] <- statdist.changeout
            }
        }
    }
    svalue(e$ci.counter) <- c("                                              ")
    ## If running out of samples, select a random starting point in first 100.
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



