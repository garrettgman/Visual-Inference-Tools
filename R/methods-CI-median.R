#' methods for exploring the coverage of confidence intervals for the median calculated in varying ways from the data. Borrows many methods from methods-CI-mean.R
load_CI_median <- function(e) {
	confidence_check(e)

	PLOT_DATA <<- PLOT_DATA
	PLOT_SAMPLE <<- plotSamplePointsAndBoxplotMedian
        SHOW_LABELS <<- ciLabels
	CALC_STAT <<- c("bootstrap: percentile" = calcCIBootPercMedian,
		"bootstrap: +/- 2 s.e." = calcCIBootSEMedian,
		"bootstrap: +/- t s.e." = calcCIBootTSEMedian)[[svalue(e$cimeth)]]

	PLOT_DATA_STAT <<- addMedianLine
	PLOT_SAMPLE_STAT <<- plotCI
	PLOT_STAT_DIST <<- plotCIDistMedian
	ANIMATE_SAMPLE <<- dropPoints1d
	ANIMATE_STAT <<- dropCI
	DISPLAY_RESULT <<- CIcounter
	HANDLE_1000 <<- ci1000
        e$replace <- FALSE
	e$results <- NULL
}

plotSamplePointsAndBoxplotMedian <- function(canvas, e, i) {
    if ("samplePlot.stat.1" %in% childNames(canvas$image))
        canvas$image <- removeGrob(canvas$image, gPath(c("samplePlot.stat.1")))
    bluecol <- "blue"
    if (e$cb) bluecol <- dichromat(bluecol)
    x <- canvas$samples[[i]]
    if (length(x) >= 100)
        plotHist(canvas, x, canvas$graphPath("data"), "dataPlot")
    else {
        y <- stackPoints(x, vp = canvas$graphPath("sample"))
        plotPoints(canvas, x, y, canvas$graphPath("sample"), "samplePlot", black = TRUE)
        plotBoxplot(canvas, x, stat = median, stat.color = bluecol, canvas$graphPath("sample"),
                    "samplePlot")
    }
}

calcCIBootPercMedian <- function(x, y = NULL){
    n <- length(x)
    nboots <- 999
    samps <- matrix(sample(x, size = nboots*n, replace = TRUE), nrow = nboots,
                    ncol = n)
    medians <- apply(samps, 1, median)
    quantile(medians, prob = c(0.025, 0.975))
}

calcCIBootSEMedian <- function(x, y = NULL){
    n <- length(x)
    nboots <- 1000
    samps <- matrix(sample(x, size = nboots*n, replace = TRUE), nrow = nboots,
                 ncol = n)
    medians <- apply(samps, 1, median)
    se <- sd(medians)
    median(x) + c(-1, 1) * 2 * se
}

calcCIBootTSEMedian <- function(x, y = NULL){
    n <- length(x)
    nboots <- 1000
    samps <- matrix(sample(x, size = nboots*n, replace = TRUE), nrow = nboots,
                    ncol = n)
    medians <- apply(samps, 1, median)
    se <- sd(medians)
    median(x) + c(-1, 1) * qt(0.975, n - 1) * se
}

addMedianLine <- function(canvas, e) {
    purplecol <- "purple3"
    if (e$cb) purplecol <- dichromat(purplecol)
    x <- median(canvas$x)
    canvas$image <- addGrob(canvas$image,
                            segmentsGrob(x0 = x, x1 = x, y0 = 0,
                                         y1 = 3, default.units = "native",
                                         gp = gpar(col = "grey60", lty = "dashed"),
                                         vp = canvas$graphPath("animation.field"),
                                         name = "hline"))
        canvas$y <- stackPoints(canvas$x, vp = canvas$graphPath("data"))
    if (length(canvas$x) >= 1000)
        plotHist(canvas, canvas$x, canvas$graphPath("data"), "dataPlot") else {
        plotPoints(canvas, canvas$x, canvas$y, canvas$graphPath("data"), "dataPlot")
        plotBoxplot(canvas, canvas$x, stat = median, stat.color = purplecol,
                    canvas$graphPath("data"), "dataPlot")
    }
}

plotCIDistMedian <- function(canvas, e) {
    greencol <- "green"
    redcol <- "red"
    if (e$cb){
        greencol <- dichromat(greencol)
        redcol <- dichromat(redcol)
    }
    i <- canvas$which.sample
    bounds <- canvas$getStat(i)
    x <- mean(bounds)
    X <- median(canvas$x)
    if (X >= bounds[1] & X <= bounds[2]) color <- greencol
    else color <- redcol
    current <- data.frame(x = x, width = diff(c(bounds)), color = color)

    if ("stat.dist" %in% childNames(canvas$image)) {
        dist.grob <- getGrob(canvas$image, gPath(c("stat.dist")))
        dist.df <- dist.grob$data
        if (nrow(dist.df) >= 40) dist.df <- dist.df[-1,]
        dist.df <- rbind(dist.df[, -4], current)
    } else dist.df <- current

    dist.df$y <- 0.02 * 1:nrow(dist.df)
    green <- dist.df[dist.df$color == greencol,]
    red <- dist.df[dist.df$color == redcol,]

    if (nrow(green) > 0) {
        greenRects <- rectGrob(x = unit(green$x, "native"),
                               y = unit(green$y, "native"), width = unit(green$width, "native"),
                               height = unit(0.015, "native"), vp = canvas$graphPath("stat"),
                               gp = gpar(col = NA, fill = greencol))
    } else greenRects <- NULL

    if (nrow(red) > 0) {
        redRects <- rectGrob(x = unit(red$x, "native"),
                             y = unit(red$y, "native"), width = unit(red$width, "native"),
                             height = unit(0.015, "native"), vp = canvas$graphPath("stat"),
                             gp = gpar(col = NA, fill = redcol))
    } else redRects <- NULL

    new.dist <- gTree(data = dist.df, name = "stat.dist",
                      childrenvp = canvas$viewports, children = gList(greenRects, redRects))

    canvas$image <- addGrob(canvas$image, new.dist)
}


