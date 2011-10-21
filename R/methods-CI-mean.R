#' methods for exploring the coverage of confidence intervals for the mean calculated in varying ways from the data.
load_CI_mean <- function(e) {
	confidence_check(e)

	PLOT_DATA <<- PLOT_DATA
	PLOT_SAMPLE <<- plotSamplePointsAndBoxplotMean
        SHOW_LABELS <<- ciLabels
	CALC_STAT <<- c("normal: +/- t s.e." = calcCITWald, "normal: +/- 2 s.e." = calcCI2Wald,
                        "bootstrap: percentile" = calcCIBootPercMean,
                        "bootstrap: +/- 2 s.e." = calcCIBootSEMean,
                        "bootstrap: +/- t s.e." = calcCIBootTSEMean)[[svalue(e$cimeth)]]
	PLOT_DATA_STAT <<- addMeanLine
	PLOT_SAMPLE_STAT <<- plotCI
	PLOT_STAT_DIST <<- plotCIDistMean
	ANIMATE_SAMPLE <<- dropPoints1d
	ANIMATE_STAT <<- dropCI
	DISPLAY_RESULT <<- CIcounter
	HANDLE_1000 <<- ci1000
        e$replace <- FALSE
	e$results <- NULL
}

plotSamplePointsAndBoxplotMean <- function(canvas, e, i) {
    bluecol <- "blue"
    if (e$cb) bluecol <- dichromat(bluecol)
    x <- canvas$samples[[i]]
    if (length(x) >= 1000)
        plotHist(canvas, x, canvas$graphPath("sample"), "samplePlot")
    else {
        y <- stackPoints(x, vp = canvas$graphPath("sample"))
        plotPoints(canvas, x, y, canvas$graphPath("sample"), "samplePlot", black = TRUE)
        plotBoxplot(canvas, x, stat = mean, stat.color = bluecol, canvas$graphPath("sample"),
                    "samplePlot")
    }
}

ciLabels <- function(canvas){

    poplabel <- textGrob("Population",
                         x = unit(0, "npc") - unit(1, "cm"),
                         y = unit(0.9, "npc"),
                         just = c("left", "top"),
                         vp = canvas$graphPath("data"),
                         gp = gpar(fontface = 2))
    samplabel <- textGrob("Sample",
                          x = unit(0, "npc") - unit(1, "cm"),
                          y = unit(0.8, "npc"),
                          just = c("left", "top"),
                          vp = canvas$graphPath("sample"),
                          gp = gpar(fontface = 2))
    statlabel <- textGrob("CI history",
                          x = unit(0, "npc") - unit(1, "cm"),
                          y = unit(0.8, "npc"),
                          just = c("left", "top"),
                          vp = canvas$graphPath("stat"),
                          gp = gpar(fontface = 2))
    cilabels <- grobTree(poplabel, samplabel, statlabel, name = "cilabels")
    canvas$image <- addGrob(canvas$image, cilabels)
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
calcCITWald <- function(x){
    n <- length(x)
    se <- sd(x)/sqrt(n)
    mean(x) + c(-1, 1)*qt(0.975, n - 1)*se
}

calcCI2Wald <- function(x){
    n <- length(x)
    se <- sd(x)/sqrt(n)
    mean(x) + c(-2, 2)*se
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
    means <- rowMeans(samps)
    se <- sd(means)
    mean(x) + c(-1, 1) * qt(0.975, n - 1) * se
}

addMeanLine <- function(canvas, e) {
    purplecol <- "purple3"
    if (e$cb) purplecol <- dichromat(purplecol)
    x <- mean(canvas$x)
    canvas$image <- addGrob(canvas$image,
                            segmentsGrob(x0 = x, x1 = x, y0 = 0,
                                         y1 = 3, default.units = "native",
                                         gp = gpar(col = "grey60"),
                                         vp = vpPath("canvas.frame", "animation.field"),
                                         name = "hline"))
    canvas$y <- stackPoints(canvas$x, vp = canvas$graphPath("data"))
    if (length(canvas$x) >= 1000)
        plotHist(canvas, canvas$x, canvas$graphPath("data"), "dataPlot")
    else {
        plotPoints(canvas, canvas$x, canvas$y, canvas$graphPath("data"), "dataPlot")
        plotBoxplot(canvas, canvas$x, stat = mean, stat.color = purplecol, canvas$graphPath("data"),
                    "dataPlot")
    }
}

plotCI <- function(canvas, e, i) {
    orangecol <- "#FF7F00"
    if (e$cb) orangecol <- dichromat(orangecol)
    bounds <- canvas$stat.dist[[i]]
    x <- mean(bounds)
    canvas$image <- addGrob(canvas$image,
                            rectGrob(x = unit(x, "native"), y = unit(0.2, "native"),
                                     width = unit(diff(bounds), "native"),
                                     height = unit(0.015, "native"),
                                     gp = gpar(col = orangecol,
                                     fill = orangecol), vp = canvas$graphPath("sample"),
                                     name = "sample.stat"))
}

plotCIDistMean <- function(canvas, e) {
    redcol <- "red"
    greencol <- "green"
    if (e$cb){
        redcol <- dichromat(redcol)
        greencol <- dichromat(greencol)
    }
    i <- canvas$which.sample
    bounds <- canvas$getStat(i)
    x <- mean(bounds)
    X <- mean(canvas$x)
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

#' Animates a sample of points dropping down from the collection of points in the data window. The ANIMATE_SAMPLE method for numeric, one dimensional data.
dropPoints1d <- function(canvas, n.steps, n.slow, keep.plot, move = TRUE) {
    if ("samplePlot.points.1" %in% childNames(canvas$image))
        canvas$image <- removeGrob(canvas$image, gPath("samplePlot.points.1"))
    if ("samplePlot.points" %in% childNames(canvas$image))
        canvas$image <- removeGrob(canvas$image, gPath(c("samplePlot.points")))
    if (!keep.plot){
        if ("samplePlot.boxplot.1" %in% childNames(canvas$image))
            canvas$image <- removeGrob(canvas$image, gPath(c("samplePlot.boxplot.1")))
        if ("samplePlot.boxplot" %in% childNames(canvas$image))
            canvas$image <- removeGrob(canvas$image, gPath(c("samplePlot.boxplot")))
        if ("sample.stat" %in% childNames(canvas$image))
            canvas$image <- removeGrob(canvas$image, gPath(c("sample.stat")))
    }
    index <- canvas$indexes[[canvas$which.sample]]
    x <- canvas$x[index]
    y.start <- y.pos <- canvas$y[index] + 2 # to place in data vp
    y.end <- stackPoints(x, vp = canvas$graphPath("sample")) + 1
    y.step <- (y.start - y.end)/n.steps
    n.slow <- min(n.slow, length(x))
    ## Lighting up of sampled points.
    if (move){
        for (i in 1:length(x)){
            canvas$image <- addGrob(canvas$image,
                                    pointsGrob(x[1:i], y = (canvas$y[index])[1:i],
                                               vp = canvas$graphPath("data"),
                                               pch = 19,
                                               name = "data.samp"))
            if (i <= n.slow) speed = 10 else speed = 1
            for (j in 1:speed) canvas$drawImage()
        }
        ## Force pause before points drop.
        for (i in 1:20) canvas$drawImage()
    }
    canvas$image <- addGrob(canvas$image,
                            pointsGrob(x, y = canvas$y[index], vp = canvas$graphPath("data"),
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
dropCI <- function(canvas, e, n.steps) {
    orangecol <- "#FF7F00"
    if (e$cb) orangecol <- dichromat(orangecol)
    canvas$drawImage()
    stat.grob <- getGrob(canvas$image, gPath(c("sample.stat")))
    grob.width <- stat.grob$width
    grob.x <- stat.grob$x
    #canvas$image <- removeGrob(canvas$image, gPath(c("sample.stat")))

    y.start <- 1.2
    y.end <- .02 * min(length(canvas$sampled.stats) + 1, 41)

    step <- (y.start - y.end)/n.steps

    for (i in 1:n.steps) {
        canvas$image <- addGrob(canvas$image,
                                rectGrob(x = grob.x,
                                         y = unit(y.start - i * step, "native"),
                                         width = grob.width,
                                         height = unit(0.015, "native"),
                                         gp = gpar(col = orangecol,
                                         fill = orangecol),
                                         vp = vpPath("canvas.frame", "animation.field"),
                                         name = "moving.stat"))

        canvas$drawImage()
    }
    canvas$pauseImage(10)
    canvas$image <- removeGrob(canvas$image, gPath(c("moving.stat")))
}





##' confidence coverage method for DISPLAY_RESULT
CIcounter <- function(canvas, env) {
    if (is.null(env$results)) {
        bounds <- do.call("rbind", canvas$stat.dist)
        X <- mean(CALC_STAT(canvas$x))
        env$results <- X >= bounds[,1] & X <= bounds[,2]
    }
    canvas$sampled.stats <- c(canvas$sampled.stats, canvas$which.sample)
    total <- length(canvas$sampled.stats)
    success <- sum(env$results[canvas$sampled.stats])
    xunit <- unit(0, "npc") + unit(1, "mm")
    countertext1 <- textGrob("Coverage:", x = xunit, y = unit(0.5, "npc"),
                             vp = canvas$graphPath("stat"), gp = gpar(fontface = 2),
                             name = "countertext1")
    countertext2 <- textGrob(paste(success, "of", total), x = xunit,
                             y = unit(0.5, "npc") - unit(1, "lines"),
                             vp = canvas$graphPath("stat"), name = "countertext2")
    countertext3 <- textGrob(paste(round(success/total*100, 1), "%"),
                             x = xunit, y = unit(0.5, "npc") - unit(2/1.3, "lines"),
                             vp = canvas$graphPath("stat"), gp = gpar(fontface = 2, cex = 1.3),
                             name = "countertext3")
    counterborder <- rectGrob(x = xunit, y = unit(0.5, "npc") + unit(0.5, "lines"),
                              width = stringWidth("1000 of 1000"),
                              height = unit(2, "mm") + unit(3, "lines"),
                              gp = gpar(fill = "white"),
                              just = c("centre", "top"), vp = canvas$graphPath("stat"),
                              name = "counterborder")
    countertext <- grobTree(counterborder, countertext1, countertext2, countertext3,
                            name = "countertext")
    canvas$image <- addGrob(canvas$image, countertext)
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
    ## If running out of samples, select a random starting point in first 100.
    for (j in c(seq(1 , 1000, by = 10), 1000)) {
        canvas$plotSample(e)
        canvas$plotSampleStat(e)
        canvas$plotStatDist(e)
        canvas$advanceWhichSample()
        success <- sum(e$results[1:j])
        xunit <- unit(0, "npc") + unit(1, "mm")
        countertext1 <- textGrob("Coverage:", x = xunit, y = unit(0.5, "npc"),
                                 vp = canvas$graphPath("stat"), gp = gpar(fontface = 2),
                                 name = "countertext1")
        countertext2 <- textGrob(paste(success, "of", j), x = xunit,
                                 y = unit(0.5, "npc") - unit(1, "lines"),
                                 vp = canvas$graphPath("stat"), name = "countertext2")
        countertext3 <- textGrob(paste(round(success/j*100, 1), "%"),
                                 x = xunit, y = unit(0.5, "npc") - unit(2/1.3, "lines"),
                                 vp = canvas$graphPath("stat"), gp = gpar(fontface = 2, cex = 1.3),
                                 name = "countertext3")
        counterborder <- rectGrob(x = xunit, y = unit(0.5, "npc") + unit(0.5, "lines"),
                                  width = stringWidth("1000 of 1000"),
                                  height = unit(2, "mm") + unit(3, "lines"),
                                  gp = gpar(fill = "white"),
                                  just = c("centre", "top"), vp = canvas$graphPath("stat"),
                                  name = "counterborder")
        countertext <- grobTree(counterborder, countertext1, countertext2, countertext3,
                                name = "countertext")
        canvas$image <- addGrob(canvas$image, countertext)
        canvas$showLabels()
        canvas$drawImage()
    }
    ## Move 1000 CIs next time something is plotted to avoid further CIs getting plotted on top.
    #canvas$image <- removeGrob(canvas$image, gPath("sample.stat"))
    canvas$image <- removeGrob(canvas$image, gPath("stat.dist"))
    canvas$image <- removeGrob(canvas$image, gPath("countertext"))
    ## Reset CI counter
    canvas$sampled.stats <- NULL
}



