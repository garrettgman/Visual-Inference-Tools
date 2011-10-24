load_bootstrap_median <- function(e){
    PLOT_DATA <<- PLOT_DATA
    PLOT_SAMPLE <<- plotSamplePointsAndBoxplotGhostMedian
    SHOW_LABELS <<- bootLabels
    CALC_STAT <<- c("mean" = mean, "median" = median)[[svalue(e$stat)]]
    PLOT_DATA_STAT <<- c("mean" = addMeanLine, "median" = addMedianLine)[[svalue(e$stat)]]
    PLOT_SAMPLE_STAT <<- notYetImplemented
    PLOT_STAT_DIST <<- plotBootDist
    ANIMATE_SAMPLE <<- dropPoints1d
    ANIMATE_STAT <<- dropStat
    DISPLAY_RESULT <<- showCIandStats
    HANDLE_1000 <<- boot1000median
    e$plotted <- NULL
}

plotSamplePointsAndBoxplotGhostMedian <- function(canvas, e, i){
    bluecol <- "blue"
    redcol <- "red"
    if (e$cb){
        bluecol <- dichromat(bluecol)
        redcol <- dichromat(redcol)
    }
    alpha = 0.25
    canvas$sampled.stats <- c(canvas$sampled.stats, canvas$which.sample)
    x <- canvas$samples[[i]]
    y <- stackPoints(x, vp = canvas$graphPath("sample"))
    plotPoints(canvas, x, y, canvas$graphPath("sample"), "samplePlot", black = FALSE)
    allinfo <- sapply(canvas$samples[canvas$sampled.stats], function(x) fivenum(x)[2:4])
    canvas$image <- addGrob(canvas$image, ghostsGrob(allinfo[1,],
                                                     allinfo[2,],
                                                     allinfo[3,],
                                                     alpha = alpha,
                                                     vp = canvas$graphPath("sample"),
                                                     name = "samplePlot.ghosts.1"))
    canvas$image <- addGrob(canvas$image, boxplotGrob(x, box.color = "black",
                                                      median.color = "black",
                                                      show.w = FALSE,
                                                      name = "samplePlot.boxplot.1",
                                                      vp = canvas$graphPath("sample")))
}

boot1000median <- function(canvas, e){
    if ("samplePlot.points.1" %in% childNames(canvas$image))
        canvas$image <- removeGrob(canvas$image, gPath("samplePlot.points.1"))
    if ("samplePlot.boxplot.1" %in% childNames(e$c1$image))
        canvas$image <- removeGrob(canvas$image, gPath("samplePlot.boxplot.1"))
    if ("samplePlot.ghosts.1" %in% childNames(e$c1$image))
        canvas$image <- removeGrob(canvas$image, gPath("samplePlot.ghosts.1"))
    allx <- c(canvas$stat.dist, recursive = TRUE)
    allinfo <- sapply(canvas$samples, function(x) fivenum(x)[2:4])
    for (i in 50*(1:20)){
        x <- allx[1:i]
        y <- stackPoints(x, vp = canvas$graphPath("stat"), y.min = 0, y.max = 0.9)
        plotPoints(canvas, x, y, canvas$graphPath("stat"), "statPlot", black = FALSE, alpha = 0.7)
        canvas$image <- addGrob(canvas$image, ghostsGrob(allinfo[1, 1:i], allinfo[2, 1:i],
                                                         allinfo[3, 1:i], alpha = 0.05,
                                                         vp = canvas$graphPath("sample"),
                                                         name = "samplePlot.ghosts.1"))

        canvas$drawImage()
    }
    ## Remove 1000 statistics next time something is plotted to avoid
    ## further statistics being plotted on top.
    #canvas$image <- removeGrob(canvas$image, gPath("statPlot.points.1"))
    ## Reset CI counter
    canvas$sampled.stats <- NULL
}
