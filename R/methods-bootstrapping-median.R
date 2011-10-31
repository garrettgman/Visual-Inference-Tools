load_bootstrap_median <- function(e){
    PLOT_DATA <<- PLOT_DATA
    PLOT_SAMPLE <<- plotSamplePointsAndBoxplotGhostMedian
    SHOW_LABELS <<- bootLabels
    CALC_STAT <<- c("mean" = mean, "median" = median)[[svalue(e$stat)]]
    PLOT_DATA_STAT <<- lineOnBoxplotMedian
    PLOT_SAMPLE_STAT <<- notYetImplemented
    PLOT_STAT_DIST <<- plotBootDist
    ANIMATE_SAMPLE <<- moveDataTextAndDropPoints
    ANIMATE_STAT <<- dropStat
    DISPLAY_RESULT <<- showCIandStats
    HANDLE_1000 <<- boot1000median
    FADE_PLOTS <<- fadeSampleAndStat
}

plotSamplePointsAndBoxplotGhostMedian <- function(canvas, e, i){
    if ("dataPlot.ci.1" %in% childNames(canvas$image))
        canvas$image <- removeGrob(canvas$image, gPath("dataPlot.ci.1"))
    if ("samplePlot.rect.1" %in% childNames(canvas$image))
        canvas$image <- removeGrob(canvas$image, gPath("samplePlot.rect.1"))
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
                                                     alpha = alpha, box.color = "lightpink",
                                                     vp = canvas$graphPath("sample"),
                                                     name = "samplePlot.ghosts.1"))
    canvas$image <- addGrob(canvas$image, boxplotGrob(x, box.color = "black",
                                                      median.color = "black",
                                                      show.w = FALSE, gp = gpar(lwd = 5),
                                                      name = "samplePlot.boxplot.1",
                                                      vp = canvas$graphPath("sample")))
    canvas$image <- addGrob(canvas$image, datatextGrob(data = x, title = "Resample",
                                                       name = "databox.text.2",
                                                       gp = gpar(col = "red"),
                                                       vp = canvas$graphPath("databox", 2)))
    canvas$image <- addGrob(canvas$image, linesGrob
                            (x = unit(canvas$stat.dist[i], "native"),
                             y = unit(c(0.05, 0.5), "npc"), gp = gpar(lwd = 4, col = "blue"),
                             vp = canvas$graphPath("sample"), name = "samplePlot.lines.1"))
}

lineOnBoxplotMedian <- function(canvas, e){
   plotBoxplot(canvas, canvas$x, stat = median, stat.color = "purple3",
               canvas$graphPath("data"), "dataPlot")
}

boot1000median <- function(canvas, e, points = FALSE){
    if ("databox.text.2" %in% childNames(canvas$image))
        canvas$image <- removeGrob(canvas$image, gPath("databox.text.2"))
    if ("dataPlot.ci.1" %in% childNames(canvas$image))
        canvas$image <- removeGrob(canvas$image, gPath("dataPlot.ci.1"))
    allx <- c(canvas$stat.dist, recursive = TRUE)
    allinfo <- sapply(canvas$samples, function(x) fivenum(x)[2:4])
    for (i in 50*(1:20)){
        canvas$plotSample(e, i)
        x <- allx[1:i]
        y <- stackPoints(x, vp = canvas$graphPath("stat"), y.min = 0, y.max = 0.9)
        if (points)
            plotPoints(canvas, x, y, canvas$graphPath("stat"), "statPlot",
                       black = FALSE, alpha = 0.7)
        canvas$image <- addGrob(canvas$image, ghostsGrob(allinfo[1, 1:i], allinfo[2, 1:i],
                                                         allinfo[3, 1:i], alpha = 0.05,
                                                         box.color = "lightpink",
                                                         vp = canvas$graphPath("sample"),
                                                         name = "samplePlot.ghosts.1"))

        canvas$drawImage()
    }
    ## Remove 1000 statistics next time something is plotted to avoid
    ## further statistics being plotted on top.
    #canvas$image <- removeGrob(canvas$image, gPath("statPlot.points.1"))
    ## Reset CI counter
    canvas$sampled.stats <- NULL
    canvas$plotted.stats <- NULL
}
