#' methods for permutation tests.
load_permutation_mean <- function(e){
    PLOT_DATA <<- PLOT_DATA
    PLOT_SAMPLE <<- plotSampleGroupPoints
    SHOW_LABELS <<- permLabels
    CALC_STAT <<- calcMeanDiff
    PLOT_DATA_STAT <<- dataDiffArrow
    PLOT_STAT_DIST <<- plotDiffDist
    ANIMATE_SAMPLE <<- permTwoSample
    TRACK_SAMPLE <<- notYetImplemented
    ANIMATE_STAT <<- dropPermArrow
    DISPLAY_RESULT <<- notYetImplemented
    HANDLE_1000 <<- perm1000
    FADE_PLOTS <<- notYetImplemented
}

plotSampleGroupPoints <- function(canvas, e, i){
    x <- canvas$samples[[i]]
    y <- stackPoints(x, canvas$levels, vp = canvas$graphPath("sample"))
    n <- 1
    for (j in unique(canvas$levels)) {
        plotPoints(canvas, x[canvas$levels == j],
                   y[canvas$levels == j],
                   vp = canvas$graphPath("sample", as.character(n)),
                   name = "samplePlot")
        plotBoxplot(canvas, x[canvas$levels == j],
                    stat = mean, stat.color = "blue", vp = canvas$graphPath("sample", as.character(n)),
                    name = "samplePlot")
        n <- n + 1
    }
    canvas$image <- addGrob(canvas$image, linesGrob(x = unit(canvas$stat.dist[[i]], "native"),
                                                    y = unit(0, "npc"),
                                                    gp = gpar(lwd = 2, col = "red"),
                                                    arrow = arrow(length = unit(0.1, "inches")),
                                                    vp = canvas$graphPath("sample", 2),
                                                    name = "samplePlot.stat.2"))
}


permLabels <- function(canvas){
    samplabel <- textGrob("Samples",
                         x = unit(0, "npc") + unit(1, "mm"),
                         y = unit(0.8, "npc"),
                         just = c("left", "top"),
                         vp = canvas$graphPath("data", 2),
                         gp = gpar(fontface = 2))
    resamplabel <- textGrob("Permuted samples",
                          x = unit(0, "npc") + unit(1, "mm"),
                          y = unit(0.6, "npc"),
                          just = c("left", "top"),
                          vp = canvas$graphPath("sample", 2),
                          gp = gpar(fontface = 2))
    statlabel <- textGrob("Permutation distribution",
                          x = unit(0, "npc") + unit(1, "mm"),
                          y = unit(0.8, "npc"),
                          just = c("left", "top"),
                          vp = canvas$graphPath("stat"),
                          gp = gpar(fontface = 2))
    bootlabels <- grobTree(samplabel, resamplabel, statlabel, name = "bootlabels")
    canvas$image <- addGrob(canvas$image, bootlabels)
}

calcMeanDiff <- function(x, y){
    ylevels <- unique(y)
    mean1 <- mean(x[y == ylevels[1]])
    mean2 <- mean(x[y != ylevels[1]])
    c(mean2, mean1)
}

dataDiffArrow <- function(canvas, e){
    x <- canvas$x
    levels <- canvas$levels
    ylevels <- unique(levels)
    mean1 <- mean(x[levels == ylevels[1]])
    mean2 <- mean(x[levels != ylevels[1]])
    y <- stackPoints(x, levels, vp = canvas$graphPath("data"))
    n <- 1
    for (i in ylevels) {
        plotPoints(canvas, x[levels == i],
                   y[levels == i],
                   vp = canvas$graphPath("data", as.character(n)),
                   name = "dataPlot")
        plotBoxplot(canvas, x[levels == i], stat = mean, stat.color = "purple",
                    vp = canvas$graphPath("data", as.character(n)),
                    name = "dataPlot")
        n <- n + 1
    }
    canvas$image <- addGrob(canvas$image, linesGrob(x = unit(c(mean2, mean1), "native"),
                                                    y = unit(0, "npc"),
                                                    gp = gpar(lwd = 2, col = "red"),
                                                    arrow = arrow(length = unit(0.1, "inches")),
                                                    vp = canvas$graphPath("data", 2),
                                                    name = "dataPlot.stat.2"))
    canvas$image <- addGrob(canvas$image, linesGrob(x = unit(rep(0, 2), "native"),
                                                    y = unit(0:1, "npc"),
                                                    gp = gpar(lty = "dashed"),
                                                    vp = canvas$graphPath("stat"),
                                                    name = "zeroline.1"))
}

permTwoSample <- function(canvas, e, n.steps){
    ## Drop samples down to middle plot.
    x <- canvas$x
    levels <- canvas$levels
    y <- stackPoints(x, levels, vp = canvas$graphPath("data"))
    y.start <- y + 2
    y.end <- y + 1
    y.step <- (y.start - y.end)/n.steps
    for (i in 1:n.steps){
        for (j in unique(levels)) {
        plotPoints(canvas, x[levels == j],
                   y.start[levels == j] - i*y.step[levels == j],
                   vp = vpPath("canvas.frame", "animation.field"),
                   name = "temp")
    }
        canvas$drawImage()
    }

}

dropPermArrow <- function(canvas, e, n.steps){
    arrowbounds <- canvas$stat.dist[c(canvas$plotted.stats, canvas$which.sample)]
    stats <- sapply(arrowbounds, diff)
    curr.arrow <- arrowbounds[[length(stats)]]
    curr.stat <- stats[length(stats)]
    y <- stackPoints(stats, vp = canvas$graphPath("stat"), y.min = 0)
    y.start <- 1.5
    y.end <- y[length(stats)]
    y.step <- (y.start - y.end)/n.steps
    xs.start <- curr.arrow
    xs.end <- curr.arrow - (curr.arrow[1] - mean(range(canvas$x)))
    xs.step <- (xs.start - xs.end)/n.steps
    for (i in 0:n.steps){
        temp.arrow <- linesGrob(x = unit(xs.start - i*xs.step, "native"),
                                          y = unit(rep(y.start - i*y.step, 2), "native"),
                                          gp = gpar(lwd = 2, col = "red"),
                                          arrow = arrow(length = unit(0.1, "inches")),
                                          vp = vpPath("canvas.frame", "animation.field"),
                                          name = "temp.arrow")
        canvas$image <- addGrob(canvas$image, temp.arrow)
        canvas$drawImage()
    }
    canvas$image <- removeGrob(canvas$image, gPath("temp.arrow"))
}

plotDiffDist <- function(canvas, e){
    canvas$plotted.stats <- c(canvas$plotted.stats, canvas$which.sample)
    stats <- sapply(canvas$stat.dist[canvas$plotted.stats], diff)
    y <- stackPoints(stats, vp = canvas$graphPath("stat"), y.min = 0)
    plotPoints(canvas, stats, y, vp = canvas$graphPath("stat"), name = "statPlot")
}

perm1000 <- function(canvas, e){
    stats <- sapply(canvas$stat.dist, diff)
    y.max <- unit(1, "npc") - unit(2, "lines")
    for (i in 20*(1:50)){
        canvas$plotSample(e, i)
        x <- stats[1:i]
        y <- stackPoints(x, vp = canvas$graphPath("stat"), y.min = 0,
                         y.max = y.max*i*0.001)
        plotPoints(canvas, x, y, canvas$graphPath("stat"),
                   "statPlot", black = FALSE, alpha = 0.7)
        canvas$drawImage()
    }
    canvas$plotted.stats <- NULL
    e$clearPanel("sample")
    canvas$drawImage()
}
