#' methods for permutation tests.
load_permutation_mean <- function(e){
    PLOT_DATA <<- PLOT_DATA
    PLOT_SAMPLE <<- plotSampleGroupPointsMean
    SHOW_LABELS <<- permLabels
    CALC_STAT <<- calcMeanDiff
    PLOT_DATA_STAT <<- dataDiffArrowMean
    PLOT_STAT_DIST <<- plotDiffDist
    ANIMATE_SAMPLE <<- permTwoSample
    TRACK_SAMPLE <<- notYetImplemented
    ANIMATE_STAT <<- dropPermArrow
    DISPLAY_RESULT <<- showTailMean
    HANDLE_1000 <<- perm1000
    FADE_PLOTS <<- notYetImplemented
}

plotSampleGroupPointsMean <- function(canvas, e, i){
    x <- canvas$samples[[i]]
    levels <- canvas$levels
    ylevels <- sort(unique(levels))
    y <- stackPoints(x, levels, vp = canvas$graphPath("sample"))
    n <- 1
    ## Plotting samples
    for (j in ylevels) {
        plotPoints(canvas, x[levels == j],
                   y[levels == j], col = c("tan4", "darkseagreen")[n],
                   vp = canvas$graphPath("sample", as.character(n)),
                   name = "samplePlot")
        plotBoxplot(canvas, x[levels == j],
                    stat = mean, stat.color = "blue", vp = canvas$graphPath("sample", as.character(n)),
                    name = "samplePlot")
        n <- n + 1
    }
    y.mixed <- stackPoints(canvas$x, vp = canvas$graphPath("sample", 1),
                           y.min = 0.8, y.max = 1)
    y.mixed <- y.mixed[canvas$indexes[[i]]]
    y.mixed.1 <- y.mixed[levels == ylevels[1]]
    y.mixed.2 <- y.mixed[levels == ylevels[2]]
    ## Plotting faded mixed sample
    plotPoints(canvas, x[levels == ylevels[1]],
               y.mixed.1, alpha = 0.25,
               vp = canvas$graphPath("sample", 1), name = "samplePlotJoin1", col = "tan4")
    plotPoints(canvas, x[levels == ylevels[2]],
               y.mixed.2, alpha = 0.25,
               vp = canvas$graphPath("sample", 1), name = "samplePlotJoin2", col = "darkseagreen")
    canvas$image <- addGrob(canvas$image, linesGrob(x = unit(canvas$stat.dist[[i]], "native"),
                                                    y = unit(0.8, "npc"),
                                                    gp = gpar(lwd = 2, col = "red"),
                                                    arrow = arrow(length = unit(0.1, "inches")),
                                                    vp = canvas$graphPath("sample", 1),
                                                    name = "samplePlot.stat.2"))
    text <- as.character(levels[canvas$indexes[[i]]])
    canvas$image <- addGrob(canvas$image, coldatatextGrob
                            (data = text, title = "Re-randomised", cols = c("tan4", "darkseagreen"),
                             xpos = 0.5, max = 50,
                             name = "databox.text.2", vp = canvas$graphPath("databox", 2)))
}


permLabels <- function(canvas){
    samplabel <- textGrob("Data",
                         x = unit(0, "npc") + unit(1, "mm"),
                         y = unit(0.8, "npc"),
                         just = c("left", "top"),
                         vp = canvas$graphPath("data", 2),
                         gp = gpar(fontface = 2))
    resamplabel <- textGrob("Re-randomised samples",
                          x = unit(0, "npc") + unit(1, "mm"),
                          y = unit(0.6, "npc"),
                          just = c("left", "top"),
                          vp = canvas$graphPath("sample", 2),
                          gp = gpar(fontface = 2))
    statlabel <- textGrob("Re-randomised distribution",
                          x = unit(0, "npc") + unit(1, "mm"),
                          y = unit(0.8, "npc"),
                          just = c("left", "top"),
                          vp = canvas$graphPath("stat"),
                          gp = gpar(fontface = 2))
    bootlabels <- grobTree(samplabel, resamplabel, statlabel, name = "bootlabels")
    canvas$image <- addGrob(canvas$image, bootlabels)
}

calcMeanDiff <- function(x, y){
    ylevels <- sort(unique(y))
    mean1 <- mean(x[y == ylevels[1]])
    mean2 <- mean(x[y != ylevels[1]])
    c(mean2, mean1)
}

dataDiffArrowMean <- function(canvas, e){
    x <- canvas$x
    levels <- canvas$levels
    ylevels <- sort(unique(levels))
    mean1 <- mean(x[levels == ylevels[1]])
    mean2 <- mean(x[levels != ylevels[1]])
    y <- stackPoints(x, levels, vp = canvas$graphPath("data"))
    n <- 1
    ## Plotting coloured data.
    for (i in ylevels) {
        plotPoints(canvas, x[levels == i],
                   y[levels == i], col = c("tan4", "darkseagreen")[n],
                   vp = canvas$graphPath("data", as.character(n)),
                   name = "dataPlot")
        plotBoxplot(canvas, x[levels == i], stat = mean, stat.color = "purple",
                    vp = canvas$graphPath("data", as.character(n)),
                    name = "dataPlot")
        n <- n + 1
    }
    ## Plotting arrow on data plot.
    canvas$image <- addGrob(canvas$image, linesGrob(x = unit(c(mean2, mean1), "native"),
                                                    y = unit(0.8, "npc"),
                                                    gp = gpar(lwd = 2, col = "red"),
                                                    arrow = arrow(length = unit(0.1, "inches")),
                                                    vp = canvas$graphPath("data", 1),
                                                    name = "dataPlot.stat.2"))
    canvas$image <- addGrob(canvas$image, linesGrob(x = unit(rep(0, 2), "native"),
                                                    y = unit(0:1, "npc"),
                                                    gp = gpar(lty = "dashed"),
                                                    vp = canvas$graphPath("stat"),
                                                    name = "zeroline.1"))
}

permTwoSample <- function(canvas, e, n.steps, mix = TRUE){
    e$clearPanel("sample")
    ## Drop samples down to middle plot.
    x <- canvas$x
    levels <- canvas$levels
    ylevels <- sort(unique(levels))
    y <- stackPoints(canvas$x, canvas$levels, vp = canvas$graphPath("data"), y.min = 0.3, y.max = 1)
    y.start <- y
    y.end <- y - 2
    y.step <- (y.start - y.end)/n.steps
    if (mix){
    ## Dropping samples
    for (i in 1:n.steps){
        plotPointGroups(canvas, x, y.start - i*y.step, levels, "data",
                        cols = c("tan4", "darkseagreen"), "temp")
        canvas$drawImage()
    }
    canvas$pauseImage(10)
    canvas$image <- removeGrob(canvas$image, gPath("temp.points.1"))
    canvas$image <- removeGrob(canvas$image, gPath("temp.points.2"))
    ## Mixing samples
    y.end <- stackPoints(x, vp = canvas$graphPath("sample"), y.min = 0, y.max = 0.2)
    y.end.1 <- y.end[levels == ylevels[1]] + 0.8
    y.end.2 <- y.end[levels == ylevels[2]] - 0.2
    y.start.1 <- y.start[levels == ylevels[1]]
    y.start.2 <- y.start[levels == ylevels[2]]
    y.step.1 <- (y.start.1 - y.end.1)/n.steps
    y.step.2 <- (y.start.2 - y.end.2)/n.steps
    for (i in 1:n.steps){
        plotPoints(canvas, x[levels == ylevels[1]],
                   y.start.1 - i*y.step.1,
                   vp = canvas$graphPath("sample", 1), name = "tempjoin", col = "tan4")
        plotPoints(canvas, x[levels == ylevels[2]],
                   y.start.2 - i*y.step.2,
                   vp = canvas$graphPath("sample", 2), name = "tempjoin", col = "darkseagreen")
        canvas$drawImage()
    }
    canvas$image <- removeGrob(canvas$image, gPath("tempjoin.points.1"))
    canvas$image <- removeGrob(canvas$image, gPath("tempjoin.points.2"))
    plotPoints(canvas, x, y.end + 0.8,
               vp = canvas$graphPath("sample", 1), name = "samplePlotJoin", col = "black")
    canvas$pauseImage(10)
    canvas$image <- removeGrob(canvas$image, gPath("samplePlotJoin.points.1"))
}
    ## Separating samples
    x.sample <- canvas$samples[[canvas$which.sample]]
    y.start <- stackPoints(x, vp = canvas$graphPath("sample", 1),
                           y.min = 0.8, y.max = 1)
    y.start <- y.start[canvas$indexes[[canvas$which.sample]]]
    y.start.1 <- y.start[levels == ylevels[1]]
    y.start.2 <- y.start[levels == ylevels[2]]
    plotPoints(canvas, x.sample[levels == ylevels[1]],
               y.start.1, alpha = 0.25,
               vp = canvas$graphPath("sample", 1), name = "samplePlotJoin1", col = "tan4")
    plotPoints(canvas, x.sample[levels == ylevels[2]],
               y.start.2, alpha = 0.25,
               vp = canvas$graphPath("sample", 1), name = "samplePlotJoin2", col = "darkseagreen")
    canvas$pauseImage(10)
    y.sample <- stackPoints(x.sample, levels, vp = canvas$graphPath("sample"))
    y.end.1 <- y.sample[levels == ylevels[1]]
    y.end.2 <- y.sample[levels == ylevels[2]] + 1
    y.step.1 <- (y.start.1 - y.end.1)/n.steps
    y.step.2 <- (y.start.2 - y.end.2)/n.steps
    for (i in 1:n.steps){
        plotPoints(canvas, x.sample[levels == ylevels[1]],
                   y.start.1 - i*y.step.1,
                   vp = canvas$graphPath("sample", 1), name = "tempjoin1", col = "tan4")
        plotPoints(canvas, x.sample[levels == ylevels[2]],
                   y.start.2 - i*y.step.2,
                   vp = canvas$graphPath("sample", 1), name = "tempjoin2", col = "darkseagreen")
        canvas$drawImage()
    }
    canvas$image <- removeGrob(canvas$image, gPath("tempjoin1.points.1"))
    canvas$image <- removeGrob(canvas$image, gPath("tempjoin2.points.1"))
    canvas$image <- removeGrob(canvas$image, gPath("samplePlotJoin1.points.1"))
    canvas$image <- removeGrob(canvas$image, gPath("samplePlotJoin2.points.1"))

}

dropPermArrow <- function(canvas, e, n.steps){
    arrowbounds <- canvas$stat.dist[c(canvas$plotted.stats, canvas$which.sample)]
    stats <- sapply(arrowbounds, diff)
    curr.arrow <- arrowbounds[[length(stats)]]
    curr.stat <- stats[length(stats)]
    y <- stackPoints(stats, vp = canvas$graphPath("stat"), y.min = 0)
    y.start <- 1.4
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
                                          vp = vpPath("canvas.all", "canvas.plots", "canvas.frame", "animation.field"),
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
    canvas$image <- removeGrob(canvas$image, gPath("databox.text.2"))
    canvas$drawImage()
}

showTailMean <- function(canvas, e){
    n.steps <- 10
    x <- canvas$x
    levels <- canvas$levels
    ylevels <- sort(unique(levels))
    mean1 <- mean(x[levels == ylevels[1]])
    mean2 <- mean(x[levels != ylevels[1]])
    y.start <- 2.4
    y.end <- 0
    y.step <- (y.start - y.end)/n.steps
    x.start <- mean2
    x.end <- mean(range(x))
    x.step <- (x.start - x.end)/n.steps
    for (i in 0:10){
        canvas$image <- addGrob(canvas$image, linesGrob(x = unit(c(mean2, mean1) - i*x.step, "native"),
                                                        y = unit(y.start - i*y.step, "native"),
                                                        gp = gpar(lwd = 2, col = "red"),
                                                        arrow = arrow(length = unit(0.1, "inches")),
                                                        vp = vpPath("canvas.all", "canvas.plots", "canvas.frame", "animation.field"),
                                                        name = "temp.arrow"))
        canvas$drawImage()
    }
    canvas$image <- removeGrob(canvas$image, gPath("temp.arrow"))
    stats <- sapply(canvas$stat.dist, diff)
    diff <- diff(c(mean2, mean1))
    y.max <- unit(1, "npc") - unit(2, "lines")
    y <- stackPoints(stats, vp = canvas$graphPath("stat"), y.min = 0,
                     y.max = y.max)
    if (diff > 0){
        x.in <- stats[stats < diff]
        x.out <- stats[stats >= diff]
        y.in <- y[stats < diff]
        y.out <- y[stats >= diff]
        tot <- sum(stats >= diff)
        p <- mean(stats >= diff)
    } else {
        x.in <- stats[stats > diff]
        x.out <- stats[stats <= diff]
        y.in <- y[stats > diff]
        y.out <- y[stats <= diff]
        tot <- sum(stats <= diff)
        p <- mean(stats <= diff)
    }
    if (length(x.in) > 0)
        canvas$image <- addGrob(canvas$image, pointsGrob
                                (x.in, y.in, gp = gpar(col = "lightgrey", lwd = 2, alpha = 0.7),
                                 vp = canvas$graphPath("stat"), name = "lightpoints"))
    if (length(x.out) > 0)
        canvas$image <- addGrob(canvas$image, pointsGrob
                                (x.out, y.out, gp = gpar(col = "grey60", lwd = 2, alpha = 0.7),
                                         vp = canvas$graphPath("stat"), name = "darkpoints"))
    canvas$image <- addGrob(canvas$image, textGrob
                            (paste(tot, "/ 1000 =", p, sep = " "),
                             x = unit(diff, "native"), y = unit(0.5, "npc"),
                             just = c("centre", "bottom"), vp = canvas$graphPath("stat"),
                             name = "proptext"))
    canvas$image <- addGrob(canvas$image, linesGrob
                            (x = unit(rep(diff, 2), "native"),
                             y = unit(c(0, 0.5), "npc") - unit(1, "mm"),
                             vp = canvas$graphPath("stat"), name = "propline"))
    canvas$image <- addGrob(canvas$image, linesGrob
                            (x = unit(c(mean2, mean1) - x.start + x.end, "native"),
                             y = unit(y.end, "native"),
                             gp = gpar(lwd = 2, col = "red"),
                             arrow = arrow(length = unit(0.1, "inches")),
                             vp = vpPath("canvas.all", "canvas.plots",
                             "canvas.frame", "animation.field"),
                             name = "statPlot.arrow.1"))
    canvas$drawImage()
    canvas$image <- removeGrob(canvas$image, gPath("proptext"))
    canvas$image <- removeGrob(canvas$image, gPath("propline"))
    canvas$image <- removeGrob(canvas$image, gPath("statPlot.arrow.1"))
    if (length(x.in) > 0)
        canvas$image <- removeGrob(canvas$image, gPath("lightpoints"))
    if (length(x.out) > 0)
        canvas$image <- removeGrob(canvas$image, gPath("darkpoints"))
}
