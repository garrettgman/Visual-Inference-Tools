load_permutation_median <- function(e){
    PLOT_DATA <<- PLOT_DATA
    PLOT_DATA <<- PLOT_DATA
    PLOT_SAMPLE <<- plotSampleGroupPointsMedian
    SHOW_LABELS <<- permLabels
    CALC_STAT <<- calcMedianDiff
    PLOT_DATA_STAT <<- dataDiffArrowMedian
    PLOT_STAT_DIST <<- plotDiffDist
    ANIMATE_SAMPLE <<- permTwoSample
    TRACK_SAMPLE <<- notYetImplemented
    ANIMATE_STAT <<- dropPermArrow
    DISPLAY_RESULT <<- showTailMedian
    HANDLE_1000 <<- perm1000
    FADE_PLOTS <<- notYetImplemented
}

plotSampleGroupPointsMedian <- function(canvas, e, i){
    x <- canvas$samples[[i]]
    levels <- canvas$levels
    ylevels <- unique(levels)
    y <- stackPoints(x, levels, vp = canvas$graphPath("sample"))
    n <- 1
    for (j in ylevels) {
        plotPoints(canvas, x[levels == j],
                   y[levels == j], col = c("darkseagreen", "tan4")[n],
                   vp = canvas$graphPath("sample", as.character(n)),
                   name = "samplePlot")
        plotBoxplot(canvas, x[levels == j],
                    stat = median, stat.color = "blue", vp = canvas$graphPath("sample", as.character(n)),
                    name = "samplePlot")
        n <- n + 1
    }
    y.mixed <- stackPoints(canvas$x, vp = canvas$graphPath("sample", 1),
                           y.min = 0.8, y.max = 1)
    y.mixed <- y.mixed[canvas$indexes[[i]]]
    y.mixed.1 <- y.mixed[levels == ylevels[1]]
    y.mixed.2 <- y.mixed[levels == ylevels[2]]
    plotPoints(canvas, x[levels == ylevels[1]],
               y.mixed.1, alpha = 0.25,
               vp = canvas$graphPath("sample", 1), name = "samplePlotJoin1", col = "darkseagreen")
    plotPoints(canvas, x[levels == ylevels[2]],
               y.mixed.2, alpha = 0.25,
               vp = canvas$graphPath("sample", 1), name = "samplePlotJoin2", col = "tan4")
    canvas$image <- addGrob(canvas$image, linesGrob(x = unit(canvas$stat.dist[[i]], "native"),
                                                    y = unit(0.8, "npc"),
                                                    gp = gpar(lwd = 2, col = "red"),
                                                    arrow = arrow(length = unit(0.1, "inches")),
                                                    vp = canvas$graphPath("sample", 1),
                                                    name = "samplePlot.stat.2"))
    text <- as.character(levels[canvas$indexes[[i]]])
    canvas$image <- addGrob(canvas$image, coldatatextGrob
                            (data = text, title = "Re-randomised", cols = c("tan4", "darkseagreen"),
                             x = 0.5, max = 50,
                             name = "databox.text.2", vp = canvas$graphPath("databox", 2)))
}

calcMedianDiff <- function(x, y){
    ylevels <- unique(y)
    median1 <- median(x[y == ylevels[1]])
    median2 <- median(x[y != ylevels[1]])
    c(median2, median1)
}

dataDiffArrowMedian <- function(canvas, e){
    x <- canvas$x
    levels <- canvas$levels
    ylevels <- unique(levels)
    median1 <- median(x[levels == ylevels[1]])
    median2 <- median(x[levels != ylevels[1]])
    y <- stackPoints(x, levels, vp = canvas$graphPath("data"))
    n <- 1
    for (i in ylevels) {
        plotPoints(canvas, x[levels == i],
                   y[levels == i], col = c("darkseagreen", "tan4")[n],
                   vp = canvas$graphPath("data", as.character(n)),
                   name = "dataPlot")
        plotBoxplot(canvas, x[levels == i], stat = median, stat.color = "purple",
                    vp = canvas$graphPath("data", as.character(n)),
                    name = "dataPlot")
        n <- n + 1
    }
    canvas$image <- addGrob(canvas$image, linesGrob(x = unit(c(median2, median1), "native"),
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

showTailMedian <- function(canvas, e){
    n.steps <- 10
    x <- canvas$x
    levels <- canvas$levels
    ylevels <- unique(levels)
    median1 <- median(x[levels == ylevels[1]])
    median2 <- median(x[levels != ylevels[1]])
    y.start <- 2.4
    y.end <- 0
    y.step <- (y.start - y.end)/n.steps
    x.start <- median2
    x.end <- mean(range(x))
    x.step <- (x.start - x.end)/n.steps
    for (i in 0:10){
        canvas$image <- addGrob(canvas$image, linesGrob(x = unit(c(median2, median1) - i*x.step, "native"),
                                                        y = unit(y.start - i*y.step, "native"),
                                                        gp = gpar(lwd = 2, col = "red"),
                                                        arrow = arrow(length = unit(0.1, "inches")),
                                                        vp = vpPath("canvas.all", "canvas.plots", "canvas.frame", "animation.field"),
                                                        name = "temp.arrow"))
        canvas$drawImage()
    }
    canvas$image <- removeGrob(canvas$image, gPath("temp.arrow"))
    stats <- sapply(canvas$stat.dist, diff)
    diff <- diff(c(median2, median1))
    tot <- sum(stats >= diff)
    p <- mean(stats >= diff)
    y.max <- unit(1, "npc") - unit(2, "lines")
    y <- stackPoints(stats, vp = canvas$graphPath("stat"), y.min = 0,
                     y.max = y.max)
    x.in <- stats[stats < diff]
    x.out <- stats[stats >= diff]
    y.in <- y[stats < diff]
    y.out <- y[stats >= diff]
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
                            (x = unit(c(median2, median1) - x.start + x.end, "native"),
                             y = unit(y.end, "native"),
                             gp = gpar(lwd = 2, col = "red"),
                             arrow = arrow(length = unit(0.1, "inches")),
                             vp = vpPath("canvas.all", "canvas.plots",
                             "canvas.frame", "animation.field"),
                             name = "statPlot.arrow.1"))
    canvas$drawImage()
    canvas$image <- removeGrob(canvas$image, gPath("proptext"))
    canvas$image <- removeGrob(canvas$image, gPath("propline"))
    if (length(x.in) > 0)
        canvas$image <- removeGrob(canvas$image, gPath("lightpoints"))
    if (length(x.out) > 0)
        canvas$image <- removeGrob(canvas$image, gPath("darkpoints"))
}
