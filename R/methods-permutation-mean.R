#' methods for permutation tests.
load_permutation_mean <- function(e){
    PLOT_DATA <<- PLOT_DATA
    PLOT_SAMPLE <<- plotSampleGroupPoints
    SHOW_LABELS <<- permLabels
    CALC_STAT <<- calcMeanDiff
    PLOT_DATA_STAT <<- dataDiffArrow
    PLOT_STAT_DIST <<- plotDiffDist
    ANIMATE_SAMPLE <<- notYetImplemented
    TRACK_SAMPLE <<- notYetImplemented
    ANIMATE_STAT <<- notYetImplemented
    DISPLAY_RESULT <<- notYetImplemented
    HANDLE_1000 <<- notYetImplemented
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
}
