load_bootstrap_mean <- function(e){
    PLOT_DATA <<- PLOT_DATA
    PLOT_SAMPLE <<- plotSamplePointsAndBoxplotGhostMean
    SHOW_LABELS <<- bootLabels
    CALC_STAT <<- c("mean" = mean, "median" = median)[[svalue(e$stat)]]
    PLOT_DATA_STAT <<- c("mean" = addMeanLine, "median" = addMedianLine)[[svalue(e$stat)]]
    PLOT_SAMPLE_STAT <<- notYetImplemented
    PLOT_STAT_DIST <<- plotBootDist
    ANIMATE_SAMPLE <<- dropPoints1d
    ANIMATE_STAT <<- dropStat
    DISPLAY_RESULT <<- showCIandStats
    HANDLE_1000 <<- boot1000mean
}
bootLabels <- function(canvas){
    samplabel <- textGrob("Sample",
                         x = unit(0, "npc") + unit(1, "mm"),
                         y = unit(0.9, "npc"),
                         just = c("left", "top"),
                         vp = canvas$graphPath("data"),
                         gp = gpar(fontface = 2))
    resamplabel <- textGrob("Resample",
                          x = unit(0, "npc") + unit(1, "mm"),
                          y = unit(0.8, "npc"),
                          just = c("left", "top"),
                          vp = canvas$graphPath("sample"),
                          gp = gpar(fontface = 2))
    statlabel <- textGrob("Statistic distribution",
                          x = unit(0, "npc") + unit(1, "mm"),
                          y = unit(0.8, "npc"),
                          just = c("left", "top"),
                          vp = canvas$graphPath("stat"),
                          gp = gpar(fontface = 2))
    bootlabels <- grobTree(samplabel, resamplabel, statlabel, name = "bootlabels")
    canvas$image <- addGrob(canvas$image, bootlabels)
}

plotSamplePointsAndBoxplotGhostMean <- function(canvas, e, i){
    if ("dataPlot.rect.1" %in% childNames(canvas$image))
        canvas$image <- removeGrob(canvas$image, gPath("dataPlot.rect.1"))
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
    allinfo <- c(canvas$stat.dist, recursive = TRUE)
    canvas$image <- addGrob(canvas$image, rectGrob
                            (x = unit(allinfo[canvas$sampled.stats], "native"),
                             y = unit(0.15, "npc"), height = unit(0.2, "npc"),
                             width = 0, gp = gpar(alpha = alpha, col = "blue", lwd = 2),
                             vp = canvas$graphPath("sample"), name = "samplePlot.ghosts.1"))
    #canvas$image <- addGrob(canvas$image, boxplotGrob(x, box.color = "black",
    #                                                  median.color = "black",
    #                                                  stat = mean, stat.color = "red",
    #                                                  show.w = FALSE,
    #                                                  name = "samplePlot.boxplot.1",
    #                                                  vp = canvas$graphPath("sample")))
    canvas$image <- addGrob(canvas$image, datatextGrob(data = x, title = "Resample",
                                                       name = "text.resample",
                                                       vp = canvas$graphPath("databox", 2)))
}

plotBootDist <- function(canvas, e){
    canvas$plotted.stats <- c(canvas$plotted.stats, canvas$which.sample)
    x <- c(canvas$stat.dist, recursive = TRUE)[canvas$plotted.stats]
    y <- stackPoints(x, vp = canvas$graphPath("stat"), y.min = 0, y.max = 0.9)
    plotPoints(canvas, x, y, canvas$graphPath("stat"), "statPlot", black = FALSE)
}

dropStat <- function(canvas, e, n.steps){
    xs <- c(canvas$stat.dist, recursive = TRUE)[c(canvas$plotted.stats,
                              canvas$which.sample)]
    x <- xs[length(xs)]
    ys <- stackPoints(xs, vp = canvas$graphPath("stat"), y.min = 0, y.max = 0.9)
    y.start <- 1
    y.end <- ys[length(ys)]
    y.step <- (y.start - y.end)/n.steps
    linelength <- 0.2
    for (i in 0:n.steps){
        canvas$image <- addGrob(canvas$image, linesGrob
                                (x = unit(rep(x, 2), "native"),
                                 y = unit(c(y.start - i*y.step, y.start - i*y.step + linelength),
                                 "native"),
                                 gp = gpar(lwd = 4, col = "red"),
                                 vp = vpPath("canvas.all", "canvas.plots", "canvas.frame", "animation.field"),
                                 name = "temp"))
        canvas$drawImage()
        linelength <- linelength - 0.15/n.steps
    }
    canvas$image <- removeGrob(canvas$image, gPath("temp"))
}


boot1000mean <- function(canvas, e){
    if ("samplePlot.points.1" %in% childNames(canvas$image))
        canvas$image <- removeGrob(canvas$image, gPath("samplePlot.points.1"))
    if ("samplePlot.boxplot.1" %in% childNames(canvas$image))
        canvas$image <- removeGrob(canvas$image, gPath("samplePlot.boxplot.1"))
    if ("samplePlot.ghosts.1" %in% childNames(canvas$image))
        canvas$image <- removeGrob(canvas$image, gPath("samplePlot.ghosts.1"))
    if ("text.resample" %in% childNames(canvas$image))
        canvas$image <- removeGrob(canvas$image, gPath("text.resample"))
    if ("dataPlot.rect.1" %in% childNames(canvas$image))
        canvas$image <- removeGrob(canvas$image, gPath("dataPlot.rect.1"))
    if ("samplePlot.rect.1" %in% childNames(canvas$image))
        canvas$image <- removeGrob(canvas$image, gPath("samplePlot.rect.1"))
    allx <- c(canvas$stat.dist, recursive = TRUE)
    allinfo <- c(canvas$stat.dist, recursive = TRUE)
    for (i in 50*(1:20)){
        x <- allx[1:i]
        y <- stackPoints(x, vp = canvas$graphPath("stat"), y.min = 0, y.max = 0.9)
        plotPoints(canvas, x, y, canvas$graphPath("stat"), "statPlot", black = FALSE, alpha = 0.7)
        canvas$image <- addGrob(canvas$image, rectGrob
                                (x = unit(allinfo[1:i], "native"),
                                 y = unit(0.15, "npc"), height = unit(0.2, "npc"),
                                 width = 0, gp = gpar(alpha = 0.05, col = "blue", lwd = 2),
                                 vp = canvas$graphPath("sample"), name = "samplePlot.ghosts.1"))

        canvas$drawImage()
    }
    ## Remove 1000 statistics next time something is plotted to avoid
    ## further statistics being plotted on top.
    #canvas$image <- removeGrob(canvas$image, gPath("statPlot.points.1"))
    ## Reset CI counter
    canvas$sampled.stats <- NULL
    canvas$plotted.stats <- NULL
}

showCIandStats <- function(canvas, e, ci = TRUE){
    if (ci){
        ## CI code.
        if ("dataPlot.rect.1" %in% childNames(canvas$image))
            canvas$image <- removeGrob(canvas$image, gPath("dataPlot.rect.1"))
        x <- c(canvas$stat.dist, recursive = TRUE)
        y <- stackPoints(x, vp = canvas$graphPath("stat"), y.min = 0, y.max = 0.9)
        ci <- round(quantile(x, prob = c(0.025, 0.975)), 1)
        x.in <- x[x >= ci[1] & x <= ci[2]]
        x.out <- x[x < ci[1] | x > ci[2]]
        y.in <- y[x >= ci[1] & x <= ci[2]]
        y.out <- y[x < ci[1] | x > ci[2]]
        points.in <- pointsGrob(x = x.in, y = y.in, gp = gpar(col = "grey60", lwd = 2, alpha = 0.7),
                                vp = canvas$graphPath("stat"))
        points.out <- pointsGrob(x = x.out, y = y.out, gp = gpar(col = "lightgrey", lwd = 2, alpha = 1),
                                 vp = canvas$graphPath("stat"))
        points.all <- grobTree(points.out, points.in, name = "statPlot.points.1")
        canvas$image <- addGrob(canvas$image, points.all)
        lines <- segmentsGrob(x0 = unit(ci, "native"), x1 = unit(ci, "native"),
                              y0 = unit(0.1, "npc"), y1 = unit(-1, "lines") - unit(1, "lines"),
                              gp = gpar(lwd = 2, col = "red"), arrow = arrow(length = unit(0.1, "inches")),
                              vp = canvas$graphPath("stat"), name = "statPlot.lines.1")
        text1 <- textGrob(label = format(ci[1], nsmall = 1), x = unit(ci[1], "native"),
                          y = unit(-2, "lines"), gp = gpar(fontface = 2, col = "red"), just = "top",
                          vp = canvas$graphPath("stat"), name = "statPlot.text1.1")
        text2 <- textGrob(label = format(ci[2], nsmall = 1), x = unit(ci[2], "native"),
                          y = unit(-2, "lines"), gp = gpar(fontface = 2, col = "red"), just = "top",
                          vp = canvas$graphPath("stat"), name = "statPlot.text2.1")
        permCI <- rectGrob(x = unit(ci[1], "native"), y = unit(0.1, "npc"),
                           height = unit(0.01, "npc"), width = unit(diff(ci), "native"),
                           just = c("left", "centre"), vp = canvas$graphPath("stat"),
                           gp = gpar(col = "red", fill = "red"), name = "statPlot.rect.1")
        ciGrob <- grobTree(permCI, lines, text1, text2, name = "statPlot.ci.1")
        canvas$image <- addGrob(canvas$image, ciGrob)
        canvas$drawImage()
        for (i in 1:10){
            canvas$image <- addGrob(canvas$image, rectGrob(x = unit(ci[1], "native"),
                                                           y = unit(0.1 + i*0.2, "native"),
                                                           height = unit(0.01, "native"),
                                                           width = unit(diff(ci), "native"),
                                                           just = c("left", "centre"), vp =
                                                           vpPath("canvas.all", "canvas.plots", "canvas.frame", "animation.field"),
                                                           gp = gpar(col = "red", fill = "red"),
                                                           name = "temp"))
            canvas$drawImage()
            if (i == 5){
                canvas$pauseImage(5)
                canvas$image <- addGrob(canvas$image, rectGrob(x = unit(ci[1], "native"),
                                                       y = unit(0.1, "npc"),
                                                       height = unit(0.01, "npc"),
                                                       width = unit(diff(ci), "native"),
                                                       just = c("left", "centre"), vp =
                                                       canvas$graphPath("sample"),
                                                       gp = gpar(col = "red", fill = "red"),
                                                       name = "samplePlot.rect.1"))
            }
        }
        canvas$image <- addGrob(canvas$image, rectGrob(x = unit(ci[1], "native"),
                                                       y = unit(0.1, "npc"),
                                                       height = unit(0.01, "npc"),
                                                       width = unit(diff(ci), "native"),
                                                       just = c("left", "centre"), vp =
                                                       canvas$graphPath("data"),
                                                       gp = gpar(col = "red", fill = "red"),
                                                       name = "dataPlot.rect.1"))
        canvas$image <- removeGrob(canvas$image, gPath("temp"))
    } else {
        ## Summary stats code.
        x <- c(canvas$stat.dist, recursive = TRUE)
        mean.x <- round(mean(x), 2)
        sd.x <- round(sd(x), 2)
        ## Calculating maximum text line width.
        widths <- c(convertX(stringWidth("Mean:"), "cm", valueOnly = TRUE),
                    convertX(stringWidth(mean.x), "cm", valueOnly = TRUE),
                    convertX(stringWidth(sd.x), "cm", valueOnly = TRUE))
        max.width <- stringWidth(c("Mean:", mean.x, sd.x)[widths == max(widths)])
        xunit <- unit(0, "npc") + unit(1, "mm") + 0.5*max.width
        summarytext1 <- textGrob("Mean:", x = xunit, y = unit(0.6, "npc"),
                                 vp = canvas$graphPath("stat"), gp = gpar(fontface = 2),
                                 name = "summarytext1")
        summarytext2 <- textGrob(format(mean.x, nsmall = 2), x = xunit,
                                 y = unit(0.6, "npc") - unit(1, "lines"),
                                 vp = canvas$graphPath("stat"), #gp = gpar(fontface = 2),
                                 name = "summarytext2")
        summarytext3 <- textGrob("SD:",
                                 x = xunit, y = unit(0.6, "npc") - unit(2, "lines"),
                                 vp = canvas$graphPath("stat"), gp = gpar(fontface = 2),
                                 name = "summarytext3")
        summarytext4 <- textGrob(format(sd.x, nsmall = 2), x = xunit,
                                 y = unit(0.6, "npc") - unit(3, "lines"),
                                 vp = canvas$graphPath("stat"), #gp = gpar(fontface = 2),
                                 name = "summarytext4")
        summaryborder <- rectGrob(x = xunit, y = unit(0.6, "npc") + unit(0.5, "lines"),
                                  width = max.width + unit(2, "mm"),
                                  height = unit(2, "mm") + unit(4, "lines"),
                                  gp = gpar(fill = "white"),
                                  just = c("centre", "top"), vp = canvas$graphPath("stat"),
                                  name = "summaryborder")
        summarytext <- grobTree(summaryborder, summarytext1, summarytext2, summarytext3,
                                summarytext4, name = "statPlot.summary.1")
        canvas$image <- addGrob(canvas$image,  summarytext)
        canvas$drawImage()
    }
}
