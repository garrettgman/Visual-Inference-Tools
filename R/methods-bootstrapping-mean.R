load_bootstrap_mean <- function(e){
    PLOT_DATA <<- PLOT_DATA
    PLOT_SAMPLE <<- plotSamplePointsAndBoxplotGhostMean
    SHOW_LABELS <<- bootLabels
    CALC_STAT <<- c("mean" = mean, "median" = median)[[svalue(e$stat)]]
    PLOT_DATA_STAT <<- lineOnBoxplotMean
    PLOT_SAMPLE_STAT <<- notYetImplemented
    PLOT_STAT_DIST <<- plotBootDist
    ANIMATE_SAMPLE <<- moveDataTextAndDropPoints
    TRACK_SAMPLE <<- trackBootstrap
    ANIMATE_STAT <<- dropStat
    DISPLAY_RESULT <<- showCIandStats
    HANDLE_1000 <<- boot1000mean
    FADE_PLOTS <<- fadeSampleAndStat
}
bootLabels <- function(canvas){
    samplabel <- textGrob("Sample",
                         x = unit(0, "npc") + unit(1, "mm"),
                         y = unit(0.9, "npc"),
                         just = c("left", "top"),
                         vp = canvas$graphPath("data"),
                         gp = gpar(fontface = 2))
    resamplabel <- textGrob("Re-sample",
                          x = unit(0, "npc") + unit(1, "mm"),
                          y = unit(0.8, "npc"),
                          just = c("left", "top"),
                          vp = canvas$graphPath("sample"),
                          gp = gpar(fontface = 2))
    statlabel <- textGrob("Bootstrap distribution",
                          x = unit(0, "npc") + unit(1, "mm"),
                          y = unit(0.8, "npc"),
                          just = c("left", "top"),
                          vp = canvas$graphPath("stat"),
                          gp = gpar(fontface = 2))
    bootlabels <- grobTree(samplabel, resamplabel, statlabel, name = "bootlabels")
    canvas$image <- addGrob(canvas$image, bootlabels)
}

lineOnBoxplotMean <- function(canvas, e){

   plotBoxplot(canvas, canvas$x, stat = mean, stat.color = "purple3",
               canvas$graphPath("data"), "dataPlot")
}

plotSamplePointsAndBoxplotGhostMean <- function(canvas, e, i){
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
    allinfo <- c(canvas$stat.dist, recursive = TRUE)
    canvas$image <- addGrob(canvas$image, rectGrob
                            (x = unit(allinfo[canvas$sampled.stats], "native"),
                             y = unit(0.15, "npc"), height = unit(0.2, "npc"),
                             width = 0, gp = gpar(alpha = alpha, col = "blue", lwd = 2),
                             vp = canvas$graphPath("sample"), name = "samplePlot.ghosts.1"))
    canvas$image <- addGrob(canvas$image, linesGrob
                            (x = unit(canvas$stat.dist[i], "native"),
                             y = unit(c(0.05, 0.5), "npc"), gp = gpar(lwd = 4, col = "blue"),
                             vp = canvas$graphPath("sample"), name = "samplePlot.lines.1"))
    #canvas$image <- addGrob(canvas$image, boxplotGrob(x, box.color = "black",
    #                                                  median.color = "black",
    #                                                  stat = mean, stat.color = "red",
    #                                                  show.w = FALSE,
    #                                                  name = "samplePlot.boxplot.1",
    #                                                  vp = canvas$graphPath("sample")))
    canvas$image <- addGrob(canvas$image, datatextGrob(data = x, title = "Resample",
                                                       max = 50, name = "databox.text.2",
                                                       vp = canvas$graphPath("databox", 2), gp = gpar(col = "red")))
}

plotBootDist <- function(canvas, e){
    canvas$plotted.stats <- c(canvas$plotted.stats, canvas$which.sample)
    x <- c(canvas$stat.dist, recursive = TRUE)[canvas$plotted.stats]
    y.max <- convertY(unit(1, "npc") - unit(2, "lines"), "npc", valueOnly = TRUE)
    y <- stackPoints(x, vp = canvas$graphPath("stat"), y.min = 0, y.max = y.max)
    plotPoints(canvas, x, y, canvas$graphPath("stat"), "statPlot", black = FALSE)
}

moveDataTextAndDropPoints <- function(canvas, drop.points = FALSE, n.steps = 10, n.slow = 5, max = 50){
    if ("dataPlot.ci.1" %in% childNames(canvas$image))
        canvas$image <- removeGrob(canvas$image, gPath("dataPlot.ci.1"))
    if ("databox.text.2" %in% childNames(canvas$image))
        canvas$image <- removeGrob(canvas$image, gPath("databox.text.2"))
    if ("samplePlot.points.1" %in% childNames(canvas$image))
        canvas$image <- removeGrob(canvas$image, gPath("samplePlot.points.1"))
    if ("samplePlot.lines.1" %in% childNames(canvas$image))
        canvas$image <- removeGrob(canvas$image, gPath("samplePlot.lines.1"))
    index <- canvas$indexes[[canvas$which.sample]]
    x <- canvas$x[index]
    y <- canvas$y[index]
    n <- canvas$n
    if (n < 100){
    n.slow <- min(n.slow, n)
    ## Calculating the position of text in text boxes.
    ntext <- min(n, max)
    npcs <- (ntext:0)/ntext
    yunit <- (unit(npcs, "npc") - unit(4*(npcs - 0.5), "mm") + unit(1 - npcs, "lines"))
    x.text.start <- 0.25
    x.text.end <- 0.75
    x.text.step <- (x.text.end - x.text.start)/n.steps
    y.start <- y + 2
    y.end <- stackPoints(x, vp = canvas$graphPath("sample")) + 1
    y.step <- (y.end - y.start)/n.steps

    ## Animation of slow points.
    for (i in seq(from = 1, by = 1, length.out = n.slow)){
        y.text.start <- yunit[min(c(max + 1, index[i] + 1))]
        y.text.end <- yunit[i + 1]
        y.text.step <- convertY(y.text.end - y.text.start, "npc", valueOnly = TRUE)/n.steps
        temp.text <- textGrob(label = format(round(x[i], 1), nsmall = 1),
                              y = y.text.start, just = "top",
                              gp = gpar(col = "red", fontface = 2), name = "temp.text",
                              vp = canvas$graphPath("databox", 1))
        temp.arrow <- linesGrob(x = c(0.9, 0.8),
                                y = y.text.start - unit(0.5, "lines"),
                                gp = gpar(lwd = 3, col = "red"),
                                arrow = arrow(length = unit(0.1, "inches")), name = "temp.arrow",
                                vp = canvas$graphPath("databox", 1))
        ## Light up point to drop
        if (drop.points){
            temp.point <- pointsGrob(x = x[i], y = (canvas$y[index])[i], pch = 19,
                                     vp = canvas$graphPath("data"), name = "temp.point")
            canvas$image <- addGrob(canvas$image, temp.point)
        }
        canvas$image <- addGrob(canvas$image, temp.text)
        canvas$image <- addGrob(canvas$image, temp.arrow)
        canvas$pauseImage(5)
        for (j in 1:n.steps){
            ## Move text
            canvas$image <- addGrob(canvas$image, textGrob
                                    (label = format(round(x[i], 1), nsmall = 1),
                                     y = y.text.start + unit(j*y.text.step, "npc"),
                                     x = unit(x.text.start + j*x.text.step, "npc"),
                                     just = "top", gp = gpar(col = "red", fontface = 2),
                                     name = "temp.text",
                                     vp = vpPath("canvas.all", "canvas.boxes")))
            ## Drop point
            if (drop.points){
                canvas$image <- addGrob(canvas$image, pointsGrob
                                        (x = x[i], y = y.start[i] + j*y.step[i], pch = 19,
                                         vp = vpPath("canvas.all", "canvas.plots",
                                         "canvas.frame", "animation.field"), name = "temp"))
            }
            canvas$drawImage()
            if (j == n.steps & drop.points)
                canvas$image <- removeGrob(canvas$image, gPath("temp"))
        }
        ## Make points permanent if dropping
        if (drop.points)
            plotPoints(canvas, x[1:i], y.end[1:i] - 1, canvas$graphPath("sample"),
                       "samplePlot", black = FALSE)
        canvas$pauseImage(5)
        ## Make text in resample databox permanent
        canvas$image <- removeGrob(canvas$image, gPath("temp.text"))
        resamp.text <- textGrob(label = c("Resample", format(round(x[1:i], 1), nsmall = 1)),
                                y = yunit[1:(i + 1)], just = "top", gp = gpar(col = "red"),
                                name = "databox.text.2", vp = canvas$graphPath("databox", 2))
        canvas$image <- addGrob(canvas$image, resamp.text)
    }
    ## Animation of fast points.
    if (n == ntext) length.out <- ntext - n.slow else length.out <- ntext - n.slow - 1
    for (i in seq(from = n.slow + 1, by = 1, length.out = length.out)){
        y.text.start <- yunit[min(c(max + 1, index[i] + 1))]
        temp.text <- textGrob(label = format(round(x[i], 1), nsmall = 1),
                              y = y.text.start, just = "top",
                              gp = gpar(col = "red", fontface = 2), name = "temp.text",
                              vp = canvas$graphPath("databox", 1))
        temp.arrow <- linesGrob(x = c(0.9, 0.8),
                                y = y.text.start - unit(0.5, "lines"),
                                gp = gpar(lwd = 3, col = "red"),
                                arrow = arrow(length = unit(0.1, "inches")), name = "temp.arrow",
                                vp = canvas$graphPath("databox", 1))
        ## Light up point to drop.
        if (drop.points){
            temp.point <- pointsGrob(x = x[i], y = (canvas$y[index])[i], pch = 19,
                                     vp = canvas$graphPath("data"), name = "temp.point")
            canvas$image <- addGrob(canvas$image, temp.point)
        }
        resamp.text <- textGrob(label = c("Resample", format(round(x[1:i], 1), nsmall = 1)),
                                y = yunit[1:(i + 1)], just = "top", gp = gpar(col = "red"),
                                name = "databox.text.2", vp = canvas$graphPath("databox", 2))
        canvas$image <- addGrob(canvas$image, temp.text)
        canvas$image <- addGrob(canvas$image, temp.arrow)
        canvas$image <- addGrob(canvas$image, resamp.text)
        ## Plot dropped point.
        if (drop.points)
            plotPoints(canvas, x[1:i], y.end[1:i] - 1, canvas$graphPath("sample"),
                       "samplePlot", black = FALSE)
        canvas$pauseImage(5)
    }
    ## Animation of points outside databox.
    for (i in seq(from = ntext, by = 1, length.out = n - ntext)){
        y.text.start <- yunit[min(c(max + 1, index[i] + 1))]
        temp.text <- textGrob(label = format(round(x[i], 1), nsmall = 1),
                              y = y.text.start, just = "top",
                              gp = gpar(col = "red", fontface = 2), name = "temp.text",
                              vp = canvas$graphPath("databox", 1))
        temp.arrow <- linesGrob(x = c(0.9, 0.8),
                                y = y.text.start - unit(0.5, "lines"),
                                gp = gpar(lwd = 3, col = "red"),
                                arrow = arrow(length = unit(0.1, "inches")), name = "temp.arrow",
                                vp = canvas$graphPath("databox", 1))
        ## Light up point to drop.
        if (drop.points){
            temp.point <- pointsGrob(x = x[i], y = (canvas$y[index])[i], pch = 19,
                                     vp = canvas$graphPath("data"), name = "temp.point")
            canvas$image <- addGrob(canvas$image, temp.point)
        }
        resamp.text <- textGrob(label = c("Resample",
                                format(round(x[1:(ntext - 1)], 1), nsmall = 1), "..."),
                                y = yunit, just = "top", gp = gpar(col = "red"),
                                name = "databox.text.2",
                                vp = canvas$graphPath("databox", 2))
        canvas$image <- addGrob(canvas$image, temp.text)
        canvas$image <- addGrob(canvas$image, temp.arrow)
        canvas$image <- addGrob(canvas$image, resamp.text)
        ## Plot dropped point.
        if (drop.points)
            plotPoints(canvas, x[1:i], y.end[1:i] - 1, canvas$graphPath("sample"),
                       "samplePlot", black = FALSE)
        canvas$pauseImage(5)
    }
    plotPoints(canvas, x, y.end - 1, canvas$graphPath("sample"),
               "samplePlot", black = FALSE)
    canvas$image <- addGrob(canvas$image, linesGrob
                            (x = unit(canvas$stat.dist[canvas$which.sample], "native"),
                             y = unit(c(0.05, 0.5), "npc"), gp = gpar(lwd = 4, col = "blue"),
                             vp = canvas$graphPath("sample"), name = "samplePlot.lines.1"))
    canvas$pauseImage(10)
    canvas$image <- removeGrob(canvas$image, gPath("temp.text"))
    canvas$image <- removeGrob(canvas$image, gPath("temp.arrow"))
    if (drop.points)
        canvas$image <- removeGrob(canvas$image, gPath("temp.point"))
}
}

trackBootstrap <- function(canvas){
    index <- canvas$indexes[[canvas$which.sample]]
    sample <- canvas$x
    sample.y <- canvas$y
    resample <- sample[index]
    resample.y <- stackPoints(resample, vp = canvas$graphPath("sample"))
    n <- canvas$n
    ntext <- min(n, 50)
    npcs <- (ntext:0)/ntext
    yunit <- (unit(npcs, "npc") - unit(4*(npcs - 0.5), "mm") + unit(1 - npcs, "lines"))
    canvas$image <- addGrob(canvas$image, datatextGrob
                            (data = resample, title = "Resample", max = 50,
                             name = "databox.text.2", vp = canvas$graphPath("databox", 2)))
    max.width <- max(convertX(stringWidth(format(round(sample, 1), nsmall = 1)), "cm", valueOnly = TRUE))
    max.width <- unit(max.width, "cm")
    for (i in 1:ntext){
        value <- sample[i]
        canvas$image <- addGrob(canvas$image, textGrob
                                (label = format(round(value, 1), nsmall = 1),
                                 y = yunit[i + 1], just = "top",
                                 gp = gpar(col = "red", fontface = 2), name = "temp.samp",
                                 vp = canvas$graphPath("databox", 1)))
        canvas$image <- addGrob(canvas$image, pointsGrob
                                (x = sample[i], y = sample.y[i], pch = 19, gp = gpar(col = "red"),
                                 name = "temp.point", vp = canvas$graphPath("data")))
        resamp.ys <- yunit[c(FALSE, index == i)]
        if (length(resamp.ys) > 0){
            canvas$image <- addGrob(canvas$image, textGrob
                                    (label = format(round(value, 1), nsmall = 1),
                                     y = yunit[c(FALSE, index == i)], just = "top",
                                     gp = gpar(col = "red", fontface = 2), name = "temp.resamp",
                                     vp = canvas$graphPath("databox", 2)))
            canvas$image <- addGrob(canvas$image, segmentsGrob
                                    (x0 = unit(0.25, "npc") + max.width*0.5 + unit(1, "mm"),
                                     x1 = unit(0.75, "npc") - max.width*0.5 - unit(1, "mm"),
                                     y0 = yunit[i + 1] - unit(0.5, "lines"),
                                     y1 = yunit[c(FALSE, index == i)] - unit(0.5, "lines"),
                                     name = "temp.segments", gp = gpar(lwd = 2, col = "red"),
                                     vp = vpPath("canvas.all", "canvas.boxes")))
            canvas$image <- addGrob(canvas$image, pointsGrob
                                    (x = rep(value, length(resamp.ys)), y = resample.y[index == i],
                                     pch = 19,
                                     gp = gpar(col = "red"), name = "temp.resamplepoints",
                                     vp = canvas$graphPath("sample")))
            canvas$pauseImage(20)
            canvas$image <- removeGrob(canvas$image, gPath("temp.resamp"))
            canvas$image <- removeGrob(canvas$image, gPath("temp.segments"))
            canvas$image <- removeGrob(canvas$image, gPath("temp.resamplepoints"))
        } else canvas$pauseImage(20)
    }
    canvas$image <- removeGrob(canvas$image, gPath("temp.samp"))
    canvas$image <- removeGrob(canvas$image, gPath("temp.point"))
    canvas$image <- addGrob(canvas$image, datatextGrob
                            (data = resample, title = "Resample", max = 50, gp = gpar(col = "red"),
                             name = "databox.text.2", vp = canvas$graphPath("databox", 2)))
}

dropStat <- function(canvas, e, n.steps){
    xs <- c(canvas$stat.dist, recursive = TRUE)[c(canvas$plotted.stats,
                              canvas$which.sample)]
    x <- xs[length(xs)]
    ys <- stackPoints(xs, vp = canvas$graphPath("stat"), y.min = 0,
                      y.max = unit(1, "npc") - unit(2, "lines"))
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


boot1000mean <- function(canvas, e, points = FALSE){
    if ("databox.text.2" %in% childNames(canvas$image))
        canvas$image <- removeGrob(canvas$image, gPath("databox.text.2"))
    if ("dataPlot.ci.1" %in% childNames(canvas$image))
        canvas$image <- removeGrob(canvas$image, gPath("dataPlot.ci.1"))
    allx <- c(canvas$stat.dist, recursive = TRUE)
    allinfo <- c(canvas$stat.dist, recursive = TRUE)
    y.max <- unit(1, "npc") - unit(2, "lines")
    for (i in 20*(1:50)){
        canvas$plotSample(e, i)
        x <- allx[1:i]
        y <- stackPoints(x, vp = canvas$graphPath("stat"), y.min = 0, y.max = y.max*i*0.001)
        if (points)
            plotPoints(canvas, x, y, canvas$graphPath("stat"),
                       "statPlot", black = FALSE, alpha = 0.7)
        canvas$image <- addGrob(canvas$image, rectGrob
                                (x = unit(allinfo[1:i], "native"),
                                 y = unit(0.15, "npc"), height = unit(0.2, "npc"),
                                 width = 0, gp = gpar(alpha = 0.05, col = "blue", lwd = 2),
                                 vp = canvas$graphPath("sample"), name = "samplePlot.ghosts.1"))

        canvas$drawImage()
    }
    canvas$image <- removeGrob(canvas$image, gPath("samplePlot.points.1"))
    canvas$image <- removeGrob(canvas$image, gPath("samplePlot.lines.1"))
    canvas$image <- removeGrob(canvas$image, gPath("databox.text.2"))
    canvas$drawImage()
    canvas$sampled.stats <- NULL
    canvas$plotted.stats <- NULL
}

showCIandStats <- function(canvas, e, ci = TRUE, points = TRUE){
    if (points) vp <- canvas$graphPath("stat") else vp <- canvas$graphPath("sample")
    if (ci){
        ## CI code.
        if ("dataPlot.ci.1" %in% childNames(canvas$image))
            canvas$image <- removeGrob(canvas$image, gPath("dataPlot.ci.1"))
        x <- c(canvas$stat.dist, recursive = TRUE)
        ci <- round(quantile(x, prob = c(0.025, 0.975)), 1)
        start <- 5
        if (points){
            start <- 1
            ## Set points outside interval to a lighter shade of grey.
            y <- stackPoints(x, vp = canvas$graphPath("stat"), y.min = 0,
                             y.max = unit(1, "npc") - unit(2, "lines"))
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
        }
        ## Plot CI.
        canvas$image <- addGrob(canvas$image, confintGrob(ci = ci,
                                                          name = "statPlot.ci.1",
                                                          vp = vp))
        canvas$drawImage()
        ## Animate CI.
        for (i in start:10){
            canvas$image <- addGrob(canvas$image, rectGrob(x = unit(ci[1], "native"),
                                                           y = unit(0.15 + i*0.2, "native"),
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
                                                       y = unit(0.15, "npc"),
                                                       height = unit(0.015, "npc"),
                                                       width = unit(diff(ci), "native"),
                                                       just = c("left", "centre"), vp =
                                                       canvas$graphPath("sample"),
                                                       gp = gpar(col = "red", fill = "red"),
                                                       name = "samplePlot.rect.1"))
            }
        }
        canvas$image <- removeGrob(canvas$image, gPath("temp"))
        canvas$image <- addGrob(canvas$image, confintGrob(ci = ci,
                                                          name = "dataPlot.ci.1",
                                                          vp = canvas$graphPath("data")))
        canvas$drawImage()

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
                                 vp = vp, gp = gpar(fontface = 2),
                                 name = "summarytext1")
        summarytext2 <- textGrob(format(mean.x, nsmall = 2), x = xunit,
                                 y = unit(0.6, "npc") - unit(1, "lines"),
                                 vp = vp, #gp = gpar(fontface = 2),
                                 name = "summarytext2")
        summarytext3 <- textGrob("SD:",
                                 x = xunit, y = unit(0.6, "npc") - unit(2, "lines"),
                                 vp = vp, gp = gpar(fontface = 2),
                                 name = "summarytext3")
        summarytext4 <- textGrob(format(sd.x, nsmall = 2), x = xunit,
                                 y = unit(0.6, "npc") - unit(3, "lines"),
                                 vp = vp, #gp = gpar(fontface = 2),
                                 name = "summarytext4")
        summaryborder <- rectGrob(x = xunit, y = unit(0.6, "npc") + unit(0.5, "lines"),
                                  width = max.width + unit(2, "mm"),
                                  height = unit(2, "mm") + unit(4, "lines"),
                                  gp = gpar(fill = "white"),
                                  just = c("centre", "top"), vp = vp,
                                  name = "summaryborder")
        summarytext <- grobTree(summaryborder, summarytext1, summarytext2, summarytext3,
                                summarytext4, name = "statPlot.summary.1")
        canvas$image <- addGrob(canvas$image,  summarytext)
        canvas$drawImage()
    }
}

fadeSampleAndStat <- function(canvas, e){
    x <- c(canvas$stat.dist, recursive = TRUE)
    ci <- round(quantile(x, prob = c(0.025, 0.975)), 1)
    canvas$image <- addGrob(canvas$image, rectGrob
                            (x = unit(0.5, "npc"), y = unit(2/3, "npc") - unit(1, "lines"),
                             width = unit(1, "npc"),
                             height = unit(2/3, "npc") - unit(1, "lines"),
                             just = "top",
                             gp = gpar(col = "white", fill = "white", alpha = 0.75),
                             vp = vpPath("canvas.all", "canvas.plots"),
                             name = "fadebox"))

    canvas$image <- addGrob(canvas$image, confintGrob(ci = ci,
                                                 name = "dataPlot.ci.1",
                                                 vp = canvas$graphPath("data")))
    canvas$drawImage()
    canvas$image <- removeGrob(canvas$image, gPath("fadebox"))
}
