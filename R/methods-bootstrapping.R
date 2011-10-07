load_bootstrap <- function(e){
    PLOT_DATA <<- PLOT_DATA
    PLOT_SAMPLE <<- plotSamplePointsAndBoxplotGhost
    SHOW_LABELS <<- bootLabels
    CALC_STAT <<- c("mean" = mean, "median" = median)[[svalue(e$stat)]]
    PLOT_DATA_STAT <<- addMeanLine
    PLOT_SAMPLE_STAT <<- notYetImplemented
    PLOT_STAT_DIST <<- notYetImplemented
    ANIMATE_SAMPLE <<- dropPoints1d
    ANIMATE_STAT <<- notYetImplemented
}
bootLabels <- function(canvas){
    samplabel <- textGrob("Sample",
                         x = unit(0, "npc") - unit(1, "cm"),
                         y = unit(0.9, "npc"),
                         just = c("left", "top"),
                         vp = graphPath("data"),
                         gp = gpar(fontface = 2))
    resamplabel <- textGrob("Resample",
                          x = unit(0, "npc") - unit(1, "cm"),
                          y = unit(0.8, "npc"),
                          just = c("left", "top"),
                          vp = graphPath("sample"),
                          gp = gpar(fontface = 2))
    statlabel <- textGrob("Statistic distribution",
                          x = unit(0, "npc") - unit(1, "cm"),
                          y = unit(0.8, "npc"),
                          just = c("left", "top"),
                          vp = graphPath("stat"),
                          gp = gpar(fontface = 2))
    bootlabels <- grobTree(samplabel, resamplabel, statlabel, name = "bootlabels")
    canvas$image <- addGrob(canvas$image, bootlabels)
}

plotSamplePointsAndBoxplotGhost <- function(canvas, e, i){
    bluecol <- "blue"
    redcol <- "red"
    if (e$cb){
        bluecol <- dichromat(bluecol)
        redcol <- dichromat(redcol)
    }
    x <- canvas$samples[[i]]
    info <- fivenum(x)
    if (!("sample.ghosts" %in% childNames(canvas$image))){
        e$sample.ghosts <- ghostsGrob(info[2], info[3], info[4], vp = graphPath("sample"),
                                    name = "sample.ghosts")
    } else {
        e$sample.ghosts <- updateGhosts(e$sample.ghosts, info)
    }
    canvas$image <- addGrob(canvas$image, e$sample.ghosts)
}
