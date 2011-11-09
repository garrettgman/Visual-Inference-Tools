## Makes a viewport scheme to facilitate animation and plotting a statistic underneath the data

makeVitGraphViewports <- function(x.scale, nlevels.y, stat.scale) {
    canvas.frame.layout <- grid.layout(nrow = 2, heights = unit(1,
                                                 c("null", "line")))
    canvas.frame <- plotViewport(c(3, 3, 0, 1), layout = canvas.frame.layout,
                                 name = "canvas.frame")


    animation.layout <- grid.layout(nrow = 3)
    animation.field <- dataViewport(xscale = x.scale, yscale = c(0, 3),
                                    layout = animation.layout, name = "animation.field")

    data <- splitDataPane(x.scale = x.scale, n = nlevels.y, layout.pos.row = 1,
                          name = "data.data")
    sample <- splitDataPane(x.scale = x.scale, n = nlevels.y,
                            layout.pos.row = 2, name = "sample.data")
    stat <- dataViewport(xscale = stat.scale, yscale = c(0, 1), layout.pos.row = 3,
                         name = "stat.data.1")

    vpTree(canvas.frame, vpList(vpTree(animation.field,
                                       vpList(data, sample, stat))))
}

makeVitGraphViewportsBoxes <- function(x.scale, nlevels.y, stat.scale){
    canvas.all.layout <- grid.layout(ncol = 2, widths = unit(1:2, "null"))
    canvas.all <- viewport(layout = canvas.all.layout, name = "canvas.all")
    canvas.boxes.layout <- grid.layout(ncol = 2)
    canvas.boxes <- viewport(layout = canvas.boxes.layout, name = "canvas.boxes",
                             layout.pos.col = 1)
    databox.1 <- viewport(layout.pos.col = 1, name = "databox.1")
    databox.2 <- viewport(layout.pos.col = 2, name = "databox.2")
    canvas.plots <- viewport(layout.pos.col = 2, name = "canvas.plots")
    canvas.frame.layout <- grid.layout(nrow = 2, heights = unit(1, c("null", "line")))
    canvas.frame <- plotViewport(c(3, 1, 0, 1), layout = canvas.frame.layout,
                                 name = "canvas.frame")
    animation.layout <- grid.layout(nrow = 3)
    animation.field <- dataViewport(xscale = x.scale, yscale = c(0, 3),
                                    layout = animation.layout, name = "animation.field")
    data <- splitDataPane(x.scale = x.scale, n = nlevels.y, layout.pos.row = 1,
                          name = "data.data")
    sample <- splitDataPane(x.scale = x.scale, n = nlevels.y,
                            layout.pos.row = 2, name = "sample.data")
    stat <- dataViewport(xscale = stat.scale, yscale = c(0, 1), layout.pos.row = 3,
                         name = "stat.data.1")
    boxes <- vpTree(canvas.boxes, vpList(databox.1, databox.2))
    plots <- vpTree(canvas.plots, vpList(vpTree(canvas.frame, vpList(vpTree(animation.field,
                                       vpList(data, sample, stat))))))
    vpTree(canvas.all, vpList(boxes, plots))
}

splitDataPane <- function(x.scale, n, layout.pos.row, name) {

    frame.layout <- grid.layout(nrow = n)
    frame <- viewport(layout.pos.row = layout.pos.row, layout = frame.layout,
                      name = paste(name, "placer", sep = "."))

    data.vps <- list()
    for(i in 1:n) {
        data.vps[[i]] <- dataViewport(xscale = x.scale, yscale = c(0, 1),
                                      layout.pos.row = n - i + 1, name = paste(name, i, sep = "."))
    }

    vpTree(frame, do.call("vpList", data.vps))
}


## returns the vpPath to the graph viewport for the specified field.

graphPathNoBoxes <- function(plot.name = "sample", number = "1") {
    if(!(plot.name %in% c("data", "sample", "stat")))
        stop("plot.name must be \'data', \'sample', or \'stat'.")
    if (is.numeric(number)) number <- as.character(number)
    if ( !(number %in% c("1", "2", "3", "4", "5")))
        stop("number must be \'1', \'2', \'3', \'4', or \'5'.")

    if (plot.name == "stat") {
        vpPath("canvas.frame", "animation.field",
               paste(plot.name, "data", number, sep = "."))
    } else {
        vpPath("canvas.frame", "animation.field",
               paste(plot.name, "data.placer", sep = "."),
               paste(plot.name, "data", number, sep = "."))
    }
}

graphPathBoxes <- function(plot.name = "sample", number = "1") {
    if(!(plot.name %in% c("data", "sample", "stat", "databox")))
        stop("plot.name must be \'data', \'sample', or \'stat'.")
    if (is.numeric(number)) number <- as.character(number)
    if ( !(number %in% c("1", "2", "3", "4", "5")))
    stop("number must be \'1', \'2', \'3', \'4', or \'5'.")
    if (plot.name == "databox")
        vpPath("canvas.all", "canvas.boxes", paste(plot.name, number, sep = ".")) else{
            if (plot.name == "stat") {
                vpPath("canvas.all", "canvas.plots", "canvas.frame", "animation.field",
                       paste(plot.name, "data", number, sep = "."))
            } else {
                vpPath("canvas.all", "canvas.plots", "canvas.frame", "animation.field",
                       paste(plot.name, "data.placer", sep = "."),
                       paste(plot.name, "data", number, sep = "."))
            }
        }
}

## If same.stat.scale is TRUE, then stat plot will have same x-axis
## scale as the other two.  Otherwise, the scale will be centred at 0,
## but the scale of units on the x-axis will still remain the
## same. That is, the distance on the x-axis for 1 unit will remain
## constant in all three plots.
buildViewports <- function(canvas, x, y = NULL, boxes = FALSE, same.stat.scale = TRUE) {

    if (is.categorical(x)){
        x.scale <- c(0, length(x))
    } else {
        x.scale <- range(x)
    }
    if (same.stat.scale){
        stat.scale <- x.scale
    } else {
        stat.scale <- x.scale - mean(x.scale)
    }
    if (is.null(y)) {
        n.y <- 1
        y.scale <- c(0,1)
    } else if (is.categorical(y)) {
        n.y <- length(unique(y))
        y.scale <- c(0,1)
    } else {
        n.y <- 1
        y.scale <- range(y)
        notYetImplemented("build viewports for 2d numeric data")
        stop("method does not exist yet.")
    }
    if (boxes){
        canvas$viewports <- makeVitGraphViewportsBoxes(x.scale, n.y, stat.scale)
    } else {
        canvas$viewports <- makeVitGraphViewports(x.scale, n.y, stat.scale)
    }
}


##' returns the number at the end of the viewport name
vpNumber <- function(vp) {
    text <- as.character(vp$name)
    m <- nchar(text)
    last <- substr(text, m, m)
    if (last %in% c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9"))
        return(last)
    else
        return("")
}


##' appends a vpPath to include the number n on the bottommost viewport. If the
##' bottom most viewport ends in a number, it replaces that number with n.
appendPath <- function(vp, n) {
    text <- as.character(vp$name)
    m <- nchar(text)
    if (substr(text, m - 1, m - 1) == ".") substr(text, m, m) <- as.character(n)
    else text <- paste(text, n, sep = ".")

    structure(list(path = vp$path, name = text, n = vp$n),
              class = c("vpPath", "path"))
}

##' helper function for programming use
showVPs <- function() {
    vps <- as.character(current.vpTree())
    vps <- gsub("viewport\\[", "", vps)
    vps <- gsub("\\]", "", vps)
    vps <- gsub("\\(", "", vps)
    vps <- gsub("\\)", "", vps)
    vps <- gsub(" ", "", vps)
    vps <- gsub("->", ",", vps)

    vlist <- strsplit(vps, ",")

    for (name in vlist[[1]][-1]) {
        seekViewport(name)
        grid.rect(gp = gpar(col = "black", alpha = 0.5))
    }
}

