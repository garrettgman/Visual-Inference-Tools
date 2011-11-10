# plotting methods for displaying two dimensional data that has one numeric dimension and one categorical dimension. These methods rely on the methods for 1d numeric data.
load_mixed_2d <- function()
	PLOT_DATA <<- plotPointGroupsAndBoxplot

# plotPoints and plotBoxplot are defined in the data-methods-numeric-1d.R file
plotPointGroupsAndBoxplot <- function(canvas) {
    canvas$y <- stackPoints(canvas$x, canvas$levels, vp = canvas$graphPath("data"),
                            y.min = 0.3, y.max = 1)
    n <- 1
    levels <- sort(unique(canvas$levels))
    ## Add more colours in here when we consider 3+ samples.
    cols <- c("tan4", "darkseagreen")
    for (i in levels) {
        plotPoints(canvas, canvas$x[canvas$levels == i],
                   canvas$y[canvas$levels == i], col = cols[n],
                   vp = canvas$graphPath("data", as.character(n)),
                   name = "dataPlot")
        plotBoxplot(canvas, canvas$x[canvas$levels == i],
                    stat = NULL, stat.color = NULL,
                    vp = canvas$graphPath("data", as.character(n)),
                    name = "dataPlot")
        canvas$image <- addGrob(canvas$image, textGrob
                                (i, x = 1, y = unit(1, "mm"), just = c("right", "bottom"),
                                 vp = canvas$graphPath("data", as.character(n)),
                                 name = paste("dataPlot.text", n, sep = ".")))
        n <- n + 1
    }
}

plotPointGroups <- function(canvas, x, y, levels, graph, cols = NULL, name){
    if (is.null(cols)) cols = "grey60"
    cols <- rep(cols, length = length(unique(levels)))
    n <- 1
    for (i in sort(unique(levels))) {
        plotPoints(canvas, x[levels == i],
                   y[canvas$levels == i], col = cols[n],
                   vp = canvas$graphPath(graph, as.character(n)),
                   name = name)
        n <- n + 1
    }
}
