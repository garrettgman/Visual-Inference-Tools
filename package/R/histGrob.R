# functions and methods for constructing histogram grobs

#' Draw a histogram grob
grid.hist <- function(...)
    grid.draw(histGrob(...))

#' Construct a histogram grob
#'
#' histGrob constructs a histogram of a vector of data.
#' histGrobs inherit the class "hist".
#'
#' @param data a numeric vector of data.
#' @param breaks if numeric, either an integer specifying the (approximate) number of histogram cells, or a vector specifying the breakpoints between histogram cells. A character string specifies the algorithm used to compute the number of cells (either "Sturges", "Scott" or "Freedman-Diaconis"). Additionally, a function can be supplied to calculate this.
#' @param freq if logical, it indicates whether the histogram should represent frequencies or densities. If 'TRUE', breaks must be equidistant. If this is not the case, 'freq' reverts to FALSE with a warning. If a numeric value, the height of the tallest bar is specified. All other bars are scaled respectively.
#' @param include.lowest a logical value that specifies whether a data value equal to the lowest (or if right = 'TRUE', the highest) break point should be included in the lowest histogram cell.
#' @param right a logical value that indicates whether histogram cells are right closed ('TRUE') or left closed ('FALSE').
#' @param fill a colour to be used to fill the histogram bars. This overrides a fill specified in 'gp'.
#' @param name a name for the grob to be constructed.
#' @param gp graphical parameters for the histogram, constructed with gpar().
#' @param vp a default viewport to be used when drawing the grob.

#' Value:
#' An object of class 'hist' with a list of components. This includes all inputted arguments, although it should be noted that 'freq' defaults to FALSE if break points are not equidistant, and therefore does not necessarily take the value of the supplied argument. In addition, a list 'histvals' is returned, which includes all values that are returned from the generic function 'hist()'. Most importantly:
#' breaks: a vector of break points that split the data into bins. This is useful for specifying the range of the x-axis (see grid.hist.example() below).
#' counts: the frequencies of data points falling into each bin.
#' density: densities for each bin.
#' mids: the cell midpoints.
#' equidist: a logical value indicating whether or not break points are equidistant.


histGrob <- function(data, breaks = 10, freq = FALSE, include.lowest = TRUE,
                     right = TRUE, fill = "grey", name = NULL,
                     gp = NULL, vp = NULL){
    histvals <- hist(x = data, breaks = breaks, include.lowest = include.lowest,
                     right = right, plot = FALSE)
    heights <- histvals$density
    if (is.numeric(freq)){
        maxHeight <- max(heights)
        scaleHeight <- freq/maxHeight
        heights <- heights*scaleHeight
    } else if (freq & histvals$equidist) heights <- histvals$counts
    if (freq & !histvals$equidist){
        freq = FALSE
        warning("if freq = TRUE, breaks must be equidistant. Plotting densities instead.")
    }
    grob(data = data, breaks = breaks, freq = freq, include.lowest = include.lowest,
         right = right, scale = scale, fill = fill, histvals = histvals, heights = heights,
         name = name, gp = gp, vp = vp, cl = "hist")
}

drawDetails.hist <- function(x, recording){
    bounds <- x$histvals$breaks
    counts <- x$histvals$counts
    density <- x$histvals$density
    equidist <- x$histvals$equidist
    nbins <- length(counts)
    binlow <- bounds[-length(bounds)]
    binwidth <- diff(bounds)
    grid.draw(rectGrob(x = unit(binlow, "native"), y = unit(rep(0, nbins), "native"),
             width = unit(binwidth, "native"), height = unit(x$heights, "native"),
             just = c("left", "bottom"), gp = gpar(fill = x$fill)))
}

validDetails.hist <- function(x){
    if (!inherits(x$data, c("integer", "numeric")))
        stop("data must be integer or numeric")
    if (!(is.logical(x$freq) | is.numeric(x$freq)))
        stop("freq must be logical or numeric")
    if (length(x$freq) != 1)
        stop("freq must have length 1")
    if (!is.logical(x$include.lowest))
        stop("include.lowest must be logical")
    if (!is.logical(x$right))
        stop("right must be logical")
    x
}

editDetails.hist <- function(x, specs){
    x <- histGrob(data = x$data, breaks = x$breaks, freq = x$freq,
                  include.lowest = x$include.lowest, right = x$right, fill = x$fill,
                  name = x$name, gp = x$gp, vp = x$vp)
    x
}

grid.hist.example <- function(data = rnorm(100, 0, 3), breaks = 3*(-4:4), freq = 1,
                              include.lowest = TRUE, right = TRUE, fill = "white",
                              name = "histExample", gp = gpar(lwd = 3)){
    require(grid)
    grid.newpage()
    hgrob <- histGrob(data = data, breaks = breaks, freq = freq,
                       include.lowest = include.lowest, right = right, fill = fill,
                       name = name, gp = gp)
    vp <- plotViewport(xscale = range(hgrob$histvals$breaks),
                       yscale = c(0, 1.05*max(hgrob$heights)))
    pushViewport(vp)
    grid.xaxis()
    grid.yaxis()
    grid.draw(hgrob)
}
