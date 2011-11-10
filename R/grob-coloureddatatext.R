## Functions and methods for writing data in data boxes

#' Draw a datatext grob
grid.coldatatext <- function(...)
    grid.draw(coldatatextGrob(...))

#' Construct a datatext grob
#'
#' datatextGrob constructs text of a vector of data with colours for different levels of a factor.
#' datatextGrobs inherit the class "coldatatext".
#'
#' @param data a numeric or character vector of data.
#' @param title a character string to provide the title, usually the name of the data vector.
#' @param cols a vector of character strings specifying the colours for each level of a factor.
#' @param name a name for the grob to be constructed.
#' @param gp graphical parameters for the text, constructed with gpar().
#' @param vp a default viewport to be used when drawing the grob.

coldatatextGrob <- function(data, title = NULL, cols = NULL, xpos = 0.5, max = 30, name = NULL, gp = NULL, vp = NULL){
    if (is.numeric(data)){
        data <- format(round(data, 1), nsmall = 1)
    } else {
        data <- as.character(data)
    }
    n <- length(data)
    if (n <= max){
        ntext <- n
        datalabs <- c(title, data)
    } else {
        ntext <- max
        datalabs <- c(title, data[1:(max - 1)], "...")
    }
    npcs <- (ntext:0)/ntext
    yunit <- unit(npcs, "npc") - unit(4*(npcs - 0.5), "mm") + unit(1 - npcs, "lines")
    grob(data = data, title = title, datalabs = datalabs, xpos = xpos, yunit = yunit, cols = cols, max = max,
         name = name, gp = gp, vp = vp, cl = "coldatatext")
}

drawDetails.coldatatext <- function(x, recording){
    data <- x$data
    datalabs <- x$datalabs
    yunit <- x$yunit
    title <- x$title
    cols <- c(x$cols,  "black", "black"[!is.null(title)])
    xpos <- x$xpos
    levels <- sort(unique(data))
    names(cols) <- c(levels, "...", title)
    colvec <- numeric(length(datalabs))
    for (i in unique(datalabs)){
        grid.text(i, x = xpos, y = yunit[datalabs == i], gp = gpar(col = cols[i]), just = "top")
    }
}

editDetails.coldatatext <- function(x, spec){
    g <- datatextGrob(x$data, x$title, x$cols, x$xpos, x$name, x$gp, x$vp)
    g
}

validDetails.coldatatext <- function(x){
    if (!is.vector(x$data))
        stop("data must be provided as a vector")
    if (length(x$title) != 1)
        stop("title must be a vector of length 1")
    if (!is.numeric(x$max) | length(x$max) != 1 | x$max < 1)
        stop("max should be a numeric vector of length 1")
    if (length(x$cols) != length(unique(x$data)))
        stop("Number of colours specified should equal the number of levels in data")
    x
}
