## Functions and methods for writing data in data boxes

#' Draw a datatext grob
grid.datatext <- function(...)
    grid.draw(datatextGrob(...))

#' Construct a datatext grob
#'
#' datatextGrob constructs text of a vector of data.
#' datatextGrobs inherit the class "datatext".
#'
#' @param data a numeric or character vector of data.
#' @param title a character string to provide the title, usually the name of the data vector.
#' @param name a name for the grob to be constructed.
#' @param gp graphical parameters for the text, constructed with gpar().
#' @param vp a default viewport to be used when drawing the grob.

datatextGrob <- function(data, title = NULL, name = NULL, gp = NULL, vp = NULL){
    n <- length(data)
    if (n <= 30){
        ntext <- n
        datalabs <- c(title, format(round(data, 1), nsmall = 1))
    } else {
        ntext <- 30
        datalabs <- c(title, format(round(data[1:29], 1), nsmall = 1), "...")
    }
    npcs <- (ntext:0)/ntext
    yunit <- unit(npcs, "npc") - unit(4*(npcs - 0.5), "mm") + unit(1 - npcs, "lines")
    grob(data = data, datalabs = datalabs, yunit = yunit,
         name = name, gp = gp, vp = vp, cl = "datatext")
}

drawDetails.datatext <- function(x, recording){
    datalabs <- x$datalabs
    yunit <- x$yunit
    grid.text(datalabs, y = yunit, just = "top")
}

editDetails.datatext <- function(x, spec){
    x <- datatextGrob(x$data, x$title, x$name, x$gp, x$vp)
    x
}

vaildDetails.datatext <- function(x){
    if (!is.vector(x$data))
        stop("data must be provided as a vector")
    if (length(x$title) != 1)
        stop("title should be a vector of length 1")
}

grid.datatext.example <- function(data = rnorm(100), title = "X", name = "datatext.example")
    grid.datatext(data = data, title = title, name = name)
