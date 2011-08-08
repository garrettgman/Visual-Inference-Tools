# functions and methods for constructing textlist grobs

#' Draw a textlist grob
grid.textlist <- function(...){
  grid.draw(textlistGrob(...))
}

#' Construct a textlist grob
#'
#' textlistGrob transforms a vector of data into a list of text and displays the list. Each
#' entry of the vector is displayed in its own line. The textlistGrob performs a manual form
#' of clipping which trims the list to fit in the viewport and displays "..." as the final
#' visible entry if the list exceeds the viewport.
#'
#' @param data A numeric vector of data
#' @param round The number of decimal places values should be rounded to
#' @param name A name for the grob to be constructed
#' @param gp graphical parameters for the boxplot, constructed with gpar()
#' @param vp A default viewport to be used when drawing the grob
textlistGrob <- function(data, round = 2, name = NULL, vp = NULL,
	gp = gpar(fontsize = 12, lineheight = 0.9, cex = 1)){
    data <- as.character(round(data, round))
    if (!is.null(vp)){
        height <- rectGrob(vp = vp)
        limit <- floor(convertHeight(grobHeight(height), "lines", valueOnly = TRUE))
        if (length(data) > limit) data <- c((data[1:(limit - 1)]), "...")
    }
    n <- length(data)
    ys <- 1 - (seq(0.5/n, 1 - (0.5/n), 1/n))
    grob(data = data, ys = ys, gp = gp, vp = vp, name = name, cl="textlist")
}

drawDetails.textlist <- function(x, recording){
    grid.text(x$data, x = 1, y = x$ys, just = c("right", "centre"))
}

editDetails.textlist <- function(x, spec){
	x <- textlistGrob(x$data, x$name, x$vp, x$gp)
	x
}

validDetails.textlist <- function(x){
	x
}

