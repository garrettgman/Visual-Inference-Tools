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
#' @param at The height on the y axis of the middle of the boxplot
#' @param height The height of the boxplot
#' @param box.color A character string. The color to use for the box and whiskers
#' @param median.color A character string. The color to use for the median line
#' @param show.w A logical value that describes whether the whiskers should be drawn
#' @param name A name for the grob to be constructed
#' @param gp graphical parameters for the boxplot, constructed with gpar()
#' @param vp A default viewport to be used when drawing the grob
textlistGrob <- function(data, name = NULL, vp = NULL,
	gp = gpar(fontsize = 12, lineheight = 0.9, cex = 1)){
	
		grob(data = data, gp = gp, vp = vp, name = name, cl="textlist")
}

drawDetails.textlist <- function(x, recording){
	limit <- convertHeight(unit(0.9, "npc"), "points", valueOnly = TRUE) / 12
	data <- as.character(x$data)
	if (length(data) > floor(limit)) data <- c(data[1:limit - 1], "...")
	text <- as.list(data)
	text$sep <- "\n"
	text <- do.call("paste", text)
	grid.text(text, gp = gpar(fontsize = 12, lineheight = 1, cex = 1), name = x$name)
}

editDetails.textlist <- function(x, spec){ 
	x <- textlistGrob(x$data, x$name, x$vp, x$gp)
	x
}

validDetails.textlist <- function(x){
	x
}

