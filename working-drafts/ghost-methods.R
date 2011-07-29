#' Turns a boxplot grob into a ghost version of itself
makeGhost <- function(canvas, boxplot.grob) {
	boxplot.grob$box.color <- "red"
	boxplot.grob$median.color <- "blue"
	boxplot.grob$show.w <- FALSE
	boxplot.grob$name <- paste("ghost", canvas$which.ghost, sep = ".")
	canvas$which.ghost <- canvas$which.ghost + 1
	boxplot.grob$gp <- gpar(alpha = 0.5)
	boxplot.grob
}