# Ghost methods for making grobs transparent

#' Convert a grob into a transparent ghost of itself
#' @name ghost
#'
#' @S3method ghost boxplot
ghost <- function(grob, ...) {
	if (!inherits(grob, c("grob", "gTree"))) stop("grob must be a graphical object or gTree")
	UseMethod("ghost", grob)
}

ghost.boxplot <- function(grob, ...) {
	editGrob(grob, box.color = "red", median.color = "blue", show.w = FALSE, 
		gp = gpar(alpha = 0.7))
}
	