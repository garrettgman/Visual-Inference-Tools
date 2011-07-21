is.categorical <- function(x) {
	inherits(x, c("character", "factor"))
}

notYetImplemented <- function(...) {
	require(grid)
	grid.newpage()
	grid.text("This feature has not yet been implemented.")
}



plotProportionBars <-
plotPoints <-
plotTriangleDist <-
plotCIStack <-
plotArrow <-
splitViewports <-
plotTriangleDist <-
yAxisViewports <- function() {
	notYetImplemented()
	stop("method does not exist yet.")
}
		
		
		
