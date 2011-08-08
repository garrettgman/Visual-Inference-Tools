is.categorical <- function(x) {
	inherits(x, c("character", "factor"))
}

notYetImplemented <- function(...) {
	require(grid)
	grid.newpage()
	grid.text("This feature has not yet been implemented.")
}