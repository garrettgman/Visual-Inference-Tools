is.categorical <- function(x) {
	inherits(x, c("character", "factor"))
}

notYetImplemented <- function(name = "This method", ...) {
	print(paste(name, "has not yet been implemented."))
}