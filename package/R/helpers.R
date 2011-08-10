is.categorical <- function(x) {
	inherits(x, c("character", "factor"))
}

notYetImplemented <- function(name, ...) {
	print(paste(name, "has not yet been implemented."))
}