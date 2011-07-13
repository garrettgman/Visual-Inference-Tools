#' Writes the data values as a list in the left text box of the VIT canvas.
writeData <- function(canvasObj, ...){
	canvasObj$writeText(canvasObj$data, textPath("data", "bottom"), "data.text")
}

#' Writes the sample values as a list in the left text box of the VIT canvas.
writeSample <- function(canvasObj, ...){
	canvasObj$writeText(canvasObj$getSample(), textPath("sample", "bottom"), "sample.text")
}

