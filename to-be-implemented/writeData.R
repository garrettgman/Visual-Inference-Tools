#' Writes the data values as a list in the left text box of the VIT canvas.
writeData <- function(canvasObj, ...){
	canvasObj$writeText("Data", textPath("data", "top"), "data.title")
	canvasObj$writeText(canvasObj$var.name, textPath("data", "mid"), "data.var.name")
	canvasObj$writeList(canvasObj$data, textPath("data", "bottom"), "data.text")
}

#' Writes the sample values as a list in the left text box of the VIT canvas.
writeSample <- function(canvasObj, ...){
	canvasObj$writeText("Sample", textPath("sample", "top"), "sample.title")
	canvasObj$writeText(canvasObj$var.name, textPath("sample", "mid"), "sample.var.name")
	canvasObj$writeList(canvasObj$getSample(), textPath("sample", "bottom"), "sample.text")
}

