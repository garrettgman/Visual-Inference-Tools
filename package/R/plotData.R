# point plotting functions

#' Plots the data as neatly stacked points in the top plot of the VIT canvas.
plotData <- function(canvasObj, ...){
		canvasObj$plotPoints(canvasObj$data, graphsPath("data"), "data.points", ...)
}

#' Plots the current sample as neatly stacked points in the middle plot of the VIT canvas.
plotSample <- function(canvasObj, ...){
		canvasObj$plotPoints(canvasObj$getSample(), graphsPath("sample"), "sample.points", ...)
}

#' Plots the current distribution of the sample statistic as neatly stacked points in the 
#' bottom plot of the VIT canvas.
plotStat <- function(canvasObj, ...){
		canvasObj$plotPoints(canvasObj$getStatDist(), graphsPath("stat"), "stat.points", pch = 2,...)
}
