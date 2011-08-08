# boxplot plotting functions

#' Plots the data as a boxplot in the top plot of the VIT canvas.
boxplotData = function(canvasObj, ...){
	canvasObj$plotBoxplot(canvasObj$data, graphsPath("data"), "data.boxplot", ...)
}

#' Plots the current sample as a boxplot in the middle plot of the VIT canvas.
boxplotSample = function(canvasObj, ...){
		
	canvasObj$plotBoxplot(canvasObj$getSample(), graphsPath("sample"), "sample.boxplot", ...)
}

#' Plots the current distribution of the sample statistic as a boxplot in the 
#' bottom plot of the VIT canvas.
boxplotStat = function(canvasObj, ...){
		
	canvasObj$plotBoxplot(canvasObj$getStatDist(), graphsPath("stat"), "stat.boxplot", ...)
}
