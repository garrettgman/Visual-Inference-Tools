# basic toolkit functions for building vit graphs

# data plot constructor. All visualizations will begin with a plot of the data.
dataPlot <- function(df) {
	data <- df
	xrange <- c(min(0, min(data$x)), max(data$x))
	yrange <- c(min(0, min(data$y)), max(data$y))
	
	gTree(x = NULL, y = NULL, childrenvp = vpTree(plotViewport(name = "plotRegion"), vpList(viewport(name = "dataRegion", xscale = xrange, yscale = yrange))), children = gList(xaxisGrob(vp = "plotRegion::dataRegion"), pointsGrob(x = data$x, y = data$y, vp = "plotRegion::dataRegion")))
}
	

