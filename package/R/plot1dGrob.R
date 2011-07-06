# functions and methods for constructing plot grobs

#' Draw a plot grob
grid.plot1d <- function(...){
  grid.draw(plot1dGrob(...))
}

#' Construct a one dimensional plot grob
#'
#' plot1dGrob constructs a horizontally oriented plot that describes a vector of data. A plot1d 
#' grob consists of an xaxis and a dataViewport to plot boxplots, points, and other objects in. 
#' plot1ds form the basis of the data, sample, and statistic plots in the vit explorer. To 
#' create a vertical plot1d, rotate the viewport it is drawn in. plot1dGrobs inherit the 
#' class "plot1d".
#' 
#' @param data A numeric vector of data
#' @param layout A grid layout object. Primarily used to plot multiple groups of data.
#' @param name A name for the grob to be constructed
#' @param gp graphical parameters for the boxplot, constructed with gpar()
#' @param vp A default viewport to be used when drawing the grob
plot1dGrob <- function(data, layout = NULL, name = NULL, gp = NULL, vp = NULL){
	
	if (is.null(layout)) layout <- grid.layout()
	if (is.null(name)) name <- "plot1d"
	
	plotvp <- plotViewport(c(2, 0, 0, 0), name = paste(name, "plot", sep = "."))
	datavp <- dataViewport(xData = data, yscale = c(0,1), layout = layout, 
								name = paste(name, "data", sep = "."))

	pgt <- gTree(data = data, layout = layout, name = name, 
				childrenvp = vpStack(plotvp, datavp), 
				gp = gp, vp = vp, cl="plot1d")
	pgt
}

# Utility for updating a boxplot grob
setPlot1dGrob <- function(pgt){
	xaxis <- NULL
	vpname1 <- paste(pgt$name, "plot", sep = ".")
	vpname2 <- paste(pgt$name, "data", sep = ".")
   
	xaxis <- xaxisGrob(vp = vpPath(vpname1, vpname2))
	pgt <- setChildren(pgt,gList(xaxis))
  	pgt
}


drawDetails.plot1d <- function(x, recording){
	x <- setPlot1dGrob(x)
 	for (i in childNames(x)) grid.draw(getGrob(x, i))
}

editDetails.plot1d <- function(x, spec){ 
	x <- plot1dGrob(x$data, x$layout, x$name, x$gp, x$vp)
	x
}

validDetails.plot1d <- function(x){
	if (!inherits(x$data, c("integer", "numeric")))
		stop("data must be integer, numeric or matrix")  

	if (!inherits(x$layout, c("layout", "NULL")))
		stop("layout must be a grid layout object")  

	x
}


# Example
# grid.newpage()
# grid.plot1d(data = mpg$hwy, layout = grid.layout(nrow = 2))
# downViewport("plot1d.data")
# grid.rect()
# grid.rect(gp = gpar(col = "red"), vp = viewport(layout.pos.row = 2))
# grid.rect(gp = gpar(col = "blue"), vp = viewport(layout.pos.row = 1))
# grid.points(x = mpg$hwy, y = stackPoints(mpg$hwy, pheight = 0.1, nbins = 40), vp = dataViewport(xData = mpg$hwy, yscale = c(0,1), layout.pos.row = 2))
# grid.boxplot(mpg$hwy, at = unit(0.5, "npc"), height = unit(0.5, "native"), vp = dataViewport(xData = mpg$hwy, yscale = c(0,1), layout.pos.row = 1))