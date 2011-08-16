

loaded_check <- function(e) {
	if(!e$loaded) {
		confirm_dialog("Please choose which inference method you would like to use and click 'Load details.'", handler = function(h, ...) { 
				dispose(h$obj)
			}
		)
	}
}


#' Ensures that confidence intervals are not being attempted with multivariate data. Vit does not currently support them.
#'
#' @param x the vit gui's e$xData
#' @param y the vit gui's e$yData
#' @param method the selected value of the gui's e$stat combobox
confidence_check <- function(e) {
	if (!is.null(e$xData) & !is.null(e$yData)) {
		confirm_dialog(
"VIT cannot apply confidence interval methods to 
more than one variable at a time. The statistic
of interest will be changed to the mean.", 
			handler = function(h, ...) { 
				svalue(e$stat) <- "mean"
				dispose(h$obj)
			}
		)
	}	
} 
	


# code borrowed straight from gwidgets vignette
confirm_dialog <- function(message, handler=NULL)  {
	window <- gwindow("Confirm")
	group <- ggroup(container = window)
	gimage("info", dirname="stock", size="dialog", container=group)

	## A group for the message and buttons
	inner.group <- ggroup(horizontal=FALSE, container = group)
	glabel(message, container=inner.group, expand=TRUE)

	## A group to organize the buttons
	button.group <- ggroup(container = inner.group)
	## Push buttons to right
	addSpring(button.group)
	gbutton("ok", handler=handler, container=button.group)
	gbutton("cancel", container=button.group, handler = handler)
  		
  	return()
}

# this list needs trimmed
plotProportionGroups <-
plotProportionBars <-
plotSampleProportionBars <-
plotSampleProportionGroups <-
plotTriangleDist <-
plotCIStack <-
plotArrow <-
splitViewports <-
plotTriangleDist <-
dropTriangle <-
yAxisViewports <- function() {
	notYetImplemented()
	stop("method does not exist yet.")
}