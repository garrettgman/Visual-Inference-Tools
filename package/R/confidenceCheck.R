#' Ensures that confidence intervals are not being attempted with multivariate data. Vit does not currently support them.
#'
#' @param x the vit gui's e$xData
#' @param y the vit gui's e$yData
#' @param method the selected value of the gui's e$stat combobox
confidenceCheck <- function(e, x, y, method){
	if (!is.null(x) & !is.null(y) & method %in% c("confidence interval - mean", 
		"confidence interval - median")) {
			confirmDialog(
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
confirmDialog <- function(message, handler=NULL)  {
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