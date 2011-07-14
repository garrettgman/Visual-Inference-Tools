# helper functions for making viewport trees
makeViewports <- function(data){
	layout <- grid.layout(ncol = 2, widths = unit(c(.6, 1), "null"))
	frameVP <- plotViewport(c(1,1,1,1), layout = layout, name = "canvas.frame")
	text <- textBoxVP(layout.pos.col = 1)
	graphs <- graphsBoxVP(data = data, layout.pos.col = 2) 
	vpTree(frameVP, vpList(text, graphs))
}


framedPlotVP <- function(name = NULL, layout.pos.row = NULL, layout.pos.col = NULL){
	if (!is.list(name)) stop("name must be a list of character strings")
	if (is.null(name$sep)) name$sep <- "."
	
	# propagating names
	frameName <- do.call("paste", c(name, "frame"))
	plotName <- do.call("paste", c(name, "plot"))
	
	# making viewports
	frame <- viewport(name = frameName, layout.pos.row = layout.pos.row, 
		layout.pos.col = layout.pos.col)
	plot <- plotViewport(c(0,.5, 0,.5), name = plotName)
	
	# making stack
	vpStack(frame, plot)
}

textColumnVP <- function(name = NULL, layout.pos.row = NULL, layout.pos.col = NULL){
	if (!is.list(name)) stop("name must be a list of character strings")
	if (is.null(name$sep)) name$sep <- "."
	
	# making viewports
	layout <- grid.layout(nrow = 3, heights = unit(c(1, 1, 1), 
		c("lines", "lines", "null")))
		
	root <- viewport(layout = layout, layout.pos.row = layout.pos.row, 
		layout.pos.col = layout.pos.col, name = do.call("paste", name))
	top <- framedPlotVP(layout.pos.row = 1, name = c(name, "top"))
	mid <- framedPlotVP(layout.pos.row = 2, name = c(name, "mid"))
	bottom <- framedPlotVP(layout.pos.row = 3, name = c(name, "bottom"))
	
	# making stack
	vpTree(root, vpList(top, mid, bottom))
}


#' Creates the vpTree needed to build the textboxes which display the values of the data and 
#' the sample. These are the left most two boxes in the visual inference tool. textBoxVP 
#' creates a viewport tree that contains 14 viewports in a heirarchy. These viewports are given 
#' the following sensible names:
#' 	"text.data"
#'	"text.data.top.frame"
#'	"text.data.top.plot"
#'	"text.data.mid.frame"
#'	"text.data.mid.plot"
#'	"text.data.bottom.frame"
#'	"text.data.bottom.plot"
#'	"text.sample"
#'	"text.sample.top.frame"
#'	"text.sample.top.plot"
#'	"text.sample.mid.frame"
#'	"text.sample.mid.plot"
#'	"text.sample.bottom.frame"
#'	"text.sample.bottom.plot"
textBoxVP <- function(name = list("text"), layout.pos.row = NULL, layout.pos.col = NULL){
	if (!is.list(name)) stop("name must be a list of character strings")
	if (is.null(name$sep)) name$sep <- "."
	
	dataName <- do.call("paste", c(name, "data"))
	sampName <- do.call("paste", c(name, "sample"))
	
	layout <- grid.layout(ncol = 3, widths = unit(c(1, 0.5, 1), c("null", "line", "null")))
	
	textBox <- viewport(layout = layout, layout.pos.row = layout.pos.row, 
		layout.pos.col = layout.pos.col, name = do.call("paste", name))
	data <- textColumnVP(layout.pos.col = 1, name = c(name, "data"))
	sample <- textColumnVP(layout.pos.col = 3, name = c(name, "sample"))
	
	vpTree(textBox, vpList(data, sample))
}

# for graphical side of the visual inference tool display
framedDataVP <- function(data = data, name = NULL, layout.pos.row = NULL, 
	layout.pos.col = NULL){
	
	if (!is.list(name)) stop("name must be a list of character strings")
	if (is.null(name$sep)) name$sep <- "."
	
	frameName <- do.call("paste", c(name, "frame"))
	plotName <- do.call("paste", c(name, "plot"))
	dataName <- do.call("paste", c(name, "data"))
	
	frame <- viewport(name = frameName, layout.pos.row = layout.pos.row, 
		layout.pos.col = layout.pos.col)
	plot <- plotViewport(c(2,1,1,1), name = plotName)
	
	dataVP <- dataViewport(xData = data, yscale = c(0,1), name = dataName)
	
	vpStack(frame, plot, dataVP)
}

#' Creates the vpTree needed to build the graphs which display the original data set, the samples, and the distribution of the sample statistic. These are the right three plots in the visual inference tool. graphsBoxVP creates a viewport tree that contains 10 viewports in a heirarchy. These viewports are given the following sensible names:
#' 	"graphs"
#'	"graphs.data.frame"
#'	"graphs.data.plot"
#'	"graphs.data.data"
#'	"graphs.sample.frame"
#'	"graphs.sample.plot"
#'	"graphs.sample.data"
#'	"graphs.stat.frame"
#'	"graphs.stat.plot"
#'	"graphs.stat.data"
graphsBoxVP <- function(data, name = list("graphs"), layout.pos.row = NULL, 
	layout.pos.col = NULL){
	if (!is.list(name)) stop("name must be a list of character strings")
	if (is.null(name$sep)) name$sep <- "."
	
	layout <- grid.layout(nrow = 3)
	
	graphsBox <- viewport(layout = layout, layout.pos.row = layout.pos.row, 
		layout.pos.col = layout.pos.col, name = do.call("paste", name))
	dataVp <- framedDataVP(data, layout.pos.row = 1, name = c(name, "data"))
	sample <- framedDataVP(data, layout.pos.row = 2, name = c(name, "sample"))
	statistic <- framedDataVP(data, layout.pos.row = 3, name = c(name, "stat"))
	
	vpTree(graphsBox, vpList(dataVp, sample, statistic))
}

#' Helper function to quickly construct vpPaths to the three plotting spaces on the VIT 
#' canvas: the data plot, the sample plot, and the statistics plot.
#'
#' @param plot.name A character string that gives the name of the plot whose vpPath you wish to retrieve. "data" retrieves the data plot, "sample" the sample plot, and "stat" the statistic plot.
graphsPath <- function(plot.name){
	vpPath("canvas.frame", "graphs", paste("graphs", plot.name, "frame", sep = "."), 
		paste("graphs", plot.name, "plot", sep = "."), paste("graphs", plot.name, "data", 
		sep = "."))
}

#' Helper function to quickly construct vpPaths to the three levels of the two text boxes on 
#' the VIT canvas: the data box and the sample box.
#'
#' @param data.or.sample A character string that gives the name of the box whose vpPath you wish to retrieve. "data" retrieves the data box, "sample" the sample box. 
#' @param top.mid.or.bottom A character string that gives the name of the level of the box whose vpPath you wish to retrieve: "top", "mid", or "bottom". The top level displays the title of the box, the mid the filepath, and the bottom the data values.
textPath <- function(data.or.sample, top.mid.or.bottom){
	vpPath("canvas.frame", "text", paste("text", data.or.sample, sep = "."), 
		paste("text", data.or.sample, top.mid.or.bottom, "frame", sep = "."), 
		paste("text", data.or.sample, top.mid.or.bottom, "plot", sep = "."))
}



