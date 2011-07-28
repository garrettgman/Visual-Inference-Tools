#' Constructs a viewport tree for the canvas that is suited for displaying the 
#' type of data provided.
loadViewports <- function(canvas, x, y = NULL) {
	if (is.categorical(x)) x.scale <- c(0, length(x))
	else x.scale <- range(x)
	
	if (is.null(y)) {
		n.y <- 1
		y.scale <- c(0,1)
	} else if (is.categorical(y)) {
		n.y <- length(unique(y))
		y.scale <- c(0,1)
	} else {
		n.y <- 1
		y.scale <- range(y)
		notYetImplemented()
		stop("method does not exist yet.")
	}
	
	canvas$viewports <- makeViewports(x.scale, y.scale, is.null(y), n.y)
}
	
	
	
makeViewports <- function(x.scale, y.scale, null.y, n.y) {
	layout <- grid.layout(ncol = 2, widths = unit(c(.6, 1), "null"))
	frameVP <- plotViewport(c(1,1,1,1), layout = layout, name = "canvas.frame")
	text <- textBoxVP(null.y = null.y, layout.pos.col = 1)
	graphs <- graphsBoxVP(x.scale = x.scale, y.scale = y.scale, 
		n.y = n.y, layout.pos.col = 2) 
	vpTree(frameVP, vpList(text, graphs))
}


#' Creates the vpTree needed to build the textboxes which display the values of #' the data and the sample. These are the left most two boxes in the visual 
#' inference tool. textBoxVP creates a viewport tree that contains 30 viewports 
#' in a heirarchy. By default, these viewports are assigned the descriptive 
#' names below. 
#' 
#' If a y variable is supplied, textBoxVP will create the viewports necessary to 
#' display two columns of values for both the data box and the sample box. If a 
#' y variable is not supplied (i.e, the user is examining one dimensional data) 
#' textBoxVP will only create the viewports necessary to display a single column 
#' of values. In this case, only the first 16 names below will be used.
#' 	"text.data"
#'  "text.data.x"
#'	"text.data.x.top.frame"
#'	"text.data.x.top.plot"
#'	"text.data.x.mid.frame"
#'	"text.data.x.mid.plot"
#'	"text.data.x.bottom.frame"
#'	"text.data.x.bottom.plot"
#'  "text.sample"
#'	"text.sample.x"
#'	"text.sample.x.top.frame"
#'	"text.sample.x.top.plot"
#'	"text.sample.x.mid.frame"
#'	"text.sample.x.mid.plot"
#'	"text.sample.x.bottom.frame"
#'	"text.sample.x.bottom.plot"
#' 	"text.data.y"
#'	"text.data.y.top.frame"
#'	"text.data.y.top.plot"
#'	"text.data.y.mid.frame"
#'	"text.data.y.mid.plot"
#'	"text.data.y.bottom.frame"
#'	"text.data.y.bottom.plot"
#'	"text.sample.y"
#'	"text.sample.y.top.frame"
#'	"text.sample.y.top.plot"
#'	"text.sample.y.mid.frame"
#'	"text.sample.y.mid.plot"
#'	"text.sample.y.bottom.frame"
#'	"text.sample.y.bottom.plot"
textBoxVP <- function(name = list("text"), layout.pos.col = NULL, 
	layout.pos.row = NULL, null.y = TRUE){
		
	if (!is.list(name)) stop("name must be a list of character strings")
	if (is.null(name$sep)) name$sep <- "."
	
	layout <- grid.layout(ncol = 3, widths = unit(c(1, 0.5, 1), 
		c("null", "line", "null")))
	
	textBox <- viewport(layout = layout, layout.pos.col = layout.pos.col, 
		layout.pos.row = layout.pos.row, name = do.call("paste", name))
	if (null.y) {
		data <- vpStack(viewport(layout.pos.col = 1, 
			name = do.call("paste", c(name, "data"))),
			textColumnVP(name = c(name, "data", "x")))
		sample <- vpStack(viewport(layout.pos.col = 3, 
			name = do.call("paste", c(name, "sample"))), 
			textColumnVP(name = c(name, "sample", "x")))
	} else {
		data <- textTwoColumnVP(layout.pos.col = 1, name = c(name, "data"))
		sample <- textTwoColumnVP(layout.pos.col = 3, name = c(name, "sample"))
	}
	vpTree(textBox, vpList(data, sample))
}

textColumnVP <- function(name = NULL, layout.pos.col = NULL, 
	layout.pos.row = NULL) {
	
	if (!is.list(name)) stop("name must be a list of character strings")
	if (is.null(name$sep)) name$sep <- "."
	
	# making viewports
	layout <- grid.layout(nrow = 3, heights = unit(c(1, 1, 1), 
		c("lines", "lines", "null")))
		
	root <- viewport(layout = layout,  layout.pos.col = layout.pos.col, 
		layout.pos.row = layout.pos.row, name = do.call("paste", name))
	top <- framedPlotVP(layout.pos.row = 1, name = c(name, "top"))
	mid <- framedPlotVP(layout.pos.row = 2, name = c(name, "mid"))
	bottom <- framedPlotVP(layout.pos.row = 3, name = c(name, "bottom"))
	
	# making stack
	vpTree(root, vpList(top, mid, bottom))
}

textTwoColumnVP <- function(name = NULL, layout.pos.col = NULL, 
	layout.pos.row = NULL) {
		
	if (!is.list(name)) stop("name must be a list of character strings")
	if (is.null(name$sep)) name$sep <- "."
	
	# making viewports
	layout <- grid.layout(ncol = 3, widths = unit(c(1, 0.3, 1), 
		c("null", "lines", "null")))
		
	root <- viewport(layout = layout, layout.pos.col = layout.pos.col, 
		layout.pos.row = layout.pos.row, name = do.call("paste", name))
	x <- textColumnVP(layout.pos.col = 1, name = c(name, "x"))
	y <- textColumnVP(layout.pos.col = 3, name = c(name, "y"))
	
	# making stack
	vpTree(root, vpList(x, y))
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
#' If a y variable is included, graphsBoxVP will further divide the data.data 
#' and sample.data plots into viewports into n viewports where n is the number 
#' of groups specified by the y variable. These data viewports will be named 
#' according to the pattern:
#'  "graphs.data.data.1"
#'  "graphs.data.data.2"
#'  etc.
#' where "graphs.data.data.1 corresponds to the viewport closest to the x axis. 
graphsBoxVP <- function(x.scale, y.scale, n.y = 1, name = list("graphs"), 
	layout.pos.row = NULL, layout.pos.col = NULL) {
		
	if (!is.list(name)) stop("name must be a list of character strings")
	if (is.null(name$sep)) name$sep <- "."

	layout <- grid.layout(nrow = 3)
	
	graphsBox <- viewport(layout = layout, layout.pos.row = layout.pos.row, 
		layout.pos.col = layout.pos.col, name = do.call("paste", name))
	dataVp <- framedDataVP(x.scale, y.scale, n.y, layout.pos.row = 1, 
		name = c(name, "data"))
	sample <- framedDataVP(x.scale, y.scale, n.y, layout.pos.row = 2, 
		name = c(name, "sample"))
	statistic <- framedDataVP(x.scale, y.scale, layout.pos.row = 3, 
		name = c(name, "stat"))
	
	vpTree(graphsBox, vpList(dataVp, sample, statistic))
}

framedDataVP <- function(x.scale = x.scale, y.scale = y.scale, n.y = 1, 
	name = NULL, layout.pos.row = NULL, layout.pos.col = NULL){
	
	if (!is.list(name)) stop("name must be a list of character strings")
	if (is.null(name$sep)) name$sep <- "."
	
	frameName <- do.call("paste", c(name, "frame"))
	plotName <- do.call("paste", c(name, "plot"))
	dataName <- do.call("paste", c(name, "data"))
	statName <- do.call("paste", c(name, "stat"))
	
	frame.layout <- grid.layout(nrow = 2, heights = unit(1, c("null", "line")))
	
	if (n.y == 1) layout <- grid.layout()
	else layout <- grid.layout(nrow = n.y)
	
	frame <- viewport(name = frameName, layout.pos.row = layout.pos.row, 
		layout.pos.col = layout.pos.col, layout = frame.layout)
	plot <- plotViewport(c(2,1,1,1), name = plotName, layout = layout, 
		layout.pos.row = 1)
	dataVPs <- list()
	for (i in 1:n.y) {
		dataVPs[[i]] <- dataViewport(xscale = x.scale, yscale = y.scale, 
			layout.pos.row = n.y - i + 1, name = paste(dataName, i, sep = "."))
	}	
	stat <- dataViewport(xscale = x.scale, yscale = y.scale, layout.pos.row = 2, 
		name = statName)
		
	vpTree(frame, vpList(stat, vpTree(plot, do.call("vpList", dataVPs))))
}


#' Helper function to quickly construct vpPaths to the three plotting spaces on 
#' the VIT canvas: the data plot, the sample plot, and the statistics plot.
#'
#' @param plot.name A character string that gives the name of the plot whose vpPath you wish to retrieve. "data" retrieves the data plot, "sample" the sample plot, and "stat" the statistic plot.
#' @param number A selection from 1, 2, 3, 4, or 5 entered as either a number or a character string. number corresponds to the number of the data plot within the  data, sample, or statistic portion of the vit canvas. 
graphsPath <- function(plot.name = "sample", number = "1") {
	if(!(plot.name %in% c("data", "sample", "stat")))
		stop("plot.name must be \'data', \'sample', or \'stat'.")
	if (is.numeric(number)) number <- as.character(number)
	if ( !(number %in% c("1", "2", "3", "4", "5", "stat")))
		stop("number must be \'1', \'2', \'3', \'4', \'5', or 'stat'.")
	
	if (number == "stat") {
		vpPath("canvas.frame", "graphs", 
			paste("graphs", plot.name, "frame", sep = "."), 
			paste("graphs", plot.name, "stat", sep = "."))
	} else {	
		vpPath("canvas.frame", "graphs", 
			paste("graphs", plot.name, "frame", sep = "."), 
			paste("graphs", plot.name, "plot", sep = "."), 
			paste("graphs", plot.name, "data", number, sep = "."))
	}
}

#' Helper function to quickly construct vpPaths to the three levels of the two 
#' text boxes on the VIT canvas: the data box and the sample box.
#'
#' @param data.or.sample A character string that gives the name of the box whose vpPath you wish to retrieve. "data" retrieves the data box, "sample" the sample box. 
#' @param x.or.y A character string that gives the name of the variable column whose vpPath you wish to retrieve: "x" or "y". Note: if there is no y variable loaded, "x" will be the only valid choice.
#' @param top.mid.or.bottom A character string that gives the name of the level of the box whose vpPath you wish to retrieve: "top", "mid", or "bottom". The top level displays the title of the box, the mid the filepath, and the bottom the data values.
textPath <- function(data.or.sample = "sample", x.or.y = "x", 
	top.mid.or.bottom = "bottom") {
		
	vpPath("canvas.frame", "text", paste("text", data.or.sample, sep = "."), 
		paste("text", data.or.sample, x.or.y),
		paste("text", data.or.sample, x.or.y, top.mid.or.bottom, "frame", 
			sep = "."), 
		paste("text", data.or.sample, x.or.y, top.mid.or.bottom, "plot", 
			sep = "."))
}

#' appends a vpPath to include the number n on the bottommost viewport. If the 
#' bottom most viewport ends in a number, it replaces that number with n.

appendPath <- function(vp, n) {
	text <- as.character(vp$name)
	m <- nchar(text)
	if (substr(text, m - 1, m - 1) == ".") substr(text, m, m) <- as.character(n)
	else text <- paste(text, n, sep = ".")
	
	structure(list(path = vp$path, name = text, n = vp$n), 
		class = c("vpPath", "path"))
}

#' helper function for programming use
showVPs <- function() {
	vps <- as.character(current.vpTree())
	vps <- gsub("viewport\\[", "", vps)
	vps <- gsub("\\]", "", vps)
	vps <- gsub("\\(", "", vps)
	vps <- gsub("\\)", "", vps)
	vps <- gsub(" ", "", vps)
	vps <- gsub("->", ",", vps)
	
	vlist <- strsplit(vps, ",")
	
	for (name in vlist[[1]][-1]) {
		seekViewport(name)		
		grid.rect(gp = gpar(col = "black", alpha = 0.5))
	}
} 
