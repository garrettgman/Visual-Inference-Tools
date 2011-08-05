# Makes a viewport scheme to facilitate animation and plotting a statistic underneath the data

makeVitGraphViewports <- function(x.scale, nlevels.y) {
	
	canvas.frame.layout <- grid.layout(nrow = 2, heights = unit(1, 
		c("null", "line")))
	canvas.frame <- plotViewport(c(3, 1, 0, 1), layout = canvas.frame.layout,
		name = "canvas.frame")
	
	
	animation.layout <- grid.layout(nrow = 3)
	animation.field <- dataViewport(xscale = x.scale, yscale = c(0, 3), 
		layout.pos.row = 1, layout = animation.layout, name = "animation.field")	
	stat.stat <- dataViewport(xscale = x.scale, yscale = c(0, 1), 
		layout.pos.row = 2, name = "stat.stat")
		
	
	stat.placer.layout <- grid.layout(nrow = 4, 
		heights = unit(1, c("line", "null", "line", "null")))
	stat.placer <- viewport(layout.pos.row = 2:3, layout = stat.placer.layout,
		name = "stat.placer")
	
	data.stat <- dataViewport(xscale = x.scale, yscale = c(0, 1), 
		layout.pos.row = 1, name = "data.stat")
	sample.stat <- dataViewport(xscale = x.scale, yscale = c(0, 1), 
		layout.pos.row = 3, name = "sample.stat")
		
	data <- splitDataPane(x.scale = x.scale, n = nlevels.y, layout.pos.row = 1, 
		name = "data.data")
	sample <- splitDataPane(x.scale = x.scale, n = nlevels.y, 
		layout.pos.row = 2, name = "sample.data")
	stat <- dataViewport(xscale = x.scale, yscale = c(0,1), layout.pos.row = 3, 
		name = "stat.data.1")
		
		vpTree(canvas.frame, vpList(vpTree(animation.field, 
			vpList(vpTree(stat.placer, vpList(data.stat, sample.stat)), data, 
			sample, stat)), stat.stat))
}

splitDataPane <- function(x.scale, n, layout.pos.row, name) {
	
	frame.layout <- grid.layout(nrow = n)
	frame <- viewport(layout.pos.row = layout.pos.row, layout = frame.layout, 
		name = paste(name, "placer", sep = "."))
	
	data.vps <- list()
	for(i in 1:n) {
		data.vps[[i]] <- dataViewport(xscale = x.scale, yscale = c(0,1), 
			layout.pos.row = i, name = paste(name, i, sep = "."))
	}
		
	vpTree(frame, do.call("vpList", data.vps))
}

# returns the vpPath to the statistic viewport for the specified field. Note the 
# statistic appears above this viewport, but it is not actually plotted in it. 
# The viewport is just used for plotting the xaxis
statPath <- function(field) {
	if(!(field %in% c("data", "sample", "stat")))
		stop("field must be \'data', \'sample', or \'stat'.")
	
	if (field == "stat")
		vpPath("canvas.frame", "stat.stat")
	else
		vpPath("canvas.frame", "animation.field", "stat.placer", 
			paste(field, "stat", sep = "."))
}

# returns the vpPath to the graph viewport for the specified field.
graphPath <- function(plot.name = "sample", number = "1") {
	if(!(plot.name %in% c("data", "sample", "stat")))
		stop("plot.name must be \'data', \'sample', or \'stat'.")
	if (is.numeric(number)) number <- as.character(number)
	if ( !(number %in% c("1", "2", "3", "4", "5")))
		stop("number must be \'1', \'2', \'3', \'4', or \'5'.")
	
	if (plot.name == "stat") {
		vpPath("canvas.frame", "animation.field", 
			paste(plot.name, "data", number,  sep = "."))		
	} else {
		vpPath("canvas.frame", "animation.field",
			paste(plot.name, "data.placer", sep = "."), 
			paste(plot.name, "data", number,  sep = "."))
	}
}


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
	
	canvas$viewports <- makeVitGraphViewports(x.scale, n.y)
}

	
#' returns the number at the end of the viewport name
vpNumber <- function(vp) {
	text <- as.character(vp$name)
	m <- nchar(text)
	last <- substr(text, m, m)
	if (last %in% c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9"))
		return(last)
	else
		return("")
}
	
	
