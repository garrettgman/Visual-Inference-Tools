# Makes a viewport scheme to facilitate animation and plotting a statistic underneath the data

makeVitGraphViewports <- function(x.scale, nlevels.y) {
	
	canvas.frame.layout <- grid.layout(nrow = 2, heights = unit(1, 
		c("null", "line")))
	canvas.frame <- plotViewport(c(3, 1, 0, 1), layout = canvas.frame.layout,
		name = "canvas.frame")
	
	
	animation.layout <- grid.layout(nrow = 3)
	animation.field <- dataViewport(xscale = x.scale, yscale = c(0, 3), 
		layout = animation.layout, name = "animation.field")	
		
	data <- splitDataPane(x.scale = x.scale, n = nlevels.y, layout.pos.row = 1, 
		name = "data.data")
	sample <- splitDataPane(x.scale = x.scale, n = nlevels.y, 
		layout.pos.row = 2, name = "sample.data")
	stat <- dataViewport(xscale = x.scale, yscale = c(0, 1), layout.pos.row = 3, 
		name = "stat.data.1")
		
		vpTree(canvas.frame, vpList(vpTree(animation.field, 
			vpList(data, sample, stat))))
}

splitDataPane <- function(x.scale, n, layout.pos.row, name) {
	
	frame.layout <- grid.layout(nrow = n)
	frame <- viewport(layout.pos.row = layout.pos.row, layout = frame.layout, 
		name = paste(name, "placer", sep = "."))
	
	data.vps <- list()
	for(i in 1:n) {
		data.vps[[i]] <- dataViewport(xscale = x.scale, yscale = c(0, 1), 
			layout.pos.row = n - i + 1, name = paste(name, i, sep = "."))
	}
		
	vpTree(frame, do.call("vpList", data.vps))
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

