# R5 implementation for canvas object

# 1. make R5 canvas class
#' The canvasClass reference class provides a class for making objects that manage the 
#' visual display of the VIT tool. I used a reference class here because we can keep all of 
#' the relevant information in one place (the canvas object) and use methods to manipulate 
#' it. This is different than the normal R approach, which is "functional." In the 
#' functional approach we must put all our information into each function and then collect 
#' it again on the other side of the function. That would be burdensome here because there 
#' is so much information to keep track of. The reference class approach is an attempt at 
#' object oriented programming.
canvas <- setRefClass("canvasClass", fields = c("image", "viewports", "data", "samples", "which.sample", "n", "stat.dist", "var.name"), methods = list(
	initialize = function(data = NA, ...){
		require(grid)
		var.name <<- deparse(substitute(data))
		data <<- data
		n <<- length(data)
		samples <<- split(sample(1:n, n * 1000, replace = TRUE), rep(1:1000, each = n))
		viewports <<- makeViewports(data)
		image <<- drawBackground(data, viewports)
		which.sample <<- 1
		stat.dist <<- vector(length = 1000)
		for(i in 1:1000)
			stat.dist[i] <<- mean(data[samples[[i]]])
		invisible(.self)
	},
	getSample = function(){
		'Returns current sample of data.'
		data[samples[[which.sample]]]
	},
	newSample = function(){
		'Takes new sample of data and returns it invisibly.'
		if (which.sample >= 1000) which.sample <<- 0
		which.sample <<- which.sample + 1
		invisible(data[samples[[which.sample]]])
	},
	getStatDist = function(){
		'Returns current distribution of sampling statistic.'
		stat.dist[1:which.sample]
	},
	drawCanvas = function(){
		'Draws current image in device.'
		grid.newpage()
		grid.draw(image)
	},
	plotPoints = function(x, vp, name, ...){
		'Plots x in specified viewport.'
		if (length(x) <= 1) df <- data.frame(x = x, y = 0)
		else {
			seekViewport(vp)
		
			pheight <- convertY(unit(1, "char"), "native", valueOnly = TRUE) * 0.8
			binwidth <- pheight * diff(range(x)) * 0.37/2
  			nbins <- ceiling(diff(range(x) / binwidth))
			breaks <- min(x) + c(0:(nbins)) * binwidth
			group <- cut(x, breaks, include.lowest = TRUE)
			max.stack <- max(table(group))
			ydist <- min(1/max.stack, as.numeric(pheight))
			df <- data.frame(x = x, group = group)
			df$y <- NA	
			for (i in levels(group)) {
				igroup <- which(df$group == i)
				j <- nrow(df[igroup, ])
				if (j > 0) df[igroup, ]$y <- seq_len(j)
			}
	
			df$y <- (df$y - 1) * ydist
		}
		
		image <<- addGrob(image, pointsGrob(x = df$x, y = df$y, vp = vp, name = name, ...))
		drawCanvas()
	},
	plotBoxplot = function(x, vp, name, ...){
		'Plots boxplot of x in specified viewport'
		image <<- addGrob(image, boxplotGrob(x, vp = vp, name = name, ...))
		drawCanvas()
	},
	writeList = function(x, vp, name, ...){
		'Writes text list of x in the specified viewport'
		image <<- addGrob(image, textlistGrob(x, vp = vp, name = name, ...))		
		drawCanvas()
	},
	writeText = function(x, vp, name, ...){
		'Writes text of x in the specified viewport'
		image <<- addGrob(image, textGrob(x, vp = vp, name = name, ...)) 		
		# drawCanvas()
	}
))

