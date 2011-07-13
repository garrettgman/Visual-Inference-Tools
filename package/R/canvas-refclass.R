# R5 implementation for canvas object

# 1. make R5 canvas class
# To consider: should I externalize some of these methods? For example, include the plotPoints method, but have plotData, plotSample, and plotStat external functions that access the plotPoints method?
canvas <- setRefClass("canvas", fields = c("image", "viewports", "data", "samples", "which.sample", "n", "stat.dist"), methods = list(
	initialize = function(data = NA, ...){
		require(grid)
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
			binwidth <- pheight * diff(range(x)) * 0.37
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
		
		image <<- addGrob(image, pointsGrob(x = df$x, y = df$y, vp = vp, name = name))
		drawCanvas()
	},
	plotBoxplot = function(x, vp, name, ...){
		'Plots boxplot of x in specified viewport'
		image <<- addGrob(image, boxplotGrob(x, vp = vp, name = name, ...))
		drawCanvas()
	},
	writeText = function(x, vp, name, ...){
		'Writes text in x in the specified viewport'
		seekViewport(vp)
		limit <- convertHeight(unit(0.9, "npc"), "points", valueOnly = TRUE) / 12
		x <- as.character(x)
		print(limit)
		if (length(x) > floor(limit)) x <- c(x[1:limit - 1], "...")
		text <- as.list(x)
		text$sep <- "\n"
		text <- do.call("paste", text)
		image <<-addGrob(image, textGrob(text, gp = gpar(fontsize = 12, lineheight = 1, cex = 1), vp = vp, name = name, ...)) # might need its own GROB
		image <<-addGrob(image, rectGrob(vp = vp))
		drawCanvas()
	}
))

