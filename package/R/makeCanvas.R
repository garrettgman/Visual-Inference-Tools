# Master function for making the vit canvas

makeCanvas <- function(data, text.box = FALSE){
	.data <- as.data.frame(data)
	.n <- nrow(.data)
	.samples <- split(sample(1:n, n * 1000, replace = TRUE), rep(1:1000, each = n)),
	.i <- 1 # sample counter
	.stat <- median
	
	# make viewports
	if (text.box) {
		layout <- grid.layout(ncol = 2, widths = unit(c(.6, 1), "null"))
		frameVP <- plotViewport(c(1,1,1,1), layout = layout, name = "canvas.frame")
		text <- textBoxVP(layout.pos.col = 1)
		graphs <- graphsBoxVP(data = .data, layout.pos.col = 2) 
		vps <- vpTree(frameVP, vpList(text, graphs))
	} else {
		frameVP <- plotViewport(c(1,1,1,1), name = "canvas.frame")
		vps <-  vpStack(frameVP, graphsBoxVP(data = .data))
	}
	
	pushVP <- function() {
		pushViewport(vps)
		seekViewport("canvas.frame")
	}
	
    list(data = .data, pushVP = pushVP)
}

mapCanvas <- function(grob){
	mapVP <- function(name){
		seekViewport(name)
		grid.rect(gp = gpar(col = "grey70"))
		grid.text(name, gp = gpar(col = "grey70"))
	}
	
	names <- c("text", "text.data", "text.data.top.frame", "text.data.top.plot", "text.data.mid.frame", "text.data.mid.plot", "text.data.bottom.frame", "text.data.bottom.plot", "text.sample", "text.sample.top.frame", "text.sample.top.plot", "text.sample.mid.frame", "text.sample.mid.plot", "text.sample.bottom.frame", "text.sample.bottom.plot", "graphs", "graphs.data.frame", "graphs.data.plot", "graphs.data.data", "graphs.sample.frame", "graphs.sample.plot", "graphs.sample.data", "graphs.stat.frame", "graphs.stat.plot", "graphs.stat.data")
	
	for (i in names) mapVP(i)
}
	
	
	
	