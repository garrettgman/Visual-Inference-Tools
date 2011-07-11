# Master function for making the vit canvas. The vit canvas is an S4 object.

setOldClass("gTree")
setOldClass("vpTree")

setClass("canvas", representation(gTree= "gTree", viewports = "vpTree", 
	data = "data.frame", samples = "list", which.sample = "numeric", n = "numeric"), 
	prototype(gTree = gTree(), viewports = vpTree(viewport(), vpList()), 
	data = data.frame(), samples = list(), which.sample = NA_real_, n = NA_real_))
	
	
makeViewports <- function(data){
	layout <- grid.layout(ncol = 2, widths = unit(c(.6, 1), "null"))
	frameVP <- plotViewport(c(1,1,1,1), layout = layout, name = "canvas.frame")
	text <- textBoxVP(layout.pos.col = 1)
	graphs <- graphsBoxVP(data = data, layout.pos.col = 2) 
	vpTree(frameVP, vpList(text, graphs))
}


makeVITgTree <- function(data, vpTree){
	dataAxis <- xaxisGrob(name = "dataAxis", vp = graphsPath("data"))
	sampleAxis <- xaxisGrob(name = "sampleAxis", vp = graphsPath("sample"))
	statAxis <- xaxisGrob(name = "statAxis", vp = graphsPath("stat"))
	
	# move stacked to a stacked points grob that can calculate a conversion for points height in the viewport it pushes.
	stacks <- data.frame(x = data[, 1], y = stackPoints(data[, 1]))
	dataPoints <- pointsGrob(x = stacks$x, y = stacks$y, name = "dataPoints", 
		vp = graphsPath("data"))
	dataBox <- boxplotGrob(data[, 1], name = "dataBox", vp = graphsPath("data"))
	
	dataTextBox <- roundrectGrob(gp=gpar(fill = "#0000FF0D"), name = "dataTextBox", 
		vp = vpPath("canvas.frame", "text", "text.data"))
	sampleTextBox <- roundrectGrob(gp=gpar(fill = "#0000FF0D"), name = "sampleTextBox",
		vp = vpPath("canvas.frame", "text", "text.sample"))
	
	# ADD DATA TEXT HERE
	
	gTree(name = "canvas", childrenvp = vpTree, children = gList(dataAxis, sampleAxis, 
		statAxis, dataPoints, dataBox, dataTextBox, sampleTextBox))
}
	

makeCanvas <- function(data){
	data <- as.data.frame(data)
	n <- nrow(data)
	samples <- split(sample(1:n, n * 1000, replace = TRUE), rep(1:1000, each = n)),
	vpt <- makeViewports()
	gt <- makeVITgTree(data, )
	
	new("canvas", gTree = gt, viewports = vpt, data = data, samples = samples, 
		which.sample = 1, n = n)
}
	




	
	# make viewports
	if (text.box) {
		layout <- grid.layout(ncol = 2, widths = unit(c(.6, 1), "null"))
		frameVP <- plotViewport(c(1,1,1,1), layout = layout, name = "canvas.frame")
		text <- textBoxVP(layout.pos.col = 1)
		graphs <- graphsBoxVP(data = data, layout.pos.col = 2) 
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
	
	
	
	