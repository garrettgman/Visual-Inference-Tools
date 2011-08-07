# loads an initial image for a canvas object. The initial image is just the background, with nothing added.

loadImage <- function(canvas){
	dataAxis <- xaxisGrob(name = "dataAxis", vp = graphPath("data"))
	sampleAxis <- xaxisGrob(name = "sampleAxis", vp = graphPath("sample"))
	statAxis <- xaxisGrob(name = "statAxis", vp = graphPath("stat"))
	
#	dataTextBox <- roundrectGrob(gp=gpar(fill = "#0000FF0D"), 
#		name = "dataTextBox", 
#		vp = vpPath("canvas.frame", "text", "text.data"))
#	sampleTextBox <- roundrectGrob(gp=gpar(fill = "#0000FF0D"), 
#		name = "sampleTextBox",
#		vp = vpPath("canvas.frame", "text", "text.sample"))
	
#	canvas$image <- gTree(name = "image", childrenvp = canvas$viewports, 
#		children = gList(dataAxis, sampleAxis, statAxis, dataTextBox, 
#		sampleTextBox))

	canvas$image <- gTree(name = "image", childrenvp = canvas$viewports, 
		children = gList(dataAxis, sampleAxis, statAxis))
}