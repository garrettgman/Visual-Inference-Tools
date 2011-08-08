
buildImage = function() {
	'builds an initial image for a canvas object. The initial image is just the background, with nothing added.'
	dataAxis <- xaxisGrob(name = "dataAxis", vp = graphPath("data"))
	sampleAxis <- xaxisGrob(name = "sampleAxis", vp = graphPath("sample"))
	statAxis <- xaxisGrob(name = "statAxis", vp = graphPath("stat"))

	image <<- gTree(name = "image", childrenvp = viewports, 
		children = gList(dataAxis, sampleAxis, statAxis))
}