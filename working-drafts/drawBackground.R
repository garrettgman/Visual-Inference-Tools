drawBackground <- function(data, vpTree){
	dataAxis <- xaxisGrob(name = "dataAxis", vp = graphsPath("data"))
	sampleAxis <- xaxisGrob(name = "sampleAxis", vp = graphsPath("sample"))
	statAxis <- xaxisGrob(name = "statAxis", vp = graphsPath("stat"))
	
	dataTextBox <- roundrectGrob(gp=gpar(fill = "#0000FF0D"), name = "dataTextBox", 
		vp = vpPath("canvas.frame", "text", "text.data"))
	sampleTextBox <- roundrectGrob(gp=gpar(fill = "#0000FF0D"), name = "sampleTextBox",
		vp = vpPath("canvas.frame", "text", "text.sample"))
	
	gTree(name = "canvas", childrenvp = vpTree, children = gList(dataAxis, sampleAxis, 
		statAxis, dataTextBox, sampleTextBox))
}
