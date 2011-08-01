# 34567891# 34567892# 34567893# 34567894# 34567895# 34567896# 34567897# 34567898

# R5 implementation for canvas object

# Basic description of Vit package.
# The vit package has three parts: the gui interface, a canvas object that manages what is displayed in the gui's graphic space, and a collection of functions that handle the interactions between the gui and the canvas. The canvas object stores information necessary to making the vit plots and has a collection of methods that help move this information onto the canvas in a sensible way. These methods are organized in a two-tiered heirarchy. The first tier handles the basic actions of the canvas, such as drawing a standard background which includes axiis and an initial plot of the data, drawing the current sample, drawing the current distribution of the sample statistic and then displaying the final results of an analysis. The methods are called by handler functions associated with the "analyze" data page of the gui notebook. The second tier of methods provide the details the first tier of methods need to  complete their job. These details will change between different instances of the vit tool. For example, plotting numeric data will involve points and boxplots, but plotting categorical data will involve dual shaded bars. The second tier methods interact with handler functions associated with the "load data" page of the gui notebook. A typical application of vit, will see the user first load the data to be analyzed. This attaches the appropriate second tier (detail) methods of the canvas to the first tier (action) methods. Next the user will analyze the data which will implement the first tier methods. This arrangement simplifies the vit package and allows new methods to be quickly written and implemented.





#' The canvasClass reference class provides a class for making objects that 
#' manage the visual display of the VIT tool. I used a reference class here 
#' because we can keep all of the relevant information in one place (the canvas 
#' object) and use methods to manipulate it. This is different than the normal R 
#' approach, which is "functional." In the functional approach we must put all 
#' our information into each function and then collect it again on the other 
#' side of the function. That would be burdensome here because there is so much 
#' information to keep track of. The reference class approach is an attempt at 
#' object oriented programming.
canvas <- setRefClass("canvasClass", fields = c("x", "y", "samples", "which.sample", "stat", "stat.dist", "viewports", "image", "which.ghost"), 
	methods = list(initialize = function(x = NULL, y = NULL, ...){
		require(grid)
		x <<- x
		y <<- y
		n <- length(x)
		samples <<- split(sample(1:n, n * 1000, replace = TRUE), 
			rep(1:1000, each = n))
		which.sample <<- 0
		stat.dist <<- vector(length = 1000)
		which.ghost <<- 1
		invisible(.self)
	},
	
	# Primary Methods (details vary based on x, y, and stat)
	plotData = function(x, vp, name) {
		'Plots a vector or dataframe of data points.'
		PLOT_DATA(.self, x, vp, name)
	},
	plotSample = function(vp, name) {
		'Retreives and plots the next sample.'
		PLOT_SAMPLE(.self, newSample(), vp, name)
	},
	calcStat = function() {
		'Calculates the sample statistic for a group of data.'
		CALC_STAT(getSample())
	},	
	calcStatDist = function() {
		'Calculates the distribution of the sample statistic for the 1000 pre-generated samples'
		CALC_STAT_DIST(.self)
	},
	plotStat = function(vp) {
		'Plots the sample statistic with the sample.'
		PLOT_STAT(.self, vp)
	},
	plotStatDist = function() {
		'Plots the distribution of the sample statistic.'
		PLOT_STAT_DIST(.self)
	},
	displayResult = function() {
		'Displays the final result of the VIT simulation.'
		DISPLAY_RESULT(.self)
	},

	# Methods for dealing with sample distribution
	getSample = function() {
		'Returns current sample of data.'
		x[samples[[which.sample]]]
	},
	newSample = function() {
		'Takes new sample of data and returns it invisibly.'
		if (which.sample >= 1000) which.sample <<- 0
		which.sample <<- which.sample + 1
		invisible(x[samples[[which.sample]]])
	},
	
	# Methods for dealing with distribution of sample statistic
	getStatDist = function() {
		'Returns current distribution of the sampling statistic.'
		stat.dist[1:which.sample]
	},
	
	# Methods for plotting 
	drawImage = function() {
		'Draws current image in device.'
		grid.newpage()
		grid.draw(image)
	},
	plotBoxplot = function(x, vp, name, ...) {
		'Plots boxplot of x in specified viewport'
		image <<- addGrob(image, boxplotGrob(x, vp = vp, name = name, ...))
		drawCanvas()
	},
	writeList = function(x, vp, name, ...) {
		'Writes text list of x in the specified viewport'
		image <<- addGrob(image, textlistGrob(x, vp = vp, name = name, ...))		
		drawCanvas()
	},
	writeText = function(x, vp, name, ...) {
		'Writes text of x in the specified viewport'
		image <<- addGrob(image, textGrob(x, vp = vp, name = name, ...)) 		
		# drawCanvas()
	}
))

