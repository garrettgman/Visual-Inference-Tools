# 34567891# 34567892# 34567893# 34567894# 34567895# 34567896# 34567897# 34567898

# R5 implementation for canvas object

# Basic description of Vit package.  The vit package has three parts:
# the gui interface, a canvas object that manages what is displayed in
# the gui's graphic space, and a collection of functions that handle
# the interactions between the gui and the canvas. The canvas object
# stores information necessary to making the vit plots and has a
# collection of methods that help move this information onto the
# canvas in a sensible way. These methods are organized in a
# two-tiered heirarchy. The first tier handles the basic actions of
# the canvas, such as drawing a standard background which includes
# axiis and an initial plot of the data, drawing the current sample,
# drawing the current distribution of the sample statistic and then
# displaying the final results of an analysis. The methods are called
# by handler functions associated with the "analyze" data page of the
# gui notebook. The second tier of methods provide the details the
# first tier of methods need to complete their job. These details will
# change between different instances of the vit tool. For example,
# plotting numeric data will involve points and boxplots, but plotting
# categorical data will involve dual shaded bars. The second tier
# methods interact with handler functions associated with the "load
# data" page of the gui notebook. A typical application of vit, will
# see the user first load the data to be analyzed. This attaches the
# appropriate second tier (detail) methods of the canvas to the first
# tier (action) methods. Next the user will analyze the data which
# will implement the first tier methods. This arrangement simplifies
# the vit package and allows new methods to be quickly written and
# implemented.





#' The canvasClass reference class provides a class for making objects that
#' manage the visual display of the VIT tool. I used a reference class here
#' because we can keep all of the relevant information in one place (the canvas
#' object) and use methods to manipulate it. This is different than the normal R
#' approach, which is "functional." In the functional approach we must put all
#' our information into each function and then collect it again on the other
#' side of the function. That would be burdensome here because there is so much
#' information to keep track of. The reference class approach is an attempt at
#' object oriented programming.
canvas <- setRefClass("canvasClass", fields = c("x", "y", "levels", "n",
	"samples", "sampled.stats", "plotted.stats", "indexes", "which.sample", "stat.dist", "viewports", "image"),
	methods = list(
	initialize = function(x = NULL, y = NULL, levels = NULL, ...){
		x <<- x
		y <<- y
		levels <<- levels
		n <<- length(x)
		which.sample <<- 0
		stat.dist <<- NULL
                sampled.stats <<- NULL
                plotted.stats <<- NULL
		invisible(.self)
	},

	# ACTIONS
	animateSample = function(...) {
		'Animates the selection of the sampel from the data.'
		ANIMATE_SAMPLE(.self, ...)
	},
	animateStat = function(env, n.steps) {
		'Animates the creation of the distribution of the statistic from the samples.'
		ANIMATE_STAT(.self, env, n.steps)
	},
	calcStat = function(i = which.sample, y = NULL) {
		'Calculates the sample statistic for a group of data.'
		CALC_STAT(samples[[i]], y)
	},
	displayResult = function(env, ...) {
		'Displays the final result of the VIT simulation.'
		DISPLAY_RESULT(.self, env, ...)
	},
        fadePlots = function(env, ...) {
            'Fades plots to emphasize those that are remaining'
            FADE_PLOTS(.self, env, ...)
        },
	handle1000 = function(env, ...) {
		'Runs simulation that uses all 1000 pre-calculated samples.'
		HANDLE_1000(.self, env, ...)
	},
	plotData = function() {
		'Plots a vector or dataframe of data points.'
		PLOT_DATA(.self)
	},
	plotDataStat = function(env, ...) {
		'Plots the sample statistic with the sample.'
		PLOT_DATA_STAT(.self, env)
            },
	plotSample = function(env, i = which.sample) {
		'Retreives and plots the next sample.'
		PLOT_SAMPLE(.self, env, i)
	},
	plotSampleStat = function(env, i = which.sample, ...) {
		'Plots the sample statistic with the sample.'
		PLOT_SAMPLE_STAT(.self, env, i, ...)
	},
	plotStatDist = function(env, ...) {
		'Plots the distribution of the sample statistic.'
		PLOT_STAT_DIST(.self, env)
	},

        showLabels = function() {
            'Plots labels on the three viewports.'
            SHOW_LABELS(.self)
        },

        trackSample = function(){
            'Tracks a sample or resample.'
            TRACK_SAMPLE(.self)
        },

        graphPath = function(plot.name = "sample", number = "1") {
            'Different graphpath methods required depending on whether or not databoxes are plotted'
            GRAPHPATH(plot.name, number)
        },
	# Helpers
	advanceWhichSample = function() {
		'Advances which.sample.'
		if (which.sample >= 1000) which.sample <<- 0
		which.sample <<- which.sample + 1
	},
        buildBoxes = function(){
            databox1 <- roundrectGrob(name = "databox1",
                                      x = unit(1, "mm"), y = unit(1, "mm"),
                                      width = unit(1, "npc") - unit(2, "mm"),
                                      height = unit(1, "npc") - unit(2, "mm"),
                                      just = c("left", "bottom"),
                                      gp = gpar(col = "gray40", fill = "gray95", lwd = 2),
                                      vp = graphPath("databox", 1))
            databox2 <- roundrectGrob(name = "databox2",
                                      x = unit(1, "mm"), y = unit(1, "mm"),
                                      width = unit(1, "npc") - unit(2, "mm"),
                                      height = unit(1, "npc") - unit(2, "mm"),
                                      just = c("left", "bottom"),
                                      gp = gpar(col = "gray40", fill = "gray95", lwd = 2),
                                      vp = graphPath("databox", 2))
            image <<- addGrob(image, databox1)
            image <<- addGrob(image, databox2)
            if (is.null(levels)){
                text.sample <- datatextGrob(data = x, title = "Sample", max = 50,
                                            name = "text.sample",
                                            vp = graphPath("databox", 1))
                image <<- addGrob(image, text.sample)
            } else {
                text.sample1 <- datatextGrob(data = x, title = "Sample", x = 0.25,
                                             max = 50, name = "text.sample1",
                                             vp = graphPath("databox", 1))
                text.sample2 <- coldatatextGrob(data = as.character(levels), title = "Levels",
                                             cols = c("tan4", "darkseagreen"), x = 0.75,
                                             max = 50, name = "text.sample2",
                                             vp = graphPath("databox", 1))
                image <<- addGrob(image, text.sample1)
                image <<- addGrob(image, text.sample2)
            }
        },
        buildImage = function(data.boxes = FALSE) {
            'builds an initial image for a canvas object. The initial image is just the background, with nothing added.'
            dataAxis <- xaxisGrob(name = "dataAxis", vp = graphPath("data"))
            sampleAxis <- xaxisGrob(name = "sampleAxis", vp = graphPath("sample"))
            statAxis <- xaxisGrob(name = "statAxis", vp = graphPath("stat"))
            image <<- gTree(name = "image", childrenvp = viewports,
                            children = gList(dataAxis, sampleAxis, statAxis))
        },
        drawImage = function() {
            'Draws current image in device.'
            grid.newpage()
            grid.draw(image)
	},
        pauseImage = function(pause = 1) {
            'Same as drawImage but pauses animation'
            for (i in rep(1, pause)){
                grid.newpage()
                grid.draw(image)
            }
        },
	getSample = function(i = which.sample) {
		'Returns ith sample of data. Defaults to current sample.'
		samples[[i]]
	},
	getStat = function(i = which.sample) {
		'Returns the statistic for the ith distribution'
		if (is.null(stat.dist)) makeStatDistribution()
		stat.dist[[i]]
	},
	getStatDist = function() {
		'Returns current distribution of the sampling statistic.'
		stat.dist[1:which.sample]
	},
	makeSamples = function(replace){
		'Generates sample groups for the vit bootstrap runs.'
		if (replace)
			index.vec <- sample(1:length(x), n * 1000, replace = TRUE)
		else {
			index.vec <- list()
			for (i in 1:1000) {
				index.vec[[i]] <- sample(1:length(x), size = n)
			}
			index.vec <- unlist(index.vec)
		}

		sample.vec <- x[index.vec]
		samples <<- split(sample.vec, rep(1:1000, each = n))
		indexes <<- split(index.vec, rep(1:1000, each = n))
		which.sample <<- 1
	},
	makeStatistics = function(){
		'Calculates the statistic for all 1000 samples. Shows progress in a progress bar on the console.'
		stat.dist <<- plyr::llply(samples, function(x) CALC_STAT(x, .self$levels), .progress = "text")

	}
))
