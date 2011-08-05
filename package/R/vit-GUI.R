# Much of the vit() function and the new.vit.env() function in new-vit-env.R are borrowed from the iNZight package. We'd like to one day add the vit tools into the iNZight tools, so I've aimed to keep the interfaces as similar as possible. However, the iNZight code is not very modular and most of the functions include many routines irrelevant to vit. Hence, most of the iNZight code that appears in these vit files is modified from its original form - Garrett


vit <- function() {
	e <- new.vit.env()
	e$win <- gwindow("Visual Inference Tools", visible = TRUE, width = 870,
    		height = 600)

    # separates space into controls and plot
    g <- gpanedgroup(container = e$win, expand = TRUE)
    e$obj <- g
    g.controls <- gnotebook(container = g, expand = TRUE)
    g.plot <- ggraphics(container = g, expand = TRUE)

    # control panel has a read data tab inspired from iNZight
    # and a VIT tab
    controls.iNZight <- ggroup(horizontal = FALSE, container = g.controls,
    	expand = TRUE, label = "Load Data")
    controls.vit <- ggroup(horizontal = FALSE, container = g.controls,
    	expand = TRUE, label = "Analyze Data")


    # adding vit controls
    addSpace(controls.vit, 10, horizontal = FALSE)
        tbl <- glayout(container = controls.vit)
	tbl[1,1] <- glabel("Statistic:    ",container = tbl)
	tbl[1,2] <- (e$stat <- gcombobox(c(), editable=TRUE, container = tbl,
		handler = function(h, ...) e$notifySamplingChange() ))
	e$stat[] <- c("mean", "median", "confidence interval - mean", 
		"confidence interval - median")
	svalue(e$stat) <- "mean"
	
	tbl[2,1] <- glabel("CI Method:    ", container = tbl)
	tbl[2,2] <- (e$cimeth <- gcombobox(c(), editable = TRUE, 
		container = tbl, handler = function(h, ...) e$notifySamplingChange() ))
	e$cimeth[] <- c("normal", "percentile bootstrap", "normal bootstrap", 
		"t bootstrap")
		
	tbl[3,1] <- glabel("Sample Size:  ", container = tbl)
	tbl[3,2] <- (e$ssize <- gedit("50", container = tbl, 
		handler = function(h, ...) e$notifySamplingChange() ))
		
	tbl[4,2] <- (e$replace <- gcheckbox("Sample with replacement", 
		container = tbl, handler = function(h,...) e$notifySamplingChange()))
		
	svalue(e$replace) <- TRUE
	svalue(e$cimeth) <- "normal"
	enabled(e$cimeth) <- FALSE
	
	addSpace(controls.vit, 10, horizontal = FALSE)
	vit.bootbox <- gframe("bootstrapping",  container = controls.vit)
	e$redraw.radio <- gradio(c("1 (all)",1, 5, 20),  horizontal=FALSE)
	add(vit.bootbox, e$redraw.radio)

	run1.but  <- gbutton(text = "Run", container = controls.vit,
		handler = function(h, ...) e$runSamplingOnly() )
	point.but <- gbutton(text = "Track", container = controls.vit)
	pause.but <- gbutton(text = "Pause", container = controls.vit)
	addSpace(controls.vit, 40, horizontal=FALSE)

	vit.diffbox <- gframe("Capture Bootstrapping Difference",
		container = controls.vit)
	bootstrap.radio <- gradio(c("1 (all)", 1, 5, 20, 1000),
		horizontal = FALSE)
	add(vit.diffbox, bootstrap.radio)

	run2.but <- gbutton(text = "Run", container = controls.vit)
	show.ci.but <- gbutton(text = "Show Confidence Interval",
		container = controls.vit)
	addSpace(controls.vit, 10, horizontal = FALSE)

	status <- glabel("", container = controls.vit)

	#enabled(run1.but)      = FALSE
	enabled(pause.but)     = FALSE
	enabled(run2.but)      = FALSE
	enabled(show.ci.but)   = FALSE
	enabled(point.but)     = FALSE


	# adding iNZight controls
	# top three buttons
	iNZ.view <- ggroup()
	e$importData <- gbutton("Import Data",
		handler = function(h,...) e$fileReader())
	e$dataView <- gbutton("View Data Set",
		handler = function(h,...) e$viewData(h,...))
	e$listView <- gbutton("View Variables",
		handler = function(h,...) e$viewList(h,...))
	font(e$importData) <- list(weight = "bold", family = "normal",
		color = "navy")
	font(e$dataView) <- list(weight = "bold", family = "normal",
		color = "navy")
	font(e$listView) <- list(weight = "bold", family = "normal",
		color = "navy")
	add(iNZ.view, e$importData)
	add(iNZ.view, e$dataView)
	add(iNZ.view, e$listView)
	add(controls.iNZight, iNZ.view)

	# Table of data frame or variables
	e$dataGp <-  ggroup(horizontal = TRUE, expand = TRUE)
	add(controls.iNZight, e$dataGp, expand = TRUE)

	# At start-up an (near)empty dataframe is embedded
	tag(e$obj,"dataSet") = data.frame(empty = " ")
	tag(e$obj,"originalDataSet") = data.frame(empty = " ")
	e$dataSt <- gdf(tag(e$obj,"dataSet"), width = 20, height = 50,
		expand = TRUE)
	add(e$dataGp, e$dataSt, expand = TRUE)
	e$dataList1 <- NULL
	e$dataList2 <- NULL


	# Variable 1 and 2 display
	# column 1
	tbl <- glayout(expand = FALSE)
	tbl[3,1, anchor = c(0,0)] = glabel(" Variable 1 :")
	tbl[5,1, anchor = c(0,0)] = glabel(" Variable 2 :")

	# column 2
	e$xVar <- glabel("Drop name here")
	font(e$xVar) <- list(weight = "bold", family = "normal")
	tag(e$obj,"e$xVarData") <- NULL
	tbl[3,3, anchor = c(0,0)] <- e$xVar

	e$yVar <- glabel("Drop name here")
	font(e$yVar) <- list(weight = "bold", family = "normal")
	tag(e$obj,"e$yVarData") <- NULL
	tbl[5,3, anchor = c(0,0)] <- e$yVar

	# column 3
	tbl[3,7, anchor = c(0,0)] <- gbutton("clear", handler = function(h,...) {
		e$xData <- NULL
		svalue(e$xVar) <- "Drop name here"
		e$buildCanvas()
		e$c1$drawImage()
	})
	tbl[5,7, anchor = c(0,0)] <- gbutton("clear", handler = function(h,...) {
		e$yData <- NULL
		svalue(e$yVar) <- "Drop name here"
		e$buildCanvas()
		e$c1$drawImage()
	})

	tbl[3,8] <- ""

	add(controls.iNZight, tbl, expand = FALSE)
	addSpace(controls.iNZight, 10, horizontal = TRUE)
	visible(tbl) <- TRUE

	# adding drop zones
	adddroptarget(e$xVar, targetType = "object", handler = function(h, ...) {
		svalue(e$xVar) <- id(h$dropdata)
		if (e$inDataView) e$xData <- svalue(h$dropdata)
		else e$xData <- tag(e$obj, "dataSet")[,id(h$dropdata)]
		svalue(e$ssize) <- length(e$xData)
		e$buildCanvas()
		e$c1$drawImage()
	})

	adddroptarget(e$yVar, targetType = "object", handler = function(h, ...) {
		svalue(e$yVar) <- id(h$dropdata)
		if (e$inDataView) e$yData <- svalue(h$dropdata)
		else e$yData <- tag(e$obj, "dataSet")[,id(h$dropdata)]
		svalue(e$ssize) <- length(e$xData)
		e$buildCanvas()
		e$c1$drawImage()
	})

}
######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
######################################################################

updateCanvas <- function() {
	print("Coming Soon!")
}


