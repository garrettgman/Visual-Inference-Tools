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
    diffFun.radio <- gradio(c("median", "mean"),  horizontal=TRUE) 
	add(controls.vit, diffFun.radio)
	
	addSpace(controls.vit, 10, horizontal = FALSE)
	vit.bootbox <- gframe("bootstrapping",  container = controls.vit)
	redraw.radio <- gradio(c("1 (all)",1, 5, 20),  horizontal=FALSE)
	add(vit.bootbox, redraw.radio)
	
	run1.but  <- gbutton(text = "Run", container = controls.vit)
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
    
	enabled(run1.but)      = FALSE
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
	
	# Setting up the labels and drag and drop locations	
	tbl <- glayout(expand = FALSE)
	tbl[3,1, anchor = c(0,0)] = glabel(" Variable 1 :")
	tbl[5,1, anchor = c(0,0)] = glabel(" Variable 2 :")
	tbl[7,1, anchor = c(0,0)] = glabel(" subset by  :")

	e$yVar <- glabel("Drop name here")
	font(e$yVar) <- list(weight = "bold", family = "normal")
	tag(e$obj,"e$yVarData") <- NULL
	tbl[5,3, anchor = c(0,0)] <- e$yVar

	e$xVar <- glabel("Drop name here")
	font(e$xVar) <- list(weight = "bold", family = "normal")
	tag(e$obj,"e$xVarData") <- NULL
	tbl[3,3, anchor = c(0,0)] <- e$xVar

	e$groupingVar <- glabel("Drop name here")
	font(e$groupingVar) <- list(weight = "bold", family = "normal")
	tag(e$obj,"e$groupingVarData") <- NULL
	tbl[7,3, anchor = c(0,0)] <- e$groupingVar

	tbl[3,7, anchor = c(0,0)] <- gbutton("clear", 
		handler = function(h,...) clear1())
	tbl[5,7, anchor = c(0,0)] <- gbutton("clear", 
		handler = function(h,...) clear2())
	tbl[7,7, anchor = c(0,0)] <- gbutton("clear", 
		handler = function(h,...) clear3())

	add(controls.iNZight, tbl, expand = FALSE)
	visible(tbl) <- TRUE

	# Group for first slider
	e$firstSliderGp <- ggroup(horizontal = FALSE)
	add(controls.iNZight, e$firstSliderGp)

	# Group for label and drag and drop box for the second subsetting
	# variable
	tbl2 <- glayout(expand = FALSE)
	tbl2[2,1] <- glabel(" subset by  :")

	e$groupingVar2 <- glabel("Drop name here")
	font(e$groupingVar2) <- list(weight = "bold", family = "normal")
	tag(e$obj,"e$groupingVarData2") <- NULL
	tbl2[2,3] = e$groupingVar2
	tbl2[2,7, anchor = c(0,0)] <- gbutton("clear", 
		handler=function(h,...) clear4())

	add(controls.iNZight, tbl2, expand = FALSE)
	visible(tbl2) <- TRUE

	e$secondSliderGp <- ggroup(horizontal = FALSE, expand = FALSE)
	add(controls.iNZight, e$secondSliderGp, expand = FALSE)

}
	
    