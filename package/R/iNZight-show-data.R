# Users can use vit as a stand alone package, but we'd like to incorporate it into the iNZight package. Hence, we are using the basic iNZight GUI framework and data reading methods to load vit. These methods are contained to this file, so vit can be easily updated as the iNZight methods change (just update this file). 

e$viewData <- function(h, ...){
	if(is.null(tag(e$obj, "dataSet"))){
		gmessage("Please load a new data set (with named columns)", parent = e$win)
	}else{
		if((names(tag(e$obj, "dataSet"))[1] == "empty"))
			gmessage("Please load a new data set", parent = e$win)
		else{
			enabled(h$obj) <- FALSE
			e$updateData()
			enabled(e$listView) <- TRUE
			e$inDataView <- TRUE
		}
	}
}

e$viewList <- function(h, ...){
	if(is.null(tag(e$obj, "dataSet"))){
		gmessage("Please load a new data set (with named columns)", parent = e$win)
	}else{
		if(names(tag(e$obj, "dataSet"))[1] == "empty")
			gmessage("Please load a new data set", parent = e$win)
		else{
			enabled(h$obj) <- FALSE
			e$updateList()
			enabled(e$dataView) <- TRUE
			e$inDataView <- FALSE
		}
	}
}

e$updateData <- function() {
	names(tag(e$obj,"dataSet")) <- make.names(names(tag(e$obj,"dataSet")), 
		unique = TRUE)
	tag(e$obj,"rowDataSet") <- data.frame( ROW_NAME = tag(e$obj,"rowDataSet")[,1], 
		tag(e$obj, "dataSet"))
	names(tag(e$obj,"rowDataSet")) <- make.names(names(tag(e$obj,"rowDataSet")), 
		unique = TRUE)

	if(!is.null(e$dataList))
		delete(e$dataGp, e$dataList, expand = TRUE)
	if(!is.null(e$dataList1))
		delete(e$dataGp, e$dataList1, expand = TRUE)
	if(!is.null(e$dataList2))
		delete(e$dataGp, e$dataList2, expand = TRUE)
	if(!is.null(e$dataSt))
		delete(e$dataGp, e$dataSt, expand = TRUE)

	e$dataSt <- gdf(tag(e$obj,"dataSet"),expand = TRUE)
	add(e$dataGp, e$dataSt, expand = TRUE)
	addHandlerChanged(e$dataSt, 
		handler = function(h,...) tag(e$obj,"dataSet") = e$dataSt[])
    e$inDataView = TRUE
}
  
  
e$updateList <- function() {
	names(tag(e$obj,"dataSet")) <- make.names(names(tag(e$obj,"dataSet")), 
		unique = TRUE)
	tag(e$obj,"rowDataSet") <- data.frame(ROW_NAME = tag(e$obj,"rowDataSet")[,1], 
		tag(e$obj, "dataSet"))
    names(tag(e$obj,"rowDataSet")) <- make.names(names(tag(e$obj,"rowDataSet")), 
    	unique = TRUE)

    if(!is.null(e$dataList))
      delete(e$dataGp, e$dataList, expand = TRUE)
    if(!is.null(e$dataList1))
      delete(e$dataGp, e$dataList1, expand = TRUE)
    if(!is.null(e$dataList2))
      delete(e$dataGp, e$dataList2, expand = TRUE)
    if(!is.null(e$dataSt))
      delete(e$dataGp, e$dataSt, expand = TRUE)

	N = 19
	if(e$sliderCreated && e$sliderCreated2) N = 14
	if((length(names(tag(e$obj,"dataSet"))) > N) && 
		(length(names(tag(e$obj,"dataSet"))) < 80)){
			x.index <- (N+1):(length(names(tag(e$obj,"dataSet"))))
			x <- length(names(tag(e$obj,"dataSet"))[x.index])
			d1 <- (names(tag(e$obj,"dataSet"))[1:N])
			d2.index <- (N+1):(length(names(tag(e$obj,"dataSet"))))
			d2 <- c(names(tag(e$obj,"dataSet"))[d2.index])
			e$dataList1 <- gtable(d1,expand = TRUE)
			names(e$dataList1) <- "VARIABLES"
			e$dataList2 <- gtable(d2,expand = TRUE)
			names(e$dataList2) <- "...CONTINUED"
			adddropsource(e$dataList1)
			adddropsource(e$dataList2)
			add(e$dataGp, e$dataList1, expand = TRUE)
			add(e$dataGp, e$dataList2, expand = TRUE)
	}else{
		d <- names(tag(e$obj,"dataSet"))
		e$dataList <- gtable(d,expand = TRUE)
		names(e$dataList) <- "VARIABLES"
		adddropsource(e$dataList)
		add(e$dataGp, e$dataList, expand = TRUE)
	}
	e$inDataView = FALSE
}

clear1 <- function() {
	svalue(xVar) <- "Drop name here"
	e$xVarData <- NULL
	tag(e$obj,"xVarData") <- NULL

    dispose(nb)
    add(nb, ggraphics(expand = TRUE),label = "plot")
    updatePlot()
}



clear2 <- function() {
	svalue(yVar) <- "Drop name here"
	e$yVarData <- NULL
	tag(e$obj,"yVarData") <- NULL
    updatePlot()
}

clear3 <- function() {
	svalue(groupingVar) <- "Drop name here"
	e$groupingVarData <- NULL
	tag(e$obj,"groupingVarData") <- NULL
	tag(e$obj,  "sliderData") <- NULL
	if(e$sliderCreated){
		delete(e$firstSliderGp, e$grpSlider)
		delete(e$firstSliderGp, e$lbl)
		e$sliderCreated <- FALSE
	}
	updatePlot()
}

clear4 <- function() {
	e$fourthVariable <- NULL
	e$fourthVarName = ""
	svalue(groupingVar2) <- "Drop name here"
	e$groupingVarData2 <- NULL
	tag(e$obj,"groupingVarData2") <- NULL
	tag(e$obj,  "sliderData2") <- NULL

	if(e$sliderCreated){
		oldLevelsNum = length(e$gpVarLbls)
		if(is.factor(svalue(tag(e$obj,"groupingVarData")))){
			e$gpVarLbls <- levels(svalue(tag(e$obj,"groupingVarData")))
		}else{
			e$gpVarLbls <- levels(convert.to.factor(svalue(tag(e$obj,
				"groupingVarData"))))
		}
		delete(e$firstSliderGp, e$lbl)
		e$lbl <- glabel(paste(c("*",e$gpVarLbls), collapse = "   "))

		if(oldLevelsNum != length(e$gpVarLbls))
			e$grpSlider[] <- seq(0,length(e$gpVarLbls),1)

		if(sum(nchar(paste(c("*",e$gpVarLbls)))) < 60)
			add(e$firstSliderGp, e$lbl)
	}
	if(e$sliderCreated2){
		delete(e$secondSliderGp, e$grpSlider2)
		delete(e$secondSliderGp, e$lbl2)
		e$sliderCreated2 <- FALSE
	}

	updatePlot()
}