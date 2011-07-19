new.vit.env <- function() {
	e <- new.env() 
	e$sliderCreated <- FALSE
	e$sliderCreated2 <- FALSE

	e$fileReader <- function(){
		print("Importing file")

		e$specifyFileForImport()
	}

	e$viewData <- function(h, ...){
		if(is.null(tag(e$obj, "dataSet"))){
			gmessage("Please load a new data set (with named columns)", 
				parent = e$win)
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
			gmessage("Please load a new data set (with named columns)", 
				parent = e$win)
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
		tag(e$obj,"rowDataSet") <- data.frame( ROW_NAME = tag(e$obj,
			"rowDataSet")[,1], tag(e$obj, "dataSet"))
		names(tag(e$obj,"rowDataSet")) <- make.names(names(tag(e$obj,
			"rowDataSet")), unique = TRUE)

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
		tag(e$obj,"rowDataSet") <- data.frame(ROW_NAME = tag(e$obj,
			"rowDataSet")[,1], tag(e$obj, "dataSet"))
	    names(tag(e$obj,"rowDataSet")) <- make.names(names(tag(e$obj,
	    	"rowDataSet")), unique = TRUE)

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
	
	e$specifyFileForImport <- function(...) {
		e1 <- new.env()
		importFileWin <- gwindow("File Browser", cont = TRUE, parent = e$win)
		fileMainGp <- ggroup(cont = importFileWin, horizontal = FALSE)

		filetbl <- glayout(cont = fileMainGp)

		l <- list()
		l[[gettext("csv files")]] = c("csv")
		l[[gettext("2007 Excel files")]] = c("xlsx")
		l[[gettext("97-2003 Excel files")]] = c("xls")

		fileExtensions <- l
		pop <- function(x) x[-length(x)]
		popchar <- function(str) paste(pop(unlist(strsplit(str,""))),
			collapse="")

		filterList <- lapply(fileExtensions, function(i) list(patterns = 
			paste("*.",i,sep="")))
		#filterList$"All files" = list(patterns=c("*"))

		ll = list()
		ll$"All files " <- list(patterns=c("*"))
		filterList <- c(ll,filterList)


		filetbl[2,2] <- glabel("Local file")
		filetbl[2,3] <- (filebrowse = gfilebrowse(text="Specify a file",
			action=invisible, container=filetbl, filter=filterList, 
			quote=FALSE))
		filetbl[3,2:3] <- gseparator(cont=filetbl)
		filetbl[4,2] = gettext("File type is")
		filetbl[4,3] <- (filetype = gdroplist(c(
			"<use file extension to determine>", sapply(names(
			filterList[!filterList %in% ll]),popchar)), cont=filetbl))
		visible(filetbl) <- TRUE

		buttonGp <- ggroup(cont = fileMainGp)
		addSpring(buttonGp)
		okButton <- gbutton("OK", 
			handler = function(h,...) e1$okButtonHandler())
		cancelButton <- gbutton("Cancel", 
			handler = function(h,...) e1$cancelButtonHandler())
		add(buttonGp, okButton)
		add(buttonGp, cancelButton)

		add(fileMainGp, glabel(
			"Space for extra options: define NA string, header presence etc."))

		e1$cancelButtonHandler <- function(h,...) dispose(importFileWin)
	
		e1$okButtonHandler <- function(h,...) {
			theFile <- svalue(filebrowse)
			ext <- NULL ## the extension, figure out

			if(theFile == "Specify a file" || !file.exists(theFile)) {
				# missing code? - Garrett
			}else{
				fileType <- svalue(filetype)
				if(fileType != "<use file extension to determine>") {
    	  			## use filterList to get
					fileType <- paste(fileType,"s", sep="", collapse="") 
					## append s back
					ext <- fileExtensions[[fileType]][1]
				sprintf("Set extension to %s \n",ext)
				} else if(is.null(ext)) {
					tmp <- unlist(strsplit(basename(theFile), split="\\."))
					ext <- tmp[length(tmp)]
				}
			e1$importFile(theFile, ext)
			}
		}
	
		e1$importFile <- function(theFile, ext){
			tmp <- unlist(strsplit(basename(theFile), split="\\."))
			ext.tmp <- tmp[length(tmp)]
		
			if(length(ext) == 0) {
				gmessage(title = "Error", message = "Check file type", 
					icon = "error", cont = TRUE, parent = importFileWin)
			} else if(ext.tmp != ext) {
				gmessage(title = "Error", message = 
					"Chosen file is different than the selected file type", 
			   		icon = "error", cont = TRUE, parent = importFileWin)
			} else if(ext == "csv") {
				out <- try(read.csv(theFile, header = TRUE, 
					na.strings = c("NULL","NA","N/A","#N/A","","<NA>"), 
					check.names = TRUE))
				if(inherits(out,"try-error")){
					sprintf("Error loading file: %s\n",out)
					enabled(okButton) = TRUE
					return(TRUE)
				}else{
					enabled(okButton) <- FALSE
					out <- data.frame(ROW_NAME = 1:nrow(out), out, 
						check.names = TRUE)
					tag(e$obj,"dataSet") <- out[,-1]
					tag(e$obj,"rowDataSet") <- out
					tag(e$obj, "originalDataSet") <- tag(e$obj,"dataSet")
					e$inDataView <- TRUE
					enabled(e$dataView) <- FALSE
					enabled(e$listView) <- TRUE
					e$updateData()
					enabled(okButton) <- TRUE
					e$removeAll_ForNewData()
					e$clearAllSlots()
					dispose(importFileWin)
				}
	      }else if(ext == "xls" || ext == "xlsx"){
				channel <- try(odbcConnectExcel2007(theFile, readOnly = TRUE, 
					readOnlyOptimize=TRUE))
				if(inherits(channel,"try-error")) {
					sprintf("Error loading file: %s\n",channel)
					enabled(okButton) <- TRUE
					e$odbcCloseAll()
					return(TRUE)
				}else{
					enabled(okButton) <- FALSE
					#no na.omit()
					out <- try(sqlFetch(channel, sqtable = "Sheet1", 
					na.strings = c("NULL","NA","N/A","#N/A","","<NA>"), 
						as.is = TRUE))           
					if(inherits(out,"try-error")){
						gmessage("Please ensure that the Excel worksheet containing the data is named as Sheet1\n\nIf the error persists, please save the dataset as a CSV (comma separated) file", parent = importFileWin)
						enabled(okButton) <- TRUE
					}else{
						out <- data.frame(ROW_NAME = 1:nrow(out), out)
						names(out) <- make.names(names(out), unique=TRUE)  
	
						for(i in 1:length(names(out))){
							x <- as.numeric(out[,i])
							if(all(is.na(x))) 
								out[,i] <- factor(as.character(out[,i]))
							else out[,i] <- x
						}
						tag(e$obj,"dataSet") <-  out[,-1]
						tag(e$obj,"rowDataSet") <- out
						tag(e$obj, "originalDataSet") <- tag(e$obj,"dataSet")
						e$inDataView <- TRUE
						enabled(e$dataView) <- FALSE
						enabled(e$listView) <- TRUE
						e$updateData()
						enabled(okButton) <- TRUE
						e$removeAll_ForNewData()
						e$clearAllSlots()
						dispose(importFileWin)
						e$odbcCloseAll()
					}
				}
			}
		}
	}
	
	e$clearAllSlots = function(){
		if(!(svalue(e$xVar) == "Drop name here" && 
			svalue(e$yVar) == "Drop name here"))
				add(nb, ggraphics(expand = TRUE),label = "plot")

		svalue(e$xVar) <- "Drop name here"
		e$e$xData <- NULL
		tag(e$obj,"e$xVarData") <- NULL
		svalue(e$yVar) <- "Drop name here"
		e$e$yData <- NULL

		e$e$groupingVarData <- NULL
		tag(e$obj,"e$groupingVarData") <- NULL
		tag(e$obj,  "sliderData") <- NULL
		if(e$sliderCreated){
			delete(e$firstSliderGp, e$grpSlider)
			delete(e$firstSliderGp, e$lbl)
			e$sliderCreated <- FALSE
		}
		if(e$sliderCreated2){
			delete(e$secondSliderGp, e$grpSlider2)
			delete(e$secondSliderGp, e$lbl2)
			e$sliderCreated2 <- FALSE
		}
		e$fourthVariable <- NULL
		e$fourthVarName <- ""
		e$e$groupingVarData2 <- NULL
		tag(e$obj,"e$groupingVarData2") <- NULL
		tag(e$obj,  "sliderData2") <- NULL
	}
	
	e$odbcCloseAll <- function(){
		require(RODBC)
		odbcCloseAll()
	}
	
	e$removeAll_ForNewData <- function(){ 
		e$addSmoother <- FALSE 
	 	e$addYequalsX <- FALSE
	 	e$addLinearTrend <- FALSE
	 	e$addQuadTrend <- FALSE
	 	e$addCubicTrend <- FALSE
		e$fifthVariable <- NULL
		e$fifthVariableSubset <- NULL
		e$fifthVarName <- ""
		e$sixthVariable <- NULL
		e$sixthVariableSubset <- NULL
		e$sixthVarName <- ""
		e$segmentVariable <- NULL
		e$segmentVariableSubset <- NULL
		e$segmentVarName <- ""
		e$addRugY <- FALSE
		e$addRugX <-FALSE
		e$addRugYhat <- FALSE
		e$addJitterX <- FALSE
		e$addJitterY <- FALSE
		e$drawBoxPlots <- TRUE
		e$drawBothInf <- FALSE
		e$drawIntervals <- FALSE
		e$useBootstrap <- FALSE
		e$basicScatterColor <- "black"
		e$basicDotColor <- "grey50"
		e$basicPlottingCharScatter <- 1
		e$basicPlottingCharDot <- 1
		e$scatterThickness1 <- 1
		e$scatterThickness2 <- 2
		e$basicCexScatter <- 1
		e$basicCexDot <- 1
		e$basicBackgroundDot <- "white"
		e$basicBackgroundScatter <- "white"
		e$drawBootLines <- FALSE
		e$joinByLines = FALSE
		#updatePlot()
	}
	
	e
}

clear1 <- function() {
	svalue(e$xVar) <- "Drop name here"
	e$e$xVarData <- NULL
	tag(e$obj,"e$xVarData") <- NULL

    dispose(nb)
    add(nb, ggraphics(expand = TRUE),label = "plot")
    updatePlot()
}



clear2 <- function() {
	svalue(e$yVar) <- "Drop name here"
	e$e$yVarData <- NULL
	tag(e$obj,"e$yVarData") <- NULL
    updatePlot()
}

clear3 <- function() {
	svalue(e$groupingVar) <- "Drop name here"
	e$e$groupingVarData <- NULL
	tag(e$obj,"e$groupingVarData") <- NULL
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
	svalue(e$groupingVar2) <- "Drop name here"
	e$e$groupingVarData2 <- NULL
	tag(e$obj,"e$groupingVarData2") <- NULL
	tag(e$obj,  "sliderData2") <- NULL

	if(e$sliderCreated){
		oldLevelsNum = length(e$gpVarLbls)
		if(is.factor(svalue(tag(e$obj,"e$groupingVarData")))){
			e$gpVarLbls <- levels(svalue(tag(e$obj,"e$groupingVarData")))
		}else{
			e$gpVarLbls <- levels(convert.to.factor(svalue(tag(e$obj,
				"e$groupingVarData"))))
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