new.vit.env <- function() {
	e <- new.env() 
	
	e$fileReader <- function(){
		print("Importing file")

		e$specifyFileForImport()
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
						e$clearAllSlots()
						dispose(importFileWin)
						e$odbcCloseAll()
					}
				}
			}
		}
	}

	e$clearAllSlots = function(){
		svalue(e$xVar) <- "Drop name here"
		e$e$xData <- NULL
		tag(e$obj,"e$xVarData") <- NULL
		svalue(e$yVar) <- "Drop name here"
		e$e$yData <- NULL
	}
	
	e$odbcCloseAll <- function(){
		require(RODBC)
		odbcCloseAll()
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
	
	e
}