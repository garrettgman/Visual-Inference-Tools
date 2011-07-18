# Users can use vit as a stand alone package, but we'd like to incorporate it into the iNZight package. Hence, we are using the basic iNZight GUI framework and data reading methods to load vit. These methods are contained to this file, so vit can be easily updated as the iNZight methods change (just update this file). 


# helper functions pulled from iNZight.R
e <- new.env()
e$inDataView <- FALSE
e$sliderCreated <- FALSE
e$sliderCreated2 <- FALSE

# modified from the iNZight() function in iNZight
iNZightGUI <- function(container = NULL) {

	dynamicWarning = "Editing values doesn't work with dynamic data"
	
	# Setting up the layout of the GUI
	if(is.null(container)){
    	e$win <- gwindow("Visual Inference Tools", visible = TRUE, width = 870, 
    		height = 600)
	}

	g <- gpanedgroup(cont = e$win, expand = TRUE)
	e$obj <- g
	e$gp1 <- ggroup(horizontal = FALSE, container = g, expand = TRUE) # Left side group
	gp2 <- ggroup(horizontal = FALSE, container = g, expand = TRUE)   # Right side group
	tag(e$obj,"dropHandlers") <- list()


	# Setting up the menu bar in the top left hand corner
	defHandler = function(h,...) gmessage("Not Implemented Yet", parent = e$win)
	
	# unneeded iNZight functionality:
	# mbl1 = list("Data IN/OUT" = list("Import Data" = list(handler = function(h,...) e$fileReader(), icon = "symbol_diamond"), "Export Data" = list(handler = function(h,...) e$fileWriter(), icon = "symbol_square")), "Filter Data" = list("Select Cases" = list(handler = function(h,...){if(!e$filterDataWinOpened) e$filterData()}, icon = "symbol_diamond"), "Restore Dataset" = list(handler = function(h,...) e$getOriginalData(), icon = "symbol_square")), "Manipulate Variables" = list("Convert to Categorical" = list(handler = function(h,...){if(!e$convertWinOpened)e$convertToFactor()}, icon = "symbol_diamond"), "Transform Variables" = list(handler = function(h,...){if(!e$transformWinOpened) e$transformHandler()}, icon = "symbol_square"), "Collapse Levels" = list(handler = function(h,...){if(!e$collapseWinOpened) e$collapseHandler()}, icon = "symbol_dntriangle"), "Reorder Levels" = list(handler = function(h,...){if(!e$reorderWinOpened)e$reorderLevels()}, icon ="symbol_dot"), "Rename Levels" = list(handler = function(h,...){if(!e$renameLWinOpened)e$renameLevels()}, icon ="symbol_star"), "Combine Categorical Variables" = list(handler = function(h,...){if(!e$combineWinOpened)e$combineFactors()}, icon = "symbol_circle"),"Create New Variables" = list(handler = function(h,...){if(!e$newVariableWinOpened)e$newVariable()}, icon ="symbol_square"),"Form Class Intervals" = list(handler = function(h,...){if(!e$binNumericWinOpened)e$binNumeric()}, icon ="symbol_diamond"),"Rename Variables" = list(handler = function(h,...){if(!e$renameVWinOpened)e$renameVariables()}, icon ="symbol_cross"),"Standardise Variables" = list(handler = function(h,...){if(!e$standardWinOpened)e$standardVariable()}, icon = "symbol_dntriangle")),"Trash" = list("Trash Variables" = list(handler = function(h,...){if(!e$trashWinOpened) e$trashVar()}, icon = "symbol_diamond")))

	# mbl2 = list("Advanced" = list("3D Scatter Plot" = list(handler = function(h,..)e$plot3D(), icon = "symbol_diamond"), "Model Fitting" = list(handler = function(h,...) e$modelFitting(), icon = "symbol_square"), "Scatter Plot Matrix" = list(handler = function(h,...){if(!e$scatterPlotWinOpened) e$scatterPlotMatrix(h,...)}, icon = "symbol_circle")))

	# mb1 = gmenu(mbl1, cont = e$gp1)
	# mb2 = gmenu(mbl2, cont = e$gp1)
  
	read.data <- gbutton("Import data", handler = function(h,...) e$fileReader())
	
	# Setting up the group for the buttons to switch between data/variable list views
	viewGroup <- ggroup()
	addSpring(viewGroup)
	e$dataView <- gbutton("View Data Set", handler = function(h,...) e$viewData(h,...))
	e$listView <- gbutton("View Variables", handler = function(h,...) e$viewList(h,...))

	font(e$dataView) <- list(weight="bold", family = "normal", color = "navy")
	font(e$listView) <- list(weight="bold", family = "normal", color = "navy")

	add(viewGroup, e$dataView)
	add(viewGroup, e$listView)
	add(e$gp1, viewGroup)

	# Setting up group to either embed the data frame or present a list of variables
	e$dataGp <-  ggroup(horizontal = TRUE, expand = TRUE)
	add(e$gp1, e$dataGp, expand = TRUE)

	# At start-up an (near)empty dataframe is embedded
	tag(e$obj,"dataSet") = data.frame(empty = " ")
	tag(e$obj,"originalDataSet") = data.frame(empty = " ")

	e$dataSt <- gdf(tag(e$obj,"dataSet"), width = 20, height = 50, expand = TRUE)
	add(e$dataGp, e$dataSt, expand = TRUE)

	e$dataList1 <- NULL
	e$dataList2 <- NULL
	
	# Setting up the labels and drag and drop places on the bottom right section
	tbl = glayout(expand = FALSE)
	tbl[3,1, anchor = c(0,0)] = glabel(" Variable 1 :")
	tbl[5,1, anchor = c(0,0)] = glabel(" Variable 2 :")
	tbl[7,1, anchor = c(0,0)] = glabel(" subset by  :")

	yVar <- glabel("Drop name here")
	font(yVar) <- list(weight = "bold", family = "normal")
	tag(e$obj,"yVarData") <- NULL
	tbl[5,3, anchor = c(0,0)] <- yVar


	xVar <- glabel("Drop name here")
	font(xVar) <- list(weight = "bold", family = "normal")
	tag(e$obj,"xVarData") <- NULL
	tbl[3,3, anchor = c(0,0)] <- xVar

	groupingVar <- glabel("Drop name here")
	font(groupingVar) <- list(weight="bold", family = "normal")
	tag(e$obj,"groupingVarData") <- NULL
	tbl[7,3, anchor = c(0,0)] <- groupingVar

	tbl[3,7, anchor = c(0,0)] <- gbutton("clear", handler=function(h,...) clear1())
	tbl[5,7, anchor = c(0,0)] <- gbutton("clear", handler=function(h,...) clear2())
	tbl[7,7, anchor = c(0,0)] <- gbutton("clear", handler=function(h,...) clear3())

	add(e$gp1,tbl,expand = FALSE)
	visible(tbl) <- TRUE

	# Group for first slider
	e$firstSliderGp <- ggroup(horizontal = FALSE)
	add(e$gp1,e$firstSliderGp)

	# Group for label and drag and drop box for the second subsetting variable
	tbl2 <- glayout(expand = FALSE)
	tbl2[2,1] <- glabel(" subset by  :")

	groupingVar2 <- glabel("Drop name here")
	font(groupingVar2) <- list(weight = "bold", family = "normal")
	tag(e$obj,"groupingVarData2") <- NULL
	tbl2[2,3] = groupingVar2
	tbl2[2,7, anchor = c(0,0)] <- gbutton("clear", handler=function(h,...) clear4())

	add(e$gp1,tbl2,expand = FALSE)
	visible(tbl2) <- TRUE

	e$secondSliderGp <- ggroup(horizontal = FALSE, expand = FALSE)
	add(e$gp1,e$secondSliderGp, expand = FALSE)

	# Setting up the plot notebook (each new plot will appear as a new leaf)
	gg <- ggraphics(container = gp2, label="plot", expand = TRUE)
	addhandlerrightclick(nb, handler = function(h, ...) {
		saveWin <- gwindow("Save Plot", height = 50, width = 120, cont = TRUE, 
			parent = e$win)
        saveMain <- ggroup(cont = saveWin)
        savelayout <- glayout()
        lbl1 <- glabel("Name of plot :")
        font(lbl1) <- list(weight = "bold")
        lbl2 <- glabel("Type of file :")
        font(lbl2) <- list(weight = "bold")
        savelayout[2,1, anchor = c(-1,0)] <- lbl1
        savelayout[2,2, anchor = c(-1,0)] <- (saveTxt = gtext("My Plot", width = 110, 
        	height = 20))
        savelayout[4,1, anchor = c(-1,0)] <- lbl2
        savelayout[4,2, anchor = c(-1,0)] <- (saveList = 
        	gdroplist(c("EMF","JPEG","PNG","BMP"), selected = 2))
        savelayout[6,1:2, anchor = c(0,0)] <- (savePlotButton = gbutton(" - SAVE - ",
             handler = function(h,...){
             	plotName <- gsub(pattern = '\\n+', replacement = "", svalue(saveTxt), 
             		perl = TRUE)
				if(plotName == "") plotName <- "My Plot"
				if(svalue(saveList) == "EMF"){
					dev.copy(win.metafile, file = paste(plotName,".emf",collapse =""), 
						width = 9)
					dev.off()
				}else if(svalue(saveList) == "JPEG"){
					dev.copy(jpeg, file = paste(plotName,".jpg",collapse =""), 
						width = 650, height = 650)
					dev.off()
				}else if(svalue(saveList) == "PNG"){
					dev.copy(png, file = paste(plotName,".png",collapse =""), 
						width = 650, height = 650)
					dev.off()
				}else if(svalue(saveList) == "BMP"){
					dev.copy(bmp, file = paste(plotName,".bmp",collapse =""),
						width = 650, height = 650)
					dev.off()
				}
				dispose(saveWin)
			})
		)

		font(savePlotButton) <- list(weight = "bold", color = "navy")
		add(saveMain, savelayout)
		visible(savelayout) <- TRUE
	})
