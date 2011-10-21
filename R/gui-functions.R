## Sets up data and analysis tabs.
setupGUI <- function(e){
    if (e$in.window)
        e$win <- gwindow("Visual Inference Tools", visible = TRUE, width = 870,
                         height = 600) else{
                             e$win <- gwindow("Visual Inference Tools", visible = TRUE,
                                              width = 375,
                                              height = 600)}
    e$loaded <- FALSE
    ## separates space into controls and plot
    if (!e$in.window)
        g <- ggroup(container = e$win, expand = TRUE) else{
            g <- gpanedgroup(container = e$win, expand = TRUE)}
    e$obj <- g
    g.controls <- gnotebook(container = g, expand = TRUE)
    if (e$in.window)
        g.plot <- ggraphics(container = g, expand = TRUE)

    ## control panel has a read data tab inspired from iNZight
    ## and a VIT tab
    e$controls.iNZight <- ggroup(horizontal = FALSE, container = g.controls,
                               expand = TRUE, label = "Load Data")
    e$controls.vit <- ggroup(horizontal = FALSE, container = g.controls,
                             expand = TRUE, label = "Analyse Data")
    svalue(g.controls) <- 1
    e$upper <- ggroup(horizontal = FALSE, container = e$controls.vit, expand = TRUE)
    e$lower <- ggroup(horizontal = FALSE, container = e$controls.vit, expand = TRUE)

    ## Disable GUI until data loaded.
    enabled(e$upper) <- FALSE
    enabled(e$lower) <- FALSE
    addSpace(e$upper, 10, horizontal = FALSE)
}

## Fills in data tab with iNZight controls.
dataGUI <- function(e){
    ## adding iNZight controls
    ## top three buttons
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
    add(e$controls.iNZight, iNZ.view)

    ## Table of data frame or variables
    e$dataGp <- ggroup(horizontal = TRUE, expand = TRUE)
    add(e$controls.iNZight, e$dataGp, expand = TRUE)

    ## At start-up an (near)empty dataframe is embedded
    tag(e$obj,"dataSet") = data.frame(empty = " ")
    tag(e$obj,"originalDataSet") = data.frame(empty = " ")
    e$dataSt <- gdf(tag(e$obj,"dataSet"), width = 20, height = 50,
                    expand = TRUE)
    add(e$dataGp, e$dataSt, expand = TRUE)
    e$dataList1 <- NULL
    e$dataList2 <- NULL

    ## Variable 1 and 2 display
    ## column 1
    tbl <- glayout(expand = FALSE)
    tbl[3,1, anchor = c(0,0)] = glabel(" Variable 1 :")
    tbl[5,1, anchor = c(0,0)] = glabel(" Variable 2 :")

    ## column 2
    e$xVar <- glabel("Drop name here")
    font(e$xVar) <- list(weight = "bold", family = "normal")
    tag(e$obj,"e$xVarData") <- NULL
    tbl[3,3, anchor = c(0,0)] <- e$xVar

    e$yVar <- glabel("Drop name here")
    font(e$yVar) <- list(weight = "bold", family = "normal")
    tag(e$obj,"e$yVarData") <- NULL
    tbl[5,3, anchor = c(0,0)] <- e$yVar

    ## column 3
    tbl[3,7, anchor = c(0,0)] <- gbutton("clear", handler = function(h,...) {
        e$xData <- NULL
        svalue(e$xVar) <- "Drop name here"
        e$variable_check()
        clear_actions(e)
        ## Only try to plot something if data is loaded.
        if (e$data.loaded){
            loadPlotDetails(e$xData, e$yData)
            e$buildCanvas()
            e$c1$drawImage()
        } else enabled(e$upper) <- FALSE
        enabled(e$lower) <- FALSE
    })
    tbl[5,7, anchor = c(0,0)] <- gbutton("clear", handler = function(h,...) {
        e$yData <- NULL
        svalue(e$yVar) <- "Drop name here"
        e$variable_check()
        clear_actions(e)
        ## Only try to plot something if data is loaded.
        if (e$data.loaded){
            loadPlotDetails(e$xData, e$yData)
            e$buildCanvas()
            e$c1$drawImage()
        } else enabled(e$upper) <- FALSE
        enabled(e$lower) <- FALSE
    })

    tbl[3,8] <- ""

    add(e$controls.iNZight, tbl, expand = FALSE)
    addSpace(e$controls.iNZight, 10, horizontal = TRUE)
    visible(tbl) <- TRUE

    ## adding drop zones
    adddroptarget(e$xVar, targetType = "object", handler = function(h, ...) {
        svalue(e$xVar) <- gWidgets::id(h$dropdata)
        if (e$inDataView) e$xData <- svalue(h$dropdata)
        else e$xData <- tag(e$obj, "dataSet")[,id(h$dropdata)]
        if (is.integer(e$xData)) e$xData <- as.numeric(e$xData)
        e$variable_check()
        clear_actions(e)
        loadPlotDetails(e, e$c1)
        e$buildCanvas()
        if (e$data.boxes) e$c1$buildBoxes()
        e$c1$drawImage()
        enabled(e$upper) <- TRUE
        enabled(e$lower) <- FALSE
    })

    adddroptarget(e$yVar, targetType = "object", handler = function(h, ...) {
        svalue(e$yVar) <- gWidgets::id(h$dropdata)
        if (e$inDataView) e$yData <- svalue(h$dropdata) else{
            e$yData <- tag(e$obj, "dataSet")[,id(h$dropdata)]}
        if (is.integer(e$yData)) e$yData <- as.numeric(e$yData)
        e$variable_check()
        clear_actions(e)
        loadPlotDetails(e, e$c1)
        e$buildCanvas()
        e$c1$drawImage()
        enabled(e$upper) <- TRUE
        enabled(e$lower) <- FALSE
    })

    home.button <- gbutton("Home", container = iNZ.view, expand = TRUE,
                           handler =
                           function(h, ...){
                               dispose(e$win)
                               vit()
                           })
}
