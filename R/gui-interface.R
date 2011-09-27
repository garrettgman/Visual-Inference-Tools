# Much of the vit() function and the new.vit.env() function in new-vit-env.R are borrowed from the iNZight package. We'd like to one day add the vit tools into the iNZight tools, so I've aimed to keep the interfaces as similar as possible. However, the iNZight code is not very modular and most of the functions include many routines irrelevant to vit. Hence, most of the iNZight code that appears in these vit files is modified from its original form - Garrett


vit <- function(in.window = FALSE) {
    plot.new()
    e <- new.vit.env()
    if (in.window)
	e$win <- gwindow("Visual Inference Tools", visible = TRUE, width = 870,
                         height = 600) else{
                             e$win <- gwindow("Visual Inference Tools", visible = TRUE,
                                              width = 375,
                                              height = 600)}
    e$loaded <- FALSE

    ## separates space into controls and plot
    if (!in.window)
        g <- ggroup(container = e$win, expand = TRUE) else{
            g <- gpanedgroup(container = e$win, expand = TRUE)}
    e$obj <- g
    g.controls <- gnotebook(container = g, expand = TRUE)
    if (in.window)
        g.plot <- ggraphics(container = g, expand = TRUE)

    ## control panel has a read data tab inspired from iNZight
    ## and a VIT tab
    controls.iNZight <- ggroup(horizontal = FALSE, container = g.controls,
                               expand = TRUE, label = "Load Data")
    e$controls.vit <- ggroup(horizontal = FALSE, container = g.controls,
                             expand = TRUE, label = "Analyze Data")


    ## adding vit controls
    addSpace(e$controls.vit, 10, horizontal = FALSE)
    tbl <- glayout(container = e$controls.vit)
    tbl[1,1] <- glabel("Statistic:    ", container = tbl)
    tbl[1,2] <- (e$stat <- gcombobox(c(), editable=TRUE, container = tbl,
                                     handler = function(h, ...) {
                                         if (svalue(e$stat) %in% c("confidence interval - mean",
                                                                   "confidence interval - median")) {
                                             enabled(e$cimeth) <- TRUE
                                             enabled(e$cilabel) <- TRUE
                                             if (svalue(e$stat) %in% c("confidence interval - median")) {
                                                 e$cimeth[] <- c("percentile bootstrap", "normal bootstrap",
                                                                 "t bootstrap")
                                                 svalue(e$cimeth) <- "percentile bootstrap"
                                             } else {
                                                 e$cimeth[] <- c("normal", "percentile bootstrap",
                                                                 "normal bootstrap", "t bootstrap")
                                             }
                                         } else {
                                             enabled(e$cimeth) <- FALSE
                                             enabled(e$cilabel) <- FALSE
                                         }
                                     }))
    e$stat[] <- c("mean", "median", "confidence interval - mean",
                  "confidence interval - median")
    svalue(e$stat) <- "mean"

    tbl[2,1] <- (e$cilabel <- glabel("CI Method:    ", container = tbl))
    tbl[2,2] <- (e$cimeth <- gcombobox(c("normal", "percentile bootstrap",
                                         "normal bootstrap", "t bootstrap"), editable = TRUE,
                                       container = tbl))

    tbl[3,1] <- (e$sizelabel <- glabel("Sample Size:  50", container = tbl))
    tbl[3,2] <- (e$ssize <- gedit("50", container = tbl,
                                  handler = function(h, ...) {
                                      svalue(e$sizelabel) <- paste("Sample Size: ",
                                                                   as.character(svalue(e$ssize)))
                                  }
                                  ))

    tbl[4,2] <- (e$replace <- gcheckbox("Sample with replacement",
                                        container = tbl))

    gbutton("Load details", container = e$controls.vit, expand = TRUE,
            handler = function(h,...) {
                e$resetCanvas()
                e$sample_check()
                loadStatDetails(e)
                e$c1$makeSamples(svalue(e$replace))
                e$c1$makeStatistics()
                e$c1$plotDataStat() #use this to rerun PLOT_DATA for your method if necessary
            })

    svalue(e$replace) <- TRUE
    svalue(e$cimeth) <- "normal"
    enabled(e$cimeth) <- FALSE
    enabled(e$cilabel) <- FALSE
    addSpace(e$controls.vit, 10, horizontal = FALSE)
    vit.bootbox <- gframe("Get bootstrapped sample(s)",
                          container = e$controls.vit)
    e$redraw.radio <- gradio(c(1, 5, 20),  horizontal=FALSE)
    add(vit.bootbox, e$redraw.radio)

    e$advance <- FALSE

    buttons1 <- ggroup(container = e$controls.vit)
    get.sample <- gbutton(text = "Generate sample", expand = TRUE,
                          container = buttons1, handler = function(h, ...) {
                              loaded_check(e)
                              n <- svalue(e$redraw.radio)
                              for (i in 1:n) {
                                  if (e$advance) e$c1$advanceWhichSample()
                                  if (svalue(e$redraw.radio) == 1) e$c1$animateSample(15, 5, TRUE)
                                  if (svalue(e$redraw.radio) == 5) e$c1$animateSample(15, 0, TRUE)
                                  if (svalue(e$redraw.radio) == 20) e$c1$animateSample(15, 0, FALSE)
                                  e$c1$plotSample()
                                  e$c1$plotSampleStat()
                                  e$c1$drawImage()
                                  e$advance <- TRUE
                              }
                          }
                          )
    add.stat <- gbutton(text = "Add statistic below", expand = TRUE,
                        container = buttons1, handler = function(h, ...) {
                            e$c1$animateStat(10)
                            e$c1$plotStatDist()
                            e$c1$displayResult(e)
                            e$c1$drawImage()
                            e$c1$advanceWhichSample()
                            e$advance <- FALSE
                        }
                        )
    addSpace(e$controls.vit, 20, horizontal=FALSE)

    vit.diffbox <- gframe("Observe sample statistic(s)",
                          container = e$controls.vit)
    e$bootstrap.radio <- gradio(c(1, 5, 20, 1000),
                                horizontal = FALSE)
    add(vit.diffbox,e$bootstrap.radio)


    buttons2 <- ggroup(horizontal = FALSE, container = e$controls.vit)
    get.dist <- gbutton(text = "Generate statistic distribution", expand = TRUE,
                        container = buttons2, handler = function(h, ...) {
                            loaded_check(e)
                            if (svalue(e$bootstrap.radio) == 1000) e$c1$handle1000(e)
                            else {
				n <- svalue(e$bootstrap.radio)
				for (i in 1:n) {
                                    if (svalue(e$bootstrap.radio) != 20)
                                        e$c1$animateSample(15, 0, TRUE) else{
                                            e$c1$animateSample(15, 0, FALSE)
                                        }
                                    e$c1$plotSample()
                                    e$c1$plotSampleStat()
                                    if (svalue(e$bootstrap.radio) != 20) e$c1$animateStat(10)
                                    e$c1$plotStatDist()
                                    e$c1$displayResult(e)
                                    e$c1$drawImage()
                                    e$c1$advanceWhichSample()
				}
				e$advance <- FALSE
                            }
                        }
                        )

    ## be sure to disable this button for confidence coverage methods
    e$show.ci <- gbutton(text = "Show Confidence Interval", expand = TRUE,
                         container = buttons2, handler = function(h, ...) e$c1$displayResult(e))
    addSpace(e$controls.vit, 10, horizontal = FALSE)

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
    add(controls.iNZight, iNZ.view)

                                        # Table of data frame or variables
    e$dataGp <-  ggroup(horizontal = TRUE, expand = TRUE)
    add(controls.iNZight, e$dataGp, expand = TRUE)

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
        loadPlotDetails(e$xData, e$yData)
        e$buildCanvas()
        e$c1$drawImage()
    })
    tbl[5,7, anchor = c(0,0)] <- gbutton("clear", handler = function(h,...) {
        e$yData <- NULL
        svalue(e$yVar) <- "Drop name here"
        e$variable_check()
        clear_actions(e)
        loadPlotDetails(e$xData, e$yData)
        e$buildCanvas()
        e$c1$drawImage()
    })

    tbl[3,8] <- ""

    add(controls.iNZight, tbl, expand = FALSE)
    addSpace(controls.iNZight, 10, horizontal = TRUE)
    visible(tbl) <- TRUE

    ## adding drop zones
    adddroptarget(e$xVar, targetType = "object", handler = function(h, ...) {
        svalue(e$xVar) <- gWidgets::id(h$dropdata)
        if (e$inDataView) e$xData <- svalue(h$dropdata)
        else e$xData <- tag(e$obj, "dataSet")[,id(h$dropdata)]
        if (is.integer(e$xData)) e$xData <- as.numeric(e$xData)
        svalue(e$ssize) <- length(e$xData)
        e$variable_check()
        clear_actions(e)
        loadPlotDetails(e$xData, e$yData)
        e$buildCanvas()
        e$c1$drawImage()
    })

    adddroptarget(e$yVar, targetType = "object", handler = function(h, ...) {
        svalue(e$yVar) <- gWidgets::id(h$dropdata)
        if (e$inDataView) e$yData <- svalue(h$dropdata)
        else e$yData <- tag(e$obj, "dataSet")[,id(h$dropdata)]
        if (is.integer(e$yData)) e$yData <- as.numeric(e$yData)
        e$variable_check()
        clear_actions(e)
        loadPlotDetails(e$xData, e$yData)
        e$buildCanvas()
        e$c1$drawImage()
    })

}



