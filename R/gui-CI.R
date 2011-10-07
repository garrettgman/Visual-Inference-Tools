CIGUIHandler <- function(e){
    e$method <- "ci"
    tbl <- glayout(container = e$upper)
    tbl[1,1] <- glabel("Quantity: ", container = tbl)
    tbl[1,2] <- (e$stat <- gcombobox(c(), editable = TRUE, container = tbl,
                                     handler = function(h, ...) {
                                             if (svalue(e$stat) == "median") {
                                                 e$cimeth[] <- c("bootstrap: percentile",
                                                                 "bootstrap: +/- 2 s.e.",
                                                                 "bootstrap: +/- t s.e.")
                                                 svalue(e$cimeth) <- "bootstrap: percentile"
                                             } else {
                                                 e$cimeth[] <- c("bootstrap: percentile",
                                                                 "bootstrap: +/- 2 s.e.",
                                                                 "bootstrap: +/- t s.e.",
                                                                 "normal: +/- 2 s.e.",
                                                                 "normal: +/- t s.e.")
                                                 svalue(e$cimeth) <- "normal: +/- t s.e."
                                             }

                                         }
                                     ))
    e$stat[] <- c("mean", "median")
    svalue(e$stat) <- "mean"

    tbl[2,1] <- (e$cilabel <- glabel("CI Method: ", container = tbl))
    tbl[2,2] <- (e$cimeth <- gcombobox(c("bootstrap: percentile",
                                         "bootstrap: +/- 2 s.e.",
                                         "bootstrap: +/- t s.e.",
                                         "normal: +/- 2 s.e.",
                                         "normal: +/- t s.e."), editable = TRUE,
                                       container = tbl))

    tbl[3,1] <- (e$sizelabel <- glabel("Sample Size: 50", container = tbl))
    tbl[3,2] <- (e$ssize <- gedit("10", container = tbl,
                                  handler = function(h, ...) {
                                      svalue(e$sizelabel) <- paste("Sample Size: ",
                                                                   as.character(svalue(e$ssize)))
                                  }
                                  ))
    tbl[4,2] <- (e$holdSample <- gcheckbox("Hold samples", container = tbl))

    gbutton("Record my choices", container = e$upper, expand = TRUE,
            handler = function(h,...) {
                if (svalue(e$ssize) != e$c1$n) svalue(e$holdSample) <- FALSE
                keep.samples <- (!(class(e$c1$samples) == "uninitializedField") &
                                 svalue(e$holdSample))
                if (keep.samples)
                    e$resetCanvasKeepSample(e$c1) else e$resetCanvas()
                e$c1$n <- as.numeric(svalue(e$ssize))
                loadStatDetails(e)
                e$sample_check()
                if (!keep.samples)
                    e$c1$makeSamples(e$replace)
                e$c1$makeStatistics()
                e$c1$plotDataStat(e) #use this to rerun PLOT_DATA for your method if necessary
                e$c1$showLabels()
                e$c1$drawImage()
                enabled(e$lower) <- TRUE
            })

    svalue(e$cimeth) <- "normal: +/- t s.e."
    addSpace(e$upper, 10, horizontal = FALSE)
    vit.popsamp <- glabel("Population and sample", container = e$lower)
    vit.bootbox <- gframe("Number of repetitions",
                          container = e$lower)
    e$redraw.radio <- gradio(c(1, 5, 20), horizontal=FALSE)
    add(vit.bootbox, e$redraw.radio)

    e$advance <- FALSE

    buttons1 <- ggroup(container = e$lower)
    get.sample <- gbutton(text = "Go", expand = TRUE,
                          container = buttons1, handler = function(h, ...) {
                              enabled(e$lower) <- FALSE
                              loaded_check(e)
                              n <- svalue(e$redraw.radio)
                              for (i in 1:n) {
                                  if (e$advance) e$c1$advanceWhichSample()
                                  if (n == 1)
                                      e$c1$animateSample(15, 5, TRUE, TRUE)
                                  if (n == 5)
                                      e$c1$animateSample(15, 0, TRUE, TRUE)
                                  if (n == 20)
                                      e$c1$animateSample(15, 0, TRUE, FALSE)
                                  e$c1$plotSample(e)
                                  e$c1$plotSampleStat(e)
                                  e$c1$showLabels
                                  if (n == 5) e$c1$pauseImage(15) else e$c1$drawImage()
                                  e$advance <- TRUE
                              }
                              enabled(e$lower) <- TRUE
                          }
                          )

    addSpace(e$lower, 20, horizontal=FALSE)

    glabel("Include confidence interval history", container = e$lower)

    vit.diffbox <- gframe("Number of repititions",
                          container = e$lower)
    e$bootstrap.radio <- gradio(c(1, 5, 20, 1000),
                                horizontal = FALSE)
    add(vit.diffbox,e$bootstrap.radio)


    buttons2 <- ggroup(horizontal = FALSE, container = e$lower)
    get.dist <- gbutton(text = "Go", expand = TRUE,
                        container = buttons2, handler = function(h, ...) {
                            enabled(e$lower) <- FALSE
                            loaded_check(e)
                            if (svalue(e$bootstrap.radio) == 1000) e$c1$handle1000(e)
                            else {
                                n <- svalue(e$bootstrap.radio)
                                for (i in 1:n) {
                                    if (n != 20)
                                        e$c1$animateSample(15, 0, FALSE, TRUE) else{
                                            e$c1$animateSample(15, 0, FALSE, FALSE)
                                        }
                                    e$c1$plotSample(e)
                                    e$c1$plotSampleStat(e)
                                    if (n != 20) e$c1$pauseImage(15)
                                    if (svalue(e$bootstrap.radio) != 20) e$c1$animateStat(e, 10)
                                    e$c1$plotStatDist(e)
                                    e$c1$displayResult(e)
                                    e$c1$showLabels()
                                    e$c1$drawImage()
                                    e$c1$advanceWhichSample()
                                }
                                e$advance <- FALSE
                            }
                            enabled(e$lower) <- TRUE
                        }
                        )
}
