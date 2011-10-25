bootstrapGUIHandler <- function(e){
    e$method <- "bootstrap"
    e$data.boxes <- TRUE
    tbl <- glayout(container = e$upper)
    tbl[1, 1] <- glabel("Quantity: ", container = tbl)
    tbl[1, 2] <- (e$stat <- gcombobox(c("mean", "median"), editable = FALSE, container = tbl))
    e$replace <- TRUE
    gbutton("Record my choices", container = e$upper, expand = TRUE,
            handler = function(h, ...) {
                e$resetCanvas()
                loadStatDetails(e)
                e$c1$makeSamples(e$replace)
                e$c1$makeStatistics()
                e$c1$showLabels()
                e$c1$drawImage()
                enabled(e$lower) <- TRUE
            })
    vit.resamp <- glabel("Resampling", container = e$lower)
    vit.bootbox <- gframe("Number of repetitions",
                          container = e$lower)
    e$redraw.radio <- gradio(c(1, 5, 20, 1000), horizontal=FALSE)
    add(vit.bootbox, e$redraw.radio)

    buttons1 <- ggroup(container = e$lower)
    e$clear.stat <- FALSE
    e$points <- FALSE
    get.sample <- gbutton("Go", container = e$lower, expand = TRUE,
                          handler = function(h, ...){
                              enabled(e$lower) <- FALSE
                              enabled(e$lowest) <- FALSE
                              if (svalue(e$redraw.radio) == 1000){
                                  e$clearStatPanel()
                                  e$c1$handle1000(e, points = FALSE)
                                  enabled(e$lower) <- TRUE
                                  enabled(e$lowest) <- TRUE
                                  enabled(show.ci) <- TRUE
                                  enabled(show.summary) <- TRUE
                                  e$points <- FALSE
                                  e$clear.stat <- TRUE
                              } else {
                                  if (e$clear.stat){
                                      e$clearStatPanel()
                                      e$clear.stat <- FALSE
                                  }
                                  n <- svalue(e$redraw.radio)
                                  for (i in 1:n){
                                      ##e$c1$animateSample(15, 5, TRUE, TRUE)
                                      e$c1$plotSample(e)
                                      e$c1$drawImage()
                                      enabled(e$lower) <- TRUE
                                      e$c1$advanceWhichSample()
                                  }
                                  enabled(e$lower) <- TRUE
                              }
                          })

    addSpace(e$lower, 20, horizontal=FALSE)

    glabel("Include statistic distribution", container = e$lower)

    vit.diffbox <- gframe("Number of repetitions",
                          container = e$lower)
    e$bootstrap.radio <- gradio(c(1, 5, 20, 1000),
                                horizontal = FALSE)
    add(vit.diffbox,e$bootstrap.radio)

    buttons2 <- ggroup(horizontal = FALSE, container = e$lower)
    get.dist <- gbutton(text = "Go", expand = TRUE,
                        container = buttons2, handler = function(h, ...) {
                            enabled(e$lower) <- FALSE
                            enabled(e$lowest) <- FALSE
                            if (e$clear.stat){
                                e$clearStatPanel()
                                e$clear.stat <- FALSE
                            }
                            if (svalue(e$bootstrap.radio) == 1000){
                                e$c1$handle1000(e, points = TRUE)
                                enabled(e$lowest) <- TRUE
                                enabled(show.ci) <- TRUE
                                enabled(show.summary) <- TRUE
                                e$clear.stat <- TRUE
                                e$points <- TRUE
                            } else {
                                n <- svalue(e$bootstrap.radio)
                                for (i in 1:n){
                                    e$c1$plotSample(e)
                                    if (n != 20) e$c1$animateStat(e, 15)
                                    e$c1$plotStatDist(e)
                                    e$c1$advanceWhichSample()
                                    e$c1$drawImage()
                                }
                            }
                            enabled(e$lower) <- TRUE
                        })

    addSpace(e$lower, 10, horizontal = FALSE)

    e$lowest <- ggroup(horizontal = FALSE, container = e$lower, expand = TRUE)
    show.ci <- gbutton(text = "Show confidence interval", expand = TRUE,
                         container = e$lowest, handler = function(h, ...){
                             enabled(e$lower) <- FALSE
                             enabled(e$lowest) <- FALSE
                             e$c1$displayResult(e, ci = TRUE, points = e$points)
                             enabled(e$lower) <- TRUE
                             enabled(e$lowest) <- TRUE
                             enabled(show.ci) <- FALSE
                             })
    show.summary <- gbutton(text = "Show summary statistics", expand = TRUE,
                            container = e$lowest, handler = function(h, ...){
                                e$c1$displayResult(e, ci = FALSE, points = e$points)
                                enabled(show.summary) <- FALSE
                                })
    enabled(e$lowest) <- FALSE
}

