permGUIHandler <- function(e){
    e$method <- "permutation"
    e$data.boxes <- FALSE
    e$replace <- FALSE
    tbl <- glayout(container = e$upper)
    tbl[1, 1] <- glabel("Quantity: ", container = tbl)
    tbl[1, 2] <- (e$stat <- gcombobox(c("mean", "median"), editable = FALSE, container = tbl))
    gbutton("Record my choices", container = e$upper, expand = TRUE,
            handler = function(h, ...) {
                e$resetCanvas()
                loadStatDetails(e)
                e$c1$makeSamples(e$replace)
                e$c1$makeStatistics()
                e$c1$showLabels()
                e$c1$plotDataStat(e)
                e$c1$drawImage()
                enabled(e$lower) <- TRUE
            })
    vit.resamp <- glabel("Permuting data", container = e$lower)
    vit.bootbox <- gframe("Number of repetitions",
                          container = e$lower)
    e$redraw.radio <- gradio(c(1, 5, 20), horizontal = FALSE)
    add(vit.bootbox, e$redraw.radio)
    buttons1 <- ggroup(container = e$lower)

    ## Handler to go in here.
    get.sample <- gbutton("Go", container = e$lower, expand = TRUE,
                          handler = function(h, ...){
                              n <- svalue(e$redraw.radio)
                              for (i in 1:n){
                                  e$c1$plotSample(e)
                                  e$c1$drawImage()
                                  e$c1$advanceWhichSample()
                              }
                          }
                          )

    addSpace(e$lower, 20, horizontal = FALSE)

    glabel("Include statistic distribution", container = e$lower)
    vit.diffbox <- gframe("Number of repetitions",
                          container = e$lower)
    e$bootstrap.radio <- gradio(c(1, 5, 20, 1000),
                                horizontal = FALSE)
    add(vit.diffbox,e$bootstrap.radio)

    buttons2 <- ggroup(horizontal = FALSE, container = e$lower)
    ## Handler to go in here
    get.dist <- gbutton(text = "Go", expane = TRUE, container = buttons2)
    addSpace(e$lower, 10, horizontal = FALSE)
}
