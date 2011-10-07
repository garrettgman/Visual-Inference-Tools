bootstrapGUIHandler <- function(e){
    e$method <- "bootstrap"
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
    gbutton("Generate sample", container = e$lower, expand = TRUE,
            handler = function(h, ...){
                #e$c1$animateSample(15, 5, TRUE, TRUE)
                e$c1$plotSample(e)
                e$c1$drawImage()
                e$c1$advanceWhichSample()
            })
}

