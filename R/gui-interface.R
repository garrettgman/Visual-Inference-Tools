# Much of the vit() function and the new.vit.env() function in
# new-vit-env.R are borrowed from the iNZight package. We'd like to
# one day add the vit tools into the iNZight tools, so I've aimed to
# keep the interfaces as similar as possible. However, the iNZight
# code is not very modular and most of the functions include many
# routines irrelevant to vit. Hence, most of the iNZight code that
# appears in these vit files is modified from its original form -
# Garrett


vit <- function(in.window = FALSE, cb = FALSE) {
    home <- gwindow("Visual Inference Tools", visible = TRUE)
    home.buttons <- ggroup(horizontal = FALSE, container = home)
    title <- glabel("Visual Inference Tools", container = home.buttons)
    ## Generates home page
    home.confint <- gbutton("Confidence interval coverage", container = home.buttons, expand = TRUE,
                       handler =
                       function(h, ...){
                           dispose(home)
                           plot.new()
                           e <- new.vit.env()
                           e$cb <- cb
                           e$in.window <- in.window
                           setupGUI(e)
                           CIGUIHandler(e)
                           dataGUI(e)
                       })
    home.bootstrap <- gbutton("Bootstrapping", container = home.buttons, expand = TRUE,
                         handler =
                         function(h, ...){
                             dispose(home)
                             plot.new()
                             e <- new.vit.env()
                             e$cb <- cb
                             e$in.window <- in.window
                             setupGUI(e)
                             bootstrapGUIHandler(e)
                             dataGUI(e)
                         })
    home.permutation <- gbutton("Permutation tests", container = home.buttons, expand = TRUE,
                              handler =
                              function(h, ...){
                                  dispose(home)
                                  plot.new()
                                  e <- new.vit.env()
                                  e$cb <- cb
                                  e$in.window <- in.window
                                  setupGUI(e)
                                  permGUIHandler(e)
                                  dataGUI(e)
                              })
    enabled(home.permutation) <- FALSE
}
