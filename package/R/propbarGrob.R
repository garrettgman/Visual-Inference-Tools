# functions and methods for constructing proportion bar grobs.

#' Draw a proportion bar grob
grid.propbar <- function(...)
    grid.draw(propbarGrob(...))

#' Construct a proportion bar grob
#'
#' propbarGrob constructs a proportion bar of a vector of categorical data.
#' propbarGrobs inherit the class "propbar".

propbarGrob <- function(data, y = 0, height = 0.2, fills = c("grey", "black"),
                        name = NULL, gp = NULL, vp = NULL){
    data <- as.character(data)
    levels <- sort(unique(data))
    if (length(levels) != 2) notYetImplemented()
    n <- length(data)
    p <- mean(data == levels[1])
    sdP <- sqrt(p*(1 - p)/n)
    range <- p + c(-1, 1)*5*sdP
    if (range[1] < 0) range[1] = 0
    if (range[2] > 1) range[2] = 1
    cropped <- c(range[1] != 0, range[2] != 1)
    if (cropped[1]){
        leftBar <- polygonGrob(x = unit(c(0, rep(c(0.05, 0), 3), rep(p, 2)), c(rep("npc", 7),
                               rep("native", 2))),
                               y = unit(c(y, y + (1:6)*height/6, y + height, y), "npc"),
                               name = "leftBar", gp = gpar(fill = fills[1]))
    } else{
        leftBar <- rectGrob(x = 0, y = y, width = unit(p, "native"),
                           height = height, just = c("left", "bottom"), name = "leftBar",
                           gp = gpar(fill = fills[1]))
    }
    if (cropped[2]){
        rightBar <- polygonGrob(x = unit(c(1, rep(c(0.95, 1), 3), rep(p, 2)), c(rep("npc", 7),
                               rep("native", 2))),
                               y = unit(c(y, y + (1:6)*height/6, y + height, y), "npc"),
                               name = "rightBar", gp = gpar(fill = fills[2]))
    } else{
        rightBar <- rectGrob(x = p, y = y, width = unit(1 - p, "native"),
                           height = height, just = c("left", "bottom"), name = "rightBar",
                           gp = gpar(fill = fills[2]))
    }
    text1 <- textGrob(levels[1], x = 0, y = unit(y + height, "npc") + unit(5, "mm"),
                      just = c("left", "bottom"), name = "text1")
    text2 <- textGrob(levels[2], x = 1, y = unit(y + height, "npc") + unit(5, "mm"),
                      just = c("right", "bottom"), name = "text2")
    gTree(data = data, y = y, height = height, fills = fills, p = p,
         range = range, cropped = cropped, name = name,
         gp = gp, vp = vp, children = gList(leftBar, rightBar, text1, text2), cl = "propbar")
}

drawDetails.propbar <- function(x, recording){
    for (i in childNames(x)) grid.draw(getGrob(x, i))
}

validDetails.propbar <- function(x) x

editDetails.propbar <- function(x, specs){
    propbarGrob(data = x$data, y = x$y, height = x$height, fills = x$fills,
                name = x$name, gp = x$gp, vp = x$vp)
}

grid.propbar.example <- function(data = sample(1:2, size = 50, replace = TRUE, prob = c(0.6, 0.4)),
                                 y = 0, height = 0.2, fills = c("white", "black"), name = "propbarExample"){
    grid.newpage()
    pbg <- propbarGrob(data = data, y = y, height = height, fills = fills, name = name)
    pushViewport(viewport(height = 0.8, width = 0.8, xscale = pbg$range))
    grid.draw(pbg)
    grid.xaxis()
}
