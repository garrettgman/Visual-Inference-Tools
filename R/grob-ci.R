## Functions and methods for a visual representation of a confidence
## intervals with arrows and labels providing the numeric
## representation.

#' Draw a ci grob
grid.confint <- function(...)
    grid.draw(confintGrob(...))

#' Construct a ci grob
#'
#' datatextGrob constructs text of a vector of data.
#' datatextGrobs inherit the class "confint".
#'
#' @param ci the confidence interval to be plotted, given by a vector of length 2.
#' @param at a unit object giving the y-axis position to plot the confidence interval.
#' @param to a unit object giving the y-axis positions of where the arrows are to point.
#' @param col the colour to plot the confidence interval.

confintGrob <- function(ci, at = unit(0.15, "npc"), to = unit(-2, "lines"), col = "red",
                        name = NULL, gp = NULL, vp = NULL){
    cigt <- gTree(ci = ci, at = at, to = to, col = col, name = name, gp = gp, vp = vp,
                  cl = "confint")
    cigt
}
## Utility for updating a ci grob
setConfintGrob <- function(cigt){
    ci <- cigt$ci
    at <- cigt$at
    to <- cigt$to
    col <- cigt$col
    ci.rect <- rectGrob(x = unit(ci[1], "native"), y = at,
                        height = unit(0.01, "npc"), width = unit(diff(ci), "native"),
                        just = c("left", "centre"),
                        gp = gpar(col = col, fill = col), name = "ci.rect")
    arrows <- segmentsGrob(x0 = unit(ci, "native"), x1 = unit(ci, "native"),
                           y0 = at, y1 = to, gp = gpar(col = col),
                           arrow = arrow(length = unit(0.1, "inches")),
                           name = "arrows")
    text1 <- textGrob(label = format(ci[1], nsmall = 1), x = unit(ci[1], "native"),
                      y = to, gp = gpar(fontface = 2, col = col),
                      just = c("right", "top"), name = "text1")
    text2 <- textGrob(label = format(ci[2], nsmall = 1), x = unit(ci[2], "native"),
                      y = to, gp = gpar(fontface = 2, col = col),
                      just = c("left", "top"), name = "text2")
    cigt <- setChildren(cigt, gList(ci.rect, arrows, text1, text2))
    cigt
}

drawDetails.confint <- function(x, recording){
    x <- setConfintGrob(x)
    for (i in childNames(x)) grid.draw(getGrob(x, i))
}

editDetails.confint <- function(x, spec){
    x <- confintGrob(ci = x$ci, at = x$at, to = x$to, col = x$col,
                     name = x$name, gp = x$gp, vp = x$vp)
    x
}

validDetails.confint <- function(x) x
