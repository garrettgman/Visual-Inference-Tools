# functions and methods for constructing confidence interval grobs

#' Draw a confidence interval grob
grid.ci <- function(...)
    grid.draw(ciGrob(...))

#' Construct a confidence interval grob
#'
#' ciGrob constructs a rectangle representing a confidence interval.
#' ciGrobs inherit the class "ci"
#'
#' @param ci a numeric vector of length 2 which specifies the bounds for a confidence interval.
#' @param y a numeric value specifying the y-axis position(in npc units) of the bottom of the rectangle.
#' @param height a numeric value which provides the height (in npc units) of the rectangle.
#' @param name a name for the grob to be constructed.
#' @param gp graphical parameters for the histogram, constructed with gpar().
#' @param vp a default viewport to be used when drawing the grob.

ciGrob <- function(ci, y = 0, height = 0.1, gp = NULL, name = NULL, vp = NULL){
    grob(ci = ci, y = y, height = height, name = name, gp = gpar(fill = "grey"), vp = vp, cl = "ci")
}

drawDetails.ci <- function(x, recording){
    grid.draw(rectGrob(x = unit(x$ci[1], "native"), y = unit(x$y, "npc"), width = unit(diff(x$ci), "native"), height = unit(x$height, "npc"), just = c("left", "bottom"), name = x$name, gp = x$gp, vp = x$vp))
}

validDetails.ci <- function(x){
    if (length(x$ci) != 2 | !is.numeric(x$ci))
        stop("ci must be a numeric vector of length 2")
    if (!is.numeric(x$y))
        stop("y must be numeric")
    if (!is.numeric(x$height))
        stop("height must be numeric")
    x
}

editDetails.ci <- function(x, specs){
    x <- ciGrob(ci = x$ci, y = x$y, height = x$height, name = x$name, gp = x$gp, vp = x$vp)
    x
}

grid.ci.example <- function(data = rnorm(30), y = 0, height = 0.1, name = "ciExample"){
    grid.newpage()
    n <- length(data)
    se <- sd(data)/sqrt(n)
    ci <- mean(data) + c(-1, 1)*qt(0.975, n - 1)*se
    vp <- plotViewport(xscale = range(data))
    pushViewport(vp)
    grid.points(data, rep(0.5, n))
    grid.xaxis()
    x <- ciGrob(ci = ci, y = y, height = height, name = name, gp = gp)
    grid.draw(x)
}
