#' a grob used to display previous boxplots as ghost boxplots
#'
#' @param p25 The 25th percentile of the data to draw a ghost boxplot for
#' @param p50 The 50th percentile of the data to draw a ghost boxplot for
#' @param p75 The 75th percentile of the data to draw a ghost boxplot for
ghostsGrob <- function(p25, p50, p75, at = unit(0.25, "native"),
height = unit(0.375, "native"), box.color = "red", median.color = "blue",
alpha = 0.5, name = NULL, gp = gpar(lwd = 2), vp = NULL){

grob(p25 = p25, p50 = p50, p75 = p75, at = at, height = height,
box.color = box.color, median.color = median.color, alpha = alpha,
name = name, gp = gp, vp = vp, cl = "ghosts")
}


#' Draw a ghosts grob
grid.ghosts <- function(...){
  grid.draw(ghostsGrob(...))
}


drawDetails.ghosts <- function(x, recording){
pad <- convertHeight(unit(as.numeric(x$height)/2, attr(x$height, "unit")),
"native")

  grid.rect(x = unit(x$p25, "native"), y = x$at,
  width = unit(x$p75 - x$p25, "native"), height = x$height,
just = "left", gp = gpar(col = x$box.color, alpha = x$alpha))

grid.segments(x0 = unit(x$p50, "native"), y0 = x$at - pad,
x1 = unit(x$p50, "native"), y1 = x$at + pad,
gp = gpar(col = x$median.color, alpha = x$alpha))
}

editDetails.ghosts <- function(x, spec){
x <- ghostsGrob(x$p25, x$p50, x$p75, x$at, x$height, x$box.color,
x$median.color, x$alpha, x$name, x$gp, x$vp)
x
}

updateGhosts <- function(ghosts, info) {

#info <- fivenum(box.plot$data)
p25 <- c(info[2], ghosts$p25)
p50 <- c(info[3], ghosts$p50)
p75 <- c(info[4], ghosts$p75)

ghostsGrob(p25, p50, p75, ghosts$at, ghosts$height, ghosts$box.color,
ghosts$median.color, ghosts$alpha, ghosts$name, ghosts$gp, ghosts$vp)
}

makeGhosts <- function(box.plot, vp, name) {
info <- fivenum(box.plot$data)

ghostsGrob(info[2], info[3], info[4], vp = vp, name = name)
}

