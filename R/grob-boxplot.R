# functions and methods for constructing boxplot grobs

#' Draw a boxplot grob
grid.boxplot <- function(...){
  grid.draw(boxplotGrob(...))
}

#' Construct a boxplot grob
#'
#' boxplotGrob constructs a horizontally oriented boxplot that describes a vector of data.
#' To create a vertical boxplot, rotate the viewport it is drawn in. boxplotGrobs inherit the #' class "boxplot".
#'
#' @param data A numeric vector of data
#' @param at The height on the y axis of the middle of the boxplot
#' @param height The height of the boxplot
#' @param box.color A character string. The color to use for the box and whiskers
#' @param median.color A character string. The color to use for the median line
#' @param show.w A logical value that describes whether the whiskers should be drawn
#' @param name A name for the grob to be constructed
#' @param gp graphical parameters for the boxplot, constructed with gpar()
#' @param vp A default viewport to be used when drawing the grob
boxplotGrob <- function(data, at = unit(0.15, "native"),
                        height = unit(0.2, "native"), box.color = "lightgrey",
                        median.color = "lightgrey", stat = NULL, show.w = TRUE,
                        name = NULL, gp = gpar(lwd = 2), vp = NULL){

    bpgt <- gTree(data = data, at = at, height = height,
                  box.color = box.color, median.color = median.color, stat = stat,
                  show.w = show.w,
                  name = name, gp = gp, vp = vp, cl="boxplot")
    bpgt
}

# Utility for updating a boxplot grob
setBoxplotGrob <- function(bpgt){
	data         <- bpgt$data
	at           <- bpgt$at
	height       <- bpgt$height
	box.color    <- bpgt$box.color
	median.color <- bpgt$median.color
        stat         <- bpgt$stat
	show.w       <- bpgt$show.w

	whisker.low <- whisker.high <- boxes <- medians <- NULL

	x <- fivenum(data)

	if (show.w) {
		whisker.low <- segmentsGrob(x0 = unit(x[1], "native"), y0 = at,
									x1 = unit(x[2], "native"), y1 = at,
									gp = gpar(col = box.color),
									name = "whisker.low")
		whisker.high <- segmentsGrob(x0 = unit(x[4], "native"), y0 = at,
									x1 = unit(x[5], "native"), y1 = at,
									gp = gpar(col = box.color),
									name = "whisker.high")
	}

	box <- rectGrob(x = unit(x[2], "native"), y = at,
		width = unit(x[4] - x[2], "native"), height = height,
		just = "left", gp = gpar(col = box.color), name = "box")

	pad <- convertHeight(unit(as.numeric(height)/2, attr(height, "unit")),
		"inches")
	median.line <- segmentsGrob(x0 = unit(x[3], "native"), y0 = at - pad,
								x1 = unit(x[3], "native"), y1 = at + pad,
								gp = gpar(col = median.color),
								name = "median.line")
        if (!is.null(stat)){
            stat.line <- segmentsGrob(x0 = unit(stat(data), "native"), y0 = at - 0.5*pad,
                                      x1 = unit(stat(data), "native"), y1 = unit(0.5, "npc"),
                                      gp = gpar(col = "blue", lwd = 3), name = "stat.line")
        } else stat.line <- median.line

  	bpgt <- setChildren(bpgt,
                            gList(whisker.low, whisker.high, box, median.line, stat.line))
  	bpgt
}


drawDetails.boxplot <- function(x, recording){
	x <- setBoxplotGrob(x)
 	for (i in childNames(x)) grid.draw(getGrob(x, i))
}

editDetails.boxplot <- function(x, spec){
	x <- boxplotGrob(x$data, x$at, x$height, x$box.color, x$median.color, x$stat,
		x$show.w, x$name, x$gp, x$vp)
	x
}

validDetails.boxplot <- function(x){
	if (!inherits(x$data, c("integer", "numeric")))
		stop("data must be integer, numeric or matrix")

	if (!(any(class(x$at) %in% "unit") & any(class(x$height) %in% "unit")))
		stop("at and height must be unit object")

	if ("unit.arithmetic" %in% class(x$at))
	 	stop("at is unit.arithmetic")

	if ("unit.arithmetic" %in% class(x$height))
		stop("height is unit.arithmetic")

	x
}

grid.boxplot.example <- function(data = rnorm(100, 0, 3), at = unit(0.5, "npc"),
	height = unit(5, "native"), box.color = "black", median.color = "black",
	show.w = TRUE, name="bxpExample", gp = gpar(lwd=3)){
		require(grid)
		vp <- viewport(width = unit(0.8, "npc"), height = unit(0.8, "npc"),
			xscale = c(-10, 10), yscale = c(-10, 10))
		pushViewport(vp)

		grid.rect()
		grid.xaxis()
		grid.yaxis()

		grid.boxplot(data = data, at = at, height = height,
			box.color = box.color, median.color = median.color, show.w = show.w,
			name = name, gp = gp)
}

# grid.boxplot.example()
# grid.boxplot.example( box.color = "red", median.color = "blue", show.w = FALSE, gp = gpar(lwd = 3, alpha = 0.5))

# bp <- boxplotGrob(data = rnorm(100, 0, 3), at = unit(0.5, "npc"), height = unit(0.5, "native"), box.color = "black", median.color = "black", show.w = TRUE, name="bxpExample", gp = gpar(lwd=3))
