# functions and methods for constructing boxplot grobs

#' Draw a boxplot grob
grid.boxplot = function(...){
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
boxplotGrob <- function(data, at, height, box.color = "black", median.color = "black", 
	show.w = TRUE, name = NULL, gp = NULL, vp = NULL){
	
		bpgt <- gTree(data = data, at = at, height = height, box.color = box.color, 
			median.color = median.color, show.w = show.w, name = name, gp = gp, vp = vp, 
			cl="boxplot")
        bpgt
}

# Utility for updating a boxplot grob
setBoxplotGrob <- function(bpgt){
	data         <- bpgt$data
	at           <- bpgt$at
	height       <- bpgt$height
	box.color    <- bpgt$box.color
	median.color <- bpgt$median.color
	show.w       <- bpgt$show.w
  
	whisker.low <- whisker.high <- boxes <- medians <- NULL
	
	x <- fivenum(data)
	
	if (show.w) {
		whisker.low <- segmentsGrob(x0 = unit(x[1], "native"), y0 = at, 
									x1 = unit(x[2], "native"), y1 = at, 
									gp = gpar(col = box.color), name = "whisker.low")	
		whisker.high <- segmentsGrob(x0 = unit(x[4], "native"), y0 = at, 
									x1 = unit(x[5], "native"), y1 = at, 
									gp = gpar(col = box.color), name = "whisker.high")
	}
	
	box <- rectGrob(x = unit(x[2], "native"), y = at, width = unit(x[4] - x[2], "native"), 
					height = height, just = "left", gp = gpar(col = box.color), name = "box")
					
	pad = convertHeight(unit(as.numeric(height)/2, attr(height, "unit")), "inches")			
	median.line <- segmentsGrob(x0 = unit(x[3], "native"), y0 = at - pad, 
								x1 = unit(x[3], "native"), y1 = at + pad, 
								gp = par(col = median.color), name = "median.line")
  	
  	bpgt = setChildren(bpgt, gList(whisker.low, whisker.high, box, median.line))
  	x
}


drawDetails.boxplot = function(x, recording){
  x = setBoxplotGrob(x)
  for (i in childNames(x)) grid.draw(getGrob(x, i))
}

## notes by Vivian
## change the default settings
## output: gTree() (boxplot grob)
editDetails.bxp = function(x, spec){ 
  x = bxpGrob(x$x5, x$at, x$width, x$cols, x$horiz, x$show.w,x$name, x$gp, x$vp)
  x
}

validDetails.bxp = function(x){
  if(class(x$x5) %in% c("integer", "numeric")){
    if(length(x$x5)!=5)
      stop("length of x5 must be 5")
  } else if(class(x$x5)=="matrix"){
    if(ncol(x$x5)!=5)
      stop("number of colume of x5 must be 5")
  } else {
    stop("x5 must be integer, numeric or matrix")  
  }
  
  if(!(any(class(x$at) %in% "unit") & any(class(x$width) %in% "unit")))
    stop("at and width must be unit object")  

  if("unit.arithmetic" %in% class(x$at))
    stop("at is unit.arithmetic")

  if("unit.arithmetic" %in% class(x$width))
    stop("width is unit.arithmetic")
    
  if(length(x$cols)!=3)
    stop("length of cols must be 3")
  
  if(!is.logical(x$horiz))
    stop("horiz must be logical")
  x
}

grid.bxp.example = function(){
  vp = viewport(width=unit(0.8, "npc"), height=unit(0.8, "npc"),
                xscale=c(-10, 10), yscale=c(-10, 10))
  pushViewport(vp)
  grid.rect()
  grid.xaxis()
  grid.yaxis()
  ## Notes by Vivian
  ## Tukey's five number summary (minimum, lower-hinge, median, upper-hinge, maximum) 
  x = fivenum(rnorm(100, 0, 3))
  col = c(rgb(1, 0, 0, alpha=0.3), rgb(1, 0, 0, alpha=0.3),
          rgb(1, 0, 0, alpha=0.3))
  at = unit(0.5, "npc")
  grid.bxp(x, at=at, width=unit(5, "native"), col,
           horiz=TRUE, name="bxpExample", gp=gpar(lwd=3))
}
#grid.bxp.example()