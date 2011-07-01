##############################################################################
## Date     : 2011-07-01
## Author   : Garrett Grolemund
## Type     : grid drawing object and functions
## Usage    : grid version ghost boxplots. Intended to draw either outline or bar
##############################################################################

grid.bxp = function(...){
  grid.draw(ghostbxpGrob(...))
}

ghostbxpGrob = function(x5, at, width, cols=rep("black", 3), horiz=TRUE, show.box=TRUE, 
	show.center=TRUE, show.w=TRUE, name=NULL, gp=NULL, vp=NULL){
  ## make boxplot grob
  ##
  ## Args:
  ##  x5       : five basic value for boxplot, can be vector or matrix
  ##             draw ghost if x5 is matrix
  ##  at       : location for the boxplot, a unit object
  ##             y-location if horiz is TRUE
  ##             x-location if horiz is FALSE
  ##  width    : box(rectangle) width, a unit object
  ##  cols     : color vector with length 3, define colors in boxplot
  ##             for boxcol, medcol whiskercol
  ##
  ## Returns:
  ##  boxplot grob
  
  ## notes by Vivian
  ## implicit graphical context (default settings)
  igt = gTree(x5=x5, at=at, width=width, cols=cols, horiz=horiz, show.box=show.box, 
  	show.center=show.center, show.w=show.w, gp=gp, name=name, vp=vp, cl="ghostbxp")
  igt
}

setghostBxpGrob = function(x){
  x5    = x$x5
  at    = x$at
  width = x$width
  cols  = x$cols
  horiz = x$horiz
  show.box = x$show.box
  show.center = x$show.center
  show.w = x$show.w
  whisker.low=whisker.high=boxes=medians=NULL
  
  if(!is.matrix(x5)) x5 = matrix(x5, nrow=1)
  n = nrow(x5)
  
  ## notes by Vivian
  ## cols = {boxcol,medcol,whiskercol}
  boxcol      = cols[1]
  medcol      = cols[2]
  whiskercol  = cols[3]
  at          = rep(at, length=n)
  width       = rep(width, length=n)
  gname       = c("wskrlow", "wskrhigh", "box", "medbar")
  
  ## use rectlinesGrob to support transprant color in Windows
  ## change back to segmentsGrob when Paul fix it
  lineFun     = rectlinesGrob
   
  if(horiz){  
    if(show.w){
      ## left whiskers
      whisker.low = rectlinesGrob(x0=unit(x5[,1], "native"), y0=at,
                                  x1=unit(x5[,2], "native"), y1=at,
                                  gp=gpar(col=whiskercol), name=gname[1])
      ## right whiskers
      whisker.high = rectlinesGrob(x0=unit(x5[,4], "native"), y0=at,
                                   x1=unit(x5[,5], "native"), y1=at,
                                   gp=gpar(col=whiskercol), name=gname[2])
    }
    ## boxes
    if(show.box){
    xl = unit(x5[,2], "native")
    w = unit(x5[,4]-x5[,2], "native")
    boxes = rectGrob(xl, at, w, width, just="left",
                     gp=gpar(col=boxcol, fill="transparent"), name="box")
    }
    if(show.center){
    ## medians
    pad = convertHeight(unit(as.numeric(width)/2, attr(width, "unit")), "inches")
    y0 = at-pad
    y1 = at+pad
    medians = rectlinesGrob(x0=unit(x5[,3], "native"), y0=y0,
                            x1=unit(x5[,3], "native"), y1=y1,
                            gp=gpar(col=medcol), name="medbar")
    }
  } else {
    
  }
  
  x = setChildren(x, gList(whisker.low, whisker.high, boxes, medians))
  x
}

drawDetails.ghostbxp = function(x, recording){
  x = setghostBxpGrob(x)
  for (i in childNames(x)) grid.draw(getGrob(x, i))
}

## notes by Vivian
## change the default settings
## output: gTree() (boxplot grob)
editDetails.ghostbxp = function(x, spec){ 
  x = bxpGrob(x$x5, x$at, x$width, x$cols, x$horiz, x$show.w, x$show.box, x$show.center, x$name, x$gp, x$vp)
  x
}

validDetails.ghostbxp = function(x){
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
    
  if(!(is.logical(x$show.w)&is.logical(x$show.box)&is.logical(x$show.center)))
    stop("show.w and show.box and show.center must be logical")
        
  x
}

# grid.bxp.example = function(){
#  vp = viewport(width=unit(0.8, "npc"), height=unit(0.8, "npc"),
#                xscale=c(-10, 10), yscale=c(-10, 10))
#  pushViewport(vp)
#  grid.rect()
#  grid.xaxis()
#  grid.yaxis()
  ## Notes by Vivian
  ## Tukey's five number summary (minimum, lower-hinge, median, upper-hinge, maximum) 
#  x = fivenum(rnorm(100, 0, 3))
#  col = c(rgb(1, 0, 0, alpha=0.3), rgb(1, 0, 0, alpha=0.3),
#          rgb(1, 0, 0, alpha=0.3))
#  at = unit(0.5, "npc")
#  grid.bxp(x, at=at, width=unit(5, "native"), col,
#           horiz=TRUE, name="bxpExample", gp=gpar(lwd=3))
#}
#grid.bxp.example()