##############################################################################
## Date     : 2011-07-01
## Author   : Garrett Grolemund
## Type     : grid drawing object and functions
## Usage    : ghost boxplot
## Note     : not support vertical boxdot yet
##############################################################################

grid.ghostboxdot = function(...){ # method for drawing the boxdotGrob
  grid.draw(ghostboxdotGrob(...))
}

ghostboxdotGrob = function(x, at, stack.height, pad.box,
                      width.box=stack.height, col.box=rep("black", 3),
                      wrange=range(x), horiz=TRUE, size.pts=unit(1, "char"),
                      show.pts=TRUE, show.box=TRUE, show.center = FALSE, show.w=TRUE,gp.pts=NULL, gp.box=NULL,
                      name=NULL, gp=NULL, vp=NULL){
  
  ## define boxdot grob
  ##
  ## Args:
  ##  x             : numeric vector
  ##  at            : bottom line of stacking points
  ##  stack.height  : stacking height
  ##  pad.box       : padding of the boxplot from "at"
  ##  width.box     : width of the box of boxplot
  ##  col.box       : color of boxplot, a vector with length 3
  ##  wrange        : y-range
  ##  horiz         : horizontal or not
  ##  size.pts      : size of points, a unit object
  ##  show.pts      : show points or not
  ##  show.box      : show outline of box or not
  ##  show.center   : show median/mean bar or not
  #   show.w        : 
  ##  gp.pts        : gpar for points
  ##  gp.box        : gpar for box
  ##
  ## Returns:
  ##  grob            
  
  # the boxdotGrob is a gTree whose children are only set when you update it. Its entirely defined by its class, "boxdot", and the class methods
  igt = gTree(x=x, at=at, stack.height=stack.height, pad.box=pad.box,
              width.box=width.box, col.box=col.box, wrange=wrange, horiz=horiz,
              size.pts=size.pts, show.pts=show.pts, show.box=show.box, show.center=show.center, show.w=show.w,
              gp.pts=gp.pts, gp.box=gp.box,
              name=name, gp=gp, vp=vp, cl="ghostboxdot")
  igt
}

drawDetails.ghostboxdot = function(x, recording){
  x = setghostBoxdotGrob(x) # add the necessary children to the boxdotGrob
  for (i in childNames(x)){
    grid.draw(getGrob(x, i)) # draw the children
  }
}

setghostBoxdotGrob = function(x){ # method for replacing or altering a boxdotGrob
  data          = x$x
  at            = x$at
  stack.height  = x$stack.height
  pad.box       = x$pad.box
  width.box     = x$width.box
  col.box       = x$col.box
  wrange        = x$wrange
  horiz         = x$horiz
  size.pts      = x$size.pts
  show.pts      = x$show.pts
  show.box      = x$show.box
  show.center   = x$show.center  
  gp.pts        = x$gp.pts
  gp.box        = x$gp.box
  show.w        = x$show.w

  n = length(data)
  show.pts = rep(show.pts, length.out=n) # show all of the points, or none of them
  pgb = NULL # clear name for points in a stackptsGrob
  boxgb = NULL # clear name for boxplot in a bxpGrob
  
  if(horiz){
    if(!all(!show.pts)){ # if any point is shown
    
      # ...build a stackptsGrob with the points shown
      pgb = stackptsGrob(data[show.pts], size.pts, at, wrange, stack.height,
                         horiz, name="pts", gp=gp.pts)
    }    
    if(show.box | show.center){ # if the box is shown
    
      # ...build a bxpGrob
      x5 = fivenum(data)
      pad.box   = convertHeight(pad.box, "inches")
      at.box    = convertY(at+pad.box, "npc")
      width.box = unit(as.numeric(width.box)*2, attr(width.box, "unit"))
      boxgb     = ghostbxpGrob(x5, at.box, width.box, cols=col.box, horiz=horiz, show.box = show.box, show.center=show.center, show.w=show.w,
                          name="bxp", gp=gp.box)
    }
  } else {
    stop("vertical plot is not yet implemented")
  }
  
  x = setChildren(x, gList(pgb, boxgb)) # make the points and boxplot children of the boxdotGrob
  x
}

# removes the children of x. Where does spec come in?
editDetails.ghostboxdot = function(x, spec){
  x = ghostboxdotGrob(x$x, x$at, x$stack.height, x$pad.box, x$width.box, x$col.box,
                 x$wrange, x$horiz, x$size.pts, x$show.pts, x$show.box, x$show.center, x$show.w,
                 x$gp.pts, x$gp.box, x$name, x$gp, x$vp)
  x
}

# method for catching errors in the boxdot arguments
validDetails.ghostboxdot = function(x){
  if(!is.unit(x$at))
    stop("at must be unit")

  if("unit.arithmetic" %in% class(x$at))
    stop("at is unit.arithmetic")

  if("unit.arithmetic" %in% class(x$pad.box))
    stop("pad.box is unit.arithmetic")    

  if(!is.unit(x$stack.height))
    stop("stack.height must be unit")

  if(!is.unit(x$pad.box))
    stop("stack.height must be unit")

  if(!is.unit(x$width.box))
    stop("width.box must be unit")

  if(!is.unit(x$size.pts))
    stop("size.pts must be unit")    

  if(length(x$col.box)!=3)
    stop("length of col.box must be 3")

  if(length(x$wrange)!=2)
    stop("length of wrange must be 2")

  if(!(is.logical(x$show.pts)&is.logical(x$show.box)&is.logical(x$show.center)))
    stop("show.pts and show.box and show.center must be logical")
  x
}

grid.ghostboxdot.example = function(){
  vp = viewport(width=unit(0.8, "npc"), height=unit(0.8, "npc"),
                xscale=c(-10, 10), yscale=c(-10, 10),name="test")
  pushViewport(vp)
  grid.rect()
  grid.xaxis()
  grid.yaxis()
  upViewport(1)
  x             = rnorm(100)
  at            = unit(2, "native")
  stack.height  = unit(2.5, "native")
  size.pts      = unit(0.05, "inches")
  pad.box       = unit(0, "inches")
  width.box     = unit(0.5, "npc")
  
  args = initArgsBootstrapData(x,median)
  args$vp = vpPath("test")
  gb  = do.call("boxdotGrob",args)
  grid.draw(gb)
}

# grid.boxdot.example()
# traceback()
# grid.ls()