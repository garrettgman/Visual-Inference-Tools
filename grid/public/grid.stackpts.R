##############################################################################
## Date     : 2011-02-23
## Author   : Danny Chang
## Type     : grid drawing object and functions template
## Usage    : stack points
## Note     : not support horizontal stack yet
##############################################################################

grid.stackpts = function(...){
  grid.draw(stackptsGrob(...))
}

stackptsGrob = function(x, size.pts, at=unit(0, "npc"), wrange=range(x),
                        stack.height=unit(1, "npc"), horiz=TRUE, method=1,shape=1,
                        show=TRUE,name=NULL, gp=gpar(), vp=NULL){
  ## define stackpts grob
  ##
  ## Args:  
  ##  x             : data, numeric vecotr
  ##  size.pts      : size of circle point, unit object
  ##  at            : bottom line of stacking points, unit object
  ##  wrange        : x range
  ##  stack.height  : stacking height, unit object
  ##  horiz         : horizontal or verticle, logocal
  ##  method        : stacking method, default 1
  ##  show          : index of point want to show, default TRUE for all
  ##
  ## Returns:
  ##  grob  
  igt = gTree(x=x, size.pts=size.pts, at=at, stack.height=stack.height,
              wrange=wrange, horiz=horiz, method=method, shape=shape, show=show,
              name=name, gp=gp, vp=vp,
              cl="stackpts")
}

drawDetails.stackpts = function(x, recording){
  cpt = setStackptsGrob(x)
  grid.draw(cpt)
}

setStackptsGrob = function(x){
  if(all(is.logical(x$show)))
    idx = rep(x$show, length.out=length(x$x))
  else
    idx = x$show
  cpt = gList(NULL)
  if(any(idx>0)){
    w       = convertWidth(x$size.pts, "native", valueOnly=TRUE)*2
    v       = convertHeight(x$size.pts, "native", valueOnly=TRUE)*2
    #at     = convertAriUnit(x$at, convertY, "native")
    #vmax   = convertAriUnit(at+x$stack.height, convertHeight, "native")
    at      = convertY(x$at, "native")
    vmax    = convertHeight(at+x$stack.height, "native")
    vrange  = as.numeric(unit.c(at, vmax))

    if(x$shape ==1)
      p     = stackpts(x$x[idx], w, v, vrange, x$wrange, 
                      method=x$method,
                      horiz=x$horiz)
    else{
      p     = stackpts(x$x, w, v, vrange, x$wrange, method=x$method,
                        horiz=x$horiz)[idx]
    }
    cpt     = circleptsGrob(p$x, p$y, x$size.pts, name="stackCircle")
  }
  cpt
}

editDetails.stackpts = function(x, spec){
  x = stackptsGrob(x$x, x$size.pts, x$at, x$wrange, x$stack.height, x$horiz,
                   x$method, x$shape, x$show,x$name, x$gp, x$vp)
  x
}

validDetails.stackpts = function(x){
  if(!is.unit(x$size.pts))
    stop("size.pts must be a unit")
  
  if(!is.unit(x$at))
    stop("at must be a unit")
  
  if(!is.unit(x$stack.height))
    stop("stack.height must be a unit")
  
  x
}

grid.stackpts.example = function(){
  vp = viewport(width=unit(0.8, "npc"), height=unit(0.8, "npc"),
                xscale=c(-10, 10), yscale=c(-10, 10))
  pushViewport(vp)
  grid.rect()
  grid.xaxis()
  grid.yaxis()
  x = rnorm(1000, 0, 3)
  size.pts=unit(0.1, "inches")
  grid.stackpts(x, size.pts, name="test", gp=gpar(fill=rgb(1, 0, 0, 0.5)))
}
# grid.stackpts.example()
# traceback()
# grid.ls()