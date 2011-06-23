##############################################################################
## Date     : 2011-05-20
## Author   : Vivian Li
## Type     : grid drawing object and functions
## Usage    : arrows point out the selections from the original sample
##############################################################################


grid.pointer = function(...){
  grid.draw(pointerGrob(...))
}

pointerGrob = function(from,to,pad=unit(1.2,"lines"),show=FALSE,fmt=NULL,name=NULL,vp=NULL){
  ####################################################
  # data : original data
  # index: bootstrapped index
  # name : name of the grob
  # vp   : viewport
  ####################################################
  igt = gTree(from=from,to=to,pad=pad,show=show,fmt=fmt,name=name,vp=vp,cl="pointer")
  igt
}

drawDetails.pointer = function(x, recording){
  x = setPointerGrob(x)
  for (i in childNames(x)) grid.draw(getGrob(x, i))
}

setPointerGrob = function(x){
  from = x$from
  to   = x$to
  pad  = x$pad
  
  if(length(pad)==1)
    pad = rep(pad,2)
  
  if(all(is.logical(x$show)))
    idx = rep(x$show,length.out=length(from$x))
  else
    idx = x$show
  
  pter.x = list(from=from$x[idx],to=to$x[idx])
  pter.y = list(from=from$y[idx],to=to$y[idx])
  
  numArrow = length(pter.x$from)
  pointers = vector("list",numArrow)
  pfmt     = x$fmt

  if(!is.null(x$vp)) pfmt$vp = x$vp
  
  if(numArrow>0){
    for(i in 1:numArrow){
      pointers[[i]] = segmentsGrob(name=paste("pointer #",i,sep=""),
                                   x0=pter.x$from[i]+pad[1],
                                   x1=pter.x$to[i]-pad[2],
                                   y0=pter.y$from[i],y1=pter.y$to[i],
                                   gp=pfmt$gp,
                                   arrow=pfmt$arr)
    }
  }
  
  x = setChildren(x, do.call("gList",pointers))
  x
}

editDetails.pointer = function(x, spec){
  x = pointerGrob(from=x$from,to=x$to,pad=x$pad,show=x$show,fmt=x$fmt,name=x$name,vp=x$vp)
  x
}

validDetails.pointer = function(x){
  if(length(x$from) != length(x$to))
    stop("From and to must have same length!")
  x
}