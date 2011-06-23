##############################################################################
## Date     : 2011-02-24
## Author   : Danny Chang
## Type     : grid drawing object and functions template
## Usage    : rectangle line, draw very thin rectangles act like line segments
## Note     : transparent color for grid.lines is failed to display under
##            windows, but grid.rect is working fine.
##
##            only support verticle and horizontal line since rect nature
##            behavior            
##
##            not support arrow yet
##############################################################################

grid.rectlines = function(...){
  grid.draw(rectlinesGrob(...))
}

## Notes by Vivian
## This function has been called in grid.bxp.R while drawing a box
##
## args:
##
## (x0,y0):   original position
## (x1,y1):   end position
rectlinesGrob = function(x0=unit(0, "npc"), y0=unit(0, "npc"),
                         x1=unit(1, "npc"), y1=unit(1, "npc"),
                         default.units="npc", 
                         arrow=NULL, name=NULL, gp=gpar(), vp=NULL){
                         
  ## Args: see ?grid.segments  
  igt = gTree(x0=x0, y0=y0, x1=x1, y1=y1, arrow=arrow,
              default.units=default.units,
              children=makeRectlinesGrob(x0, y0, x1, y1, default.units, arrow),
              name=name, gp=gp, vp=vp, cl="rectlines")
  igt
}

makeRectlinesGrob = function(x0, y0, x1, y1, default.units, arrow){
  if(!is.unit(x0))
    x0 <- unit(x0, default.units)
  if(!is.unit(x1)) 
    x1 <- unit(x1, default.units)
  if(!is.unit(y0)) 
    y0 <- unit(y0, default.units)
  if(!is.unit(y1)) 
    y1 <- unit(y1, default.units)
  
  n = length(x0)
  gl = list()
  length(gl) = n
  for(i in 1:n){
    w = x1[i]-x0[i]
    h = y1[i]-y0[i]
    child.name = paste("rectlineChild", i, sep="")
    gl[[i]] = rectGrob(x=x0[i], y=y0[i], width=w, height=h, hjust=0, vjust=0,
                        name=child.name)
  }
  do.call("gList", gl)
}

editDetails.rectlines = function(x, spec){
  x = rectlinesGrob(x$x0, x$y0, x$x1, x$y1, x$default.units, x$arrow,
                    x$name, x$gp, x$vp)
  x  
}

validDetails.rectlines = function(x){
  n = length(x$x0)
  if(!all(c(length(x$y0), length(x$x1), length(x$y1))==n))
    stop("x0, x1, y0, y1 must have same length")

  # x0 = as.numeric(x$x0)
  # x1 = as.numeric(x$x1)
  # y0 = as.numeric(x$y0)
  # y1 = as.numeric(x$y1)
    
  # if(!all(x0==x1 | y0==y1))
    # stop("only support verticle and horizontal line")
    
  x
}

grid.rectlines.example = function(){
  vp = viewport(width=unit(0.8, "npc"), height=unit(0.8, "npc"),
                xscale=c(-10, 10), yscale=c(-10, 10))
  pushViewport(vp)
  grid.rect()
  grid.xaxis()
  grid.yaxis()
  
  x0 = c(0.1, 0.5)
  y0 = c(0.5, 0.1)
  x1 = c(0.9, 0.5)
  y1 = c(0.5, 0.9)  
  
  grid.rectlines(x0, y0, x1, y1, gp=gpar(col=rgb(0, 0, 1, 0.3), lwd=8),
                 name="test")
}