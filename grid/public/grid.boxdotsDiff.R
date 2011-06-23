##############################################################################
## Date     : 2011-03-01
## Author   : Danny Chang
## Type     : grid drawing object and functions
## Usage    : draw boxdots with difference bar - binary group
##############################################################################

grid.boxdotsDiff = function(...){
  grid.draw(boxdotsDiffGrob(...))
}

boxdotsDiffGrob = function(x, group, pad=unit(0.25, "npc"), arrow=NULL,
                           stack.height=unit(0.1, "npc"),
                           size.pts=unit(0.05, "inches"), wrange=range(x),
                           diffFun=median, show=rep(TRUE, 3), show.box=TRUE, show.pts=TRUE, show.w=TRUE,
                           gp.pts.x=gpar(), gp.pts.group1=gpar(),
                           gp.pts.group2=gpar(), gp.box.x=gpar(),
                           gp.box.group1=gpar(), gp.box.group2=gpar(),
                           col.box.group1=rep("black", 3),
                           col.box.group2=rep("black", 3),
                           gp.bar=gpar(), gp.indbar=gpar(),
                           name=NULL, gp=gpar(), vp=NULL){
  ## define boxdotsDiff grob
  ##
  ## Args:
  ##  x               : dataset, a numeric vector
  ##  group           : grouping variable, a factor
  ##  pad             : padding distance of top/bottom boxdot, a unit object
  ##  arrow           : arrow object, see ?arrow
  ##  stack.height    : stacking height, a unit object
  ##  size.pts        : size of points, a unit object
  ##  wrange          : range of x-axis, a numeric vector with length 2
  ##  diffFUN         : difference function, default median
  ##  show            : show boxdots or not, a logical vector with length 3
  ##                    c(show.x, show.group1, show.group2)
  ##  gp.pts.x        : gpar for x points
  ##  gp.pts.group1   : gpar for group1 points
  ##  gp.pts.group2   : gpar for group2 points
  ##  gp.box.x        : gpar for x box, not used
  ##  gp.box.group1   : gpar for group1 box
  ##  gp.box.group2   : gpar for group2 box
  ##  col.box.group1  : color for group1 box, a vector with length 3
  ##  col.box.group2  : color for group2 box, a vector with length 3
  ##  gp.bar          : gpar for arrow bar
  ##  gp.indbar       : gpar for indicate bar
  ##
  ## Returns:
  ##  grob
  
  igt = gTree(x=x, group=group, pad=pad, arrow=arrow, stack.height=stack.height,
              size.pts=size.pts, wrange=wrange, diffFun=diffFun, show=show, show.pts=show.pts,
              show.box=show.box, show.w=show.w,
              gp.pts.x=gp.pts.x, gp.pts.group1=gp.pts.group1,
              gp.pts.group2=gp.pts.group2, gp.box.x=gp.box.x,
              gp.box.group1=gp.box.group1, gp.box.group2=gp.box.group2,
              col.box.group1=col.box.group1, col.box.group2=col.box.group2,
              gp.bar=gp.bar, gp.indbar=gp.indbar,
              gp=gp, name=name, vp=vp, cl="boxdotsDiff")
  igt
}

drawDetails.boxdotsDiff = function(x, recording){
  x = setBoxdotsDiffGrob(x)
  for (i in childNames(x)) grid.draw(getGrob(x, i))
}

setBoxdotsDiffGrob = function(x){
  data            = x$x
  group           = x$group
  pad             = x$pad
  arrow           = x$arrow
  stack.height    = x$stack.height
  size.pts        = x$size.pts
  wrange          = x$wrange
  diffFun         = x$diffFun
  show            = x$show
  show.pts        = x$show.pts
  show.box        = x$show.box
  show.w          = x$show.w
  gp.pts.x        = x$gp.pts.x
  gp.pts.group1   = x$gp.pts.group1
  gp.pts.group2   = x$gp.pts.group2
  gp.box.x        = x$gp.box.x
  gp.box.group1   = x$gp.box.group1
  gp.box.group2   = x$gp.box.group2
  col.box.group1  = x$col.box.group1
  col.box.group2  = x$col.box.group2
  gp.bar          = x$gp.bar
  gp.indbar       = x$gp.indbar
              
  xgb = g1gb = g2gb = arrowgb = indgb= NULL
  show.x  = show[1]
  show.g1 = show[2]
  show.g2 = show[3]
  pad.box = unit(0, "inches")
  pad     = convertHeight(pad, "inches")
  group.idx = (as.numeric(group)==1)
  at        = unit(0.5, "npc")  
  at1       = convertY(at+pad, "npc")
  at2       = convertY(at-pad, "npc")

  if(show.x){   
    xgb = boxdotGrob(x=data, at=at, stack.height=stack.height, pad.box=pad.box,
                     size.pts=size.pts, show.box=FALSE, gp.pts=gp.pts.x,
                     gp.box=gp.box.x, name="x")
  }
  
  if(show.g1){
    g1gb = boxdotGrob(x=data[group.idx], at=at1, stack.height=stack.height,
                      pad.box=pad.box, size.pts=size.pts, show.pts=show.pts,show.box=show.box, show.w=show.w,
                      gp.pts=gp.pts.group1, gp.box=gp.box.group1,
                      col.box=col.box.group1, name="group1bxp")
  }
  
  if(show.g2){
    g2gb = boxdotGrob(x=data[!group.idx], at=at2, stack.height=stack.height,
                      pad.box=pad.box, size.pts=size.pts, show.pts=show.pts,show.box=show.box, show.w=show.w,
                      gp.pts=gp.pts.group2, gp.box=gp.box.group2,
                      col.box=col.box.group2, name="group2bxp")
  }
  
  if(!is.null(arrow)){
    diff.g1 = unit(diffFun(data[group.idx]), "native")
    diff.g2 = unit(diffFun(data[!group.idx]), "native")
    
    arrowgb = segmentsGrob(x0=diff.g1, y0=at, x1=diff.g2, y1=at,
                           arrow=arrow, name="diffbar", gp=gp.bar)
    
    x0 = unit.c(diff.g1, diff.g2)
    x1 = unit.c(diff.g1, diff.g2)
    y0 = unit.c(at,      at)
    y1 = unit.c(at1,     at2)
    indgb   = segmentsGrob(x0=x0, y0=y0, x1=x1, y1=y1,
                           name="indicate", gp=gp.indbar)
  }
 

  x = setChildren(x, gList(xgb, indgb, g1gb, g2gb, arrowgb))
  x
}

editDetails.boxdotsDiff = function(x, spec){
  x = boxdotsDiffGrob(x$x, x$group, x$pad, x$arrow, x$stack.height, x$size.pts,
                      x$wrange, x$diffFun, x$show, x$show.box, x$show.pts, x$show.w,
                      x$gp.pts.x, x$gp.pts.group1, x$gp.pts.group2,
                      x$gp.box.x, x$gp.box.group1, x$gp.box.group2,
                      x$col.box.group1, x$col.box.group2, x$gp.bar, x$gp.indbar,
                      gp=x$gp, name=x$name, vp=x$vp)
  x
}

validDetails.boxdotsDiff = function(x){
  if(!is.factor(x$group))
    stop("group is not factor")
  
  if(length(levels(x$group))!=2)
    stop("only support binary groups")
  
  if(!is.unit(x$pad))
    stop("pad must be unit")
 
  if(!(is.null(x$arrow)|class(x$arrow)=="arrow"))
    stop("arrow must be NULL or arrow object, see ?arrow")

  if(!is.unit(x$stack.height))
    stop("stack.height must be unit")

  if(!is.unit(x$size.pts))
    stop("size.pts must be unit")

  if(length(x$wrange)!=2)
    stop("length of wrange must be 2")

  if(length(x$col.box.group1)!=3)
    stop("length of col.box.group1 must be 3")

  if(length(x$col.box.group2)!=3)
    stop("length of col.box.group2 must be 3")    
    
  if("unit.arithmetic" %in% class(x$at))
    stop("at is unit.arithmetic")
    
  if("unit.arithmetic" %in% class(x$pad))
    stop("pad is unit.arithmetic")

  if("unit.arithmetic" %in% class(x$stack.height))
    stop("stack.height is unit.arithmetic")
  
  if("unit.arithmetic" %in% class(x$size.pts))
    stop("size.pts is unit.arithmetic")
  x
}

grid.boxdotsDiff.example = function(){
  vp = viewport(#y=unit(0.75, "npc"),
                width=unit(0.8, "npc"), height=unit(0.8, "npc"),
                xscale=c(-10, 10), yscale=c(-10, 10))
  pushViewport(vp)
  grid.rect()
  grid.xaxis()
  grid.yaxis()
  x = rnorm(100, 0, 4)
  g = factor(sample(c("A", "B"), size=100, replace=TRUE))
  col.box = rep(rgb(0, 0, 0, 0.3), 3)
  arr = arrow(angle=30, length=unit(0.1, "inches"), ends="first", type="open")
  grid.boxdotsDiff(x=x, group=g, arrow=arr,
                   pad=unit(0.25, "npc"), 
                   stack.height=unit(1, "native"),
                   show = c(FALSE, TRUE, TRUE),
                   gp.pts.x=gpar(col="black"),
                   gp.pts.group1=gpar(col="blue"),
                   gp.pts.group2=gpar(col="red"),
                   gp.box.group1=gpar(lwd=2),
                   gp.box.group2=gpar(lwd=2),
                   col.box.group1=col.box,
                   gp.bar=gpar(lwd=3, col="red"),
                   name="test")
                   
  
}

# grid.boxdotsDiff.example()
# traceback()
# # # grid.ls()
# # for(i in seq(0, 7.5, length.out=10))
  # # grid.edit("test", pad=unit(i, "native"))
# # traceback()
# for(i in 1:20){
  # g = factor(sample(c("A", "B"), size=100, replace=TRUE))
  # grid.edit("test", group=g)
# }