##############################################################################
## Date     : 2011-04-04
## Author   : Danny Chang
## Type     : grid drawing object and functions
## Usage    : draw boxdots with difference bar - multi-group
##############################################################################

grid.boxdotsDiffMulti = function(...){
  grid.draw(boxdotsDiffMultiGrob(...))
}

boxdotsDiffMultiGrob = function(x, group, pad=1, arrow=NULL, glab=NULL,
                                size.pts=unit(0.05, "inches"), wrange=range(x),
                                diffFun=median, show=TRUE, show.box=TRUE,
                                col.pts="black", gp.pts=gpar(), gp.box=gpar(),
                                col.box=rep("black", 3), gp.lab=gpar(),
                                gp.bar=gpar(), gp.indbar=gpar(),
                                name=NULL, gp=gpar(), vp=NULL){
  ## define boxdotsDiffMulti grob
  ##
  ## Args:
  ## Returns:
  ##  grob
  
  igt = gTree(x=x, group=group, pad=pad, arrow=arrow, glab=glab,
              size.pts=size.pts, wrange=wrange,
              diffFun=diffFun, show=show, show.box=show.box, col.pts=col.pts,
              gp.pts=gp.pts, gp.box=gp.box, col.box=col.box, gp.lab=gp.lab,
              gp.bar=gp.bar, gp.indbar=gp.indbar, gp=gp, name=name, vp=vp,
              cl="boxdotsDiffMulti")
  igt
}

drawDetails.boxdotsDiffMulti = function(x, recording){
  x = setBoxdotsDiffMultiGrob(x)
  for (i in childNames(x)) grid.draw(getGrob(x, i))
}

yat.boxdotsDiffMulti = function(pad, ngroup){
  d = seq(0.5+pad/2, 0.5-pad/2, length.out=2*ngroup+1)[c(FALSE, TRUE)]
  at = unit(d, "npc")
  at
}

stackHeight.boxdotsDiffMulti = function(ngroup){
  d  = yat.boxdotsDiffMulti(1, ngroup)
  unit(0.9*abs(unique(diff(d)))/2, "npc")
}

setBoxdotsDiffMultiGrob = function(x){
  data            = x$x
  group           = x$group
  pad             = x$pad
  arrow           = x$arrow
  glab            = x$glab
  size.pts        = x$size.pts
  wrange          = x$wrange
  diffFun         = x$diffFun
  show            = x$show
  show.box        = x$show.box
  col.pts         = x$col.pts
  gp.pts          = x$gp.pts
  gp.box          = x$gp.box
  col.box         = x$col.box
  gp.lab          = x$gp.lab
  gp.bar          = x$gp.bar
  gp.indbar       = x$gp.indbar

  n   = length(levels(group))
  pad.box = unit(0, "inches")
  col.pts = rep(col.pts, length.out=n)
  
  igt = replicate(4*n+1, NULL)
  if(show){
    at = yat.boxdotsDiffMulti(pad, n)
    stack.height = stackHeight.boxdotsDiffMulti(n)
    mg = tapply(data, group, diffFun)
    
    if(!is.null(glab)){
      stack.height = convertHeight(stack.height-unit(1.2, "lines"), "npc")
      for(i in 1:n){
        igt[[i]] = textGrob(glab[i], x=unit(mg[i], "native"),
                            y=at[i]-stack.height-unit(0.6, "lines"),
                            name=paste("glab", glab[i], sep=""),
                            gp=gp.lab)
      }
    }
    
    xg = split(data, group)
    for(i in 1:n){
      gp.pts$col = col.pts[i]
      igt[[n+i]] = boxdotGrob(x=xg[[i]], at=at[i],
                            stack.height=stack.height, pad.box=pad.box,
                            size.pts=size.pts, show.box=show.box,
                            gp.pts=gp.pts, gp.box=gp.box, col.box=col.box,
                            name=paste("boxdot", i, sep=""))
    }
    
    if(!is.null(arrow)){
      m = diffFun(data)
      igt[[4*n+1]] = segmentsGrob(x0=unit(m, "native"), y0=unit(0, "npc"),
                                  x1=unit(m, "native"), y1=unit(1, "npc"),
                                  gp=gp.indbar, name="indbar")      
      for(i in 1:n){
        igt[[2*n+i]] = segmentsGrob(x0=unit(m,  "native"), y0=at[i],
                                  x1=unit(mg[i], "native"), y1=at[i],
                                  gp=gp.bar, arrow=arrow,
                                  name=paste("diffbar", i, sep=""))
                                  
        if(!identical(diffFun, median)){
          igt[[3*n+i]] = segmentsGrob(x0=unit(mg[i],  "native"),
                                      y0=at[i]+stack.height*0.7,
                                      x1=unit(mg[i], "native"),
                                      y1=at[i]-stack.height*0.7,
                                      gp=gp.indbar,
                                      name=paste("meanBar", i, sep=""))
        }
      }
    }    
  }
  
  x = setChildren(x, do.call("gList", igt))
  x
}

editDetails.boxdotsDiffMulti = function(x, spec){
  x = boxdotsDiffMultiGrob(x=x$x, group=x$group, pad=x$pad, arrow=x$arrow,
                           glab=x$glab, size.pts=x$size.pts, wrange=x$wrange,
                           diffFun=x$diffFun, show=x$show, show.box=x$show.box,
                           col.pts=x$col.pts, gp.pts=x$gp.pts, gp.box=x$gp.box,
                           col.box=x$col.box, gp.lab=x$gp.lab, gp.bar=x$gp.bar,
                           gp.indbar=x$gp.indbar, gp=x$gp, name=x$name, vp=x$vp)
  x
}

validDetails.boxdotsDiffMulti = function(x){
  # if(!is.factor(x$group))
    # stop("group is not factor")
  
  # if(length(levels(x$group))!=2)
    # stop("only support binary groups")
  
  # if(!is.unit(x$pad))
    # stop("pad must be unit")
 
  # if(!(is.null(x$arrow)|class(x$arrow)=="arrow"))
    # stop("arrow must be NULL or arrow object, see ?arrow")

  # if(!is.unit(x$stack.height))
    # stop("stack.height must be unit")

  # if(!is.unit(x$size.pts))
    # stop("size.pts must be unit")

  # if(length(x$wrange)!=2)
    # stop("length of wrange must be 2")

  # if(length(x$col.box.group1)!=3)
    # stop("length of col.box.group1 must be 3")

  # if(length(x$col.box.group2)!=3)
    # stop("length of col.box.group2 must be 3")    
    
  # if("unit.arithmetic" %in% class(x$at))
    # stop("at is unit.arithmetic")
    
  # if("unit.arithmetic" %in% class(x$pad))
    # stop("pad is unit.arithmetic")

  # if("unit.arithmetic" %in% class(x$stack.height))
    # stop("stack.height is unit.arithmetic")
  
  # if("unit.arithmetic" %in% class(x$size.pts))
    # stop("size.pts is unit.arithmetic")
  x
}

grid.boxdotsDiffMulti.example = function(){
  x = c(rnorm(100, -10, 4), rnorm(100, 0, 5),
        rnorm(100, 3, 3), rnorm(100, 20, 10))
  g = factor(rep(c("A", "B", "C", "D"), each=100))
  # g = factor(sample(c("A", "B", "C"), size=length(x), replace=TRUE,
             # prob=c(0.6, 0.3, 0.1)))
  
  at = pretty(range(x))
  
  vp = viewport(width=unit(0.8, "npc"), height=unit(0.8, "npc"),
                xscale=range(at), yscale=c(-10, 10))
  pushViewport(vp)
  #grid.rect()
  grid.xaxis(at=at)
  #grid.yaxis()
  col.box = rep(rgb(0, 0, 0, 0.3), 3)
  arr = arrow(angle=30, length=unit(0.1, "inches"), ends="last", type="open")
  glab = levels(g)
  col.pts = topo.colors(4)
  grid.boxdotsDiffMulti(x=x, group=g, pad=1, arrow=arr, glab=glab,
                        size.pts=unit(0.04, "inches"), wrange=range(x),
                        show=TRUE, show.box=TRUE, diffFun=median,
                        col.pts=col.pts, gp.pts=gpar(), gp.box=gpar(lwd=2),
                        col.box=col.box, gp.lab=gpar(),
                        gp.bar=gpar(col="red", lwd=3), gp.indbar=gpar(lwd=3),
                        name="test")
}

# grid.boxdotsDiffMulti.example()

