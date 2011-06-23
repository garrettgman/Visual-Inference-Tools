##############################################################################
## Date     : 2011-05-13
## Author   : Vivian Li
## Type     : grid drawing object and functions
## Usage    : ghost boxes for bootstrap model
##############################################################################

grid.ghostBox = function(...){
  grid.draw(ghostBoxGrob(...))
}

ghostBoxGrob = function(data,tab=list(),diffFun=median,name=NULL, vp=NULL){
  
  igt = gTree(data=data,tab=tab,name=name, diffFun=diffFun, vp=vp, cl="ghostBox")
  igt
}

drawDetails.ghostBox = function(x, recording){
  x = setghostBoxGrob(x)
  for (i in childNames(x)) grid.draw(getGrob(x, i))
}

setghostBoxGrob = function(x){
  
  tab      = x$tab
  data     = x$data
  diffFun  = x$diffFun
  
  numBox   = length(tab)
  ghostBox = vector("list",numBox)
  gbfmt    = initArgsBootstrap1Ghost(data[,1],data[,2],diffFun)
  
  if(!is.null(x$vp)) gbfmt$vp = x$vp

  index    = 1
  for(i in tab){
    x.bt        = data[i,]
    gbfmt$x     = x.bt[,1]
    gbfmt$group = x.bt[,2]
    gbfmt$name  = paste("GhostBox #",index,sep="")
    ghostBox[[index]] = do.call("boxdotsDiffGrob",gbfmt)
    index = index + 1
  }
  x = setChildren(x, do.call("gList",ghostBox))
  x
}

editDetails.ghostBox = function(x, spec){

  x = ghostBoxGrob(data=x$data,tab=x$tab,name=x$name, diffFun=x$diffFun,vp=x$vp)
  x
}

validDetails.ghostBox = function(x){
  x
}
