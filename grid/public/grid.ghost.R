##############################################################################
## Date     : 2011-05-13
## Author   : Vivian Li
## Type     : grid drawing object and functions
## Usage    : ghost boxes for bootstrap model
##############################################################################

grid.ghost = function(...){
  grid.draw(ghostGrob(...))
}

ghostGrob = function(data,tab=list(),diffFun=median,gb="boxdotsDiffGrob",gbfmt=NULL,name=NULL, vp=NULL){
  igt = gTree(data=data,tab=tab,name=name, diffFun=diffFun, gb=gb, gbfmt=gbfmt, vp=vp, cl="ghost")
  igt
}

drawDetails.ghost = function(x, recording){
  x = setghostGrob(x)
  for (i in childNames(x)) grid.draw(getGrob(x, i))
}

setghostGrob = function(x){
  tab      = x$tab
  data     = x$data
  diffFun  = x$diffFun
  
  numBox   = length(tab)
  ghost    = vector("list",numBox)
  gbfmt    = x$gbfmt
  
  if(!is.null(x$vp)) gbfmt$vp = x$vp
  index    = 1
  for(i in tab){
    x.bt        = data[i,]
    
    if(ncol(data)>1){
      # two sample case
      if(identical(x$diffFun, median)){
        gbfmt$x     = x.bt[,1]
        gbfmt$group = x.bt[,2]
      }
      else{
        gbfmt$xat = unit(tapply(x.bt[,1],x.bt[,2],x$diffFun),"native")
      }
    }
    else{
      # one sample case
      if(identical(x$diffFun, median)){
        gbfmt$x      = x.bt
      }
      else{
        gbfmt$xat    = unit(mean(x.bt),"native")
      }
    }
    gbfmt$name  = paste("ghost #",index,sep="")
    ghost[[index]] = do.call(x$gb,gbfmt)
    index = index + 1
  }
  x = setChildren(x, do.call("gList",ghost))
  x
}

editDetails.ghost = function(x, spec){
  x = ghostGrob(data=x$data,tab=x$tab,name=x$name,diffFun=x$diffFun,gb=x$gb, gbfmt=x$gbfmt,vp=x$vp)
  x
}

validDetails.ghost = function(x){
  x
}
