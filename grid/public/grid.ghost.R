##############################################################################
## Date     : 2011-05-13
## Author   : Vivian Li
## Type     : grid drawing object and functions
## Usage    : ghost boxes for bootstrap model
##############################################################################

grid.ghost = function(...){ # method to draw simultaneously draw and create ghostGrob
  grid.draw(ghostGrob(...))
}

# constructs/defines ghostGrobs. ghostGrobs are the ghost images of previous boxplots. Like boxdotGrobs, they are gTrees that don't inherit children until updated with a set method.
ghostGrob = function(data,tab=list(),diffFun=median,gb="boxdotsDiffGrob",gbfmt=NULL,name=NULL, vp=NULL){
  igt = gTree(data=data,tab=tab,name=name, diffFun=diffFun, gb=gb, gbfmt=gbfmt, vp=vp, cl="ghost")
  igt
}

drawDetails.ghost = function(x, recording){
  x = setghostGrob(x) # adds children
  for (i in childNames(x)) grid.draw(getGrob(x, i)) # draws children
}

setghostGrob = function(x){
  tab      = x$tab  # rows in the bootstrap sample
  data     = x$data # original data
  diffFun  = x$diffFun # mean or median
  
  numBox   = length(tab) # size of sample
  ghost    = vector("list",numBox) 
  gbfmt    = x$gbfmt # format?
  
  if(!is.null(x$vp)) gbfmt$vp = x$vp # if a vp has been assigned, save it to the gbfmt vp
  index    = 1
  for(i in tab){ # once for each previous bootstrap
    x.bt        = data[i,] # grab the rows of data in the current sample
    
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
    ghost[[index]] = do.call(x$gb,gbfmt) # each ghost is a x$gb grob, by default that's a boxdotsDiffGrob
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
