##############################################################################
## Date     : 2011-05-27
## Author   : Vivian Li
## Type     : grid drawing object and functions
## Usage    : a bar for mean/median or something else
## Note     : haven't tested for horizontal bar yet
##############################################################################

grid.meanBar = function(...){
  grid.draw(meanBarGrob(...))
}

meanBarGrob = function(xat,yat,len,pad=0.2,horiz=FALSE,show=TRUE,name=NULL,gp=gpar(lwd=3),vp=NULL){
  ####################################################
  ####################################################
  igt = gTree(xat=xat,yat=yat,len=len,pad=pad,horiz=horiz,show=show,name=name,gp=gp,vp=vp,cl="meanBar")
  igt
}

drawDetails.meanBar = function(x, recording){
  x = setMeanBarGrob(x)
  if(x$show){
    for (i in childNames(x)) grid.draw(getGrob(x, i))
  }
}
setMeanBarGrob = function(x){
  if(x$horiz){
    y0 = y1 = x$yat
    if(length(x$yat)==1){
      x0 = x1 = x$xat
    }
    else if(length(x$yat)==2){
      #seperate to two positions by pad
      x0 = x1 = unit(as.numeric(x$xat)+c(-1,1)*x$pad,attr(x$xat,"unit"))
    }
    x0 = x0 - unit(as.numeric(x$len)/2,attr(x$len,"unit"))
    x1 = x1 + unit(as.numeric(x$len)/2,attr(x$len,"unit"))
  }
  else{
    x0 = x1 = x$xat
    if(length(x$xat)==1){
      y0 = y1 = x$yat
    }
    else if(length(x$xat)==2){
      #seperate to two positions by pad
      y0 = y1 = unit(as.numeric(x$yat)+c(1,-1)*x$pad,attr(x$yat,"unit"))
    }
    y0 = y0 - unit(as.numeric(x$len)/2,attr(x$len,"unit"))
    y1 = y1 + unit(as.numeric(x$len)/2,attr(x$len,"unit"))
  }

  numMeanBar = length(x0)
  bars       = vector("list",numMeanBar)
  
  if(numMeanBar>0){
    for(i in 1:numMeanBar){
      #bars[[i]] = segmentsGrob(x0=x0[i],y0=y0[i],x1=x1[i],y1=y1[i],
      #                         gp=x$gp, name=paste("meanBar #",i,sep=""))
      bars[[i]] = rectlinesGrob(x0=x0[i],y0=y0[i],x1=x1[i],y1=y1[i],
                               gp=x$gp, name=paste("meanBar #",i,sep=""))
    }
  }

  x = setChildren(x, do.call("gList",bars))
  x
}
editDetails.meanBar = function(x, spec){
  x = meanBarGrob(xat=x$xat,yat=x$yat,len=x$len,pad=x$pad,horiz=x$horiz,show=x$show,name=x$name,gp=x$gp,vp=x$vp)
  x
}

validDetails.meanBar = function(x){
  if(!is.unit(x$x) || !is.unit(x$y) || !is.unit(x$len))
    stop("x,y and len must be the unit objects!")
  x
}


