## generate colours
barcols <- function(n, chroma=50, lum1=60, lum2=30,alpha=1) {
  if(alpha==1)
    hcl(h=seq(360/n,360,length=n), c=chroma, l=seq(lum1,lum2,length=n))
  else
    hcl(h=seq(360/n,360,length=n), c=chroma, l=seq(lum1,lum2,length=n),alpha=alpha)
}

getrefPoint = function(x, y, vpname){
  ## misc.getrePoint
  ## get the reference points between viewpots
  
  if(length(x)!=length(y))
    stop("length of x and y must be the same")
  
  n = length(x)
  x.new = y.new = unit(rep(0, n), "npc")
  for(i in 1:n){
    # "native": Locations and sizes are relative to the x- and y- scales for the current viewport
    ref = nullGrob(x=x[i], y=y[i], default.units="native", vp=vpname)
    x.new[i] = convertX(grobX(ref, 0), "npc")
    y.new[i] = convertY(grobY(ref, 0), "npc")
  }  
  list(x=x.new, y=y.new)
}

dialog = function(message, main, handler=NULL){
  window = gwindow(main, width=200, height=200)
  group  = ggroup(container  =  window)
  gimage("info",  dirname="stock",  size="dialog",  container=group)

  inner.group = ggroup(horizontal=FALSE,  container  =  group)
  glabel(message,  container=inner.group,  expand=TRUE)

  button.group = ggroup(container=inner.group)
  addSpring(button.group)
  gbutton("     OK     ",  handler=function(h,...) dispose(window),
          container=button.group)
    
  return()
}



convertAriUnit = function(x, funName, unitTo){
  ## convert arithmetic unit
  ## funName is unit convert function, such as convertY, convertX ... etc
  
  if("unit.arithmetic" %in% class(x)){
    u = convertAriUnit(x$arg1, funName, unitTo)
    v = eval(call(x$fname, as.numeric(u),
                           funName(x$arg2, unitTo, valueOnly=TRUE)))
    r = unit(v, unitTo)
  } else {
    r = funName(x, unitTo)
  }
  r
}

seq.unit = function(from.unit, to.unit, ...){
  ## sequence funciton for "unit" class
  
  if(attributes(from.unit)$unit != attributes(to.unit)$unit)
    stop("unit different, cannnot do sequence bewteen these two units")

  if(length(from.unit)!=1 || length(to.unit)!=1)
    stop("length of from.unit and to.unit must be 1")
 
  u = attributes(from.unit)$unit
  x = seq(as.numeric(from.unit), as.numeric(to.unit), ...)
  unit(x, u)
}

ciMNprop = function(x, level=0.95){
  ## confidence interval for Multinomial propertions
  
  n = sum(x)
  p = x/n
  se = sqrt(p*(1-p)/n)
  alpha = qnorm(0.5+level/2)
  upper = n*(p+alpha*se)
  lower = n*(p-alpha*se)
  list(upper=upper, lower=lower)
}

rounddf = function(x.df, digit, fmt="f"){
  ## round data.frame by digit
  
  isFactor = unlist(lapply(x.df, is.factor))
  
  fmt = paste("%.", digit, fmt, sep="")
  for(i in which(!isFactor))
    x.df[[i]] = sprintf(fmt, x.df[[i]])
  x.df
}

move2edge = function(p, intercept, slope, xrange, yrange){
  ## given straight line coeffs: intercept and slope,
  ## and range of x and range of y,
  ## if the point is out of boundry, move the point to the closest edge
  if(!isInRange.pts(p, xrange, yrange)){
    i = which.min(abs(p$x-xrange))
    z = pts(xrange[i], slope*xrange[i]+intercept)  
    j = which.min(abs(p$y-yrange))    
    z = c.pts(z, pts((yrange[j]-intercept)/slope, yrange[j]))
    z = z[isInRange.pts(z, xrange, yrange)]
    if(length(z)==2){
      k = which.min(c(dist.pts(z[1], p), dist.pts(z[2], p)))
      z = z[k]
    }
  } else {
    z = p
  }
  z
}


adjustRange = function(fit){  
  xrange = range(fit$model[,2])
  yrange = range(fit$model[,1])
  slopeRrange = fit$coeff[2] + 3*c(-1, 1)*summary(fit)$coeff[2,2]
  
  ratio = diff(yrange)/diff(xrange)
  if(all(slopeRrange>0)){
    slopeYrange = c(0, ratio)
    from.y = min(slopeYrange)
  } else if(all(slopeRrange<0)){
    slopeYrange = c(-ratio, 0)
    from.y = max(slopeYrange)
  } else{
    slopeYrange = c(-ratio/2, ratio/2)
    from.y = mean(slopeYrange)
  }

  slopeXrange = c(0, 1)
  slopeXYrange = adjustRange.slope(slopeRrange, slopeXrange,
                                   slopeYrange, from.y)
  linearXYrange = adjustRange.linear(xrange, yrange, slopeXYrange)
  
  list(linearXYrange=linearXYrange, slopeXYrange=slopeXYrange, from.y=from.y)
}

adjustRange.linear = function(xrange, yrange, slopeXYrange){
  slopeRatio = diff(slopeXYrange$yrange)/diff(slopeXYrange$xrange)
  yrange = mean(yrange) + c(-1, 1)*slopeRatio*diff(xrange)/2
  list(xrange=xrange, yrange=yrange)
}

adjustRange.slope = function(slope, xrange, yrange, from.y){
  ## adjust proper xrange and yrange for slope plot
  ## therefore all slope lines can end on max(xrange)
  n = length(slope)
  a = diff(xrange)
  centre = pts(x=min(xrange), y=from.y)
  x0 = rep(centre$x, n)
  y0 = rep(centre$y, n)  
  x1 = rep(max(xrange), n)
  y1 = centre$y + slope*a
  yrange[1] = min(min(yrange), min(y1))
  yrange[2] = max(max(yrange), max(y1))
  list(xrange=xrange, yrange=yrange)
}

## Added by Vivian
## paste labels with group sizes
label.subscript = function(label, value){
  n = length(label)
  if(length(value)!=n)
    stop("length of label and value must be same")
  newlabel = c()
  length(newlabel) = n
  for(i in 1:n){
    newlabel<-append(newlabel,
                     as.expression(substitute(g[s],list(g=label[i],s=value[i]))))
  }
  return(newlabel)
}

bootstrapGetPointerInfo = function(index){
  # misc.bootstrapGetPointerInfo
  # create track arrows for bootstrapping process
  # from: where the track pointer arrows start
  # to  : where the track pointer arrows end
  # pad : adjust the start and end position of arrows
  
  nc    = grid.get("datatable")$rc.layout[2]
  gb1   = grid.get(gPath("datatable","dataTxt"))
  gb2   = grid.get(gPath("bootstraptable","dataTxt")) 

  from  = getrefPoint(gb1$x, gb1$y, "bootstrap::tablevp::tablelay::datavp")
  to    = getrefPoint(gb2$x, gb2$y, "bootstrap::btablevp::tablelay::datavp")
  
  if(nc==1){
    from$x=from$x[index]
    from$y=from$y[index]
  
    pad   = max(as.numeric(convertWidth(stringWidth(gb1$label),"lines"))/2)
    pad   = ceiling(pad*2)/2
  }
  else if(nc==2){
    edge  = rep(c(F,T),length(index)) #only need right edge of the tablevp and left edge of the bstrapvp
    
    from$x= from$x[edge]
    from$y= from$y[edge]
    to$x  = to$x[!edge]
    to$y  = to$y[!edge]
    from$x=from$x[index]
    from$y=from$y[index]
    
    pad1   = max(as.numeric(convertWidth(stringWidth(gb1$label[edge]),"lines"))/2)
    pad2   = max(as.numeric(convertWidth(stringWidth(gb2$label[!edge]),"lines"))/2)
    pad    = ceiling(c(pad1,pad2)*2)/2
  }  
  gb3 = pointerGrob(name="pointergp",from=from,to=to,pad=unit(pad,"lines"),show=FALSE,fmt=initArgsBootstrapPointer())
  list(gb3=gb3)
}

## example of move2edge
# plot(c(-3, 13), c(-3, 13), type="n")
# xrange = c(1, 9)
# yrange = c(1, 9)
# rect(xrange[1], yrange[1], xrange[2], yrange[2])
# a = 10
# b = -0.5
# abline(a, b, lty=2)
# x = 10
# p = pts(x, a+x*b)
# p
# points(p$x, p$y, pch=19)
# p2 = move2edge(p, a, b, xrange, yrange)
# p2
# points(p2$x, p2$y, pch=17)