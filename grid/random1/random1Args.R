##############################################################################
## Date     : 2011-03-08
## Author   : Danny Chang
## Type     : funcitons
## Usage    : random1 initial arguments setup
##############################################################################

initArgsRandom1 = function(){
  #gcol  = c("purple", "forestgreen")
  gcol  = barcols(2)
  #gcol = c(rgb(34,0,174,alpha=255*0.7,maxColorValue=255),
  #         rgb(0,102,0,alpha=255*0.7,maxColorValue=255))
  
  #gcol2 = c(rgb(160, 32, 240, alpha=255*0.4, maxColorValue=255),
  #          rgb(34, 139,  34, alpha=255*0.4, maxColorValue=255))
  
  gcol2  = barcols(2,alpha=0.4)
  
  #gcol2 = gcol
  gp.pts = list(gpar(cex=0.7, lwd=2, col="black"),
                gpar(cex=0.7, lwd=2, col=gcol[1]),
                gpar(cex=0.7, lwd=2, col=gcol[2]),
                gpar(cex=0.7, lwd=2, col=rgb(0, 0, 1, alpha=0.3)),
                gpar(cex=0.7, lwd=2, col=rgb(1, 0, 0, alpha=0.3)))  
                 
  col.box    = rep(rgb(0, 0, 0, alpha=0.3), 3)
  gp.box     = gpar(lwd=2)
  gp.diffbar = gpar(col="orangered", lwd=3)
  gp.indbar = gpar(col="grey75", lty=2)
  arr = arrow(angle=30, length=unit(0.1, "inches"), ends="last", type="open")
  size.pts = unit(0.05, "inches")
  maxRow   = 80
  
  list(gcol=gcol, gcol2=gcol2, gp.pts=gp.pts, col.box=col.box, gp.box=gp.box,
       gp.diffbar=gp.diffbar, gp.indbar=gp.indbar, arr=arr, size.pts=size.pts,
       maxRow=maxRow)
}

initArgsRandom1TableFormat = function(x.df){
  maxRow = initArgsRandom1()$maxRow
  n = nrow(x.df)
  
  if(n>maxRow){
    rc.layout = c(maxRow+5, ncol(x.df)+1)
    x.df = x.df[1:maxRow,]
  } else {
    rc.layout = c(nrow(x.df), ncol(x.df)+1)
  }  

  list(x.df=x.df, rc.layout=rc.layout)
}

initArgsRandom1Table = function(x.df, main, digit){
  maxRow    = initArgsRandom1()$maxRow
  fmtTable  = initArgsRandom1TableFormat(x.df)
  x.df      = rounddf(fmtTable$x.df, digit, "g")  
  rc.layout = fmtTable$rc.layout
  varname   = names(x.df)
  gcol      = initArgsRandom1()$gcol
  col.data  = cbind("black", ifelse(as.numeric(x.df[,2])==1, gcol[1], gcol[2]))
  
  cex.main    = 1.5
  cex.varname = 1.2
  cex.data    = 1.1 + nrow(x.df)*(0.7-1.1)/maxRow
  col.varname = "black"
  col.main    = "black"
  height.varname = 1.2
  height.main    = 1.5
  
  list(data=x.df, varname=varname, main=main, rc.layout=rc.layout,
       cex.data=cex.data, cex.varname=cex.varname, cex.main=cex.main,
       col.data=col.data, col.varname=col.varname, col.main=col.main,
       height.varname=height.varname, height.main=height.varname,
       name="datatable", vp=vpPath("random1", "tablevp"))
}

initArgsRandom1Data = function(x, group, diffFun){
  fmt           = initArgsRandom1()
  pad           = unit(0.2, "npc")
  stack.height  = unit(0.1, "npc")
  wrange        = range(x)
  show          = c(FALSE, TRUE, TRUE)
  
  if(identical(median, diffFun))
    show.box    = TRUE
  else
    show.box    = FALSE
  
  size.pts      = fmt$size.pts
  arr           = fmt$arr
  
  gp.pts.x      = gpar()
  gp.pts.group1 = fmt$gp.pts[[2]]
  gp.pts.group2 = fmt$gp.pts[[3]]
  
  gp.box.x      = gpar()
  gp.box.group1 = fmt$gp.box
  gp.box.group2 = fmt$gp.box

  col.box.group1 = fmt$col.box
  col.box.group2 = fmt$col.box
  
  gp.diffbar    = fmt$gp.diffbar
  gp.indbar     = fmt$gp.indbar
  
  list(x=x, group=group, pad=pad, arrow=arr, stack.height=stack.height,
       size.pts=size.pts, wrange=wrange, diffFun=diffFun, show=show,
       show.box=show.box,
       gp.pts.x=gp.pts.x, gp.pts.group1=gp.pts.group1,
       gp.pts.group2=gp.pts.group2, gp.box.x=gp.box.x,
       gp.box.group1=gp.box.group1, gp.box.group2=gp.box.group2,
       col.box.group1=col.box.group1, col.box.group2=col.box.group2,
       gp.bar=gp.diffbar, gp.indbar=gp.indbar,
       name="dataBoxDots", vp=vpPath("random1", "datavp"))
}

initArgsRandom1Random = function(x, group, diffFun){
  # fmt  = initArgsRandom1()
  fmt2 = initArgsRandom1Data(x, group, diffFun)
  fmt2$arrow = NULL
  fmt2$pad   = unit(0.35, "npc")
  fmt2$show  = c(FALSE, FALSE, FALSE)
  fmt2$name  = "randomBoxDots"
  fmt2$vp    = vpPath("random1", "randomvp")
  fmt2
}

initArgsRandom1Dist = function(x, x.diff){
  fmt = initArgsRandom1()
  size.pts = fmt$size.pts
  at = unit(0, "npc")
  wrange = range(xatRandom1(x, x.diff)$xat2)
  stack.height = unit(1, "npc")
  horiz = TRUE
  name = "dist"
  gp = fmt$gp.pts[[1]]
  vp = vpPath("random1", "distvp")
  show = FALSE
  
  if(!is.null(x.diff))
    x = x.diff
  
  list(x=x, size.pts=size.pts, at=at, stack.height=stack.height,
       wrange=wrange, horiz=horiz, show=show,
       name=name, gp=gp, vp=vp)
}

checkGroupRandom1 = function(x.df, diffFun){
  d = diff(tapply(x.df[,1], x.df[,2], diffFun))
  if(d<0){
    lv = levels(x.df[,2])[2:1]
    x.df[,2] = factor(x.df[,2], levels=lv)
  }
  x.df
}

xatRandom1 = function(x, x.diff=NULL){
  xat1 = pretty(range(x), n=9)
  xat2 = xat1 - mean(xat1)
  
  if(!is.null(x.diff)){
    if(max(x.diff) > max(xat2) || min(x.diff) < min(xat2)){
      x = x.diff + mean(x)
      xat1 = pretty(range(x), n=9)
      xat2 = xat1 - mean(xat1)
    }
  }
  
  if(!(0 %in% xat2))
    xat2 = c(xat2[xat2<0],0,xat2[xat2>0])
  
  list(xat1=xat1, xat2=xat2)
}