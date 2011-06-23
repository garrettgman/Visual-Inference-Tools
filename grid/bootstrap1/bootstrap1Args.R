##############################################################################
## Date     : 2011-05-09
## Author   : Vivian Li
## Type     : funcitons
## Usage    : bootstrap1 initial arguments setup
##############################################################################

initArgsBootstrap1RoundData = function(x.df,digit){
  rounddf(x.df, digit, "g")
}
initArgsBootstrap1Pointer = function(){
  arr = arrow(angle=30, length=unit(0.1, "inches"), ends="last", type="open")
  gp  = gpar(col = "red",lwd = 2)
  
  list(arr=arr,gp=gp)
}
initArgsBootstrap1 = function(){
  gcol = rep("black",2)
  gcol2 = gcol
  gp.pts = list(gpar(cex=0.7, lwd=2, col="black"),
                gpar(cex=0.7, lwd=2, col="grey55"),
                gpar(cex=0.7, lwd=2, col="grey55"),
                gpar(cex=0.7, lwd=2, col=rgb(0, 0, 1, alpha=0.3)),
                gpar(cex=0.7, lwd=2, col=rgb(1, 0, 0, alpha=0.3)))  
                 
  col.box    = rep(rgb(0, 0, 0, alpha=0.7), 3)
  gp.box     = gpar(lwd=2)
  gp.diffbar = gpar(col="forestgreen", lwd=3)
  gp.indbar = gpar(col="black", lty=2)
  arr = arrow(angle=30, length=unit(0.1, "inches"), ends="last", type="open")
  size.pts = unit(0.05, "inches")
  maxRow   = 60
  
  list(gcol=gcol, gcol2=gcol2, gp.pts=gp.pts, col.box=col.box, gp.box=gp.box,
       gp.diffbar=gp.diffbar, gp.indbar=gp.indbar, arr=arr, size.pts=size.pts,
       maxRow=maxRow)
}

initArgsBootstrap1TableFormat = function(x.df){
  maxRow = initArgsBootstrap1()$maxRow
  n = nrow(x.df)
  
  if(n>maxRow){
    rc.layout = c(maxRow+5, ncol(x.df))
    x.df = x.df[1:maxRow,]
  } else {
    rc.layout = c(nrow(x.df), ncol(x.df))
  }  

  list(x.df=x.df, rc.layout=rc.layout)
}

initArgsBootstrap1Table = function(x.df, main, digit){
  maxRow    = initArgsBootstrap1()$maxRow
  fmtTable  = initArgsBootstrap1TableFormat(x.df)
  x.df      = initArgsBootstrap1RoundData(fmtTable$x.df,digit)
  rc.layout = fmtTable$rc.layout
  varname   = names(x.df)
  gcol      = initArgsBootstrap1()$gcol
  col.data  = cbind("black", ifelse(as.numeric(x.df[,2])==1, gcol[1], gcol[2]))
  
  cex.main    = 1.5
  cex.varname = 1.2
  cex.data    = 1.2 + nrow(x.df)*(0.8-1.2)/maxRow
  col.varname = "black"
  col.main    = "black"
  height.varname = 1.2
  height.main    = 1.5
  
  list(data=x.df, varname=varname, main=main, rc.layout=rc.layout,
       cex.data=cex.data, cex.varname=cex.varname, cex.main=cex.main,
       col.data=col.data, col.varname=col.varname, col.main=col.main,
       height.varname=height.varname, height.main=height.varname,
       name="datatable", vp=vpPath("bootstrap1", "tablevp"))
}

initArgsBootstrap1BootstrapTable = function(x.df,main,digit){
  tableFmt              = initArgsBootstrap1Table(x.df,main,digit)
  tableFmt$main         = "Resampling Data"
  tableFmt$showTxt = FALSE
  tableFmt$name         = "bootstraptable"
  tableFmt$vp           = vpPath("bootstrap1", "btablevp")
  tableFmt$col.data     = cbind("red",rep("red",length(x.df[,2])))
  tableFmt
}

initArgsBootstrap1Data = function(x, group, diffFun){
  fmt           = initArgsBootstrap1()
  pad           = unit(0.2, "npc")
  wrange        = range(x)
  
  stack.height  = unit(0.1, "npc")
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
       show.box=show.box, show.pts=TRUE,
       gp.pts.x=gp.pts.x, gp.pts.group1=gp.pts.group1,
       gp.pts.group2=gp.pts.group2, gp.box.x=gp.box.x,
       gp.box.group1=gp.box.group1, gp.box.group2=gp.box.group2,
       col.box.group1=col.box.group1, col.box.group2=col.box.group2,
       gp.bar=gp.diffbar, gp.indbar=gp.indbar,
       name="dataBoxDots", vp=vpPath("bootstrap1", "datavp"))
}

initArgsBootstrap1Random = function(x, group, diffFun){
  # fmt  = initArgsBootstrap1()
  fmt2 = initArgsBootstrap1Data(x, group, diffFun)
  fmt2$arrow = NULL
  fmt2$gp.box.group1=fmt2$gp.box.group2=gpar(lwd=3)
  #fmt2$pad  = unit(0.35, "npc")
  fmt2$show  = c(FALSE, FALSE, FALSE)
  fmt2$name  = "bstrapBoxDots"
  fmt2$vp    = vpPath("bootstrap1", "bstrapvp")
  fmt2
}

initArgsBootstrap1GhostBox = function(x, group, diffFun){
  fmt = initArgsBootstrap1Data(x,group,diffFun)
  fmt$col.box.group1 = fmt$col.box.group2 = c(rgb(255,69,0,alpha=255*0.5,max=255), rgb(0,0,205,alpha=255*0.9,max=255),"white")
  #fmt$gp.box.group1=fmt$gp.box.group2=gpar(lwd=3)
  fmt$arrow   = NULL
  fmt$show.pts= FALSE
  fmt$show.w  = FALSE
  fmt$vp      = vpPath("bootstrap1", "bstrapvp")
  fmt
}
initArgsBootstrap1GhostBar = function(x,group,diffFun){
  center = tapply(x,group,diffFun)
  fmt = list(xat=unit(center,"native"),yat=unit(0.5,"npc"),len=unit(0.2,"npc"),
            name="meanBars",
            gp=gpar(col=rgb(58,95,205,alpha=255*0.9,max=255),lwd=2),
            vp=vpPath("bootstrap1","bstrapvp"))
  fmt
}

initArgsBootstrap1Dist = function(data, diffFun, x.diff){
  x   = data[,1]
  fmt = initArgsBootstrap1()
  size.pts = fmt$size.pts
  at = unit(0, "npc")
  wrange = range(xatBootstrap1(data, diffFun, x.diff)$xat2)
  stack.height = unit(1, "npc")
  horiz = TRUE
  name = "bdist"
  gp = fmt$gp.pts[[1]]
  vp = vpPath("bootstrap1", "bdistvp")
  show = FALSE
  
  if(!is.null(x.diff))
    x = x.diff
  
  list(x=x, size.pts=size.pts, at=at, stack.height=stack.height,
       wrange=wrange, horiz=horiz, show=show,
       name=name, gp=gp, vp=vp)
}


checkGroupBootstrap1 = function(x.df, diffFun){
  d = diff(tapply(x.df[,1], x.df[,2], diffFun))
  if(d<0){
    lv = levels(x.df[,2])[2:1]
    x.df[,2] = factor(x.df[,2], levels=lv)
  }
  x.df
}

xatBootstrap1 = function(data, diffFun, x.diff=NULL){
  x    = data[,1]
  g    = data[,2]
  dff  = abs(diff(tapply(x,g,diffFun)))
  
  xat1 = pretty(range(x), n=9)
  xat2 = round(xat1-median(xat1)+pretty(dff)[1])
  if(!is.null(x.diff)){
    rg  = range(xat2)
    rg1 = range(x.diff,na.rm=T)
    if(rg1[1]<rg[1] || rg1[2]>rg[2]){
      xat2 = round(xat1-median(xat1)+pretty(mean(x.diff))[1])
    }
  }

  if(!(0 %in% xat2))
     xat2 = c(xat2[xat2<0],0,xat2[xat2>0])
     
  list(xat1=xat1, xat2=xat2)
}
