##############################################################################
## Date     : 2011-05-09
## Author   : Vivian Li
## Type     : funcitons
## Usage    : bootstrap initial arguments setup
##############################################################################

initArgsBootstrapRoundData = function(x.df,digit){
  rounddf(x.df, digit, "g")
}
initArgsBootstrapPointer = function(){
  arr = arrow(angle=30, length=unit(0.1, "inches"), ends="last", type="open")
  gp  = gpar(col = "red",lwd = 2)
  
  list(arr=arr,gp=gp)
}
initArgsBootstrap = function(){
  gcol   = rep("black",2)
  gcol2  = gcol
  gp.pts = list(gpar(cex=0.7, lwd=2, col="black"),
                gpar(cex=0.7, lwd=2, col="grey55")) 
                 
  col.box =rep(rgb(0, 0, 0, alpha=0.7), 3)
  gp.box  = gpar(lwd=2)
  arr     = arrow(angle=30, length=unit(0.1, "inches"), ends="last", type="open")
  size.pts= unit(0.05, "inches")
  maxRow  = 60
  
  list(gcol=gcol, gcol2=gcol2, gp.pts=gp.pts, col.box=col.box, gp.box=gp.box,
       arr=arr, size.pts=size.pts,maxRow=maxRow)
}

initArgsBootstrapTableFormat = function(x.df){
  maxRow = initArgsBootstrap()$maxRow
  n = nrow(x.df)
  
  if(n>maxRow){
    rc.layout = c(maxRow+2, ncol(x.df))
    vname = names(x.df)
    x.df = data.frame(x.df[1:maxRow,])
    names(x.df)=vname
  } else {
    rc.layout = c(nrow(x.df), ncol(x.df))
  }  

  list(x.df=x.df, rc.layout=rc.layout)
}

initArgsBootstrapTable = function(x.df, main, digit){
  maxRow    = initArgsBootstrap()$maxRow
  fmtTable  = initArgsBootstrapTableFormat(x.df)
  x.df      = initArgsBootstrapRoundData(fmtTable$x.df,digit)
  rc.layout = fmtTable$rc.layout
  varname   = names(x.df)
  gcol      = initArgsBootstrap()$gcol
  col.data  = rep("black", ifelse(rc.layout[1]>maxRow, maxRow, rc.layout[1]))
  
  cex.main    = 1.3
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
       name="datatable", vp=vpPath("bootstrap", "tablevp"))
}

initArgsBootstrapBootstrapTable = function(x.df,main,digit){
  tableFmt              = initArgsBootstrapTable(x.df,main,digit)
  rc.layout             = tableFmt$rc.layout
  maxRow                = initArgsBootstrap()$maxRow
  tableFmt$main         = "Resampling Data"
  tableFmt$showTxt = FALSE
  tableFmt$name         = "bootstraptable"
  tableFmt$vp           = vpPath("bootstrap", "btablevp")
  tableFmt$col.data     = rep("red",ifelse(rc.layout[1]>maxRow, maxRow, rc.layout[1]))
  tableFmt
}

initArgsBootstrapData = function(x, diffFun){
  fmt           = initArgsBootstrap()
  stack.height  = unit(0.1, "npc")
  pad.box       = unit(0, "inches")
  at            = unit(0.5, "npc")
  
  if(identical(median, diffFun))
    show.box    = TRUE
  else
    show.box    = FALSE
  
  size.pts      = fmt$size.pts
  gp.pts        = fmt$gp.pts[[2]]
  gp.box        = fmt$gp.box
  col.box       = fmt$col.box
  vp            = vpPath("bootstrap", "datavp")
            
  list(x=x, at=at, stack.height=stack.height, pad.box=pad.box, 
       col.box=col.box, size.pts=size.pts, show.box=show.box, 
       gp.pts=gp.pts, gp.box=gp.box, name="dataBoxDot",vp=vp)
}

initArgsBootstrapRandom = function(x,diffFun){
  fmt2 = initArgsBootstrapData(x, diffFun)
  fmt2$gp.box= gpar(lwd=3)
  fmt2$show.pts = FALSE
  fmt2$show.box = FALSE
  fmt2$show.w   = FALSE
  fmt2$name  = "bstrapBoxDot"
  fmt2$vp    = vpPath("bootstrap", "bstrapvp")
  fmt2
}

initArgsBootstrapGhostBox = function(x,diffFun){
  fmt = initArgsBootstrapData(x,diffFun)
  fmt$col.box =  c(rgb(255,69,0,alpha=255*0.5,max=255), rgb(58,95,205,alpha=255*0.9,max=255),"white")
  fmt$show.pts= FALSE
  fmt$show.w  = FALSE
  fmt$vp      = vpPath("bootstrap", "bstrapvp")
  fmt
}
initArgsBootstrapGhostBar = function(x,diffFun){
  center = diffFun(x)
  fmt = list(xat=unit(center,"native"),yat=unit(0.5,"npc"),len=unit(0.3,"npc"),
            name="meanBars",
            gp=gpar(col=rgb(58,95,205,alpha=255*0.9,max=255),lwd=2),
            vp=vpPath("bootstrap","bstrapvp"))
  fmt
}

initArgsBootstrapDist = function(data, diffFun, x.sim){
  fmt = initArgsBootstrap()
  x      = data[,1]

  stack.height = unit(1, "npc")
  size.pts = fmt$size.pts
  wrange   = range(xatBootstrap(data, diffFun, x.sim)$xat2)
  at       = unit(0, "npc")
  horiz    = TRUE
  name     = "bdist"
  gp       = fmt$gp.pts[[1]]
  vp       = vpPath("bootstrap", "bdistvp")
  show     = FALSE
  
  if(!is.null(x.sim))
    x = x.sim
  
  list(x=x, size.pts=size.pts, at=at, stack.height=stack.height,
       wrange=wrange, horiz=horiz, show=show,
       name=name, gp=gp, vp=vp)
}


checkGroupBootstrap = function(x.df, diffFun){
  d = diff(tapply(x.df[,1], x.df[,2], diffFun))
  if(d<0){
    lv = levels(x.df[,2])[2:1]
    x.df[,2] = factor(x.df[,2], levels=lv)
  }
  x.df
}

xatBootstrap = function(data, diffFun, x.sim=NULL){
  x    = data[,1]
  m    = diffFun(x)

  xat1 = pretty(range(x), n=9)
  xat2 = round(xat1-median(xat1)+pretty(m)[1])

  if(!is.null(x.sim)){
    rg  = range(xat2)
    rg1 = range(x.sim)
    if(rg1[1]<rg[1] || rg1[2]>rg[2]){
      xat2 = round(xat1-median(xat1)+pretty(mean(x.sim))[1])
    }
  }
  # temporary setting xat1=xat2
  list(xat1=xat1, xat2=xat1)
}
