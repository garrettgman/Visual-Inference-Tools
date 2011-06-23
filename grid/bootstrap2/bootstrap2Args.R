##############################################################################
## Date     : 2011-06-01
## Author   : Vivian Li
## Type     : funcitons
## Usage    : bootstrap2 initial arguments setup
##############################################################################

initArgsBootstrap2 = function(){
  gcol   = gcol2 = rgb(160, 32, 240, alpha=255*0.4, maxColorValue=255)
  #gcol   = gcol2 = barcols(1)
  gp.pts = list(gpar(cex=0.7, lwd=2, col="black"),
                gpar(cex=0.7, lwd=2, col="grey55"))                
  gp.box = list(gpar(lwd=1, fill=gcol), gpar(fill="transparent"))
  gp.lab = gpar(cex=2, col="black")
  arr    = arrow(angle=30, length=unit(0.1, "inches"), ends="last", type="open")
  size.pts= unit(0.05, "inches")
  maxRow  = 60

  list(gcol=gcol, gcol2=gcol2, gp.pts=gp.pts, gp.box=gp.box, gp.lab=gp.lab,
       arr=arr,size.pts=size.pts, maxRow=maxRow)
}

initArgsBootstrap2TableFormat = function(x.df){
  maxRow = initArgsBootstrap2()$maxRow
  n = nrow(x.df)
  
  if(n>maxRow){
    rc.layout = c(maxRow+5, ncol(x.df))
    vname = names(x.df)
    x.df = data.frame(x.df[1:maxRow,])
    names(x.df)=vname
  } else {
    rc.layout = c(nrow(x.df), ncol(x.df))
  }  
  list(x.df=x.df, rc.layout=rc.layout)
}

initArgsBootstrap2Table = function(x.df, main){
  maxRow    = initArgsBootstrap2()$maxRow
  fmtTable  = initArgsBootstrap2TableFormat(x.df)
  x.df      = fmtTable$x.df
  rc.layout = fmtTable$rc.layout
  varname   = names(x.df)
  gcol      = initArgsBootstrap2()$gcol
  col.data  = rep("black", rc.layout[1])
  
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
       name="datatable", vp=vpPath("bootstrap2", "tablevp"))
}

initArgsBootstrap2BootstrapTable = function(x.df,main){
  tableFmt              = initArgsBootstrap2Table(x.df,main)
  tableFmt$main         = "Resampling Data"
  tableFmt$showTxt      = FALSE
  tableFmt$name         = "bootstraptable"
  tableFmt$vp           = vpPath("bootstrap2", "btablevp")
  tableFmt$col.data     = rep("red",tableFmt$rc.layout[1])
  tableFmt
}
initArgsBootstrap2Pointer = function(){
  arr = arrow(angle=30, length=unit(0.1, "inches"), ends="last", type="open")
  gp  = gpar(col = "red",lwd = 2)
  
  list(arr=arr,gp=gp)
}
initArgsBootstrap2Data = function(x){
  # Notes: setting for the graph
  # fmt       : color and size setting
  # label     : levels of group1 (first column)
  # gp.boxi   : edge box color for group i
  # gp.inboxi : inner box color for group i
  # gp.bar    : arrow color
  # gp.indbar : arrow head and tail position lines
  # gp.lab    : label color
  
  fmt           = initArgsBootstrap2()
  prop          =  table(x)/length(x)
  fill          = NULL
  label         = paste(levels(x),"(",table(x),")",sep="")
  width         = unit(1, "native")
  height        = unit(0.3, "npc")
  gp.box        = fmt$gp.box
  gp.lab        = fmt$gp.lab
  
  list(p=prop,fill=fill, width=width, height=height,
       label=label, gp.box=gp.box,gp.lab=gp.lab,
       name="dataPropBox", vp=vpPath("bootstrap2", "datavp"))
}

initArgsBootstrap2Random = function(x){
  fmt2 = initArgsBootstrap2Data(x)
  fmt2$show  = FALSE
  fmt2$name  = "bstrapPropBox"
  fmt2$vp    = vpPath("bootstrap2", "bstrapvp")
  fmt2
}

initArgsBootstrap2GhostBox = function(x,diffFun){
  fmt = initArgsBootstrap2Data(x,diffFun)
  fmt$col.box =  c(rgb(255,69,0,alpha=255*0.5,max=255), rgb(58,95,205,alpha=255*0.9,max=255),"white")
  fmt$show.pts= FALSE
  fmt$show.w  = FALSE
  fmt$vp      = vpPath("bootstrap2", "bstrapvp")
  fmt
}
initArgsBootstrap2GhostBar = function(x,diffFun){
  center = diffFun(x)
  fmt = list(xat=unit(center,"native"),yat=unit(0.5,"npc"),len=unit(0.3,"npc"),
            name="meanBars",
            gp=gpar(col=rgb(58,95,205,alpha=255*0.9,max=255),lwd=2),
            vp=vpPath("bootstrap2","bstrapvp"))
  fmt
}

initArgsBootstrap2Dist = function(data, x.sim){
  fmt = initArgsBootstrap2()
  x      = data[,1]

  stack.height = unit(1, "npc")
  size.pts = fmt$size.pts
  wrange   = range(xatBootstrap2(x.sim)$xat2)
  at       = unit(0, "npc")
  horiz    = TRUE
  name     = "bdist"
  gp       = fmt$gp.pts[[1]]
  vp       = vpPath("bootstrap2", "bdistvp")
  show     = FALSE
  
  if(!is.null(x.sim))
    x = x.sim
  
  list(x=x, size.pts=size.pts, at=at, stack.height=stack.height,
       wrange=wrange, horiz=horiz, show=show,
       name=name, gp=gp, vp=vp)
}


checkGroupBootstrap2 = function(x.df,vname){
  
  if(!is.data.frame(x.df))
    x.df = as.data.frame(as.factor(x.df))
  else{
    vname = names(x.df)[1]
    x.df = as.data.frame(as.factor(x.df[,1]))
  }
  
  if(!is.null(vname))
    names(x.df) = vname
  x.df
}

xatBootstrap2 = function(x.sim=NULL, r=c(0,1)){
  if(diff(r)==1){
    xat1 = pretty(r, n=11)
  }
  else{
    xat1 = pretty(r, diff(r)/0.1)
  }
  #xat2 = xat1 - mean(xat1) # centered by mean
  xat2 = xat1
  list(xat1=xat1, xat2=xat2)
}
