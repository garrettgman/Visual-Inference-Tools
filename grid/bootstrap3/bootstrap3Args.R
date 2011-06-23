##############################################################################
## Date     : 2011-05-09
## Author   : Vivian Li
## Type     : funcitons
## Usage    : bootstrap3 initial arguments setup
##############################################################################

initArgsBootstrap3 = function(){
  # Notes:
  # gcol1      : inner box color
  # gcol2      : data groups color
  # gp.box     : line width of the edge box
  # gp.diffbar : arrow color
  # gp.indbar  : arrow head and tail position line
  # gp.lab     : label color
  # arr        : description of the arrow
  # size.pts   : points size
  # gp.pts     : grid shape of the points
  
  gcol1 = c(rgb(160, 32, 240, alpha=255*0.4, maxColorValue=255),
            rgb(34, 139,  34, alpha=255*0.4, maxColorValue=255))
  gcol2      = c("purple", "forestgreen") 
  gp.box     = list(gpar(lwd=2),
                    gpar(lwd=2))
  gp.diffbar = gpar(col="forestgreen", lwd=3)
  gp.indbar  = gpar(col="grey75", lty=2)
  gp.lab     = gpar(cex=2, col="black")
  
  arr = arrow(angle=30, length=unit(0.1, "inches"), ends="last", type="open")
  
  size.pts = unit(0.05, "inches")  
  gp.pts     = gpar(cex=0.7, lwd=2, col="black")
  
  # maximum rows of data displayed
  maxRow   = 60
  
  list(gcol1=gcol1, gcol2=gcol2, gp.pts=gp.pts, gp.box=gp.box,
       gp.diffbar=gp.diffbar, gp.lab=gp.lab,
       gp.indbar=gp.indbar, arr=arr, size.pts=size.pts, maxRow=maxRow)
}

initArgsBootstrap3TableFormat = function(x.df){
  # Notes: define the layout of data panel
  # maxRow    : maximum rows
  # rc.layout : c(rows,columns+1) for randomisation column
  maxRow = initArgsBootstrap3()$maxRow
  n = nrow(x.df)
  
  if(n>maxRow){
    rc.layout = c(maxRow+5, ncol(x.df))
    x.df = x.df[1:maxRow,]
  } else {
    rc.layout = c(nrow(x.df), ncol(x.df))
  }  
  list(x.df=x.df, rc.layout=rc.layout)
}

initArgsBootstrap3Table = function(x.df, main){
  # Notes:initiate the settings
  # fmt      : color and size settings
  # maxRow   : maximumn rows of data displayed
  # fmtTable : layout of the table
  # col.data : determine the color of second column
  # cex.*    : magnified shape related to default

  fmt       = initArgsBootstrap3()
  maxRow    = fmt$maxRow
  fmtTable  = initArgsBootstrap3TableFormat(x.df)
  x.df      = fmtTable$x.df
  rc.layout = fmtTable$rc.layout
  varname   = names(x.df)
  gcol1     = fmt$gcol1
  gcol2     = fmt$gcol2
  #col.data  = cbind("black",ifelse(as.numeric(x.df[,2])==1, gcol2[1], gcol2[2]))
  #col.data  = matrix(rep(ifelse(as.numeric(x.df[,2])==1, gcol2[1], gcol2[2]),2),nc=rc.layout[2])
  col.data  = cbind("black",rep("black",rc.layout[1]))
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
       name="datatable", vp=vpPath("bootstrap3", "tablevp"))
}

initArgsBootstrap3BootstrapTable = function(x.df,main){
  tableFmt              = initArgsBootstrap3Table(x.df,main)
  tableFmt$main         = "Resampling Data"
  tableFmt$showTxt = FALSE
  tableFmt$name         = "bootstraptable"
  tableFmt$vp           = vpPath("bootstrap3", "btablevp")
  tableFmt$col.data     = cbind("red",rep("red",length(x.df[,2])))
  tableFmt
}

initArgsBootstrap3Data = function(x, group){
  # Notes: setting for the graph
  # fmt       : color and size setting
  # label     : levels of group1 (first column)
  # arr       : arrow description
  # gp.boxi   : edge box color for group i
  # gp.inboxi : inner box color for group i
  # gp.bar    : arrow color
  # gp.indbar : arrow head and tail position lines
  # gp.lab    : label color
  
  fmt           = initArgsBootstrap3()
  fill          = NULL
  label         = levels(x)
  width         = unit(1, "native")
  height        = unit(0.3, "npc")
  pad           = unit(0.2, "npc") 
  arr           = fmt$arr
  show.box      = TRUE
  gp.box1       = fmt$gp.box[[1]]
  gp.box2       = fmt$gp.box[[2]]
  gp.inbox1     = list(gpar(lwd=1, fill=fmt$gcol1[1]), gpar(fill="transparent"))
  gp.inbox2     = list(gpar(lwd=1, fill=fmt$gcol1[2]), gpar(fill="transparent"))
  gp.bar        = fmt$gp.diffbar
  gp.indbar     = fmt$gp.indbar
  gp.lab        = fmt$gp.lab

  
  list(x=x, group=group, fill=fill, width=width, height=height,
       label=label, pad=pad, arrow=arr, show.box=show.box,
       gp.box1=gp.box1, gp.box2=gp.box2,
       gp.inbox1=gp.inbox1, gp.inbox2=gp.inbox2, gp.bar=gp.bar,
       gp.indbar=gp.indbar, gp.lab=gp.lab,
       name="dataPropBox", vp=vpPath("bootstrap3", "datavp"))
}

initArgsBootstrap3Random = function(x, group, diffFun){
  # fmt  = initArgsBootstrap3()
  fmt2 = initArgsBootstrap3Data(x, group)
  fmt2$arrow = NULL
  fmt2$show.box = FALSE
  fmt2$name  = "bstrapPropBox"
  fmt2$vp    = vpPath("bootstrap3", "bstrapvp")
  fmt2
}

initArgsBootstrap3Pointer = function(){
  arr = arrow(angle=30, length=unit(0.1, "inches"), ends="last", type="open")
  gp  = gpar(col = "red",lwd = 2)
  
  list(arr=arr,gp=gp)
}

initArgsBootstrap3Dist = function(x, x.diff){
  fmt = initArgsBootstrap3()
  size.pts = fmt$size.pts
  at = unit(0, "npc")
  wrange = range(xatBootstrap3(x.diff)$xat2)
  stack.height = unit(1, "npc")
  horiz = TRUE
  name = "bdist"
  gp = fmt$gp.pts
  vp = vpPath("bootstrap3", "bdistvp")
  show = FALSE
  
  if(!is.null(x.diff))
    x = x.diff
  else
    x = 0
  
  list(x=x, size.pts=size.pts, at=at, stack.height=stack.height,
       wrange=wrange, horiz=horiz, show=show,
       name=name, gp=gp, vp=vp)
}

checkGroupBootstrap3 = function(x.df){
  # Notes:
  # d: differences within group1
  # x: group1
  # g: group2
  
  x = x.df[,1]
  g = x.df[,2]
  
  d = getPropBoxValue(x, g)$d
  if(d<0){
    # swap the factor level to get positive differences
    lv = levels(x.df[,2])[2:1]
    x.df[,2] = factor(x.df[,2], levels=lv)
  }
  x.df
}

xatBootstrap3 = function(x.diff=NULL, r=c(0,1)){
  if(diff(r)==1)
    xat1 = pretty(r, n=9)
  else if(diff(r)>1){
	r    = r - (diff(r)-1)/2
	xat1 = pretty(r,n=9)
  }
  else
    xat1 = pretty(r, diff(r)/0.1)
 
  xat2 = xat1 - mean(xat1) # centered by mean
  
  # final check by the x.diff distribution data 
  # in case any point falls outside of the plot
  if(!is.null(x.diff)){
    range.diff = range(x.diff)
	range.xat2 = range(xat2)
	if(range.diff[1]<range.xat2[1] || range.diff[2]>range.xat2[2]){
		shift = round(mean(x.diff)*10)/10
		xat2  = xat2 + shift
	}
  }
  if(!(0 %in% xat2))
    xat2 = c(xat2[xat2<0],0,xat2[xat2>0])
  list(xat1=xat1, xat2=xat2)
}
