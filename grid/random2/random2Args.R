##############################################################################
## Date     : 2011-03-30
## Author   : Danny Chang
## Notes    : Vivian Li
## Type     : funcitons
## Usage    : random2 initial arguments setup
##############################################################################

initArgsRandom2 = function(){
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
  gp.diffbar = gpar(col="orangered", lwd=3)
  gp.indbar  = gpar(col="grey75", lty=2)
  gp.lab     = gpar(cex=2, col="black")
  
  arr = arrow(angle=30, length=unit(0.1, "inches"), ends="last", type="open")
  
  size.pts = unit(0.05, "inches")  
  gp.pts     = gpar(cex=0.7, lwd=2, col="black")
  
  # maximum rows of data displayed
  maxRow   = 80
  
  list(gcol1=gcol1, gcol2=gcol2, gp.pts=gp.pts, gp.box=gp.box,
       gp.diffbar=gp.diffbar, gp.lab=gp.lab,
       gp.indbar=gp.indbar, arr=arr, size.pts=size.pts, maxRow=maxRow)
}

initArgsRandom2TableFormat = function(x.df){
  # Notes: define the layout of data panel
  # maxRow    : maximum rows
  # rc.layout : c(rows,columns+1) for randomisation column
  maxRow = initArgsRandom2()$maxRow
  n = nrow(x.df)
  
  if(n>maxRow){
    rc.layout = c(maxRow+5, ncol(x.df)+1)
    x.df = x.df[1:maxRow,]
  } else {
    rc.layout = c(nrow(x.df), ncol(x.df)+1)
  }  

  list(x.df=x.df, rc.layout=rc.layout)
}

initArgsRandom2Table = function(x.df, main){
  # Notes:initiate the settings
  # fmt      : color and size settings
  # maxRow   : maximumn rows of data displayed
  # fmtTable : layout of the table
  # col.data : determine the color of second column
  # cex.*    : magnified shape related to default

  fmt       = initArgsRandom2()
  maxRow    = fmt$maxRow
  fmtTable  = initArgsRandom2TableFormat(x.df)
  x.df      = fmtTable$x.df
  rc.layout = fmtTable$rc.layout
  varname   = names(x.df)
  gcol1     = fmt$gcol1
  gcol2     = fmt$gcol2
  col.data  = cbind("black",
                    ifelse(as.numeric(x.df[,2])==1, gcol2[1], gcol2[2]))
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
       name="datatable", vp=vpPath("random2", "tablevp"))
}

# propBoxDiffGrob = function(x, group, fill=c("red", "blue"), label=NULL,
                           # width=unit(1, "npc"), height=unit(0.2, "npc"),
                           # pad=unit(0.25, "npc"), arrow=NULL, show.box=TRUE,
                           # gp.box1=gpar(), gp.box2=gpar(),
                           # gp.bar=gpar(), gp.indbar=gpar(), gp.lab=gpar(),
                           # name=NULL, gp=gpar(), vp=NULL){

initArgsRandom2Data = function(x, group){
  # Notes: setting for the graph
  # fmt       : color and size setting
  # label     : levels of group1 (first column)
  # arr       : arrow description
  # gp.boxi   : edge box color for group i
  # gp.inboxi : inner box color for group i
  # gp.bar    : arrow color
  # gp.indbar : arrow head and tail position lines
  # gp.lab    : label color
  
  fmt           = initArgsRandom2()
  fill          = NULL
  label         = levels(x)
  width         = unit(1, "native")
  height        = unit(0.3, "npc")
  pad           = unit(0.2, "npc") #??
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
       name="dataPropBox", vp=vpPath("random2", "datavp"))
}

initArgsRandom2Random = function(x, group){
  # fmt  = initArgsRandom2()
  fmt2 = initArgsRandom2Data(x, group)
  fmt2$arrow    = NULL
  #fmt2$pad      = unit(0.35, "npc")
  fmt2$show.box = FALSE
  fmt2$name     = "randomPropBox"
  fmt2$vp       = vpPath("random2", "randomvp")
  fmt2
}

initArgsRandom2Dist = function(x, x.diff){
  # Notes: distribution
  # fmt      : color and size settings
  # size.pts : size of points
  # at       : position of points ??
  # wrange   : range of centered differences
  
  fmt          = initArgsRandom2()
  size.pts     = fmt$size.pts
  at           = unit(0, "npc")
  wrange       = range(xatRandom2(x.diff)$xat2)
  stack.height = unit(1, "npc")
  horiz        = TRUE
  name         = "dist"
  gp           = fmt$gp.pts
  vp           = vpPath("random2", "distvp")
  show         = FALSE
  
  if(!is.null(x.diff))
    x = x.diff
  else
    x = 0
  
  list(x=x, size.pts=size.pts, at=at, stack.height=stack.height,
       wrange=wrange, horiz=horiz, show=show,
       name=name, gp=gp, vp=vp)
}

xatRandom2 = function(x.diff=NULL, r=c(0,1)){
  # Notes:
  # pretty(): equally spaced values with length n
  # modified by Vivian
  # usage: adjust the axis scale depends on data
  
  
  
  if(diff(r)==1){
    xat1 = pretty(r, n=11)
  }
  else{
    xat1 = pretty(r, diff(r)/0.1)
  }
  xat2 = xat1 - mean(xat1) # centered by mean
  
  #if(!is.null(x.diff)){
  #  if(max(x.diff) > max(xat2) || min(x.diff) < min(xat2)){
  #    # recalculate the position if overflow
  #    r    = range(x.diff + mean(r))
  #    xat1 = pretty(r, n=11)
  #    xat2 = xat1 - mean(xat1) # centered by mean
  #  }
  #}

  list(xat1=xat1, xat2=xat2)
}

checkGroupRandom2 = function(x.df){
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


random2Xaxis <- function(xat,w.scale,dr=0){
  # original scale
  xat1      = xat
  xat1Label = xat
  
  if(w.scale < 1)
    stop("w.scale must be greater or eqaul to 1")
  if(w.scale > 1){
    ## calculate new axis for datavp and randomvp
    if(dr==0){ # start from 0
      xend = 1/w.scale
      if(decimalplaces(xend)==1){
        xat1Label = seq(0,xend,0.1)
        xat1      = seq(0,1,length.out=length(xat1Label))  
      }
      else{
        xat1Label = c(seq(0,xend,0.1),round(xend,2))
        xat1      = c(seq(0,1,0.1*w.scale),1)
      }
    }
    else if(dr==1){ # end at 1
      xstart = 1-1/w.scale
      if(decimalplaces(xstart)==1){
        xat1Label = seq(xstart,1,0.1)
        xat1      = seq(0,1,length.out=length(xat1Label))
      }
      else{
        xat1Label = rev(c(seq(1,xstart,-0.1),round(xstart,2)))
        xat1      = rev(c(seq(1,0,-0.1*w.scale),0))
      }
    }
    #center = median(xat1Label)
    #if(!(center %in% xat1Label)){
    #  after    = length(which(xat1Label)<center)
    #  xat2Label = append(xat1Label,median(xat1Label),after=after)
    #  xat2      = append(xat1,median(xat1),after=after)
    #}
    #xat2Label = xat2Label - median(xat2Label)
    #xat2      = xat2 - median(xat2)   
  }
  return(data.frame(xat1=xat1,xat1Label=xat1Label))
}
