##############################################################################
## Date     : 2011-04-05
## Author   : Danny Chang
## Type     : funcitons
## Usage    : random3 initial arguments setup
##############################################################################

initArgsRandom3 = function(ngp=3){
  gcol       = topo.colors(10)
  #gcol       = barcols(ngp)
  gp.pts     = gpar(cex=0.7, lwd=2)                 
  col.box    = rep(rgb(0, 0, 0, alpha=0.2), 3)
  gp.box     = gpar(lwd=3)
  gp.lab     = gpar()
  gp.diffbar = gpar(col="orangered", lwd=3)
  gp.indbar  = gpar(lty=2, lwd=2)
  arr        = arrow(angle=30, length=unit(0.1, "inches"),
                     ends="last", type="open")
  size.pts   = unit(0.05, "inches")
  maxRow     = 80
  
  list(gcol=gcol, gp.pts=gp.pts, col.box=col.box, gp.box=gp.box, gp.lab=gp.lab,
       gp.diffbar=gp.diffbar, gp.indbar=gp.indbar, arr=arr, size.pts=size.pts,
       maxRow=maxRow)
}

initArgsRandom3TableFormat = function(x.df,ngp=3){
  maxRow = initArgsRandom3(ngp)$maxRow
  n = nrow(x.df)
  
  if(n>maxRow){
    rc.layout = c(maxRow+5, ncol(x.df)+1)
    x.df = x.df[1:maxRow,]
  } else {
    rc.layout = c(nrow(x.df), ncol(x.df)+1)
  }

  list(x.df=x.df, rc.layout=rc.layout)
}

initArgsRandom3Table = function(x.df, main, digit,ngp=3){
  maxRow    = initArgsRandom3(ngp)$maxRow
  fmtTable  = initArgsRandom3TableFormat(x.df)
  x.df      = rounddf(fmtTable$x.df, digit, "g")  
  rc.layout = fmtTable$rc.layout
  varname   = names(x.df)
  gcol      = initArgsRandom3(ngp)$gcol
  col.data  = cbind("black", gcol[as.numeric(x.df[,2])])
  
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
       name="datatable", vp=vpPath("random3", "tablevp"))
}

initArgsRandom3Data = function(x, group, diffFun,ngp=3){
  fmt           = initArgsRandom3(ngp)
  pad           = 1
  arrow         = fmt$arr
  #glab          = levels(group)
  glab          = NULL
  size.pts      = fmt$size.pts  
  wrange        = range(x)
  diffFun       = diffFun
  show          = TRUE  
  show.box      = identical(median, diffFun)
  col.pts       = fmt$gcol
  gp.pts        = fmt$gp.pts
  gp.box        = fmt$gp.box
  col.box       = fmt$col.box
  gp.lab        = fmt$gp.lab  
  gp.bar        = fmt$gp.diffbar
  gp.indbar     = fmt$gp.indbar
  
  list(x=x, group=group, pad=pad, arrow=arrow, glab=glab,
       size.pts=size.pts, wrange=wrange, diffFun=diffFun, show=show,
       show.box=show.box, col.pts=col.pts, gp.pts=gp.pts, gp.box=gp.box,
       col.box=col.box, gp.lab=gp.lab, gp.bar=gp.bar, gp.indbar=gp.indbar,
       name="dataBoxDotsMulti", vp=vpPath("random3", "datavp"))
}

initArgsRandom3Random = function(x, group, diffFun,ngp=3){
  # fmt  = initArgsRandom3(ngp)
  fmt2 = initArgsRandom3Data(x, group, diffFun)
  
  fmt2$arrow = NULL
  fmt2$show  = FALSE
  fmt2$name  = "randomBoxDotsMulti"
  fmt2$vp    = vpPath("random3", "randomvp")
  fmt2
}

initArgsRandom3Funnel = function(w){
  x      = unit(0.5, "npc")
  y      = unit(2,   "lines")
  width  = w
  height = unit(2, "lines")
  name   = "funnel"
  vp     = vpPath("random3", "funnelvp")
  gp     = gpar(lwd=2, fill="black")
  list(x=x, y=y, width=width, height=height, name=name, gp=gp, vp=vp)
}

initArgsRandom3Dist = function(x, g, p, ngp){
  fmt = initArgsRandom3(ngp)
  size.pts = fmt$size.pts
  at = unit(0, "npc")
  wrange = range(xatRandom3(x, g, p)$xat2)
  stack.height = unit(1, "npc")
  horiz = TRUE
  name = "dist"
  gp = fmt$gp.pts
  vp = vpPath("random3", "distvp")
  show = FALSE
  
  if(!is.null(p))
    x = p
  
  list(x=x, size.pts=size.pts, at=at, stack.height=stack.height,
       wrange=wrange, horiz=horiz, show=show,
       name=name, gp=gp, vp=vp)
}

xatRandom3 = function(x, g, p=NULL, FUN=mean){
  xat1 = pretty(range(x), n=11)  

  if(!is.null(p)){
    p    = c(p, fValRandom3(x, g, FUN))
    xat2 = pretty(range(p), n=11)
  }
  else {
    ng   = length(levels(g))
    dfb  = ng-1
    dfw  = length(x)-ng
    y    = qf(0.99, dfb, dfw)    
    xat2 = pretty(c(0, y),  n=11)
  }
  
  list(xat1=xat1, xat2=xat2)
}

fValRandom3 = function(x, g, FUN=mean){
  ng  = length(levels(g))
  dfb = ng-1
  dfw = length(x)-ng
  SSB = sum((tapply(x, g, FUN)-mean(x))^2*tapply(x, g, length))
  SSW = sum((x-tapply(x, g, FUN)[g])^2)
  MSB = SSB/dfb
  MSW = SSW/dfw
  F   = MSB/MSW
  F
}

  # x = c(rnorm(100, -10, 4), rnorm(100, 0, 5),
        # rnorm(100, 3, 3), rnorm(100, 20, 10))
  # g = sample(factor(rep(c("A", "B", "C", "D"), each=100)))
  
  # n = 1000
  # f = numeric()
  # for(i in 1:n)
    # f[i] = fValRandom3(x, sample(g), mean)
  
  # hist(f, breaks=40, freq=FALSE)
  # xf = seq(0, 10, by=0.01)
  # ng  = length(levels(g))
  # dfb = ng-1
  # dfw = length(x)-ng
  # yf = df(xf, dfb, dfw)
  # lines(xf, yf, lwd=2)
  # points(fValRandom3(x, g, mean), 0, pch=19)
  
  
  