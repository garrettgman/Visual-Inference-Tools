##############################################################################
## Date     : 2011-06-01
## Author   : Vivian Li
## Type     : grid drawing object and functions
## Usage    : a bar for mean/median or something else
## Note     : haven't tested for horizontal bar yet
##############################################################################
grid.ci = function(...){
  grid.draw(ciGrob(...))
}

ciGrob = function(value,at,wrange,digits=1,show=TRUE,name=NULL,gp=NULL,vp=NULL){
  ####################################################
  ####################################################
  igt = gTree(value=value,at=at,wrange=wrange,digits=digits,show=show,name=name,gp=gp,vp=vp,cl="cigrob")
  igt
}

drawDetails.cigrob = function(x, recording){
  x = setciGrob(x)
  if(x$show){
    for (i in childNames(x)) grid.draw(getGrob(x, i))
  }
}
setciGrob = function(x){
  ci  = x$value
  xat = x$at
  lpt = unit(ci[1],"native")
  rpt = unit(ci[2],"native")
  ypt = unit(c(-1,1),"lines")
  yarr= unit(c(-1.5,-2.3),"lines")
  mpt = unit(xat,"native")
  wr  = abs(diff(x$wrange))
  digits = x$digits
  
  # left and right blankets for confidence intervals
  sgl = segmentsGrob(sg,x0=lpt, x1=lpt,y0=ypt[1],y1=ypt[2],
                    gp=gpar(col="red",lwd=2),name="lowerline")
  
  sgr = editGrob(sgl,x0=rpt, x1=rpt,name="upperline")
  
  sgltp = editGrob(sgl,x1=unit(ci[1]+wr*0.01,"native"),y0=ypt[2],name="lowertop")
  sglbt = editGrob(sgl,x1=unit(ci[1]+wr*0.01,"native"),y1=ypt[1],name="lowerbot")
  
  sgrtp = editGrob(sgr,x0=unit(ci[2]-wr*0.01,"native"),y0=ypt[2],name="uppertop")
  sgrbt = editGrob(sgr,x0=unit(ci[2]-wr*0.01,"native"),y1=ypt[1],name="upperbot")
  
  # lower bound
  txtl   = textGrob(round(ci[1],digits), x=lpt, y=unit(-3.6,"lines"),
                    gp=gpar(cex=0.8), name="lower")
  sglarr = editGrob(sgl,y0=yarr[1],y1=yarr[2],
                    arrow = arrow(angle=30, length=unit(0.1, "inches"),
                                     ends="last", type="open"),name="lowerarr")
  # upper bound
  txtr   = editGrob(txtl,label=round(ci[2],digits), x=rpt,name="upper")
  sgrarr = editGrob(sglarr,x0=rpt,x1=rpt,name="upperarr")

  # center (mean or median)
  sgmarr = txtm = NULL
  mindist = wr*0.08
  if(abs(diff(c(xat,ci[2]))) < mindist || abs(diff(c(ci[1],xat))) < mindist){
    # if it's too narrow to draw....
    sgmarr = editGrob(sglarr,x0=mpt,x1=mpt,y0=unit(-0.1,"native"),y1=yarr[2]-unit(0.8,"lines"),name="centerarr")
    txtm   = editGrob(txtr,label=round(xat,digits), x=mpt,
                       y=unit(-4.2,"lines"),name="center")  
  }
  else{
      sgmarr = editGrob(sglarr,x0=mpt,x1=mpt,y0=unit(-0.1,"native"),name="centerarr")
      txtm   = editGrob(txtr,label=round(xat,digits), x=mpt, name="center")
  }
  
  x = setChildren(x, gList(sgl,sgr,sgltp,sglbt,sgrtp,sgrbt,sglarr,sgrarr,sgmarr,txtl,txtr,txtm))
  x
}
editDetails.cigrob = function(x, spec){
  x = ciGrob(value=x$value,at=x$at,wrange=x$wrange,digits=x$digits,show=x$show,name=x$name,gp=x$gp,vp=x$vp)
  x
}

validDetails.cigrob = function(x){
  x
}
