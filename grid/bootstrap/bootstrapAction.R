##############################################################################
## Date     : 2011-05-09
## Author   : Vivian Li
## Type     : action functions
## Usage    : bootstrap 1 animation action
##############################################################################

bootstrapGetCurrentGrob = function(){
  grid.get(grid.ls(print=FALSE)$name[1])
}

bootstrapGetDataDigit = function(){
  bootstrapGetCurrentGrob()$digit
}

bootstrapGetCurrentDf = function(){
  bootstrapGetCurrentGrob()$data
}

bootstrapGetCurrentData = function(){
  grid.get("dataBoxDot")$x
}

bootstrapGetDiffFun = function(){
  bootstrapGetCurrentGrob()$diffFun
}

boostrapRoundDf = function(x.df){
  digit = bootstrapGetDataDigit() # sigh: digit = bootstrapGetCurrentGrob()$digit
  initArgsBootstrapRoundData(x.df,digit) #?
}

bootstrapGetRedrawGroup = function(tab, i){
  if(class(tab)!="list")
    stop("tab is not list")  
  tab[[i]]
}

###############################################
# reset the panel
###############################################
# remove table from bootstraptable
bootstrapClearTable = function(){
  if(all(grid.get("bootstraptable::dataTxt")$label != "")){
    grid.edit("bootstraptable::dataTxt",label="")
  }
}
# remove ghost boxes from bstravp
bootstrapFinalise = function(){
  Sys.sleep(2)
  gb = bootstrapGetCurrentGrob()
  gb = editGrob(gb,gPath("bstrapBoxDot"),show.pts=FALSE,show.box=FALSE,show.w=FALSE) # removes points, box, and w - but a blue line showed up. Does it make the ghost box? Is the ghost box always there?
  grid.newpage()
  grid.draw(gb)
}
# remove ghost boxes, bstrpvp and bootstrap distribution
bootstrapRefresh = function(){
  gb = bootstrapGetCurrentGrob()
  gb$tab = list() # makes table an empty list
  gb = editGrob(gb,gPath("ghostBox"),tab=list()) # ghostbox's tab slot to empty list?
  # hides box points and whiskers
  gb = editGrob(gb,gPath("bstrapBoxDot"), show.pts=FALSE,show.box=FALSE,show.w=FALSE)
  # hides "bdist" - top distribution?
  gb = editGrob(gb,"bdist",show=FALSE)
  grid.newpage() # erases everything/opens a new device
  grid.draw(gb) # redraws the page without the bootstrap information
}

bootstrapSimulation = function(x, n){
  est   = numeric(n)
  tab    = list()
  length(tab) = n
  FUN    = bootstrapGetDiffFun()

  for(i in 1:n){
    index    = sample(1:length(x),replace=T)
    newx     = x[index]
    tab[[i]] = index
    est[i]  = FUN(newx) 
  }
  list(est=est, tab=tab)
}

# draw a new bootstrap
bootstrapUpdateBox = function(index,id){
  gb      = bootstrapGetCurrentGrob() # grab the screen
  data.df = gb$data # extract the data
  x.bt    = as.data.frame(data.df[index,]) # grab the sample of the data to be plotted
  names(x.bt)=names(data.df) # grab the names used for the data
  x.bt.str= boostrapRoundDf(x.bt) # seems to round each value to 3 signficant digits
  
  label   = as.vector(t(as.matrix(x.bt.str))) # captures data vaues as a character vector

  #update tab
  if(length(gb$tab)==0)
    gb$tab = list(index)
  else{ 
    gb$tab[[length(gb$tab)+1]]=index # appends index to end of table
  }

  gb = editGrob(gb,gPath("ghostBox"),tab=gb$tab) # make the tab of the ghost box equal the tab of gb
  gb = editGrob(gb,gPath("bootstraptable","mainTxt"),label=paste("Resampling #",id,sep="")) # change title of sample table
  gb = editGrob(gb,gPath("bootstraptable","dataTxt"),label=label) # fill the data values into the sample table
  gb = editGrob(gb, "bstrapBoxDot", x=x.bt[,1]) # replace bootstrap x dots with new locations
  
  mgb = NULL
  if(identical(bootstrapGetDiffFun(), mean)){ # if mean is selected
    gb = editGrob(gb, "bstrapBoxDot", show.pts=TRUE, show.w=FALSE) # show the points, but not the w
    center =  mean(x.bt) # calculate mean
    mgb = meanBarGrob(xat=unit(center,"native"),yat=unit(0.5,"npc"),len=unit(0.3,"npc"),
                      name="meanBar",gp=gpar(lwd=4),vp=vpPath("bootstrap","bstrapvp")) # creates the mean bar
  }
  else{ # if median is selected
    gb = editGrob(gb, "bstrapBoxDot", show.pts=TRUE,show.box=TRUE,show.w=TRUE) # what does w mean?
  }

  grid.newpage() # clear and then draw...
  grid.draw(gList(gb,mgb)) # gb and mgb
}

bootstrapShowPointer = function(pointer,index,i){
  gb    = bootstrapGetCurrentGrob()
  gb1   = grid.get(gPath("datatable","dataTxt"))
  gb2   = grid.get(gPath("bootstraptable","dataTxt"))
  
  gb3   = pointer$gb3

  col1  = rep("black",length(index)*2)
  col2  = rep("red",length(index)*2)
  
  gb1$gp$col = col1
  gb2$gp$col = col2
  gb1$gp$col[i]  = "red"  

  idx  = which(index==i)
  if(length(idx)>0){
    gb2$gp$col[idx] = "black"
    gb3 = editGrob(gb3,show=idx)
  } 
  else
    gb3 = editGrob(gb3,show=FALSE)
  
  gb    = editGrob(gb,"datatable::dataTxt",gp=gb1$gp) 
  gb    = editGrob(gb,"bootstraptable::dataTxt",gp=gb2$gp)
   
  grid.newpage()
  grid.draw(gb)
  grid.draw(gb3)
  list(gb3=gb3)
}


bootstrapRemovePointer = function(){
  gb    = bootstrapGetCurrentGrob()
  data  = gb$data
  nr    = nrow(data)
  nc    = ncol(data)
  col1  = rep("black",nr*nc)
  col2  = rep("red",nr*nc)
  
  gb1   = grid.get(gPath("datatable","dataTxt"))
  gb2   = grid.get(gPath("bootstraptable","dataTxt"))
  
  gb1$gp$col = col1
  gb2$gp$col = col2
  
  gb    = editGrob(gb,"datatable::dataTxt",gp=gb1$gp)
  gb    = editGrob(gb,"bootstraptable::dataTxt",gp=gb2$gp)
  grid.newpage()
  grid.draw(gb)
}

bootstrapInitMovingTxt = function(index){
  gb    = bootstrapGetCurrentGrob()
  gb1   = grid.get(gPath("datatable","dataTxt"))
  gb2   = grid.get(gPath("bootstraptable","dataTxt"))
  
  from  = getrefPoint(gb1$x, gb1$y, "bootstrap::tablevp::tablelay::datavp")
  to    = getrefPoint(gb2$x, gb2$y, "bootstrap::btablevp::tablelay::datavp")  
  posAt = getrefPoint(unit(1,"npc"),unit(1,"npc"),"bootstrap::tablevp::tablelay::datavp")
  
  x.df  = gb$data
  digit = gb$digit
  x.df  = initArgsBootstrapRoundData(x.df,digit)
  x.bt  = x.df[index,]
  label = as.vector(t(as.matrix(x.bt)))

  col1  = rep("black", prod(dim(x.df)))
  
  movgp     = gb1$gp
  movgp$col = "red"
  indic     = segmentsGrob(name="indicator",x0=posAt$x,x1=posAt$x-unit(1,"line"),
                                 y0=unit(0,"npc"),y1=unit(0,"npc"),
                                 gp=gpar(col="red",lwd=3),
                                 arrow=initArgsBootstrap()$arr)
  movTxt    = textGrob("",x=unit(0,"npc"),y=unit(0,"npc"),gp=movgp,name="mvrow")
  grid.edit(gPath("bootstraptable","dataTxt"),label="")
  list(col1=col1,from=from,to=to,x=gb2$x,y=gb2$y,label=label,indic=indic,movTxt=movTxt)
}
bootstrapMoveTableFromTablevp = function(movObj,index,i,step=8, sleep=1.2){
  
  gb    = bootstrapGetCurrentGrob()
  data  = gb$data
  
  col1  = movObj$col1
  label = movObj$label
  from  = movObj$from
  to    = movObj$to
  tX    = movObj$x
  tY    = movObj$y

  indic = movObj$indic
  movTxt= movObj$movTxt

  idx   = index[i]
  pos0 = idx
  pos1 = i
    
  x0  = from$x[pos0]
  y0  = from$y[pos0]
  x1  = to$x[pos1]
  y1  = to$y[pos1]

  pr = (length(index)-i)/length(index)
  pr = ifelse(pr<0.25,0.25,pr)

  if(i>=8)
    step  = 2
  
  gp1 = getGrob(gb,"datatable::dataTxt")$gp
  gp2 = getGrob(gb,"bootstraptable::dataTxt")$gp
  
  # data table
  gp1$col = col1
  gp1$col[pos0] = "red"
  gb  = editGrob(gb,"datatable::dataTxt", gp=gp1)
  
  # indicator (small arrow)
  indic = editGrob(indic,y0=y0[1],y1=y0[1])
  
  # animation
  grid.newpage()
  grid.draw(gList(gb,indic,movTxt))
    
  Sys.sleep(sleep*pr)
  
  #movement effect
  if(step>2){   
    string = label[pos1]

    smoothMov = function(v0,v1,step){
      v0 = as.numeric(v0)
      v1 = as.numeric(v1)
      route = matrix(numeric((step+2)*length(v0)),nc=length(v0))
      
      df = (v1-v0)[1]*c(0,0.1,1:step)/step
      for(cnum in 1:length(v0)){
        route[,cnum]=v0[cnum]+df
      }
      route
    }
    x.route = smoothMov(x0,x1,step)
    y.route = smoothMov(y0,y1,step)
    for(a in 1:step){
      grid.edit("mvrow",x=unit(x.route[a,],"npc"),y=unit(y.route[a,],"npc"),label=string)
    } 
  }  
  # bootstrap table
  gb = editGrob(gb,"bootstraptable::dataTxt", x=tX[1:pos1],y=tY[1:pos1],label=label[1:pos1])
  grid.newpage()
  grid.draw(gb)
  if(i == length(index)){
    Sys.sleep(sleep*pr)
    gp1$col = col1
    grid.edit(gPath("datatable","dataTxt"),gp=gp1)
  }
}

bootstrapMoveBarFromBstrapvp = function(step=10,type=1){

  if(is.null(grid.get("meanBar"))){
    agb   = setBoxdotGrob(grid.get("bstrapBoxDot"))
    agb   = setBxpGrob(getGrob(agb, "bxp"))
    width = as.numeric(agb$width)
    at    = as.numeric(agb$at)
    agb = getGrob(agb,"medbar")
  }
  else{
    agb   = setMeanBarGrob(grid.get("meanBar"))
    width = as.numeric(agb$len)
    at    = as.numeric(agb$yat)
    agb   = getGrob(agb, "meanBar #1")
  }
  
  agb = editGrob(agb, name="medbarTemp",
                 y0=unit(at-width/2,"npc"), 
                 y1=unit(at+width/2,"npc"),
                 gp=gpar(col="black",lwd=3))

  from0 = getrefPoint(agb$x0, agb$y0, "bootstrap::bstrapvp")
  from1 = getrefPoint(agb$x1, agb$y1, "bootstrap::bstrapvp")
  x0    = as.numeric(agb$x0)
  x1    = as.numeric(agb$x1)
  
  if(type==1){
    to0   = getrefPoint(unit(x0, "native"), unit(-width/2*0.4, "npc"), "bootstrap::bdistvp")
    to1   = getrefPoint(unit(x1, "native"), unit(+width/2*0.4, "npc"), "bootstrap::bdistvp")  
  }
  else if(type==2){
    to0   = getrefPoint(unit(x0, "native"), unit(0, "npc"), "bootstrap::bdistvp")
    to1   = getrefPoint(unit(x1, "native"), unit(0, "npc"), "bootstrap::bdistvp")
  }
  
  step0 = list(x=seq.unit(from0$x, to0$x, length.out=step),
               y=seq.unit(from0$y, to0$y, length.out=step))
  step1 = list(x=seq.unit(from1$x, to1$x, length.out=step),
               y=seq.unit(from1$y, to1$y, length.out=step))
  
  agb   = editGrob(agb, x0=from0$x, y0=from0$y, x1=from1$x, y1=from1$y)
  grid.draw(agb)
               
  for(i in 1:step){
    grid.edit("medbarTemp", x0=step0$x[i], y0=step0$y[i],
                             x1=step1$x[i], y1=step1$y[i])
  }
}

bootstrapMoveBarFromDatavp = function(step=20,type=1){
  
  agb   = setBoxdotGrob(grid.get("dataBoxDot"))
  if(!agb$show.box){
    y     = agb$x
    agb   = setMeanBarGrob(meanBarGrob(xat=unit(mean(y),"native"),yat=agb$at,len=unit(0.3,"npc"),name="meanBar"))
    width = as.numeric(agb$len)
    at    = as.numeric(agb$yat)
    agb   = getGrob(agb, "meanBar #1")
    
  }
  else{
    agb   = setBxpGrob(getGrob(agb, "bxp"))
    width = as.numeric(agb$width)
    at    = as.numeric(agb$at)
    agb = getGrob(agb,"medbar")
  }
  
  agb = editGrob(agb, name="medbarTemp",
                  y0=unit(at-width/2,"npc"), 
                  y1=unit(at+width/2,"npc"),
                  gp=gpar(col="red",lwd=3))
  
  from0 = getrefPoint(agb$x0, agb$y0, "bootstrap::datavp")
  from1 = getrefPoint(agb$x1, agb$y1, "bootstrap::datavp")  
  x0    = as.numeric(agb$x0)
  x1    = as.numeric(agb$x1)

  if(type==1){
    to0   = getrefPoint(unit(x0, "native"), unit(-width/2*0.4, "npc"), "bootstrap::bdistvp")
    to1   = getrefPoint(unit(x1, "native"), unit(+width/2*0.4, "npc"), "bootstrap::bdistvp")  
  }
  else if(type==2){
    to0   = getrefPoint(unit(x0, "native"), unit(-width/2, "npc"), "bootstrap::bdistvp")
    to1   = getrefPoint(unit(x1, "native"), unit(width/2, "npc"), "bootstrap::bdistvp")
  }
 
  step0 = list(x=seq.unit(from0$x, to0$x, length.out=step),
               y=seq.unit(from0$y, to0$y, length.out=step))
  step1 = list(x=seq.unit(from1$x, to1$x, length.out=step),
               y=seq.unit(from1$y, to1$y, length.out=step))
 
  agb   = editGrob(agb, x0=from0$x, y0=from0$y, x1=from1$x, y1=from1$y)
  grid.draw(agb)
  Sys.sleep(1)                   
  for(i in 1:step){
    grid.edit("medbarTemp", x0=step0$x[i], y0=step0$y[i],
                             x1=step1$x[i], y1=step1$y[i])
  }                        
  grid.points(x=unit(x0, "native"), y=unit(0, "npc"), pch=19,
              name="medPts", gp=gpar(col="red"), vp="bootstrap::bdistvp")
  grid.remove("medbarTemp")
}

bootstrapMoveBar = function(agb,vpfrom,vpto){
  
}

bootstrapInitAddition = function(){
  gb = bootstrapGetCurrentGrob()
  gb = editGrob(gb, "bdist", gp=gpar(col="black"))    
  grid.newpage()
  grid.draw(gb)
}

bootstrapGetDistData = function(){
  grid.get("bdist")$x
}

bootstrapUpdateDistData = function(x){
  grid.edit("bdist", x=x)
}

bootstrapUpdateDistShow = function(idx=TRUE){
  if(!is.logical(idx)){
    gb  = grid.get("bdist")
    if(!is.logical(gb$show)){
      if(all(idx%in%gb$show))
        idx = gb$show
      else
        idx = c(gb$show,idx)
    }
  }
  grid.edit("bdist", show=idx, shape=1)
}
bootstrapUpdateDistShow2 = function(idx=TRUE, shape=1){
  grid.edit("bdist", show=idx, shape=shape)
}

bootstrapMoveBarFromDistvp = function(ci,step=20){
  x0    = unit(ci[1], "native")
  x1    = unit(ci[2], "native")
  y0=y1 = unit(0,"npc")
  
  agb   = rectlinesGrob(x0=x0, y0=y0,x1=x1, y1=y1,
                        gp=gpar(col="red",lwd=4), name="cibar")
                
  from0 = getrefPoint(x0, y0, "bootstrap::bdistvp")
  from1 = getrefPoint(x1, y1, "bootstrap::bdistvp")
  to0   = getrefPoint(x0, unit(0.5,"npc"),"bootstrap::datavp")
  to1   = getrefPoint(x1, unit(0.5,"npc"), "bootstrap::datavp")

  step0 = list(x=seq.unit(from0$x, to0$x, length.out=step),
               y=seq.unit(from0$y, to0$y, length.out=step))
  step1 = list(x=seq.unit(from1$x, to1$x, length.out=step),
               y=seq.unit(from1$y, to1$y, length.out=step))
  
  agb   = editGrob(agb, x0=from0$x, y0=from0$y, x1=from1$x, y1=from1$y)
  grid.draw(agb)
  agb   = editGrob(agb,name="cibarTemp")
  grid.draw(agb)
  Sys.sleep(1)  

  for(i in 1:step){
    grid.edit("cibarTemp", x0=step0$x[i], y0=step0$y[i],
                           x1=step1$x[i], y1=step1$y[i])
  }                        
}

bootstrapShowCI = function(xat,method="p"){
  x   = bootstrapGetDistData()[grid.get("bdist")$show]
  n   = length(x)
  
  if(method=="p")
    ci  = quantile(x,c(0.025,0.975))
  else{
    std = sd(x)
    ci  = xat+c(-1,1)*1.96*std
  }
  downViewport("bdistvp")  
  upViewport(2)
  
  # information frame
  m    = mean(x)
  cent = mean(grid.get("bdist")$wrange)
  if(m>=cent)
    xat2 = unit(0.15,"npc")  
  else
    xat2 = unit(0.75,"npc")
    
  y    = unit(0.5, "npc")-unit(1, "lines")
  
  string = paste("Est: ",round(xat,2),"\n","CI : [",round(ci[1],2),",",round(ci[2],2),"]",sep="")
  sg  = segmentsGrob(x0=unit(xat, "native"), y0=unit(0, "npc"),
                     x1=unit(xat, "native"), y1=y,
                     gp=gpar(col="grey"), vp="bootstrap::bdistvp")                  
  txt = textGrob(string, x=xat2, y=unit(0.8,"npc"),
                 just="left", gp=gpar(cex=0.9), vp="bootstrap::bdistvp",name="legend")
  lg  = rectGrob(x=xat2-unit(0.01,"npc"),y=unit(0.8,"npc"),just="left",
                 width=unit(0.25,"npc"),height=unit(0.25,"npc"),
                 vp="bootstrap::bdistvp",name="lgframe")
  
  # segments for confidence interval
  sgci = bootstrapCI(xat,ci,sg)
  
  if("medPts" %in% grid.ls(print=FALSE)$name){
    mptsgb = grid.get("medPts")
  } else {
    mptsgb = NULL
  }

  idx = (x<=ci[2] & x>=ci[1])
  col = ifelse(!idx, "grey55",  "black")

  gb = bootstrapGetCurrentGrob()
  gb = editGrob(gb, "bdist", gp=gpar(col=col))
  grid.newpage()
  grid.draw(gb)
  grid.draw(gList(sgci,lg,txt,mptsgb))
  Sys.sleep(1.5)
  bootstrapMoveBarFromDistvp(ci)
}

bootstrapCI = function(xat,ci,sg){
  lpt = unit(ci[1],"native")
  rpt = unit(ci[2],"native")
  ypt = unit(c(-1,1),"lines")
  yarr= unit(c(-1.5,-2.3),"lines")
  mpt = unit(xat,"native")
  
  # left and right blankets for confidence intervals
  sgl = editGrob(sg,x0=lpt, x1=lpt,
                    y0=ypt[1],y1=ypt[2],
                    gp=gpar(col="red",lwd=2))
  
  sgr = editGrob(sgl,x0=rpt, x1=rpt)
  
  sgltp = editGrob(sgl,x1=unit(ci[1]+1.5,"native"),y0=ypt[2])
  sglbt = editGrob(sgl,x1=unit(ci[1]+1.5,"native"),y1=ypt[1])
  
  sgrtp = editGrob(sgr,x0=unit(ci[2]-1.5,"native"),y0=ypt[2])
  sgrbt = editGrob(sgr,x0=unit(ci[2]-1.5,"native"),y1=ypt[1])
  
  # lower bound
  txtl   = textGrob(round(ci[1],2), x=lpt, y=unit(-3.6,"lines"),
                    gp=gpar(cex=0.8), vp="bootstrap::bdistvp",name="lower")
  sglarr = editGrob(sgl,y0=yarr[1],y1=yarr[2],
                    arrow = arrow(angle=30, length=unit(0.1, "inches"),
                                     ends="last", type="open"))
  # upper bound
  txtr   = editGrob(txtl,label=round(ci[2],2), x=rpt,name="upper")
  sgrarr = editGrob(sglarr,x0=rpt,x1=rpt)
  
  # center (mean or median)
  sgmarr = txtm = NULL
  mindist = diff(grid.get("bdist")$wrange)*0.08
  if(abs(diff(c(xat,ci[2]))) < mindist || abs(diff(c(ci[1],xat))) < mindist){
    # if it's too narrow to draw....
    sgmarr = editGrob(sglarr,x0=mpt,x1=mpt,y0=unit(-0.1,"native"),y1=yarr[2]-unit(0.8,"lines"))
    txtm   = editGrob(txtr,label=round(xat,2), x=mpt,
                       y=unit(-4.2,"lines"),name="center")  
  }
  else{
      sgmarr = editGrob(sglarr,x0=mpt,x1=mpt,y0=unit(-0.1,"native"))
      txtm   = editGrob(txtr,label=round(xat,2), x=mpt)
  }
  gList(sgl,sgr,sgltp,sglbt,sgrtp,sgrbt,sglarr,sgrarr,sgmarr,txtl,txtr,txtm)
}