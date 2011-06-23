##############################################################################
## Date     : 2011-05-09
## Author   : Vivian Li
## Type     : action functions
## Usage    : bootstrap 1 animation action (two sample difference)
##############################################################################

bootstrap1GetCurrentGrob = function(){
  grid.get(grid.ls(print=FALSE)$name[1])
}

bootstrap1GetDataDigit = function(){
  bootstrap1GetCurrentGrob()$digit
}

bootstrap1GetCurrentDf = function(){
  bootstrap1GetCurrentGrob()$data
}

bootstrap1GetCurrentData = function(){
  grid.get("dataBoxDots")$x
}

bootstrap1GetDataGroup = function(){
  grid.get("dataBoxDots")$group
}

bootstrap1GetDiffFun = function(){
  bootstrap1GetCurrentGrob()$diffFun
}

boostrap1RoundDf = function(x.df){
  digit = bootstrap1GetDataDigit()
  initArgsBootstrap1RoundData(x.df,digit)
}

bootstrap1GetRedrawGroup = function(tab, i){
  if(class(tab)!="list")
    stop("tab is not list")  
  tab[[i]]
}

# clear the bootstrap table
bootstrap1ClearTable = function(){
  if(all(grid.get("bootstraptable::dataTxt")$label != "")){
    grid.edit("bootstraptable::dataTxt",label="")
  }
}

# clear the most current bootstrap boxplot
bootstrap1Finalise = function(){
  Sys.sleep(2)
  gb = bootstrap1GetCurrentGrob()
  gb = editGrob(gb,gPath("bstrapBoxDots"),show=rep(FALSE,3),arrow=NULL)
  grid.newpage()
  grid.draw(gb)
}

# clear bstravp, bdistvp
bootstrap1Refresh = function(){
  gb = bootstrap1GetCurrentGrob()
  gb$tab = list()
  gb = editGrob(gb,gPath("ghostBox"),tab=list())
  gb = editGrob(gb,gPath("bstrapBoxDots"), show=rep(FALSE,3),arrow=NULL)
  gb = editGrob(gb,"bdist",show=FALSE)
  grid.newpage()
  grid.draw(gb)
}

bootstrap1Simulation = function(x.df, n){
  diff   = numeric(n)
  tab    = list()
  length(tab) = n
  FUN    = bootstrap1GetDiffFun()
 
  for(i in 1:n){
    OK = FALSE; try=0
    while(!OK) { # fix to overcome bootstrap groups in small samples with no entries in one of the categories
       try = try + 1; if (try>2) stop("Too many tries to overcome 0-entry groups")
       index    = sample(1:nrow(x.df),replace=T)
       newx.df  = x.df[index,]
       x        = newx.df[,1]
       g        = newx.df[,2]
       if (min(table(g)>0)) OK=TRUE
    }    
 
    tab[[i]] = index
    diff[i]  = diff(tapply(x, g, FUN))
   
  }
  list(diff=diff, tab=tab)
}

# old simulation function regardless whether there is no obs in either group
bootstrap1SimulationOld = function(x.df, n){
  diff   = numeric(n)
  tab    = list()
  length(tab) = n
  FUN    = bootstrap1GetDiffFun()

  for(i in 1:n){
    index    = sample(1:nrow(x.df),replace=T)
    newx.df  = x.df[index,]
    x        = newx.df[,1]
    g        = newx.df[,2]
	
    tab[[i]] = index
    diff[i]  = diff(tapply(x, g, FUN))
  }
  list(diff=diff, tab=tab)
}

bootstrap1UpdateGhostBox = function(index,id){
  
  gb      = bootstrap1GetCurrentGrob()
  data.df = gb$data
  x.bt    = data.df[index,]
  x.bt.str= boostrap1RoundDf(x.bt)
  label   = as.vector(t(as.matrix(x.bt.str)))

  #update tab
  if(length(gb$tab)==0)
    gb$tab = list(index)
  else{ 
    gb$tab[[length(gb$tab)+1]]=index
  }
  
  gb = editGrob(gb,gPath("ghostBox"),tab=gb$tab)

  gb = editGrob(gb,gPath("bootstraptable","mainTxt"),label=paste("Resampling #",id,sep=""))
  
  gb = editGrob(gb,gPath("bootstraptable","dataTxt"),label=label)
  
  gb = editGrob(gb, "bstrapBoxDots", x=x.bt[,1],group= x.bt[,2],
                                     show  = c(FALSE, TRUE, TRUE),
                                     arrow = initArgsBootstrap1()$arr)
  mgb = NULL
  if(identical(bootstrap1GetDiffFun(), mean)){
    center =  tapply(x.bt[,1],x.bt[,2],mean)
    mgb = meanBarGrob(xat=unit(center,"native"),yat=unit(0.5,"npc"),len=unit(0.2,"npc"),
                      name="meanBars",gp=gpar(lwd=4),vp=vpPath("bootstrap1","bstrapvp"))
  }

  grid.newpage()
  grid.draw(gList(gb,mgb))
}

bootstrap1RemoveArrow = function(){
  if(!is.null(grid.get("diffbarTemp")))
   grid.remove("diffbarTemp")
}

bootstrap1GetPointerInfo = function(index){
  # creating the pointer
  # from: where the pointer arrows start
  # to  : where the pointer arrows end
  
  gb1   = grid.get(gPath("datatable","dataTxt"))
  gb2   = grid.get(gPath("bootstraptable","dataTxt")) 
  
  from  = getrefPoint(gb1$x, gb1$y, "bootstrap1::tablevp::tablelay::datavp")
  to    = getrefPoint(gb2$x, gb2$y, "bootstrap1::btablevp::tablelay::datavp")
  edge  = rep(c(F,T),length(index)) #only need right edge of the tablevp and left edge of the bstrapvp
  
  from$x= from$x[edge]
  from$y= from$y[edge]
  to$x  = to$x[!edge]
  to$y  = to$y[!edge]
  from$x=from$x[index]
  from$y=from$y[index]
  
  gb3 = pointerGrob(name="pointergp",from=from,to=to,show=FALSE,fmt=initArgsBootstrap1Pointer())
  
  list(gb3=gb3)
}

bootstrap1ShowPointer = function(pointer,index,i){
  gb    = bootstrap1GetCurrentGrob()
  gb1   = grid.get(gPath("datatable","dataTxt"))
  gb2   = grid.get(gPath("bootstraptable","dataTxt"))
  
  gb3   = pointer$gb3
  
  col1  = rep("black",length(index)*2)
  col2  = rep("red",length(index)*2)
  
  pos = c(i*2-1,i*2)
  gb1$gp$col = col1
  gb2$gp$col = col2
  gb1$gp$col[pos]  = "red"  

  idx  = which(index==i)
  if(length(idx)>0){
    pos2 = c(idx*2-1,idx*2)
    gb2$gp$col[pos2] = "black"
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


bootstrap1RemovePointer = function(){
  gb    = bootstrap1GetCurrentGrob()
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

bootstrap1InitMovingTxt = function(index){
  gb    = bootstrap1GetCurrentGrob()
  gb1   = grid.get(gPath("datatable","dataTxt"))
  gb2   = grid.get(gPath("bootstraptable","dataTxt"))
  
  from  = getrefPoint(gb1$x, gb1$y, "bootstrap1::tablevp::tablelay::datavp")
  to    = getrefPoint(gb2$x, gb2$y, "bootstrap1::btablevp::tablelay::datavp")  
  posAt = getrefPoint(unit(1,"npc"),unit(1,"npc"),"bootstrap1::tablevp::tablelay::datavp")
  
  x.df  = gb$data
  digit = gb$digit
  x.df  = initArgsBootstrap1RoundData(x.df,digit)
  x.bt  = x.df[index,]
  label = as.vector(t(as.matrix(x.bt)))

  col1  = rep("black", prod(dim(x.df)))
  
  movgp     = gb1$gp
  movgp$col = "red"
  indic     = segmentsGrob(name="indicator",x0=posAt$x,x1=posAt$x-unit(1,"line"),
                                 y0=unit(0,"npc"),y1=unit(0,"npc"),
                                 gp=gpar(col="red",lwd=3),
                                 arrow=initArgsBootstrap1()$arr)
  movTxt    = textGrob("",x=unit(0,"npc"),y=unit(0,"npc"),gp=movgp,name="mvrow")
  grid.edit(gPath("bootstraptable","dataTxt"),label="")
  list(col1=col1,from=from,to=to,x=gb2$x,y=gb2$y,label=label,indic=indic,movTxt=movTxt)
}
bootstrap1MoveTableFromTablevp2 = function(movObj,index,i,step=8, sleep=1.2){
  gb    = bootstrap1GetCurrentGrob()
  data  = gb$data
  nc    = ncol(data)
  
  col1  = movObj$col1
  label = movObj$label
  from  = movObj$from
  to    = movObj$to
  tX    = movObj$x
  tY    = movObj$y

  indic = movObj$indic
  movTxt= movObj$movTxt

  idx   = index[i]
  pos0 = c(idx*nc-1,idx*nc)
  pos1 = c(i*nc-1,i*nc)
    
  x0  = from$x[pos0]
  y0  = from$y[pos0]
  x1  = to$x[pos1]
  y1  = to$y[pos1]
  
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
  
  pr = (length(index)-i)/length(index)
  
  if(pr<0.25)
    pr  = 0.25
  
  sleep = sleep*pr
  
  if(i>=8){
    #sleep = sleep*0.50
    step  = 2
  }
  
  Sys.sleep(sleep)
  
  #movement effect
  if(step>2){   
    string = label[pos1]

    smoothMov = function(v0,v1,step){
      v0 = as.numeric(v0)
      v1 = as.numeric(v1)
      route = matrix(numeric((step+2)*length(v0)),nc=length(v0))
      
      df = (v1-v0)[1]*c(0,0.1,1:step)/step
      route[,1]=v0[1]+df
      route[,2]=v0[2]+df
      route
    }
    x.route = smoothMov(x0,x1,step)
    y.route = smoothMov(y0,y1,step)

    for(a in 1:step){
      grid.edit("mvrow",x=unit(x.route[a,],"npc"),y=unit(y.route[a,],"npc"),label=string)
    } 
  }  
  # bootstrap table
  gb = editGrob(gb,"bootstraptable::dataTxt", x=tX[1:pos1[2]],y=tY[1:pos1[2]],label=label[1:pos1[2]])
  grid.newpage()
  grid.draw(gb)
  if(i == length(index)){
    Sys.sleep(sleep)
    gp1$col = col1
    grid.edit(gPath("datatable","dataTxt"),gp=gp1)
  }
}

bootstrap1MoveArrowBarFromBstrapvp = function(step=10){
  agb = setBoxdotsDiffGrob(grid.get("bstrapBoxDots"))
  agb = setBoxdotsDiffGrob(editGrob(agb, arrow=initArgsBootstrap1()$arr))  
  agb = getGrob(agb, "diffbar")
  agb = editGrob(agb, name="diffbarTemp")

  from0 = getrefPoint(agb$x0, agb$y0, "bootstrap1::bstrapvp")
  from1 = getrefPoint(agb$x1, agb$y1, "bootstrap1::bstrapvp")
  x0    = as.numeric(agb$x0)
  x1    = as.numeric(agb$x1)
  # starts at 0, then extends to length of the arrow
  to0   = getrefPoint(unit(x1-x1, "native"), unit(0, "npc"), "bootstrap1::bdistvp")
  to1   = getrefPoint(unit(x1-x0, "native"), unit(0, "npc"), "bootstrap1::bdistvp")
  step0 = list(x=seq.unit(from0$x, to0$x, length.out=step),
               y=seq.unit(from0$y, to0$y, length.out=step))
  step1 = list(x=seq.unit(from1$x, to1$x, length.out=step),
               y=seq.unit(from1$y, to1$y, length.out=step))
 
  agb   = editGrob(agb, x0=from0$x, y0=from0$y, x1=from1$x, y1=from1$y)
  grid.draw(agb)
                     
  for(i in 1:step){
    grid.edit("diffbarTemp", x0=step0$x[i], y0=step0$y[i],
                             x1=step1$x[i], y1=step1$y[i])
  }
  
  grid.edit("diffbarTemp", x0=unit(0, "native"), y0=unit(0, "npc"),
                           x1=unit(x1-x0,     "native"), y1=unit(0, "npc"),
                           vp="bootstrap1::bdistvp")  
}

bootstrap1MoveArrowBarFromDatavp = function(step=20){
  agb = setBoxdotsDiffGrob(grid.get("dataBoxDots"))
  agb = getGrob(agb, "diffbar")
  agb = editGrob(agb, name="diffbarTemp")
  from0 = getrefPoint(agb$x0, agb$y0, "bootstrap1::datavp")
  from1 = getrefPoint(agb$x1, agb$y1, "bootstrap1::datavp")  
  x0    = as.numeric(agb$x0)
  x1    = as.numeric(agb$x1)
  to0   = getrefPoint(unit(x1-x1, "native"), unit(0, "npc"), "bootstrap1::bdistvp")
  to1   = getrefPoint(unit(x1-x0, "native"), unit(0, "npc"), "bootstrap1::bdistvp")
  step0 = list(x=seq.unit(from0$x, to0$x, length.out=step),
               y=seq.unit(from0$y, to0$y, length.out=step))
  step1 = list(x=seq.unit(from1$x, to1$x, length.out=step),
               y=seq.unit(from1$y, to1$y, length.out=step))
 
  agb   = editGrob(agb, x0=from0$x, y0=from0$y, x1=from1$x, y1=from1$y)
  grid.draw(agb)
                     
  for(i in 1:step){
    grid.edit("diffbarTemp", x0=step0$x[i], y0=step0$y[i],
                             x1=step1$x[i], y1=step1$y[i])
  }
  
  grid.edit("diffbarTemp", x0=unit(0, "native"), y0=unit(0, "npc"),
                           x1=unit(x1-x0,     "native"), y1=unit(0, "npc"),
                           vp="bootstrap1::bdistvp")
                           
  grid.points(x=unit(x1-x0, "native"), y=unit(0, "npc"), pch=19,
              name="diffPts", gp=gpar(col="red"), vp="bootstrap1::bdistvp")
}

bootstrap1ShowBstrapArror = function(show=TRUE){
  if(show)
    grid.edit("bstrapBoxDots", arrow=initArgsbootstrap1()$arr)
  else
    grid.edit("bstrapBoxDots", arrow=NULL)
}

bootstrap1InitAddition = function(){
  gb = bootstrap1GetCurrentGrob()
  gb = editGrob(gb, "bdist", gp=gpar(col="black"))    
  grid.newpage()
  grid.draw(gb)
}

bootstrap1InitTableGroup = function(){
  gb = bootstrap1GetCurrentGrob()
  gb = editGrob(gb, "bstrapBoxDots", show=c(FALSE, FALSE, FALSE), arrow=NULL)
  gb = editGrob(gb, "datatable",
                data     = grid.get("datatable")$data[,1:2],
                col.data = grid.get("datatable")$col.data[,1:2])
  grid.newpage()
  grid.draw(gb)  
}

bootstrap1GetDistData = function(){
  grid.get("bdist")$x
}

bootstrap1UpdateDistData = function(x){
  grid.edit("bdist", x=x)
}

bootstrap1UpdateDistShow = function(idx=TRUE){
  if(!is.logical(idx)){
    gb  = grid.get("bdist")
    if(!is.logical(gb$show)){
      if(all(idx%in%gb$show))
        idx = gb$show
      else
        idx = c(gb$show,idx)
    }
  }
  grid.edit("bdist", show=idx)
}
bootstrap1UpdateDistShow2 = function(idx=TRUE, shape=2){
  grid.edit("bdist", show=idx, shape=shape)
}

bootstrap1ShowCI = function(xat,method="p"){
  x   = bootstrap1GetDistData()[grid.get("bdist")$show]
  n   = length(x)
  
  if(method=="p")
    ci  = quantile(x,c(0.025,0.975))
  else{
    std = sd(x)
    ci  = xat+c(-1,1)*1.96*std
  }

  downViewport("bdistvp")

  if(xat >= 0){
    idx  = x >= xat
  }
  else {
    idx  = x <= xat 
  }    
  upViewport(2)
  xat2 = unit(0.75,"npc")
  y = unit(0.5, "npc")                     
  if(as.numeric(xat2) %in% c(0, 1) || all(!idx)){
    arr = NULL
    y1  = y-unit(1, "lines")
  } else {
    y1  = y
  }
  string = paste("Est: ",round(xat,1),"\n","CI : [",round(ci[1],1),",",round(ci[2],1),"]",sep="")
  sg  = segmentsGrob(x0=unit(xat, "native"), y0=unit(0, "npc"),
                     x1=unit(xat, "native"), y1=y1,
                     gp=gpar(col="grey"), vp="bootstrap1::bdistvp")                  
  txt = textGrob(string, x=xat2, y=unit(0.8,"npc"),
                 just="left", gp=gpar(cex=0.9), vp="bootstrap1::bdistvp")
  lg  = rectGrob(x=xat2-unit(0.01,"npc"),y=unit(0.8,"npc"),just="left",
                 width=unit(0.25,"npc"),height=unit(0.25,"npc"),
                 vp="bootstrap1::bdistvp")
  
  if("diffPts" %in% grid.ls(print=FALSE)$name){
    dptsgb = grid.get("diffPts")
  } else {
    dptsgb = NULL
  }

  idx = (x<=ci[2] & x>=ci[1])
  col = ifelse(!idx, "grey55",  "black")

  arr = grid.get("diffbarTemp")
  if(!is.null(arr)){
    arr$gp$col = "forestgreen"
    arr$gp$lwd = 4
    arr$arrow$length=unit(0.15,"inches")
  }
  
  # confidence interval
  con.in = ciGrob(ci,xat,grid.get("bdist")$wrange,name="confidence",vp="bootstrap1::bdistvp")
  
  gb = bootstrap1GetCurrentGrob()
  gb = editGrob(gb, "bdist", gp=gpar(col=col))
  gb = editGrob(gb, "dataBoxDots", gp.bar=gpar(col="forestgreen",lwd=3))
  grid.newpage()
  grid.draw(gb)
  grid.draw(gList(con.in,lg,txt,dptsgb,arr))
}
