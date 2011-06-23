##############################################################################
## Date     : 2011-05-09
## Author   : Vivian Li
## Type     : action functions
## Usage    : bootstrap2 animation action
##############################################################################

bootstrap2GetCurrentGrob = function(){
  grid.get(grid.ls(print=FALSE)$name[1])
}

bootstrap2GetDataDigit = function(){
  bootstrap2GetCurrentGrob()$digit
}

bootstrap2GetCurrentDf = function(){
  bootstrap2GetCurrentGrob()$data
}

bootstrap2GetCurrentData = function(){
  grid.get("dataPropBox")$p
}

bootstrap2GetDiffFun = function(){
  bootstrap2GetCurrentGrob()$diffFun
}

bootstrap2GetRedrawGroup = function(tab, i){
  if(class(tab)!="list")
    stop("tab is not list")  
  tab[[i]]
}

bootstrap2GetDistData = function(){
  grid.get("bdist")$x
}

bootstrap2UpdateDistData = function(x){
  grid.edit("bdist", x=x)
}
bootstrap2UpdateDistShow = function(idx=TRUE,shape=1){
  # print i to j points
  if(!is.logical(idx)){
    gb  = grid.get("bdist")
    if(!is.logical(gb$show)){
      if(all(idx%in%gb$show))
        idx = gb$show
      else
        idx = c(gb$show,idx)
    }
  }
  grid.edit("bdist", show=idx, shape=shape)
}
bootstrap2UpdateDistShow2 = function(idx=TRUE, shape=1){
  # print 1 to i points
  grid.edit("bdist", show=idx, shape=shape)
}

bootstrap2ClearTable = function(){
  if(all(grid.get("bootstraptable::dataTxt")$label != "")){
    grid.edit("bootstraptable::dataTxt",label="")
  }
}
bootstrap2Finalise = function(){
  Sys.sleep(1)
  gb = bootstrap2GetCurrentGrob()
  grid.newpage()
  grid.draw(gb)
}

bootstrap2Refresh = function(){
  gb = bootstrap2GetCurrentGrob()
  gb = editGrob(gb,gPath("bstrapPropBox"), show=FALSE)
  gb = editGrob(gb,"bdist",show=FALSE)
  grid.newpage()
  grid.draw(gb)
}

bootstrap2Simulation = function(x, n){
  freq   = numeric(n)
  tab    = list()
  length(freq)= n
  length(tab) = n

  for(i in 1:n){
    tab[[i]] = sample(1:length(x),replace=T)
    freq[i]= (table(x[tab[[i]]])/length(x))[1]
  }
  list(p=freq,tab=tab)
}

bootstrap2UpdateBox = function(index,id){
  gb          = bootstrap2GetCurrentGrob()
  data.df     = gb$data
  x.bt        = as.data.frame(data.df[index,])
  names(x.bt) = names(data.df)
  label       = as.vector(t(as.matrix(x.bt)))
  
  x           = x.bt[,1]
  prop        = table(x)/length(x)
  boxlabel    = paste(levels(x),"(",table(x),")",sep="")
  
  #update tab
  if(length(gb$tab)==0)
    gb$tab = list(index)
  else{ 
    gb$tab[[length(gb$tab)+1]]=index
  }

  gb = editGrob(gb,gPath("bootstraptable","mainTxt"),label=paste("Resampling #",id,sep=""))
  gb = editGrob(gb,gPath("bootstraptable","dataTxt"),label=label)
  gb = editGrob(gb, "bstrapPropBox", p=prop, label=boxlabel,show=TRUE)

  grid.newpage()
  grid.draw(gb)
}

bootstrap2GetPointerInfo = function(index){
  # creating the pointer
  # from: where the pointer arrows start
  # to  : where the pointer arrows end
  # pad : the arrow starting position from the position of text
  
  gb1   = grid.get(gPath("datatable","dataTxt"))
  gb2   = grid.get(gPath("bootstraptable","dataTxt")) 
  
  from  = getrefPoint(gb1$x, gb1$y, "bootstrap2::tablevp::tablelay::datavp")
  to    = getrefPoint(gb2$x, gb2$y, "bootstrap2::btablevp::tablelay::datavp")
  
  from$x= from$x[index]
  from$y= from$y[index]
  
  pad   = max(as.numeric(convertWidth(stringWidth(gb1$label),"lines"))/2)
  pad   = unit(ceiling(pad*2)/2,"lines")

  gb3 = pointerGrob(name="pointergp",from=from,to=to,pad=pad,show=FALSE,fmt=initArgsBootstrap2Pointer())
  list(gb3=gb3)
}

bootstrap2ShowPointer = function(pointer,index,i){
  gb    = bootstrap2GetCurrentGrob()
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


bootstrap2RemovePointer = function(){
  gb    = bootstrap2GetCurrentGrob()
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

bootstrap2InitMovingTxt = function(index){
  gb    = bootstrap2GetCurrentGrob()
  gb1   = grid.get(gPath("datatable","dataTxt"))
  gb2   = grid.get(gPath("bootstraptable","dataTxt"))
  
  from  = getrefPoint(gb1$x, gb1$y, "bootstrap2::tablevp::tablelay::datavp")
  to    = getrefPoint(gb2$x, gb2$y, "bootstrap2::btablevp::tablelay::datavp")  
  posAt = getrefPoint(unit(1,"npc"),unit(1,"npc"),"bootstrap2::tablevp::tablelay::datavp")
  
  x.df  = gb$data
  x.bt  = x.df[index,]
  label = as.vector(t(as.matrix(x.bt)))

  col1  = rep("black", prod(dim(x.df)))
  
  movgp     = gb1$gp
  movgp$col = "red"
  indic     = segmentsGrob(name="indicator",x0=posAt$x,x1=posAt$x-unit(1,"line"),
                                 y0=unit(0,"npc"),y1=unit(0,"npc"),
                                 gp=gpar(col="red",lwd=3),
                                 arrow=initArgsBootstrap2()$arr)
  movTxt    = textGrob("",x=unit(0,"npc"),y=unit(0,"npc"),gp=movgp,name="mvrow")
  grid.edit(gPath("bootstraptable","dataTxt"),label="")
  list(col1=col1,from=from,to=to,x=gb2$x,y=gb2$y,label=label,indic=indic,movTxt=movTxt)
}
bootstrap2MoveTableFromTablevp = function(movObj,index,i,step=10, sleep=1.2){
  gb    = bootstrap2GetCurrentGrob()
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

bootstrap2MoveBarFromBstrapvp = function(step=10,type=1){

  agb   = grid.get("bstrapPropBox")
  agb   = setMeanBarGrob(meanBarGrob(xat=unit(agb$p[1],"native"),yat=agb$y,len=agb$height,name="propBar"))
  width = as.numeric(agb$len)
  at    = as.numeric(agb$yat)
  agb   = getGrob(agb, "meanBar #1")
  
  agb = editGrob(agb, name="propBarTemp",
                 y0=unit(at-width/2,"npc"), 
                 y1=unit(at+width/2,"npc"),
                 gp=gpar(col="black",lwd=3))

  from0 = getrefPoint(agb$x0, agb$y0, "bootstrap2::bstrapvp")
  from1 = getrefPoint(agb$x1, agb$y1, "bootstrap2::bstrapvp")
  x0    = as.numeric(agb$x0)
  x1    = as.numeric(agb$x1)
  
  if(type==1){
    to0   = getrefPoint(unit(x0, "native"), unit(-width/2*0.4, "npc"), "bootstrap2::bdistvp")
    to1   = getrefPoint(unit(x1, "native"), unit(+width/2*0.4, "npc"), "bootstrap2::bdistvp")  
  }
  else if(type==2){
    to0   = getrefPoint(unit(x0, "native"), unit(0, "npc"), "bootstrap2::bdistvp")
    to1   = getrefPoint(unit(x1, "native"), unit(0, "npc"), "bootstrap2::bdistvp")
  }
  
  step0 = list(x=seq.unit(from0$x, to0$x, length.out=step),
               y=seq.unit(from0$y, to0$y, length.out=step))
  step1 = list(x=seq.unit(from1$x, to1$x, length.out=step),
               y=seq.unit(from1$y, to1$y, length.out=step))
  
  agb   = editGrob(agb, x0=from0$x, y0=from0$y, x1=from1$x, y1=from1$y)
  grid.draw(agb)
               
  for(i in 1:step){
    grid.edit("propBarTemp", x0=step0$x[i], y0=step0$y[i],
                             x1=step1$x[i], y1=step1$y[i])
  }
}

bootstrap2MoveBarFromDatavp = function(step=20,type=1){
  
  agb   = grid.get("dataPropBox")
  agb   = setMeanBarGrob(meanBarGrob(xat=unit(agb$p[1],"native"),yat=agb$y,len=agb$height,name="propBar"))
  width = as.numeric(agb$len)
  at    = as.numeric(agb$yat)
  agb   = getGrob(agb, "meanBar #1")
  
  agb = editGrob(agb, name="propBarTemp",
                  y0=unit(at-width/2,"npc"), 
                  y1=unit(at+width/2,"npc"),
                  gp=gpar(col="red",lwd=3))

  from0 = getrefPoint(agb$x0, agb$y0, "bootstrap2::datavp")
  from1 = getrefPoint(agb$x1, agb$y1, "bootstrap2::datavp")  
  x0    = as.numeric(agb$x0)
  x1    = as.numeric(agb$x1)

  if(type==1){
    to0   = getrefPoint(unit(x0, "native"), unit(-width/2*0.4, "npc"), "bootstrap2::bdistvp")
    to1   = getrefPoint(unit(x1, "native"), unit(+width/2*0.4, "npc"), "bootstrap2::bdistvp")  
  }
  else if(type==2){
    to0   = getrefPoint(unit(x0, "native"), unit(-width/2, "npc"), "bootstrap2::bdistvp")
    to1   = getrefPoint(unit(x1, "native"), unit(width/2, "npc"), "bootstrap2::bdistvp")
  }
 
  step0 = list(x=seq.unit(from0$x, to0$x, length.out=step),
               y=seq.unit(from0$y, to0$y, length.out=step))
  step1 = list(x=seq.unit(from1$x, to1$x, length.out=step),
               y=seq.unit(from1$y, to1$y, length.out=step))
 
  agb   = editGrob(agb, x0=from0$x, y0=from0$y, x1=from1$x, y1=from1$y)
  grid.draw(agb)
  Sys.sleep(1)                   
  for(i in 1:step){
    grid.edit("propBarTemp", x0=step0$x[i], y0=step0$y[i],
                             x1=step1$x[i], y1=step1$y[i])
  }                        
  grid.points(x=unit(x0, "native"), y=unit(0, "npc"), pch=19,
              name="propPts", gp=gpar(col="red"), vp="bootstrap2::bdistvp")
  grid.remove("propBarTemp")
}

bootstrap2MoveBar = function(agb,vpfrom,vpto){
  
}

bootstrap2InitAddition = function(){
  gb = bootstrap2GetCurrentGrob()
  gb = editGrob(gb, "bdist", gp=gpar(col="black"))    
  grid.newpage()
  grid.draw(gb)
}



bootstrap2MoveBarFromDistvp = function(ci,step=20){
  x0    = unit(ci[1], "native")
  x1    = unit(ci[2], "native")
  y0=y1 = unit(0,"npc")
  
  agb   = rectlinesGrob(x0=x0, y0=y0,x1=x1, y1=y1,
                        gp=gpar(col="red",lwd=4), name="cibar")
                
  from0 = getrefPoint(x0, y0, "bootstrap2::bdistvp")
  from1 = getrefPoint(x1, y1, "bootstrap2::bdistvp")
  to0   = getrefPoint(x0, unit(0.5,"npc"),"bootstrap2::datavp")
  to1   = getrefPoint(x1, unit(0.5,"npc"), "bootstrap2::datavp")

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

bootstrap2ShowCI = function(xat,method="p"){
  x   = bootstrap2GetDistData()[grid.get("bdist")$show]
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
    xat2 = unit(0.1,"npc")  
  else
    xat2 = unit(0.75,"npc")
    
  y    = unit(0.5, "npc")-unit(1, "lines")
  
  string = paste("Est: ",round(xat,2),"\n","CI : [",round(ci[1],2),",",round(ci[2],2),"]",sep="")
  sg  = segmentsGrob(x0=unit(xat, "native"), y0=unit(0, "npc"),
                     x1=unit(xat, "native"), y1=y,
                     gp=gpar(col="grey"), vp="bootstrap2::bdistvp")                  
  txt = textGrob(string, x=xat2, y=unit(0.8,"npc"),
                 just="left", gp=gpar(cex=0.9), vp="bootstrap2::bdistvp",name="legend")
  lg  = rectGrob(x=xat2-unit(0.01,"npc"),y=unit(0.8,"npc"),just="left",
                 width=unit(0.25,"npc"),height=unit(0.25,"npc"),
                 vp="bootstrap2::bdistvp",name="lgframe")
  
  if("medPts" %in% grid.ls(print=FALSE)$name){
    mptsgb = grid.get("medPts")
  } else {
    mptsgb = NULL
  }

  idx = (x<=ci[2] & x>=ci[1])
  col = ifelse(!idx, "grey55",  "black")
  
  # confidence interval
  con.in = ciGrob(ci,xat,grid.get("bdist")$wrange,digits=2,name="confidence",vp="bootstrap2::bdistvp")

  gb = bootstrap2GetCurrentGrob()
  gb = editGrob(gb, "bdist", gp=gpar(col=col))
  pts= grid.get("propPts")
  grid.newpage()
  grid.draw(gb)
  grid.draw(gList(con.in,lg,txt,mptsgb,pts))
  Sys.sleep(1.5)
  bootstrap2MoveBarFromDistvp(ci)
}
