##############################################################################
## Date     : 2011-03-01
## Author   : Danny Chang
## Type     : action functions
## Usage    : random 1 animation action
##############################################################################

random1GetCurrentGrob = function(){
  grid.get(grid.ls(print=FALSE)$name[1])
}

random1GetCurrentDf = function(){
  random1GetCurrentGrob()$data
}

random1GetCurrentData = function(){
  grid.get("dataBoxDots")$x
}

random1GetDataGroup = function(){
  grid.get("dataBoxDots")$group
}

random1GetDiffFun = function(){
  random1GetCurrentGrob()$diffFun
}

random1Simulation = function(x.df, n){
  x = x.df[,1]
  group = x.df[,2]

  diff = numeric(n)
  tab = list()
  length(tab) = n
  # tab = data.frame(matrix(nrow=length(group), ncol=n))
  for(i in 1:n){
    g = sample(group)
    tab[[i]] = g
    # tab[,i] = g
    diff[i] = diff(tapply(x, g, random1GetDiffFun()))
  }
  list(diff=diff, tab=tab)
}

random1GetRedrawGroup = function(tab, i){
  if(class(tab)!="list")
    stop("tab is not list")
  
  tab[[i]]
}

random1RemoveArrow = function(){
  #grid.remove("diffbarTemp")
  gb = random1GetCurrentGrob()
  grid.newpage()
  grid.draw(gb)
}

random1MoveArrowBarFromRandomvp = function(step=10){
  agb = setBoxdotsDiffGrob(grid.get("randomBoxDots"))
  agb = setBoxdotsDiffGrob(editGrob(agb, arrow=initArgsRandom1()$arr))  
  agb = getGrob(agb, "diffbar")
  agb = editGrob(agb, name="diffbarTemp")
  from0 = getrefPoint(agb$x0, agb$y0, "random1::randomvp")
  from1 = getrefPoint(agb$x1, agb$y1, "random1::randomvp")
  x0    = as.numeric(agb$x0)
  x1    = as.numeric(agb$x1)
  to0   = getrefPoint(unit(x1-x1, "native"), unit(0, "npc"), "random1::distvp")
  to1   = getrefPoint(unit(x1-x0, "native"), unit(0, "npc"), "random1::distvp")
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
                           vp="random1::distvp")  
  
  #grid.remove("diffbarTemp")
}

random1MoveArrowBarFromDatavp = function(step=20){
  agb = setBoxdotsDiffGrob(grid.get("dataBoxDots"))
  agb = getGrob(agb, "diffbar")
  agb = editGrob(agb, name="diffbarTemp")
  from0 = getrefPoint(agb$x0, agb$y0, "random1::datavp")
  from1 = getrefPoint(agb$x1, agb$y1, "random1::datavp")  
  x0    = as.numeric(agb$x0)
  x1    = as.numeric(agb$x1)
  to0   = getrefPoint(unit(x1-x1, "native"), unit(0, "npc"), "random1::distvp")
  to1   = getrefPoint(unit(x1-x0, "native"), unit(0, "npc"), "random1::distvp")
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
                           vp="random1::distvp")
                           
  grid.points(x=unit(x1-x0, "native"), y=unit(0, "npc"), pch=19,
              name="diffPts", gp=gpar(col="red"), vp="random1::distvp")
  #grid.remove("diffbarTemp")
}

random1MoveDataPtsToRandomVP = function(step=15){
  random1InitTableGroup()
  x = setBoxdotsDiffGrob(grid.get("dataBoxDots"))
  downViewport("datavp")
  
  gp1 = setBoxdotGrob(getGrob(x, "group1bxp"))
  gp1 = setStackptsGrob(getGrob(gp1, "pts"))
  gp1 = editGrob(gp1, name="gp1DataPtsTemp", gp=initArgsRandom1()$gp.pts[[2]])  
  gp2 = setBoxdotGrob(getGrob(x, "group2bxp"))
  gp2 = setStackptsGrob(getGrob(gp2, "pts"))
  gp2 = editGrob(gp2, name="gp2DataPtsTemp", gp=initArgsRandom1()$gp.pts[[3]])
  upViewport(2)
  
  moveDataPts = function(cpt, step){    
    p = getrefPoint(cpt$x, cpt$y, "random1::datavp")
    cpt = editGrob(cpt, x=p$x, y=p$y)
    from.y = unit(min(as.numeric(p$y)), "npc")
    to.y = getrefPoint(unit(0.5, "npc"), unit(0.5, "npc"),
                       "random1::randomvp")$y
    
    step.size = unit((as.numeric(from.y)-as.numeric(to.y))/step, "npc") 
    
    grid.draw(cpt)
    for(i in 1:step){
      grid.edit(cpt$name, y=grid.get(cpt$name)$y-step.size)
    }
  }  
  moveDataPts(gp1, step)
  moveDataPts(gp2, step)

  gb = random1GetCurrentGrob()
  gp1 = editGrob(grid.get("gp1DataPtsTemp"), gp=initArgsRandom1()$gp.pts[[1]])
  gp2 = editGrob(grid.get("gp2DataPtsTemp"), gp=initArgsRandom1()$gp.pts[[1]])
  gb = gList(gb, gp1, gp2)
  grid.newpage()
  grid.draw(gb)
}

random1ShowRandomvp = function(show=c(TRUE, TRUE, TRUE)){  
  grid.edit("randomBoxDots", show=show)
}

random1MoveRandomApart = function(group, step=10){
  random1InitTableGroup()
  x   = random1GetCurrentData()
  if(length(group)!=length(x))
    stop("length of group and x is different")
    
  arg = initArgsRandom1Random(x, group, random1GetDiffFun())
  fmt = initArgsRandom1()
  
  arg$pad  = unit(0, "npc")
  arg$show = c(TRUE, TRUE, TRUE)
  arg$gp.pts.x = gpar(col=rep("black",length(group)), lwd=2)
  arg$gp.pts.group1$col = "black"
  arg$gp.pts.group2$col = "black"
                      
  arg$name = "dataBoxDotsTemp"
  arg$show = c(TRUE,FALSE,FALSE)

  if("dataBoxDotsTemp" %in% grid.ls(print=FALSE)$name)
    grid.remove("dataBoxDotsTemp")
    
  if(any(c("gp1DataPtsTemp", "gp2DataPtsTemp")%in%grid.ls(print=FALSE)$name)){
    grid.draw(do.call("boxdotsDiffGrob", arg))
    grid.remove("gp1DataPtsTemp")
    grid.remove("gp2DataPtsTemp")
  } else {
    grid.draw(do.call("boxdotsDiffGrob", arg))
  }  
  
  random1UpdateTableGroup(group)
  Sys.sleep(1)
  grid.edit(arg$name, gp.pts.x=gpar(col=ifelse(as.numeric(group)==1, fmt$gcol2[1],
                                               fmt$gcol2[2]),lwd=2),
                      gp.pts.group1=fmt$gp.pts[[2]], gp.pts.group2=fmt$gp.pts[[3]],
                      show = rep(TRUE,3))
  
  p = seq(0, 0.35, length.out=step)

  for(i in p)
   grid.edit("dataBoxDotsTemp", pad=unit(i, "npc"))
}

random1ShowRandomArror = function(show=TRUE){
  if(show)
    grid.edit("randomBoxDots", arrow=initArgsRandom1()$arr)
  else
    grid.edit("randomBoxDots", arrow=NULL)
}

random1InitAddition = function(){
  gb = random1GetCurrentGrob()
  gb = editGrob(gb, "dist", gp=gpar(col="black"))    
  grid.newpage()
  grid.draw(gb)
}

random1InitTableGroup = function(){
  gb = random1GetCurrentGrob()
  gb = editGrob(gb, "randomBoxDots", show=c(FALSE, FALSE, FALSE), arrow=NULL)
  gb = editGrob(gb, "datatable",
                data     = grid.get("datatable")$data[,1:2],
                col.data = grid.get("datatable")$col.data[,1:2])
  grid.newpage()
  grid.draw(gb)  
}

random1UpdateTableGroup = function(group){ 
  x.df = random1GetCurrentDf()
  if(nrow(x.df)!=length(group))
    stop("new update group has different length")
      
  fmt = initArgsRandom1()
  col = ifelse(as.numeric(group)==1,  fmt$gcol[1],  fmt$gcol[2])  
  
  x2.df    = grid.get("datatable")$data
  col.data = grid.get("datatable")$col.data
  nr = nrow(x2.df)
  if(ncol(x2.df)==2){
    x2.df = cbind(x2.df, as.character(group[1:nr]))
    col.data = cbind(col.data, col[1:nr])
  } else if(ncol(x2.df)==3){
    x2.df[,3] = as.character(group[1:nr])
    col.data[,3] = col[1:nr]
  } else {
    ## should not happend
    temp = paste("ncol(x2.df) is", ncol(x.df), "!? What happend?")
    stop(temp)
  }
  
  varname = grid.get("datatable")$varname
  if(length(varname)==2)
    varname = c(varname, "Rerandom")
  grid.edit("datatable", data=x2.df, varname=varname, col.data=col.data)
}


random1UpdateGroup = function(group){
  #x.df = grid.get("datatable")$data 
  #random1InitAddition()
  
  x.df = random1GetCurrentDf()
  if(nrow(x.df)!=length(group))
    stop("new update group has different length")
      
  fmt = initArgsRandom1()
  col = ifelse(as.numeric(group)==1,  fmt$gcol[1],  fmt$gcol[2])
  col2 = ifelse(as.numeric(group)==1, fmt$gcol2[1],  fmt$gcol2[2])
  gp.pts.x = gpar(col=col2, lwd=2)
  
  
  x2.df    = grid.get("datatable")$data
  col.data = grid.get("datatable")$col.data
  nr = nrow(x2.df)
  if(ncol(x2.df)==2){
    x2.df = cbind(x2.df, as.character(group[1:nr]))
    col.data = cbind(col.data, col[1:nr])
  } else if(ncol(x2.df)==3){
    x2.df[,3] = as.character(group[1:nr])
    col.data[,3] = col[1:nr]
  } else {
    ## should not happend
    temp = paste("ncol(x2.df) is", ncol(x.df), "!? What happend?")
    stop(temp)
  }
  
  varname = grid.get("datatable")$varname
  if(length(varname)==2)
    varname = c(varname, "Rerandom")  
  
  gb = random1GetCurrentGrob()
  gb = editGrob(gb, "randomBoxDots", group = group, gp.pts.x=gp.pts.x,
                                     show  = rep(TRUE, 3),
                                     arrow = initArgsRandom1()$arr)
  gb = editGrob(gb, "datatable", data=x2.df, varname=varname, col.data=col.data)  
  if(identical(random1GetDiffFun(), mean)){
    xat = tapply(random1GetCurrentData(), group, random1GetDiffFun())
    gbTemp1 = segmentsGrob(x0=unit(xat[1], "native"),
                           y0=unit(0.75, "npc"),
                           x1=unit(xat[1], "native"),
                           y1=unit(0.95, "npc"),
                           gp=gpar(lwd=3), vp="random1::randomvp")
    gbTemp2 = segmentsGrob(x0=unit(xat[2], "native"),
                           y0=unit(0.05, "npc"),
                           x1=unit(xat[2], "native"),
                           y1=unit(0.25, "npc"),
                           gp=gpar(lwd=3), vp="random1::randomvp")
    gb = gList(gb, gbTemp1, gbTemp2)
  }
  grid.newpage()
  grid.draw(gb)
}

random1GetDistData = function(){
  grid.get("dist")$x
}

random1UpdateDistData = function(x){
  grid.edit("dist", x=x)
}

random1UpdateDistShow = function(idx=TRUE, shape=1){
  grid.edit("dist", show=idx, shape=shape)
}

random1ShowTailProp = function(xat){
  x = random1GetDistData()[grid.get("dist")$show]
  n = length(x)
  
  makeStr = function(n, idx){
    s = sum(idx)
    r = sprintf("%.3f", round(s/n, 3))
    paste(s, "/", n, " = ", r, sep="")
  }
  
  downViewport("distvp")
  if(xat >= 0){
    idx  = x >= xat
    string = makeStr(n, idx)
    linewidth = stringWidth(string)        
    w = convertX(unit(max(x), "native")+linewidth, "inches", valueOnly=TRUE)
    a = convertX(unit(1, "npc"), "inches", valueOnly=TRUE)
    if(w<a){
      just = "left"
      xat2 = unit(max(x), "native")
    } else {
      just = "right"
      xat2 = unit(1, "npc")
    }
  }
  else {
    idx  = x <= xat 
    string = makeStr(n, idx)
    linewidth = stringWidth(string)
    w = convertX(unit(min(x), "native")-linewidth, "inches", valueOnly=TRUE)
    a = convertX(unit(0, "npc"), "inches", valueOnly=TRUE)
    if(w>a){
      just = "right"
      xat2 = unit(min(x), "native")
    } else {
      just = "left"
      xat2 = unit(0, "npc")
    }
  }    
  upViewport(2)
  
  y = unit(0.5, "npc")                     
  if(as.numeric(xat2) %in% c(0, 1) || all(!idx)){
    arr = NULL
    y1  = y-unit(1, "lines")
  } else {
    y1  = y
    arr = segmentsGrob(x0=unit(xat, "native"), y0=y, x1=xat2, y1=y,
                       arrow = arrow(angle=30, length=unit(0.1, "inches"),
                                     ends="last", type="open"),
                       gp=gpar(col="grey"), vp="random1::distvp")
  }

  sg  = segmentsGrob(x0=unit(xat, "native"), y0=unit(0, "npc"),
                     x1=unit(xat, "native"), y1=y1,
                     gp=gpar(col="grey"), vp="random1::distvp")
  
  txt = textGrob(string, x=xat2, y=y,
                 just=just, gp=gpar(cex=1.1), vp="random1::distvp")

  if("diffPts" %in% grid.ls(print=FALSE)$name){
    dptsgb = grid.get("diffPts")
  } else {
    dptsgb = NULL
  }

                 
  col = ifelse(!idx, "grey",  "black")
  gb = random1GetCurrentGrob()
  gb = editGrob(gb, "dist", gp=gpar(col=col))
  gb = gList(gb, sg, txt, arr, dptsgb)
  grid.newpage()
  grid.draw(gb)
}