##############################################################################
## Date     : 2011-04-05
## Author   : Danny Chang
## Type     : action functions
## Usage    : random 1 animation action
##############################################################################

random3GetCurrentGrob = function(){
  grid.get(grid.ls(print=FALSE)$name[1])
}

random3GetCurrentDf = function(){
  random3GetCurrentGrob()$data
}

random3GetCurrentData = function(){
  grid.get("dataBoxDots")$x
}

random3GetDataGroup = function(){
  grid.get("dataBoxDots")$group
}

random3GetDiffFun = function(){
  random3GetCurrentGrob()$diffFun
}

random3GetpFUN = function(){
  random3GetCurrentGrob()$pFUN
}

random3GetFunnelXY = function(){
  getrefPoint(unit(0.5, "npc"), unit(2, "lines"), "random3::funnelvp")
}

random3Simulation = function(x.df, n){
  x = x.df[,1]
  group = x.df[,2]
  
  p = numeric(n)
  tab = list()
  length(tab) = n
  diffFUN = random3GetDiffFun()
  pFUN = random3GetpFUN()
  
  for(i in 1:n){
    g = sample(group)
    tab[[i]] = g
    p[i] = pFUN(x, g, diffFUN)
  }
  list(p=p, tab=tab)
}

random3GetRedrawGroup = function(tab, i){
  if(class(tab)!="list")
    stop("tab is not list")
  
  tab[[i]]
}

random3MoveArrowBarToFunnel = function(step=20, fromvp, gbname){  
  agb = setBoxdotsDiffMultiGrob(grid.get(gbname))
  agb = gTree(children=getGrob(agb, "diffbar", grep=TRUE, global=TRUE),
              name="diffbarTemp")
  
  n   = length(childNames(agb))
  from0 = from1 = step0 = step1 = list()
  length(from0) = length(from1) = length(step0) = length(step1) = n
  
  to0 = random3GetFunnelXY()
  to0$y = to0$y + unit(1, "lines")
  to1 = to0
  
  to.pad = unit(seq(1, 0, length.out=2*n+1)[c(TRUE, FALSE)], "lines")
  for(i in 1:n){
    agbChild = getGrob(agb, childNames(agb)[i])
    from0[[i]] = getrefPoint(agbChild$x0, agbChild$y0, fromvp)
    from1[[i]] = getrefPoint(agbChild$x1, agbChild$y1, fromvp)
    
    d = convertWidth(from0[[i]]$x-from1[[i]]$x, "npc")
    to0x = to0$x
    to1x = convertX(to0x-d, "npc")
    to0x = convertX(to0x+0.5*d, "npc")
    to1x = convertX(to1x+0.5*d, "npc")

    
    to0y = convertY(to0$y+to.pad[i], "npc")
    to1y = convertY(to1$y+to.pad[i], "npc")    
    step0[[i]] = list(x=seq.unit(from0[[i]]$x, to0x, length.out=step),
                      y=seq.unit(from0[[i]]$y, to0y,         length.out=step))
    step1[[i]] = list(x=seq.unit(from1[[i]]$x, to1x, length.out=step),
                      y=seq.unit(from1[[i]]$y, to1y,         length.out=step))
  }

  setXY = function(agb, step0, step1, i){
    x0 = lapply(step0, function(x) x$x[i])
    y0 = lapply(step0, function(x) x$y[i])
    x1 = lapply(step1, function(x) x$x[i])
    y1 = lapply(step1, function(x) x$y[i])
    
    n  = length(childNames(agb))
    for(j in 1:n){
      agb = editGrob(agb, childNames(agb)[j], x0=x0[[j]], y0=y0[[j]],
                                              x1=x1[[j]], y1=y1[[j]])
    }
    agb
  }
  
  agb = setXY(agb, step0, step1, 1)
  grid.draw(agb)
  
  for(i in 2:step){
    # if(i!=step){
      grid.remove("diffbarTemp")
      agb = setXY(agb, step0, step1, i)
      grid.draw(agb)
    # } else {
      # grid.remove("diffbarTemp")
      # grid.circle(toXY$x, toXY$y, name="diffbarTemp", r=unit(0.05, "inches"),
                  # gp=gpar(col="orangered", fill="orangered"),
                  # vp="random3::funnelvp")
    # }
  }
  #grid.remove("diffbarTemp")
}

random3MoveArrowBarFromDatavp = function(step=10){
  random3MoveArrowBarToFunnel(step, "random3::datavp", "dataBoxDotsMulti")
}

random3MoveArrowBarFromRandomvp = function(step=5){
  random3MoveArrowBarToFunnel(step, "random3::randomvp", "randomBoxDotsMulti")
}

random3MoveDataPtsToRandomVP = function(step=10){  
  random3InitTableGroup()  
  
  downViewport("datavp")
  x = setBoxdotsDiffMultiGrob(grid.get("dataBoxDotsMulti"))
  x = getGrob(x, "boxdot", grep=TRUE, global=TRUE)  
  x = lapply(x, function(x) setStackptsGrob(getGrob(setBoxdotGrob(x), "pts")))  
  upViewport(2)

  moveDataPts = function(cpt, step){
    p = getrefPoint(cpt$x, cpt$y, "random3::datavp")
    cpt = editGrob(cpt, x=p$x, y=p$y)
    from.y = unit(min(as.numeric(p$y)), "npc")
    to.y   = getrefPoint(unit(0.5, "npc"), unit(0.5, "npc"),
                         "random3::randomvp")$y    
    
    step.size = unit((as.numeric(from.y)-as.numeric(to.y))/step, "npc")
    
    grid.draw(cpt)
    for(i in 1:step)
      grid.edit(cpt$name, y=grid.get(cpt$name)$y-step.size)
  }
  
  fmt = initArgsRandom3()
  for(i in 1:length(x)){
    gp     = fmt$gp.pts
    gp$col = fmt$gcol[i]
    x[[i]] = editGrob(x[[i]], name=paste("dataPtsTemp", i, sep=""), gp=gp)
    moveDataPts(x[[i]], step)
  }
  
  grid.edit("dataPtsTemp", gp=gpar(col="black"), grep=TRUE, global=TRUE)  
}

random3MoveFVal = function(x, step=10){
  chName = childNames(grid.get("diffbarTemp"))
  fxy = random3GetFunnelXY()
  w = NULL
  for(i in 1:length(chName)){
    agb = grid.get(gPath("diffbarTemp", chName[i]))
    w[i] = as.numeric(convertWidth(agb$x1-fxy$x, "inches"))
  }
  w = unit(max(abs(w))*2.5, "inches")
  rdg = do.call("roundrectGrob", initArgsRandom3Funnel(w))
  grid.draw(rdg)
  
  agb = grid.get("diffbarTemp")
  
  toy = convertY(fxy$y, "npc")
  
  # keep some distance between lines
  dt  = seq(0,0.005*length(chName),length.out=length(chName))
  dt  = dt - median(dt)
  
  step2 = 4
  step0 = step1 = list()
  length(step0) = length(step1) = length(chName)
  for(i in 1:length(chName)){
    agbChild = getGrob(agb, chName[i])
    toydt    = unit(as.numeric(toy)-dt[i],"npc")
    step0[[i]] = list(x=seq.unit(agbChild$x0, agbChild$x0, length.out=step2),
                      y=seq.unit(agbChild$y0, toydt,       length.out=step2))
    step1[[i]] = list(x=seq.unit(agbChild$x1, agbChild$x1, length.out=step2),
                      y=seq.unit(agbChild$y1, toydt,       length.out=step2))
  }
  
  setXY = function(agb, step0, step1, i){
    x0 = lapply(step0, function(x) x$x[i])
    y0 = lapply(step0, function(x) x$y[i])
    x1 = lapply(step1, function(x) x$x[i])
    y1 = lapply(step1, function(x) x$y[i])
    
    n  = length(childNames(agb))
    for(j in 1:n){
      agb = editGrob(agb, childNames(agb)[j], x0=x0[[j]], y0=y0[[j]],
                                              x1=x1[[j]], y1=y1[[j]])
    }
    agb
  }

  fgb = grid.get("funnel")
  w = fgb$width
  h = fgb$height
  wr = unit(runif(10, 0, 2), "lines")
  hr = unit(runif(10, 0, 2), "lines")
  
  for(i in 2:step2){
      grid.remove("diffbarTemp")
      agb = setXY(agb, step0, step1, i)
      grid.draw(agb)
  }
  

  for(i in 1:5){
    grid.edit("funnel", width=w+wr[i], height=h+hr[i])
  }
  grid.remove("funnel")
  grid.remove("diffbarTemp")
  grid.draw(rdg)
  

  from = fxy
  to   = getrefPoint(unit(x, "native"), unit(0, "npc"), "random3::distvp")
  
  s    = list(x=seq.unit(from$x, to$x, length.out=step),
              y=seq.unit(from$y, to$y, length.out=step))
  
  txgb = grid.text(round(x, 2), x=from$x, y=from$y, gp=gpar(cex=1, col="red"),
                   name="fVal")
  
  tcex = seq(1, 2, length.out=5)
  for(i in 2:5)
    grid.edit("fVal", gp=gpar(cex=tcex[i]))
  
  for(i in 2:step)
    grid.edit("fVal", x=s$x[i], y=s$y[i])
  
  grid.remove("fVal")
  grid.points(x=unit(x, "native"), y=unit(0, "npc"), pch=19,
              gp=gpar(cex=1, col="red"), vp="random3::distvp")
  grid.remove("funnel")
}

random3ShowRandomvp = function(show=TRUE){
  grid.edit("randomBoxDotsMulti", show=show)
}

random3MoveRandomApart = function(group, step=5){
  fmt = initArgsRandom3()
  show.box = identical(random3GetDiffFun(), median)
  random3UpdateTableGroup(group)
  pad = seq(0, 1, length.out=step)
  for(i in 2:step)
    grid.edit("randomBoxDotsMulti", pad=pad[i])
}

random3ShowRandomArror = function(show=TRUE){
  if(show)
    grid.edit("randomBoxDotsMulti", arrow=initArgsRandom3()$arr)
  else
    grid.edit("randomBoxDotsMulti", arrow=NULL)
}

random3InitAddition = function(){
  gb = random3GetCurrentGrob()
  gb = editGrob(gb, "dist", gp=gpar(col="black"))    
  grid.newpage()
  grid.draw(gb)
}

random3InitTableGroup = function(){
  gb = random3GetCurrentGrob()
  gb = editGrob(gb, "randomBoxDotsMulti", show=FALSE, arrow=NULL)
  gb = editGrob(gb, "datatable",
                data     = grid.get("datatable")$data[,1:2],
                col.data = grid.get("datatable")$col.data[,1:2])
  grid.newpage()
  grid.draw(gb)  
}


random3UpdateGroup = function(group){ 
  x.df = random3GetCurrentDf()
  if(nrow(x.df)!=length(group))
    stop("new update group has different length")
      
  fmt = initArgsRandom3()
  col = fmt$gcol[as.numeric(group)]
  
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
  
  
  show.box = identical(random3GetDiffFun(), median)
  
  gb = random3GetCurrentGrob()
  gb = editGrob(gb, "datatable", data=x2.df, varname=varname, col.data=col.data)
  gb = editGrob(gb, "randomBoxDotsMulti", group=group, show.box=show.box,
                show=TRUE, arrow=fmt$arr)
  grid.newpage()
  grid.draw(gb)
}

random3UpdateTableGroup = function(group){
  x.df = random3GetCurrentDf()
  if(nrow(x.df)!=length(group))
    stop("new update group has different length")
      
  fmt = initArgsRandom3()
  col = fmt$gcol[as.numeric(group)]
  
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
  
  show.box = identical(random3GetDiffFun(), median)
  gb = random3GetCurrentGrob()
  gb = editGrob(gb, "datatable", data=x2.df, varname=varname, col.data=col.data)
  gb = editGrob(gb, "randomBoxDotsMulti", group=group, pad=0, arrow=NULL,
                                          show=TRUE, show.box=show.box)
  grid.newpage()
  grid.draw(gb)
}

random3GetDistData = function(){
  grid.get("dist")$x
}

random3UpdateDistData = function(x){
  grid.edit("dist", x=x)
}

random3UpdateDistShow = function(idx=TRUE,shape=1){
  grid.edit("dist", show=idx,shape=shape)
}

random3ShowTailProp = function(xat){
  x = random3GetDistData()[grid.get("dist")$show]
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
                       gp=gpar(col="grey"), vp="random3::distvp")
  }

  sg  = segmentsGrob(x0=unit(xat, "native"), y0=unit(0, "npc"),
                     x1=unit(xat, "native"), y1=y1,
                     gp=gpar(col="grey"), vp="random3::distvp")
  
  txt = textGrob(string, x=xat2, y=y,
                 just=just, gp=gpar(cex=1.1), vp="random3::distvp")

  if("diffPts" %in% grid.ls(print=FALSE)$name){
    dptsgb = grid.get("diffPts")
  } else {
    dptsgb = NULL
  }

                 
  col = ifelse(!idx, "grey",  "black")
  gb = random3GetCurrentGrob()
  gb = editGrob(gb, "dist", gp=gpar(col=col))
  gb = gList(gb, sg, txt, arr, dptsgb)
  grid.newpage()
  grid.draw(gb)
}