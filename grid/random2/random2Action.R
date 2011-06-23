##############################################################################
## Date     : 2011-03-01
## Author   : Danny Chang/Vivian Li
## Notes    : Vivian Li
## Type     : action functions
## Usage    : random 1 animation action
##############################################################################

random2GetCurrentGrob = function(){
  # Notes:
  # get the main grob at the bottom
  ##first name of the list
  grid.get(grid.ls(print=FALSE)$name[1])
}

random2GetCurrentDf = function(){
  # Notes:
  # get whole data
  random2GetCurrentGrob()$data
}

random2GetCurrentDiff = function(){
  random2GetCurrentGrob()$x.diff
}

random2GetCurrentData = function(){
  # Notes:
  # get x (group1)
  grid.get("dataPropBox")$x
}

random2GetDataGroup = function(){
  # Notes:
  # get group (group2)
  grid.get("dataPropBox")$group
}

random2GetRedrawGroup = function(tab, i){
  # get i th simulated group2
  if(class(tab)!="list")
    stop("tab is not list")
  
  tab[[i]]
}

random2Simulation = function(x.df, n){
  # Notes: random2Action.random2Simulation
  # diff  : difference of proportion between Control.A and Drug.A
  # tab   : randomly sampled group 
  x = x.df[,1]
  group = x.df[,2]
  
  diff = numeric(n)
  tab = list()
  length(tab) = n
  for(i in 1:n){
    g = sample(group)
    tab[[i]] = g # randomise group2 (Control & Drug)
    diff[i] = getPropBoxValue(x, g)$d # length of arrows: differences between two group As in (Control, Drug)
  }
  list(diff=diff, tab=tab)
}

random2MoveArrowBarFromRandomvp = function(step=10){
  agb = setPropBoxDiffGrob(grid.get("randomPropBox"))
  agb = setPropBoxDiffGrob(editGrob(agb, arrow=initArgsRandom2()$arr))
  agb = getGrob(agb, "diffbar")
  agb = editGrob(agb, name="diffbarTemp")
  from0 = getrefPoint(agb$x0, agb$y0, "random2::randomvp")
  from1 = getrefPoint(agb$x1, agb$y1, "random2::randomvp")
  x0    = as.numeric(agb$x0)
  x1    = as.numeric(agb$x1)
  to0   = getrefPoint(unit(x1-x1, "native"), unit(0, "npc"), "random2::distvp")
  to1   = getrefPoint(unit(x1-x0, "native"), unit(0, "npc"), "random2::distvp")
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
                           vp="random2::distvp")
  grid.remove("diffbarTemp")
}

random2MoveArrowBarFromDatavp = function(step=20){
  # Notes:
  # random2Action.random2MoveArrowBarFromDatavp
  agb = setPropBoxDiffGrob(grid.get("dataPropBox"))
  agb = getGrob(agb, "diffbar")
  agb = editGrob(agb, name="diffbarTemp")
  from0 = getrefPoint(agb$x0, agb$y0, "random2::datavp")
  from1 = getrefPoint(agb$x1, agb$y1, "random2::datavp")  
  x0    = as.numeric(agb$x0)
  x1    = as.numeric(agb$x1)
  to0   = getrefPoint(unit(x1-x1, "native"), unit(0, "npc"), "random2::distvp")
  to1   = getrefPoint(unit(x1-x0, "native"), unit(0, "npc"), "random2::distvp")
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
                           vp="random2::distvp")
                           
  grid.points(x=unit(x1-x0, "native"), y=unit(0, "npc"), pch=19,
              name="diffPts", gp=gpar(col="red"), vp="random2::distvp")
  grid.remove("diffbarTemp")
}

random2MoveDataPtsToRandomVP = function(step=10){
  random2InitTableGroup()
  x = setBoxdotsDiffGrob(grid.get("dataBoxDots"))
  downViewport("datavp")
  
  gp1 = setBoxdotGrob(getGrob(x, "group1bxp"))
  gp1 = setStackptsGrob(getGrob(gp1, "pts"))
  gp1 = editGrob(gp1, name="gp1DataPtsTemp", gp=initArgsRandom2()$gp.pts[[2]])  
  gp2 = setBoxdotGrob(getGrob(x, "group2bxp"))
  gp2 = setStackptsGrob(getGrob(gp2, "pts"))
  gp2 = editGrob(gp2, name="gp2DataPtsTemp", gp=initArgsRandom2()$gp.pts[[3]])
  upViewport(2)
  
  moveDataPts = function(cpt, step){    
    p = getrefPoint(cpt$x, cpt$y, "random2::datavp")
    cpt = editGrob(cpt, x=p$x, y=p$y)
    from.y = unit(min(as.numeric(p$y)), "npc")
    to.y = getrefPoint(unit(0.5, "npc"), unit(0.5, "npc"),
                       "random2::randomvp")$y
    
    step.size = unit((as.numeric(from.y)-as.numeric(to.y))/step, "npc") 
    
    grid.draw(cpt)
    for(i in 1:step)
      grid.edit(cpt$name, y=grid.get(cpt$name)$y-step.size)
  }  
  
  moveDataPts(gp1, step)
  moveDataPts(gp2, step)
  
  gb = random2GetCurrentGrob()
  gp1 = editGrob(grid.get("gp1DataPtsTemp"), gp=initArgsRandom2()$gp.pts[[1]])
  gp2 = editGrob(grid.get("gp2DataPtsTemp"), gp=initArgsRandom2()$gp.pts[[1]])
  gb = gList(gb, gp1, gp2)
  grid.newpage()
  grid.draw(gb)

}

random2ShowRandomvp = function(show=TRUE){
  if(show)
    grid.edit("randomPropBox", show.box=show, arrow=initArgsRandom2()$arr)
  else
    grid.edit("randomPropBox", show.box=show, arrow=NULL)
}

random2InitAddition = function(){
  gb = random2GetCurrentGrob()
  gb = editGrob(gb, "dist", gp=gpar(col="black"))
  grid.newpage()
  grid.draw(gb)
}

random2InitTableGroup = function(){
  # show.box: control the randomvp box
  # arrow: control the display of arrow in randomvp
  gb = random2GetCurrentGrob()
  gb = editGrob(gb, "randomPropBox", show.box=FALSE, arrow=NULL)
  gb = editGrob(gb, "datatable",
                data     = grid.get("datatable")$data[,1:2],
                col.data = grid.get("datatable")$col.data[,1:2])
  grid.newpage()
  grid.draw(gb)
}

random2UpdateGroup = function(group,wrange=c(0,1)){
  # Notes: random2Action.random2UpdateGroup
  # update the randomPropBox and datatable
  x.df = random2GetCurrentDf()
  x.diff = random2GetCurrentDiff()

  if(nrow(x.df)!=length(group))
    stop("new update group has different length")
  
  # formats
  fmt = initArgsRandom2()
  col  = ifelse(as.numeric(group)==1, fmt$gcol1[1],  fmt$gcol1[2])
  col2 = ifelse(as.numeric(group)==1, fmt$gcol2[1],  fmt$gcol2[2])
    
  x2.df    = grid.get("datatable")$data
  col.data = grid.get("datatable")$col.data
  nr = nrow(x2.df)
  if(ncol(x2.df)==2){
    x2.df = cbind(x2.df, as.character(group[1:nr]))
    col.data = cbind(col.data, col2[1:nr])
  } else if(ncol(x2.df)==3){
    x2.df[,3] = as.character(group[1:nr])
    col.data[,3] = col2[1:nr]
  } else {
    ## should not happend
    temp = paste("ncol(x2.df) is", ncol(x.df), "!? What happened?")
    stop(temp)
  }
  
  varname = grid.get("datatable")$varname
  if(length(varname)==2)
    varname = c(varname, "Rerandom")
  
  gb = random2GetCurrentGrob()

  # change something in the main grob

  if(abs(diff(wrange))!=1){
    gb = editGrob(gb,wrange=wrange,childrenvp=makeRandom2Viewports(NULL,wrange=wrange))
    gb = editGrob(gb, "dataPropBox", wrange=wrange)
  }
  
  gb = editGrob(gb, "randomPropBox", group=group, wrange=wrange,show.box=TRUE,
		  arrow=initArgsRandom2()$arr)
  gb = editGrob(gb, "datatable", data=x2.df, varname=varname, col.data=col.data)	  

  grid.newpage()
  # redraw the whole panel
  grid.draw(gb)
}

random2GetDistData = function(){
  grid.get("dist")$x
}

random2UpdateDistData = function(x){
  grid.edit("dist", x=x)
}

random2UpdateDistShow = function(idx=TRUE,shape=1){
  grid.edit("dist",show=idx,shape=shape)
}

random2ShowTailProp = function(xat){
  x = random2GetDistData()[grid.get("dist")$show]
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
                       gp=gpar(col="grey"), vp="random2::distvp")
  }

  sg  = segmentsGrob(x0=unit(xat, "native"), y0=unit(0, "npc"),
                     x1=unit(xat, "native"), y1=y1,
                     gp=gpar(col="grey"), vp="random2::distvp")
  
  txt = textGrob(string, x=xat2, y=y,
                 just=just, gp=gpar(cex=1.1), vp="random2::distvp")

  if("diffPts" %in% grid.ls(print=FALSE)$name){
    dptsgb = grid.get("diffPts")
  } else {
    dptsgb = NULL
  }

                 
  col = ifelse(!idx, "grey",  "black")
  gb = random2GetCurrentGrob()
  gb = editGrob(gb, "dist", gp=gpar(col=col))
  gb = gList(gb, sg, txt, arr, dptsgb)
  grid.newpage()
  grid.draw(gb)
}

xatRange<-function(randomgroups){
  data  = random2GetCurrentDf()
  tb    = table(data)   
  x     = data[,1]
  if(all(tb[1,]>tb[2,])){
    w.scale<-xatRescale(x,randomgroups,2)
    return(c(1-w.scale,1))
  }
  else if(all(tb[1,]<tb[2,])){
    w.scale<-xatRescale(x,randomgroups,1)   
    return(c(0,w.scale))
  }
  else{
    return(c(0,1))
  }
}

xatRescale<-function(x,groups,index){
  w.scale = 1
  
  secondSmall = function(m){
    sort(m)[2]
  }
  
  props<-lapply(groups,function(gi){
    freq<-table(x,gi)
    sums<-colSums(freq)
    p<-freq/rep(sums,each=2)
    p[index,]
  })

  if(all(unlist(props)<0.3)){
      secondmin = lapply(props,secondSmall)
      w.scale = 3*min(unlist(secondmin)) # 3 times as big as the bigger width of the boxes
      w.scale = round(w.scale,1)
  }
  return(w.scale)
}

rescale = function(prop,w.scale){
  lapply(prop,function(pi){
    mag<-pi*w.scale
    maxIndex = which(mag==max(mag))
    minIndex = which(mag==min(mag))
    mag[maxIndex] = 1-mag[minIndex]
    mag
  })
}
random2Rescale = function(tab){
  data        = random2GetCurrentDf()
  tb          = table(data)
  
  if(!all(tb[1,]>tb[2,]) && !all(tb[1,]<tb[2,]))
    return(1)
  else{
    if(all(tb[1,]>tb[2,]))
        dr = 1
    else if(all(tb[1,]<tb[2,]))
        dr = 0
    # can be improved
    x           = data[,1]
    len         = length(tab)
    w.scale       = 1
    threshold   = 0.3 # threshold should be >0.2333 to keep 2pad+height<1
    secondSmall = function(m){
      sort(m)[2]
    }

    # 1. if the smallest two groups <0.25
    # 2. if their diff is too small to see
    # 3. at same ends
  
    # calculate the proportions for group1 within group2
    props = lapply(tab,function(tabi){
      freq<-table(x,tabi)
      sums<-colSums(freq)
      freq/rep(sums,each=2)
    })
    
    # rescale if any proportion in the table is smaller than 0.3
    if(all(lapply(props,min)<threshold)){  
      secondmin = lapply(props,secondSmall)
      w.scale = 3*min(unlist(secondmin)) # 3 times as big as the bigger width of the boxes
      w.scale = round(w.scale/0.05)*0.05
    }
    return(data.frame(dr=dr,w.scale=1/ifelse(w.scale>1,1,w.scale)))
  }
}

decimalplaces <- function(x) {
    if ((x %% 1) != 0) {
        nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
    } else {
        return(0)
    }
}

polygonTail<- function(tooth=3,depth=0.08,regular=T){
  # build the relative sizes for the tooth of the bar tail
  # depth  : percentage of the bar width
  # tooth  : number of tooth
  # regular: shape of tooth
  if(regular){
    polygonX = rep(c(depth,0),tooth)[-2*tooth]
    polygonY = (c(1:(2*tooth+1))*1/(2*(tooth+1)))[c(-1,-(2*tooth+1))]
  }
  else{
    polygonX = sample(sort(runif(tooth*2,0,0.08))[rep(c(F,T),3)])
    polygonY = sort(runif(tooth*2,0,1))[rep(c(F,T),3)]
  }
  return(data.frame(px=polygonX,py=polygonY))
}


