##############################################################################
## Date     : 2011-05-09
## Author   : Vivian Li
## Type     : grid draw functions
## Usage    : bootstrap 1 animation layout and initialize only
##          : numerical data
##############################################################################


grid.bootstrap1 = function(...){
  grid.draw(bootstrap1Grob(...))
}

bootstrap1Grob = function(data, diffFun=median, x.diff=NULL,
                       main="MyData", digit=3,
                       name=NULL, gp=gpar(), vp=NULL, tab=list()){
                       
  if(which(unlist(lapply(data, is.factor)))==1)
    data = data[,2:1]
  
  data = checkGroupBootstrap1(data, diffFun)

  igt = gTree(data=data, diffFun=diffFun, digit=digit, x.diff=x.diff,
              children=makeBootstrap1Grob(data, diffFun, x.diff, main, digit,tab),
              childrenvp=makeBootstrap1Viewports(data, diffFun, x.diff),
              name=name, gp=gp, vp=vp, tab=tab,
              cl="bootstrap1")
  igt
}

makeBootstrap1Grob = function(data, diffFun, x.diff, main, digit, tab){
  
  tabgb = datgb = ghostgb = rangb = distgb = NULL
  x     = data[,1]
  group = data[,2]
  
  args  = list(data,tab,diffFun, gb="boxdotsDiffGrob", 
               gbfmt=initArgsBootstrap1GhostBox(x,group,diffFun), name="ghostBox")
  
  if(identical(diffFun, mean)){
    args$gb = "meanBarGrob"
    args$gbfmt = initArgsBootstrap1GhostBar(x,group,diffFun)
  }
  
  ## setting table grob at tablevp (col1)
  tabgb = do.call("tableGrob", initArgsBootstrap1Table(data, main, digit))

  ## setting table grob at btablevp (col3)
  btabgb = do.call("tableGrob", initArgsBootstrap1BootstrapTable(data, main, digit))
  
  ## setting boxdotsDiff grob at datavp (row1)
  datgb = do.call("boxdotsDiffGrob", initArgsBootstrap1Data(x, group, diffFun))
  
   ## setting ghost boxeds at bstrapvp (row3)
  ghostgb = do.call("ghostGrob", args)
  #ghostgb = do.call("ghostBoxGrob", list(data,tab,diffFun,name="ghostBox"))
  
  ## setting boxdotsDiff grob at bstrapvp (row3)
  rangb = do.call("boxdotsDiffGrob", initArgsBootstrap1Random(x, group, diffFun))
  
  ## setting stackpts grob at distvp (row5)
  distgb = do.call("stackptsGrob", initArgsBootstrap1Dist(data, diffFun, x.diff))
    
  gList(tabgb, btabgb, datgb, ghostgb,rangb, distgb)
}

makeBootstrap1Viewports = function(data,diffFun,x.diff){
  xat     = xatBootstrap1(data, diffFun, x.diff)$xat1
  xrange  = range(xat)
  xat2    = xatBootstrap1(data, diffFun, x.diff)$xat2
  xrange2 = range(xat2)
  
  nr = 6
  nc = 5
  # layout width
  # "null" is used to divide the remaining space proportionally
  widths  = unit(c(   0.5,  1,  0.5,      2,    1),
                 c("null","line","null", "line", "null"))
  heights = unit(c(      1,      2,     1,       2,      1,  0.2),
                  c("null", "line", "null", "line", "null","null"))
  
  mylay  = grid.layout(nrow=nr, ncol=nc, widths=widths, heights=heights)

  mar    = unit(c(2.5, 1, 1, 1), c("lines", "lines", "lines", "lines"))
  width  = unit(1, "npc") - mar[2] - mar[4]
  height = unit(1, "npc") - mar[1] - mar[3]
  
  tree = vpTree(viewport(layout=mylay, name="bootstrap1",
                         x=mar[2], y=mar[1],
                         width=width, height=height, just=c(0, 0)),
                vpList(viewport(layout.pos.col=1, name="tablevp"),
                       viewport(layout.pos.col=3, name="btablevp"),
                       viewport(layout.pos.row=1, layout.pos.col=5,
                                xscale=xrange, name="datavp"),
                       viewport(layout.pos.row=3, layout.pos.col=5,
                                xscale=xrange, name="bstrapvp"),
                       viewport(layout.pos.row=5, layout.pos.col=5,
                                xscale=xrange2, name="bdistvp"),
                       viewport(layout.pos.row=6, layout.pos.col=5,
                                xscale=xrange2, name="extravp")
                      )
               )
  tree
}

drawDetails.bootstrap1 = function(x, recording){

  y     = x$data[,1]
  g     = x$data[,2]
  bxat  = xatBootstrap1(x$data, x$diffFun, x$x.diff)
  
  xat   = bxat$xat1
  xat2  = bxat$xat2

  depth = downViewport(vpPath("bootstrap1", "tablevp"))
  grid.roundrect(gp=gpar(fill=rgb(0, 0, 1, alpha=0.05)))

  if(length(y)>initArgsBootstrap1()$maxRow){
    grid.text("more...", x=unit(1/6, "npc"), y=unit(3/170, "npc"),
              just="left", gp=gpar(cex=0.7))
  }
  upViewport(depth)
  
  depth = downViewport(vpPath("bootstrap1","btablevp"))
  grid.roundrect(gp=gpar(fill=rgb(0, 0, 1, alpha=0.05)))
  upViewport(depth)
  
  depth = downViewport(vpPath("bootstrap1","datavp"))
  
  grid.xaxis(at=xat)
  
  xat3 = tapply(y, g, x$diffFun)

  grid.text(levels(g)[1], x=unit(xat3[1], "native"),
                          y=unit(1, "npc")-unit(1, "lines"))
  grid.text(levels(g)[2], x=unit(xat3[2], "native"),
                          y=unit(0, "npc")+unit(1, "lines"))
  grid.text(round(abs(diff(xat3)),1),x=unit((xat3[1]+xat3[2])/2,"native"),
                                     y=unit(0.5,"npc")+unit(0.5,"line"))
  
  if(identical(x$diffFun, mean)){
    grid.meanBar(xat=unit(xat3[1:2],"native"),yat=unit(0.5,"npc"),len=unit(0.2,"npc"),name="meanBar0")              
  }
                
  upViewport(depth)
  
  depth = downViewport(vpPath("bootstrap1", "bstrapvp"))
  grid.xaxis(at=xat)
  
  upViewport(depth)
  
  depth = downViewport(vpPath("bootstrap1", "bdistvp"))
  grid.xaxis(name="axis3",at=xat2)
  if(0 %in% xat2)
    grid.text(name="origin",label="0",x=unit(0,"native"),y=unit(0,"npc")-unit(1.5,"lines"),gp=gpar(fontface="bold"))
  upViewport(depth)
}

editDetails.bootstrap1 = function(x, spec){
  x
}

validDetails.bootstrap1 = function(x){
  if(ncol(x$data)!=2)
    stop("Only allowed data with 2 columns")   
  
  type = unlist(lapply(x$data, class))

  if(!any(c("numeric", "integer", "double")%in%type))
    stop("numeric/integer/double data not found")
  
  if(!("factor" %in% type))
    stop("factor data not found")
  x
}

bstrap1Test = function(nsim=1,ran=F){
  sample.df=createRandom1Data()$data
  
  grid.newpage()
  grid.bootstrap1(sample.df,main="Simulated Data",name="Bootstrap1Movie")
  dff     = abs(diff(tapply(sample.df[,1],sample.df[,2],median)))
  sim.bt  = bootstrap1Simulation(sample.df,nsim)
  sim.tab = sim.bt$tab
  sim.diff= sim.bt$diff
  grid.newpage()
  grid.bootstrap1(sample.df,x.diff=sim.diff,main="Simulated Data",name="Bootstrap1Movie")
  if(ran){
    if(nsim<30){
      for(i in 1:nsim){
        index  = sim.tab[[i]]
        bootstrap1UpdateGhostBox(index,i)
        bootstrap1MoveArrowBarFromBstrapvp()
        bootstrap1UpdateDistShow(1:i)
        Sys.sleep(1)
      }
      bootstrap1Finalise()
      bootstrap1MoveArrowBarFromDatavp()
    }
    else{
      
      for(i in round(seq(1, nsim, length.out=10))[-1]){
        bootstrap1UpdateDistShow(1:i)
      }
      bootstrap1MoveArrowBarFromDatavp()
      bootstrap1ShowCI(dff)
    } 
  }
  else{
    for(i in 1:nsim){
      index = sim.tab[[i]]
      bootstrap1MoveTableFromTablevp2(index)
    }
  }
}