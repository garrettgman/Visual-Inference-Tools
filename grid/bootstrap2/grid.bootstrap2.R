##############################################################################
## Date     : 2011-05-09
## Author   : Vivian Li
## Type     : grid draw functions
## Usage    : bootstrap2 animation layout and initialize only
##          : numerical data
##############################################################################


grid.bootstrap2 = function(...){
  grid.draw(bootstrap2Grob(...))
}

bootstrap2Grob = function(data, vname=NULL, x.sim=NULL, wrange=c(0,1), main="MyData", 
                       name=NULL, gp=gpar(), vp=NULL, tab=list()){
  
  data = checkGroupBootstrap2(data,vname)
  igt = gTree(data=data, x.sim=x.sim, wrange=wrange,
              children=makebootstrap2Grob(data,x.sim, main, tab),
              childrenvp=makebootstrap2Viewports(x.sim,wrange),
              name=name, gp=gp, vp=vp, tab=tab,
              cl="bootstrap2")
  igt
}

makebootstrap2Grob = function(data, x.sim, main, tab){
  tabgb = btabgb = datgb = rangb = distgb = NULL

  ## setting table grob at tablevp (col1)
  tabgb = do.call("tableGrob", initArgsBootstrap2Table(data, main))
  
  ## setting table grob at btablevp (col3)
  btabgb = do.call("tableGrob", initArgsBootstrap2BootstrapTable(data, main))

  ## setting propBoxGrob grob at datavp (row1)
  datgb = do.call("propBoxGrob", initArgsBootstrap2Data(data[,1]))
  
  ## setting boxdotsDiff grob at bstrapvp (row3)
  rangb = do.call("propBoxGrob", initArgsBootstrap2Random(data[,1]))
  
  ## setting stackpts grob at distvp (row5)
  distgb = do.call("stackptsGrob", initArgsBootstrap2Dist(data, x.sim))
  gList(tabgb, btabgb, datgb, rangb, distgb)
}

makebootstrap2Viewports = function(x.sim,wrange){
  at      = xatBootstrap2(x.sim,wrange)
  xat     = at$xat1
  xrange  = range(xat)
  xat2    = at$xat2
  xrange2 = range(xat2)
  
  nr = 6
  nc = 5
  # layout width
  # "null" is used to divide the remaining space proportionally
  widths  = unit(c(   0.3,  1,  0.3,      2,    1),
                 c("null","line","null", "line", "null"))
  heights = unit(c(      1,      2,     1,       2,      1,  0.2),
                  c("null", "line", "null", "line", "null","null"))
  
  mylay  = grid.layout(nrow=nr, ncol=nc, widths=widths, heights=heights)

  mar    = unit(c(2.5, 1, 1, 1), c("lines", "lines", "lines", "lines"))
  width  = unit(1, "npc") - mar[2] - mar[4]
  height = unit(1, "npc") - mar[1] - mar[3]
  
  tree = vpTree(viewport(layout=mylay, name="bootstrap2",
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

drawDetails.bootstrap2 = function(x, recording){
  y         = x$data[,1]
  at        = xatBootstrap2(x$x.sim,x$wrange)
  xat1      = at$xat1
  xat2      = at$xat2
  
  depth = downViewport(vpPath("bootstrap2", "tablevp"))
  grid.roundrect(gp=gpar(fill=rgb(0, 0, 1, alpha=0.05)))

  if(length(y)>initArgsBootstrap2()$maxRow){
    grid.text("more...", x=unit(1/6, "npc"), y=unit(3/170, "npc"),
              just="left", gp=gpar(cex=0.7))
  }
  upViewport(depth)
  
  depth = downViewport(vpPath("bootstrap2","btablevp"))
  grid.roundrect(gp=gpar(fill=rgb(0, 0, 1, alpha=0.05)))
  upViewport(depth)
  
  depth = downViewport(vpPath("bootstrap2","datavp"))
  grid.xaxis(at=xat1)
  upViewport(depth)
  
  depth = downViewport(vpPath("bootstrap2", "bstrapvp"))
  grid.xaxis(at=xat1)
  upViewport(depth)
  
  depth = downViewport(vpPath("bootstrap2", "bdistvp"))
  grid.xaxis(at=xat2)
  upViewport(depth)
}

editDetails.bootstrap2 = function(x, spec){
  x
}

validDetails.bootstrap2 = function(x){
  ngp = length(levels(x$data[,1]))
  if(ngp>2)
    stop("This software only handle two groups!")   
  if(ngp<2)
    stop("Only one group in the data!")
  x
}

bstrap2Test = function(nsim=1,ran=T,fly=F,type=1){
  sample.df= createRandom2Data(p1=0.4)$data
  vname    = names(sample.df)[1]
  x        = sample.df[,1]
  prop     = table(x)/length(x)
  grid.newpage()
  grid.bootstrap2(x,vname=vname,main="Simulated Data",name="Bootstrap2Movie")
  
  sim.bt  = bootstrap2Simulation(x,nsim)
  sim.tab = sim.bt$tab
  sim.p   = sim.bt$p

  grid.newpage()
  grid.bootstrap2(x,x.sim=sim.p,vname=vname,main="Simulated Data",name="Bootstrap2Movie")
  
  if(ran){
    if(nsim<=30){
      for(i in 1:nsim){
        index  = sim.tab[[i]]
        bootstrap2UpdateBox(index,i)
        if(nsim==1 && fly){
           pointer=bootstrap2GetPointerInfo(index)
           for(j in 1:length(x)){
             bootstrap2ShowPointer(pointer,index,j)
             if(length(which(index==j))>=2)
                Sys.sleep(1.2)
             else
                Sys.sleep(0.4)
           }
           bootstrap2RemovePointer()
        }
        bootstrap2MoveBarFromBstrapvp(type=type)
        bootstrap2UpdateDistShow(1:i)
      }
      bootstrap2Finalise()
  #    bootstrap2MoveArrowBarFromDatavp()
    }
    else{ 
      for(i in round(seq(1, nsim, length.out=10))[-1]){
        bootstrap2UpdateDistShow(1:i,shape=2)
      }
      bootstrap2MoveBarFromDatavp(type=type)
      bootstrap2ShowCI(prop[1])
    } 
  }
  else{
    for(i in 1:nsim){
      index = sim.tab[[i]]
      obj   = bootstrap2InitMovingTxt(index)      
      for(i in 1:length(index)){
        bootstrap2MoveTableFromTablevp(obj,index,i)
      }
    }
  }
}
