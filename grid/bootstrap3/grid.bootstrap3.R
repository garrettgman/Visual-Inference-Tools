##############################################################################
## Date     : 2011-05-09
## Author   : Vivian Li
## Type     : grid draw functions
## Usage    : bootstrap 1 animation layout and initialize only
##          : numerical data
##############################################################################


grid.bootstrap3 = function(...){
  grid.draw(bootstrap3Grob(...))
}

bootstrap3Grob = function(data, x.diff=NULL, wrange=c(0,1),
                          main="MyData", name=NULL, gp=gpar(), vp=NULL){
                       
  data = checkGroupBootstrap3(data)

  igt = gTree(data=data, x.diff=x.diff, wrange=wrange,
              children=makeBootstrap3Grob(data, x.diff, main),
              childrenvp=makeBootstrap3Viewports(x.diff,wrange),
              name=name, gp=gp, vp=vp,cl="bootstrap3")
  igt
}

makeBootstrap3Grob = function(data, x.diff, main){
  
  tabgb = btabgb = datgb = rangb = distgb = NULL
  x     = data[,1]
  group = data[,2]
  
  ## setting table grob at tablevp (col1)
  tabgb = do.call("tableGrob", initArgsBootstrap3Table(data, main))

  ## setting table grob at btablevp (col3)
  btabgb = do.call("tableGrob", initArgsBootstrap3BootstrapTable(data, main))
  
  ## setting boxdotsDiff grob at datavp (row1)
  datgb = do.call("propBoxDiffGrob", initArgsBootstrap3Data(x, group))
  
  ## setting boxdotsDiff grob at bstrapvp (row3)
  rangb = do.call("propBoxDiffGrob", initArgsBootstrap3Random(x, group))
  
  ## setting stackpts grob at distvp (row5)
  distgb = do.call("stackptsGrob", initArgsBootstrap3Dist(x, x.diff))
    
  gList(tabgb, btabgb, datgb, rangb, distgb)
}

makeBootstrap3Viewports = function(x.diff,wrange){
  at      = xatBootstrap3(x.diff,wrange)
  xat     = at$xat1
  xrange  = range(xat)
  xat2    = at$xat2
  xrange2 = range(xat2)
  
  nr = 6
  nc = 5
  # layout width
  # "null" is used to divide the remaining space proportionally
  widths  = unit(c(   1,  1,  1,      2,    2),
                 c("null","line","null", "line", "null"))
  heights = unit(c(      1,      2,     1,       2,      1,  0.2),
                  c("null", "line", "null", "line", "null","null"))
  
  mylay  = grid.layout(nrow=nr, ncol=nc, widths=widths, heights=heights)

  mar    = unit(c(2.5, 1, 1, 1), c("lines", "lines", "lines", "lines"))
  width  = unit(1, "npc") - mar[2] - mar[4]
  height = unit(1, "npc") - mar[1] - mar[3]
  
  tree = vpTree(viewport(layout=mylay, name="bootstrap3",
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

drawDetails.bootstrap3 = function(x, recording){
  y     = x$data[,1]
  g     = x$data[,2]
  at    = xatBootstrap3(x$x.diff,x$wrange)
  xat1  = at$xat1
  xat2  = at$xat2
  
  depth = downViewport(vpPath("bootstrap3", "tablevp"))
  grid.roundrect(gp=gpar(fill=rgb(0, 0, 1, alpha=0.05)))

  if(length(y)>initArgsBootstrap3()$maxRow){
    grid.text("more...", x=unit(1/6, "npc"), y=unit(3/170, "npc"),
              just="left", gp=gpar(cex=0.7))
  }
  upViewport(depth)
  
  depth = downViewport(vpPath("bootstrap3","btablevp"))
  grid.roundrect(gp=gpar(fill=rgb(0, 0, 1, alpha=0.05)))
  upViewport(depth)
  
  depth = downViewport(vpPath("bootstrap3","datavp"))
  
  grid.xaxis(at=xat1)
  grid.text(name="origin1",label=c("0","1"),x=unit(c(0,1),"native"),y=unit(0,"npc")-unit(1.5,"lines"),gp=gpar(fontface="bold"))
  grid.text(levels(g)[1], x=unit(0.5, "npc"),
                          y=unit(1, "npc")-unit(1, "lines"))
  grid.text(levels(g)[2], x=unit(0.5, "npc"),
                          y=unit(0, "npc")+unit(1, "lines"))

  upViewport(depth)
  
  depth = downViewport(vpPath("bootstrap3", "bstrapvp"))
  grid.xaxis(at=xat1)
  grid.text(name="origin2",label=c("0","1"),x=unit(c(0,1),"native"),y=unit(0,"npc")-unit(1.5,"lines"),gp=gpar(fontface="bold"))
  grid.text(paste("\"",levels(g)[1],"\""), x=unit(0.5, "npc"),
                          y=unit(1, "npc")-unit(1, "lines"))
  grid.text(paste("\"",levels(g)[2],"\""), x=unit(0.5, "npc"),
                          y=unit(0, "npc")+unit(1, "lines"))
                          
  upViewport(depth)
  
  depth = downViewport(vpPath("bootstrap3", "bdistvp"))
  
  grid.xaxis(at=xat2)
  grid.text(name="origin3",label="0",x=unit(0,"native"),y=unit(0,"npc")-unit(1.5,"lines"),gp=gpar(fontface="bold"))
  
  upViewport(depth)
}

editDetails.bootstrap3 = function(x, spec){
  x = bootstrap3Grob(data=x$data, x.diff=x$x.diff, wrange=x$wrange,
              name=x$name, gp=x$gp, vp=x$vp)
  x
}

validDetails.bootstrap3 = function(x){
  if(ncol(x$data)!=2)
    stop("Only allowed data with 2 columns")   
  
  if(!all(unlist(lapply(x$data, is.factor))))
    stop("both variable must be factor")
  x
}

bstrap3Test = function(nsim=1,ran=T,fly=F,type=1){
  sample.df= createRandom2Data(n1=35,n2=25,p1=0.5,p2=0.2)$data
  grid.newpage()
  grid.bootstrap3(sample.df,main="Simulated Data",name="Bootstrap3Movie")
  
  sample.df = bootstrap3GetCurrentDf()
  sim.bt  = bootstrap3Simulation(sample.df,nsim)
  sim.tab = sim.bt$tab
  sim.diff= sim.bt$diff
  sample.p = getPropBoxValue(sample.df[,1],sample.df[,2])$d
  grid.newpage()
  grid.bootstrap3(sample.df,x.diff=sim.diff,main="Simulated Data",name="Bootstrap3Movie")

  if(ran){
    if(nsim<=30){
      for(i in 1:nsim){
        index  = sim.tab[[i]]
        bootstrap3UpdateBox(index,i)
        if(nsim==1 && fly){
           pointer=bootstrap3GetPointerInfo(index)
           for(j in 1:nrow(sample.df)){
             bootstrap3ShowPointer(pointer,index,j)
             if(length(which(index==j))>=2)
                Sys.sleep(1.2)
             else
                Sys.sleep(0.4)
           }
           bootstrap3RemovePointer()
        }
        bootstrap3MoveArrowBarFromBstrapvp()
        bootstrap3UpdateDistShow(i)
        Sys.sleep(1)
      }
      bootstrap3Finalise()
      
    }
    else{ 
      for(i in round(seq(1, nsim, length.out=10))[-1]){
        bootstrap3UpdateDistShow(1:i,shape=2)
      }
      bootstrap3MoveArrowBarFromDatavp()
      bootstrap3ShowCI(sample.p)
    } 
  }
  else{
    for(i in 1:nsim){
      index = sim.tab[[i]]
      
      obj   = bootstrap3InitMovingTxt(index)      
      for(i in 1:length(index)){
        bootstrap3MoveTableFromTablevp(obj,index,i)
      }
    }
  }
}