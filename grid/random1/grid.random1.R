##############################################################################
## Date     : 2011-03-14
## Author   : Danny Chang
## Type     : grid draw functions
## Usage    : random 1 animation layout and initialize only
## bug      : wrong group order when swaping - done
##            mean version, carful about xrange, normal distribution
##############################################################################

grid.random1 = function(...){
  grid.draw(random1Grob(...))
}

random1Grob = function(data, diffFun=median, x.diff=NULL,
                       main="MyData", digit=3,
                       name=NULL, gp=gpar(), vp=NULL){
                       
  if(which(unlist(lapply(data, is.factor)))==1)
    data = data[,2:1]   
  
  data = checkGroupRandom1(data, diffFun)
    
  igt = gTree(data=data, diffFun=diffFun, digit=digit, x.diff=x.diff,
              children=makeRandom1Grob(data, diffFun, x.diff, main, digit),
              childrenvp=makeRandom1Viewports(data, x.diff),
              name=name, gp=gp, vp=vp,
              cl="random1")
  igt
}

makeRandom1Grob = function(data, diffFun, x.diff, main, digit){
  x = data[,1]
  group = data[,2]
  
  ## setting table grob at tablevp (col1)
  tabgb = do.call("tableGrob", initArgsRandom1Table(data, main, digit))
  
  ## setting boxdotsDiff grob at datavp (row1)
  datgb = do.call("boxdotsDiffGrob", initArgsRandom1Data(x, group, diffFun))
  
  ## setting boxdotsDiff grob at randomvp (row3)
  rangb = do.call("boxdotsDiffGrob", initArgsRandom1Random(x, group, diffFun))
  
  ## setting stackpts grob at distvp (row5)
  distgb = do.call("stackptsGrob", initArgsRandom1Dist(x, x.diff))
    
  gList(tabgb, datgb, rangb, distgb)
}

makeRandom1Viewports = function(data, x.diff){
  x       = data[,1]
  xat     = xatRandom1(x, x.diff)$xat1
  xrange  = range(xat)
  xat2    = xatRandom1(x, x.diff)$xat2
  xrange2 = range(xat2)
  yrange  = c(-10, 10)
  
  nr = 5
  nc = 3
  widths  = unit(c(   0.5,      2,    1),
                 c("null", "line", "null"))
  heights = unit(c(      1,      2,     1,       2,      1),
                  c("null", "line", "null", "line", "null"))
  
  mylay  = grid.layout(nrow=nr, ncol=nc, widths=widths, heights=heights)
  mar    = unit(c(2.5, 1, 1, 1), c("lines", "lines", "lines", "lines"))
  width  = unit(1, "npc") - mar[2] - mar[4]
  height = unit(1, "npc") - mar[1] - mar[3]
  
  tree = vpTree(viewport(layout=mylay, name="random1",
                         x=mar[2], y=mar[1],
                         width=width, height=height, just=c(0, 0)),
                vpList(viewport(layout.pos.col=1, name="tablevp"),
                       viewport(layout.pos.row=1, layout.pos.col=3,
                                xscale=xrange, name="datavp"),
                       viewport(layout.pos.row=3, layout.pos.col=3,
                                xscale=xrange, name="randomvp"),
                       viewport(layout.pos.row=5, layout.pos.col=3,
                                xscale=xrange2, name="distvp")
                      )
               )
  tree
}

drawDetails.random1 = function(x, recording){
  y    = x$data[,1]
  g    = x$data[,2]
  xat  = xatRandom1(y, x$x.diff)$xat1
  xat2 = xatRandom1(y, x$x.diff)$xat2
  
  depth = downViewport(vpPath("random1", "tablevp"))
  grid.roundrect(gp=gpar(fill=rgb(0, 0, 1, alpha=0.05)))
  if(length(y)>initArgsRandom1()$maxRow)
    grid.text("more...", x=unit(1/6, "npc"), y=unit(3/170, "npc"),
              just="left", gp=gpar(cex=0.7))
  
  upViewport(depth)
  depth = downViewport(vpPath("random1", "datavp"))
  grid.xaxis(at=xat)
  xat3 = tapply(y, g, x$diffFun)
  grid.text(levels(g)[1], x=unit(xat3[1], "native"),
                          y=unit(1, "npc")-unit(1, "lines"))
  grid.text(levels(g)[2], x=unit(xat3[2], "native"),
                          y=unit(0, "npc")+unit(1, "lines"))
  
  if(identical(x$diffFun, mean)){
    grid.segments(x0=unit(xat3[1], "native"),
                  y0=unit(0.6, "npc"),
                  x1=unit(xat3[1], "native"),
                  y1=unit(0.8, "npc"),
                  gp=gpar(lwd=3))
    grid.segments(x0=unit(xat3[2], "native"),
                  y0=unit(0.4, "npc"),
                  x1=unit(xat3[2], "native"),
                  y1=unit(0.2, "npc"),
                  gp=gpar(lwd=3))                  
  }
                
  upViewport(depth)
  depth = downViewport(vpPath("random1", "randomvp"))
  grid.xaxis(at=xat)
  
  upViewport(depth)
  depth = downViewport(vpPath("random1", "distvp"))
  grid.xaxis(at=xat2)
  if(0 %in% xat2)
    grid.text(name="origin",label="0",x=unit(0,"native"),y=unit(0,"npc")-unit(1.5,"lines"),gp=gpar(fontface="bold"))
  
  upViewport(depth)
}

editDetails.random1 = function(x, spec){
  x
}

validDetails.random1 = function(x){
  if(ncol(x$data)!=2)
    stop("Only allowed data with 2 columns")   
  
  type = unlist(lapply(x$data, class))

  if(!any(c("numeric", "integer", "double")%in%type))
    stop("numeric/integer/double data not found")
  
  if(!("factor" %in% type))
    stop("factor data not found")
    
  x
}


random1Test<-function(nsim=1,slp=0,ran=T,step=10){
  # n    : number of simulated data
  # nsim : number of simulation for each data
  # slp  : how long of system sleep time
  sim.x.df = createRandom1Data()$data
  
  grid.random1(sim.x.df,main="Simulated Data",name="Random1Movie")
  sim.ran  = random1Simulation(sim.x.df,nsim)
  grid.newpage()
  grid.random1(sim.x.df,x.diff=sim.ran$diff,main="Simulated Data",name="Random1Movie")
  
  tab.ran  = sim.ran$tab
  if(ran){
    for(i in 1:length(tab.ran)){
      random1MoveDataPtsToRandomVP(step)
      random1MoveRandomApart(tab.ran[[i]],step)
      #random1UpdateGroup(tab.ran[[1]])
    }
  }
  else{
  }
}

# mydf = read.csv("GUI/data/BloodPressure2.csv")
# grid.random1(mydf, name="test")

# makeData = function(n=80){
  # # Make up a dataset with 2 groups
  # type  = c(rep("Control", 100), rep("Drug", 100))
  # BP = c(rnorm(100, 90, 30), rnorm(100, 100, 30))
  # house.df = data.frame(BP=BP, type=type)
  # example.df = house.df[sample(200, size=n),]
  # rownames(example.df) = 1:n
  # example.df
# }

# write.csv(makeData(n=80), "GUI/BloodPressure5.csv", row.names=FALSE)



# sim = random1Simulation(mydf[1:60,], 100)
# random1ShowRandomvp()
# for(i in 1:30)
  # random1UpdateGroup(sim$tab[,i])



# pushViewport(viewport(width=0.8, height=0.8))
# grid.rect()
# grid.xaxis()
# grid.yaxis()
# grid.random1(mydf[1:60,], name="test")
# random1MoveDataPtsToRandomVP()
# random1MoveRandomApart(mydf[1:60,2])
#random1UpdateGroup(mydf[,2])
# random1MoveArrowBar()
  
# traceback()
# grid.ls()
# grid.edit("randomBoxDots", show=c(TRUE, TRUE, TRUE))

# for(i in 1:30){
  # g = sample(mydf[,2], 60, replace=TRUE)
  # grid.edit("randomBoxDots", group=g)
# }

# x = grid.get("test::dataBoxDots::x::pts::stackCircle::circle")
# print.default(x)

# grid.ls()
# for(i in 1:60)
  # grid.edit("dist", show=1:i)
# traceback()