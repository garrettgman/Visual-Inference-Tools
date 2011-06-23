##############################################################################
## Date     : 2011-03-29
## Author   : Danny Chang/Vivian Li
## Type     : grid draw functions
## Usage    : random 2 animation layout and initialize only
##############################################################################

grid.random2 = function(...){
  grid.draw(random2Grob(...))
}

random2Grob = function(data, x.diff=NULL, main="MyData", wrange=c(0,1),
                       name=NULL, gp=gpar(), vp=NULL){
                         
  data = checkGroupRandom2(data)
    
  igt = gTree(data=data, x.diff=x.diff, wrange=wrange,
              children=makeRandom2Grob(data, x.diff, main),
              childrenvp=makeRandom2Viewports(x.diff,wrange),
              name=name, gp=gp, vp=vp,
              cl="random2")
  igt
}

makeRandom2Grob = function(data, x.diff, main){
  x = data[,1]
  group = data[,2]
  
  ## setting table grob at tablevp
  tabgb = do.call("tableGrob", initArgsRandom2Table(data, main))
  
  ## setting boxdotsDiff grob at datavp
  datgb = do.call("propBoxDiffGrob", initArgsRandom2Data(x, group))
  
  ## setting boxdotsDiff grob at randomvp
  rangb = do.call("propBoxDiffGrob", initArgsRandom2Random(x, group))
  
  ## setting stackpts grob at distvp
  distgb = do.call("stackptsGrob", initArgsRandom2Dist(x, x.diff))
  
  #     layout    #
  #################
  #       # datgb #
  #       #########
  # tabgb # rangb #
  #       #########
  #       # distgb#
  #################
  gList(tabgb, datgb, rangb, distgb)
}

makeRandom2Viewports = function(x.diff,wrange){
  xat     = xatRandom2(x.diff,wrange)$xat1
  xrange  = range(xat)
  xat2    = xatRandom2(x.diff,wrange)$xat2
  xrange2 = range(xat2)
  yrange  = c(-10, 10)
  
  nr = 5
  nc = 3
  # layout width
  # "null" is used to divide the remaining space proportionally
  widths  = unit(c(   0.5,      2,    1),
                 c("null", "line", "null"))
  heights = unit(c(      1,      2,     1,       2,      1),
                  c("null", "line", "null", "line", "null"))
  
  mylay  = grid.layout(nrow=nr, ncol=nc, widths=widths, heights=heights)
  mar    = unit(c(2.5, 1, 1, 1), c("lines", "lines", "lines", "lines"))
  width  = unit(1, "npc") - mar[2] - mar[4]
  height = unit(1, "npc") - mar[1] - mar[3]
  
  tree = vpTree(viewport(layout=mylay, name="random2",
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

drawDetails.random2 = function(x, recording){
  y         = x$data[,1]
  g         = x$data[,2]
  xat1      = xatRandom2(x$x.diff,x$wrange)$xat1
  xat2      = xatRandom2(x$x.diff,x$wrange)$xat2

  # go to "tablevp"
  depth = downViewport(vpPath("random2", "tablevp"))
  grid.roundrect(gp=gpar(fill=rgb(0, 0, 1, alpha=0.05)))
  if(length(y)>initArgsRandom2()$maxRow)
    grid.text("more...", x=unit(1/6, "npc"), y=unit(3/170, "npc"),
              just="left", gp=gpar(cex=0.7))
  
  # go back to "random2"
  upViewport(depth)
  
  # go to "datavp"
  depth = downViewport(vpPath("random2", "datavp"))
  grid.xaxis(at=xat1)
  grid.text(levels(g)[1], x=unit(0.5, "npc"),
                          y=unit(1, "npc")-unit(1, "lines"))
  grid.text(levels(g)[2], x=unit(0.5, "npc"),
                          y=unit(0, "npc")+unit(1, "lines"))
  
  # go back to "random2"
  upViewport(depth)
  
  # go to "randomvp"
  depth = downViewport(vpPath("random2", "randomvp"))

  grid.xaxis(at=xat1)
  grid.text(paste("\"",levels(g)[1],"\""), x=unit(0.5, "npc"),
                          y=unit(1, "npc")-unit(1, "lines"))
  grid.text(paste("\"",levels(g)[2],"\""), x=unit(0.5, "npc"),
                          y=unit(0, "npc")+unit(1, "lines"))  
  
  # go back to "random2"
  upViewport(depth)
  
  # go to "distvp"
  depth = downViewport(vpPath("random2", "distvp"))
  grid.xaxis(at=xat2)
  
  # go back to "random2"
  upViewport(depth)
}

editDetails.random2 = function(x, spec){
  x
}

validDetails.random2 = function(x){
  if(ncol(x$data)!=2)
    stop("only 2 variables are allowed")   
  
  if(!all(unlist(lapply(x$data, is.factor))))
    stop("both variable must be factor")
    
  x
}
random2Test<-function(n=1,nsim=10,slp=0,ran=T){
	# n    : number of simulated data
	# nsim : number of simulation for each data
	# slp  : how long of system sleep time
	library(DAAG)
	for(j in 1:n){
		sim.x.df = createRandom2Data(p1=0.2,p2=0.15)$data
		print(table(sim.x.df))
		sim.d    = getPropBoxValue(sim.x.df[,1], sim.x.df[,2])$d
		sim.ran  = random2Simulation(sim.x.df,nsim)
		tab.ran  = sim.ran$tab
		grid.newpage()
		grid.random2(sim.x.df,sim.ran$diff,main="Testing",name="Random2Movie")
    
    Sys.sleep(2)
    pause()
		wrange = xatRange(tab.ran)
    sim.d  = abs(sim.d)
    print(sim.ran$diff)
		if(ran){
			if(nsim<=30){
				for(i in 1:length(tab.ran)){
					random2UpdateGroup(tab.ran[[i]],wrange=wrange)
					random2UpdateDistShow(1:i)
					#if(sim.ran$diff[i]<0.05)
					#  pause()
					Sys.sleep(slp)
				}
				random2MoveArrowBarFromDatavp()
				random2ShowTailProp(sim.d)
				if(n>1) pause()
			}
			else{
				for(i in round(seq(1, nsim, length.out=10))[-1]){
					random2UpdateDistShow(1:i,shape=2)
				}
				random2MoveArrowBarFromDatavp()
				random2ShowTailProp(sim.d)
			}
		}
		else{
			Sys.sleep(slp)
		}
	}
}

# mydf = read.csv("GUI/BloodPressure2.csv")
# grid.random2(mydf, name="test")

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



# sim = random2Simulation(mydf[1:60,], 100)
# random2ShowRandomvp()
# for(i in 1:30)
  # random2UpdateGroup(sim$tab[,i])



# pushViewport(viewport(width=0.8, height=0.8))
# grid.rect()
# grid.xaxis()
# grid.yaxis()
# grid.random2(mydf[1:60,], name="test")
# random2MoveDataPtsToRandomVP()
# random2MoveRandomApart(mydf[1:60,2])
#random2UpdateGroup(mydf[,2])
# random2MoveArrowBar()
  
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