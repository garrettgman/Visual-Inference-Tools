##############################################################################
## Date     : 2011-05-09
## Author   : Vivian Li
## Type     : grid draw functions
## Usage    : bootstrap 1 animation layout and initialize only
##          : numerical data
##############################################################################

# draws a bootstrapGrob to the device
grid.bootstrap = function(...){
  grid.draw(bootstrapGrob(...))
}

# defining a bootstrap Grob
bootstrapGrob = function(data, diffFun=median, vname=NULL, x.sim=NULL, main="MyData", digit=3,
                       name=NULL, gp=gpar(), vp=NULL, tab=list()){
  # converting data to a data frame
  if(is.numeric(data))
    data = data.frame(x=data)
  else if(is.data.frame(data)){
    data = data.frame(x=data[,1])
  }
 
  names(data)=vname

  # The grob is a gTree with these slots:
  igt = gTree(data=data, # data, which holds the data
  	diffFun=diffFun, # diffFun which tracks mean or median
  	digit=digit, # digit which tracks the number of significant digits?
  	# it has a child grob
    children=makeBootstrapGrob(data,diffFun,x.sim, main, digit,tab), # makes the overall grob for the entire screen
    childrenvp=makeBootstrapViewports(data, diffFun,x.sim), # makes the viewports that help the overall grob place things in the right place
    name=name, # identifier for the gTree
    gp=gp, # user defined graphical parameters
    vp=vp, # the veiwport the gTree is to be drawn in
    tab=tab,
    cl="bootstrap") # it receives its own class
    
  igt
}

# makes a list of grobs to populate the bootstrapGrob gTree. This list includes the following types of grobs:
# tablegrob - a tree of textGrobs that combine to form a box like the type that hold the data on the lefthand side of the GUI
# boxdotgrob - a tree that draws just the data points and the boxplot
# ghostgrob - a tree that draws a group of ghostboxes (red and blue versions of boxplot)
# stackptsgrob - a group of points neatly stacked and arranged
# it includes the following instances of these types:
# tabgb - the leftmost table of data
# btabgb - the next table of sampled data displayed in text form
# datgb - the boxplot and points of the original data. Drawn at the top of the page
# ghostgb - the accumulated ghost plots on the second axis down in the GUI
# rangb - the visible points and boxplot on the second axis down in the GUI
# distgb - the points (only) on the bottom axis that capture the mean/medians
makeBootstrapGrob = function(data, diffFun, x.sim, main, digit, tab){
  
  tabgb = datgb = ghostgb = rangb = distgb = NULL
  
  args  = list(data,tab,diffFun, gb="boxdotGrob", 
               gbfmt=initArgsBootstrapGhostBox(data[,1],diffFun), name="ghostBox")
  
  # default args assume a median diffFun, but we can update them for a mean diffFun
  if(identical(diffFun, mean)){
    args$gb = "meanBarGrob"
    args$gbfmt = initArgsBootstrapGhostBar(data[,1],diffFun)
  }
  
  ## setting table grob at tablevp (col1)
  # defined in grid/public/grid.table.R
  tabgb = do.call("tableGrob", initArgsBootstrapTable(data, main, digit))
  ## setting table grob at btablevp (col3)
  btabgb = do.call("tableGrob", initArgsBootstrapBootstrapTable(data, main, digit))
  
  ## setting boxdotsDiff grob at datavp (row1)
  # defined in grid/public/grid.boxdot.R
  # THIS IS WHERE POINTS AND BOXPLOT ARE DRAWN
  datgb = do.call("boxdotGrob", initArgsBootstrapData(data[,1], diffFun))
  
  ## setting ghost boxeds at bstrapvp (row3)
  # defined in grid/public/grid.ghost.R
  # THIS IS WHERE BLUE AND RED LINES ARE DRAWN
  ghostgb = do.call("ghostGrob", args)
  
  ## setting boxdotsDiff grob at bstrapvp (row3)
  # but this appears to be a boxdotGrob. Disregard above?
  rangb = do.call("boxdotGrob", initArgsBootstrapRandom(data[,1], diffFun))
  
  ## setting stackpts grob at distvp (row5)
  distgb = do.call("stackptsGrob", initArgsBootstrapDist(data, diffFun, x.sim))
  gList(tabgb, btabgb, datgb, ghostgb, rangb, distgb)
}


makeBootstrapViewports = function(data,diffFun,x.sim){
  xat     = xatBootstrap(data,diffFun,x.sim)$xat1 # tickmarks for axis
  xrange  = range(xat) # range of xaxis
  xat2    = xat
  #xat2    = xatBootstrap(data,diffFun,x.sim)$xat2
  xrange2 = range(xat2)
 
  nr = 6
  nc = 5
  # layout settings for the entire page
  # "null" is used to divide the remaining space proportionally
  # Data text table = 0.3 null
  # space between text boxes = 1 line
  # Sample text table = 0.3 null
  # space between text table and graphs = 2 lines
  # Width of graphs = 1 null
  widths  = unit(c(   0.3,  1,  0.3,      2,    1),
                 c("null","line","null", "line", "null"))
  heights = unit(c(      1,      2,     1,       2,      1,  0.2),
                  c("null", "line", "null", "line", "null","null"))
  
  # using settings to make layout
  mylay  = grid.layout(nrow=nr, ncol=nc, widths=widths, heights=heights)

  # margins around edges of the display
  mar    = unit(c(2.5, 1, 1, 1), c("lines", "lines", "lines", "lines"))
  
  # width of central display area (non-margin area)
  width  = unit(1, "npc") - mar[2] - mar[4]
  height = unit(1, "npc") - mar[1] - mar[3]
  
  # the actual viewports exist in a tree
  # "bootstrap" is the viewport that captures the entire display area inside the margins
  # it has the following children (all in parallel to each other)
  # tablevp - the leftmost text box for the data
  # btablevp - the next textbox for the sample
  # datavp - the top most plot
  # bstrapvp - the middle plot
  # bdistvp - the bottom plot
  # extravp - an extra viewport at the bottom of the plot column?
  tree = vpTree(viewport(layout=mylay, name="bootstrap",
                         x=mar[2], y=mar[1],
                         width=width, height=height, just=c(0, 0)),
                vpList(viewport(layout.pos.col=1, name="tablevp"), # row = NULL, so occupies all of the column
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

# method used to draw the entire device
# Note: this doesn't fill the data r sample text into the two lefthand tables, not does it plot data in any of the plots. If the method is mean, it will draw a meanbar grob in the top plot. This apears to be just a vertical line where the mean occurs.
drawDetails.bootstrap = function(x, recording){
  
  y     = x$data[,1] # collect the the data
  bxat  = xatBootstrap(x$data,x$diffFun, x$x.sim) # calculate x ticks
  
  xat   = bxat$xat1 # use for first two plots
  xat2  = bxat$xat1
  
  # draw data table background - a grey rectangle with three lists of text
  depth = downViewport(vpPath("bootstrap", "tablevp")) # moves vp down, and returns how many levels it went
  grid.roundrect(gp=gpar(fill=rgb(0, 0, 1, alpha=0.05))) #"#0000FF0D"

  if(length(y)>initArgsBootstrap()$maxRow){
    grid.text("more...", x=unit(1/6, "npc"), y=unit(2/170, "npc"),
              just="left", gp=gpar(cex=0.7))
  }
  upViewport(depth) # back to top
  
  # draws sample table background
  depth = downViewport(vpPath("bootstrap","btablevp"))
  grid.roundrect(gp=gpar(fill=rgb(0, 0, 1, alpha=0.05))) #"#0000FF0D"
  upViewport(depth)
  
  # draws top xaxis and meanbar if applicable
  depth = downViewport(vpPath("bootstrap","datavp"))
  grid.xaxis(at=xat) # draws xaxis

  if(identical(x$diffFun, mean)){
    grid.meanBar(xat=unit(x$diffFun(y),"native"),yat=unit(0.5,"npc"),len=unit(0.3,"npc"),name="meanBar0")              
  }
 
  upViewport(depth)
  
  # draws middle xaxis
  depth = downViewport(vpPath("bootstrap", "bstrapvp"))
  grid.xaxis(at=xat)
  
  upViewport(depth)
  
  # draws bottom xaxis
  depth = downViewport(vpPath("bootstrap", "bdistvp"))
  grid.xaxis(name="axis3",at=xat2)
  upViewport(depth)
}

editDetails.bootstrap = function(x, spec){
  x
}

validDetails.bootstrap = function(x){
  if(!is.numeric(x$data[,1]))
    stop("The numeric variable should be at column one!")   
  
  type = unlist(lapply(x$data, class))

  if(!any(c("numeric", "integer", "double")%in%type))
    stop("numeric/integer/double data not found")

  x
}

bstrapTest = function(nsim=1,ran=T,fly=F,type=1){
  sample.df=createRandom1Data()$data
  vname    = names(sample.df)[1]
  x        = sample.df[,1]

  grid.newpage()
  grid.bootstrap(x,vname=vname,main="Simulated Data",name="BootstrapMovie")
  m        = median(x)
  sim.bt  = bootstrapSimulation(x,nsim)
  
  sim.tab = sim.bt$tab
  sim.est= sim.bt$est

  grid.newpage()
  grid.bootstrap(x,x.sim=sim.est,vname=vname,main="Simulated Data",name="BootstrapMovie")
  if(ran){
    if(nsim<30){
      for(i in 1:nsim){
        index  = sim.tab[[i]]
        bootstrapUpdateBox(index,i)
        if(nsim==1 && fly){
         pointer=bootstrapGetPointerInfo(index)
         for(j in 1:length(x)){
           bootstrapShowPointer(pointer,index,j)
           if(length(which(index==j))>=2)
              Sys.sleep(1.2)
         }
         bootstrapRemovePointer()
        }
        bootstrapMoveBarFromBstrapvp(type=type)
        bootstrapUpdateDistShow(1:i)
        Sys.sleep(1)
        bootstrapRefresh()
      }
  #    bootstrapFinalise()
  #    bootstrapMoveArrowBarFromDatavp()
    }
    else{ 
      for(i in round(seq(1, nsim, length.out=10))[-1]){
        bootstrapUpdateDistShow(1:i)
      }
      #bootstrapMoveBarFromDatavp(type=type)
      #bootstrapShowCI(m)
    } 
  }
  else{
    for(i in 1:nsim){
      index = sim.tab[[i]]
      obj   = bootstrapInitMovingTxt(index) 
      for(i in 1:length(index)){
        bootstrapMoveTableFromTablevp(obj,index,i)
      }
    }
  }
}
