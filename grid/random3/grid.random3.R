##############################################################################
## Date     : 2011-04-05
## Author   : Danny Chang
## Type     : grid draw functions
## Usage    : random 1 animation layout and initialize only
##############################################################################

grid.random3 = function(...){
  grid.draw(random3Grob(...))
}

random3Grob = function(data, diffFun=median,
                       pFUN=fValRandom3, p=NULL,
                       main="MyData", digit=3,
                       name=NULL, gp=gpar(), vp=NULL){
                       
  if(which(unlist(lapply(data, is.factor)))==1)
    data = data[,2:1]  
    
  igt = gTree(data=data, diffFun=diffFun, pFUN=pFUN, p=p, digit=digit,
              children=makeRandom3Grob(data, diffFun, pFUN, p, main, digit),
              childrenvp=makeRandom3Viewports(data, p),
              name=name, gp=gp, vp=vp,
              cl="random3")
  igt
}

makeRandom3Grob = function(data, diffFun, pFUN, p, main, digit){
  x = data[,1]
  group = data[,2]
  ngp   = length(levels(group))
  ## setting table grob at tablevp
  tabgb = do.call("tableGrob", initArgsRandom3Table(data, main, digit,ngp))
  
  ## setting boxdotsDiffMulti grob at datavp
  datgb = do.call("boxdotsDiffMultiGrob",
                  initArgsRandom3Data(x,group, diffFun,ngp))
  
  ## setting boxdotsDiffMulti grob at randomvp
  rangb = do.call("boxdotsDiffMultiGrob",
                   initArgsRandom3Random(x,group, diffFun,ngp))
  
  ## setting funnel grob at funnelvp
  # funnelgb = do.call("rectGrob", initArgsRandom3Funnel())
  
  ## setting stackpts grob at distvp
  distgb = do.call("stackptsGrob", initArgsRandom3Dist(x, group, p, ngp))
    
  gList(tabgb, datgb, rangb, distgb)
}

makeRandom3Viewports = function(data, x.diff){
  x       = data[,1]
  g       = data[,2]
  xat     = xatRandom3(x, g, x.diff)$xat1
  xrange  = range(xat)
  xat2    = xatRandom3(x, g, x.diff)$xat2
  xrange2 = range(xat2)
  yrange  = c(-10, 10)
  
  nr = 5
  nc = 3
  widths  = unit(c(   0.5,      2,    1),
                 c("null", "line", "null"))
  heights = unit(c(      1,      2,     1,       7,      1),
                  c("null", "line", "null", "line", "null"))
  
  mylay  = grid.layout(nrow=nr, ncol=nc, widths=widths, heights=heights)
  mar    = unit(c(2.5, 1, 1, 1), c("lines", "lines", "lines", "lines"))
  width  = unit(1, "npc") - mar[2] - mar[4]
  height = unit(1, "npc") - mar[1] - mar[3]
  
  tree = vpTree(viewport(layout=mylay, name="random3",
                         x=mar[2], y=mar[1],
                         width=width, height=height, just=c(0, 0)),
                vpList(viewport(layout.pos.col=1, name="tablevp"),
                       viewport(layout.pos.row=1, layout.pos.col=3,
                                xscale=xrange, name="datavp"),
                       viewport(layout.pos.row=3, layout.pos.col=3,
                                xscale=xrange, name="randomvp"),
                       viewport(layout.pos.row=4, layout.pos.col=3,
                                xscale=xrange2, name="funnelvp"),
                       viewport(layout.pos.row=5, layout.pos.col=3,
                                xscale=xrange2, name="distvp")
                      )
               )
  tree
}

drawDetails.random3 = function(x, recording){
  y    = x$data[,1]
  g    = x$data[,2]
  xat  = xatRandom3(y, g, x$p)$xat1
  xat2 = xatRandom3(y, g, x$p)$xat2
  
  depth = downViewport(vpPath("random3", "tablevp"))
  grid.roundrect(gp=gpar(fill=rgb(0, 0, 1, alpha=0.05)))
  if(length(y)>initArgsRandom3()$maxRow)
    grid.text("more...", x=unit(1/6, "npc"), y=unit(3/170, "npc"),
              just="left", gp=gpar(cex=0.7))  
  upViewport(depth)
  
  depth = downViewport(vpPath("random3", "datavp"))
  grid.xaxis(at=xat) 
  upViewport(depth)
  
  depth = downViewport(vpPath("random3", "randomvp"))
  grid.xaxis(at=xat)  
  upViewport(depth)
  
  depth = downViewport(vpPath("random3", "funnelvp"))
  #grid.rect(gp=gpar(lty=2))
  upViewport(depth)
  
  depth = downViewport(vpPath("random3", "distvp"))
  grid.xaxis(at=xat2)  
  upViewport(depth)
}

editDetails.random3 = function(x, spec){
  x
}

validDetails.random3 = function(x){
  if(ncol(x$data)!=2)
    stop("Only allowed data with 2 columns")   
  
  type = unlist(lapply(x$data, class))

  if(!any(c("numeric", "integer", "double")%in%type))
    stop("numeric/integer/double data not found")
  
  if(!("factor" %in% type))
    stop("factor data not found")
    
  x
}

# mydf = read.csv("GUI/data/BloodPressureMG1.csv")
# grid.random3(mydf, name="test")

#random3MoveFVal(2)
# fValRandom3(mydf[,1], mydf[,2], mean)

# g = sample(mydf[,2])
# random3InitTableGroup()
# random3UpdateGroup(g)
# random3MoveDataPtsToRandomVP()
# random3MoveRandomApart()
# random3UpdateTableGroup(g)


# grid.edit("randomBoxDotsMulti", show=TRUE, arrow=initArgsRandom3()$arr)
# random3MoveArrowBarFromDatavp()
# random3MoveArrowBarFromRandomvp()
# traceback()

# makeData = function(n=20){
  # x = c(rnorm(n, -10, 4), rnorm(n, 0, 5),
        # rnorm(n, 3, 3), rnorm(n, 20, 10))
  # g = factor(rep(c("A", "B", "C", "D"), each=n))
  # data.frame(BP=x, type=g)
# }

# write.csv(makeData(n=20), "GUI/data/BloodPressureMG1.csv", row.names=FALSE)