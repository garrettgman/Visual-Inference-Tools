##############################################################################
## Date     : 2011-03-28
## Author   : Danny Chang
## Type     : grid drawing object
## Usage    : proportion box difference - only support binary group
##############################################################################

grid.propBoxDiff = function(...){
  grid.draw(propBoxDiffGrob(...))
}
propBoxDiffGrob = function(x, group, fill=c("red", "blue"), label=NULL,
                           width=unit(1, "npc"), height=unit(0.2, "npc"), wrange=c(0,1),
                           pad=unit(0.25, "npc"), arrow=NULL, show.box=TRUE,
                           gp.box1=gpar(), gp.box2=gpar(),
                           # not correct
                           # gp.inbox1=replicate(length(group), gpar()),
                           # gp.inbox2=replicate(length(group), gpar()),
                           # modified by Vivian
                           gp.inbox1=replicate(length(table(group)),gpar()),
                           gp.inbox2=replicate(length(table(group)),gpar()),
                           gp.bar=gpar(), gp.indbar=gpar(), gp.lab=gpar(cex=1),
                           name=NULL, gp=gpar(), vp=NULL){
  
  ## x, group : factor object
  
  igt = gTree(x=x, group=group, fill=fill, width=width, height=height, wrange=wrange,
              label=label, pad=pad, arrow=arrow, show.box=show.box,
              gp.box1=gp.box1, gp.box2=gp.box2,
              gp.inbox1=gp.inbox1, gp.inbox2=gp.inbox2, gp.bar=gp.bar,
              gp.indbar=gp.indbar, gp.lab=gp.lab, name=name, gp=gp, vp=vp,
              cl="propBoxDiff")
  igt
}

drawDetails.propBoxDiff = function(x, recording){
  x = setPropBoxDiffGrob(x)
  for (i in childNames(x)) grid.draw(getGrob(x, i))
}

setPropBoxDiffGrob = function(gt){
  x         = gt$x             # group1
  group     = gt$group         # group2
  fill      = gt$fill          # fill the box
  width     = gt$width         # width of the propBoxDiff
  height    = gt$height        # height of the propBOxDiff
  label     = gt$label         # label of groups within the box
  pad       = gt$pad           # padding between two boxes
  arrow     = gt$arrow         # arrow of the difference bar
  show.box  = gt$show.box      # whether show the box or not
  gp.box1   = gt$gp.box1       # edge box1
  gp.box2   = gt$gp.box2       # edge box2
  gp.inbox1 = gt$gp.inbox1     # inner colorful box1
  gp.inbox2 = gt$gp.inbox2     # inner colorful box2
  gp.bar    = gt$gp.bar        # difference bar
  gp.indbar = gt$gp.indbar     # ??
  gp.lab    = gt$gp.lab        #
  gp.lab1   = gp.lab
  gp.lab2   = gp.lab
  wrange    = abs(diff(gt$wrange))
  w.scale   = 1/wrange
  z         = 0.9
  cutEdge   = list(c(FALSE,FALSE),c(FALSE,FALSE))
  

  n   = length(group)
  r   = table(group)/max(table(group))
  pvd = getPropBoxValue(x, group)
  p   = pvd$p
  q   = pvd$q

  if(w.scale > 1){
    #pad = pad*w.scale*0.9
    #z   = z  *w.scale*0.9
    cutEdge = lapply(p,function(pi) pi>0.5)
    
    for(j in 1:length(p)){
      pj=p[[j]]
      p[[j]][cutEdge[[j]]] = pj[cutEdge[[j]]]-(1-wrange)
      p[[j]]=p[[j]]*w.scale
    }
  }  
  
  
  at  = unit(0.5, "npc")  
  at1 = convertY(at+pad, "npc") # position of box1
  at2 = convertY(at-pad, "npc") # position of box2

  g1gb = g2gb = indgb = arrgb = NULL
  
  # Added by Vivian Li
  # label with group sizes
  fancy = FALSE
  if(!fancy){
    label1 = paste(label," (",q[[1]],")",sep="")
    label2 = paste(label," (",q[[2]],")",sep="")
  }
  else{
    label1 = label.subscript(label,q[[1]])
    label2 = label.subscript(label,q[[2]])
  }

  if(show.box){
    gp.lab1$cex = gp.lab$cex*z
    gp.lab2$cex = gp.lab$cex*z
    width = width*wrange
    g1gb = propBoxGrob(p=p[[1]], fill=fill, horiz=TRUE, label=label1,
                       x=unit(0.5, "npc"), y=at1,
                       width=width, height=height*z, cutEdge=cutEdge[[1]],
                       just="centre", hjust=NULL, vjust=NULL, gp.lab=gp.lab1,
                       gp.box=gp.inbox1,
                       name=paste("propBox1"), gp=gp.box1)

    g2gb = propBoxGrob(p=p[[2]], fill=fill, horiz=TRUE, label=label2,
                       x=unit(0.5, "npc"), y=at2,
                       width=width, height=height*z,cutEdge=cutEdge[[2]],
                       just="centre", hjust=NULL, vjust=NULL, gp.lab=gp.lab2,
                       gp.box=gp.inbox2,
                       name=paste("propBox2"), gp=gp.box2)
  }
  
  if(!is.null(arrow)){
    x0 = unit(pvd$v[1], "native")
    x1 = unit(pvd$v[2], "native") 
    
    at3 = unit(mean(c(convertY(at1-0.5*z*height, "npc", valueOnly=TRUE),
                      convertY(at2+0.5*z*height, "npc", valueOnly=TRUE))),
               "npc")
        
    arrgb = segmentsGrob(x0=x0, y0=at3, x1=x1, y1=at3,
                         arrow=arrow, name="diffbar", gp=gp.bar)
    
    x0 = unit.c(x0, x1)
    x1 = x0
    y0 = unit.c(at3, at3)
    y1 = unit.c(at1, at2)
    indgb = segmentsGrob(x0=x0, y0=y0, x1=x1, y1=y1,
                         name="indicate", gp=gp.indbar)
  }  
  
  gt = setChildren(gt, gList(indgb, arrgb, g1gb, g2gb))
  gt
}

getPropBoxValue = function(x, group){
  # Notes: grid.proBoxDiff.getPropBoxValue
  # f(),p    : percentage of frequencies
  # freq(),q : frequencies
  # d        : difference between groups
  # v        : difference between Control.A and Drug.A
  f = function(group){
    x = table(group)
    x = x/sum(x)
  }
  freq = function(group){
    x = table(group)
  }

  p = tapply(x, group, f)
  q = tapply(x, group, freq)
  v = unlist(lapply(p, function(x) x[1])) ## proportion of two As
  d = diff(v) ## length of the arrow

  list(p=p, q=q,v=v, d=d)
}

editDetails.propBoxDiff = function(x, spec){
  x = propBoxDiffGrob(x=x$x, group=x$group, fill=x$fill,
                      width=x$width, height=x$height,wrange=x$wrange,
                      label=x$label, pad=x$pad,
                      arrow=x$arrow, show.box=x$show.box,
                      gp.lab=x$gp.lab, gp.box1=x$gp.box1, gp.box2=x$gp.box2,
                      gp.inbox1=x$gp.inbox1, gp.inbox2=x$gp.inbox2,
                      gp.bar=x$gp.bar, gp.indbar=x$gp.indbar,
                      name=x$name, gp=x$gp, vp=x$vp)
  x
}

validDetails.propBoxDiff = function(x){  
  if(!is.factor(x$x))
    stop("x must be a factor")
  
  if(!is.factor(x$group))
    stop("group must be a factor")
    
  if(!is.unit(x$width) | !is.unit(x$height) | !is.unit(x$pad))
    stop("width, height, pad must be unit")
  
  if(!is.null(x$arrow))
    if(class(x$arrow)!="arrow")
      stop("arrow must be arrow class")
      
  x
}



grid.propBoxDiff.example = function(x.df){
  grid.newpage()
  vp = viewport(width=unit(0.8, "npc"), height=unit(0.8, "npc"),
                xscale=c(-0.5, 1.5), yscale=c(-10, 10))
  pushViewport(vp)
  grid.rect()
  grid.xaxis()
  grid.yaxis()
  
  x     = x.df[,1]
  group = x.df[,2]
  
  setting = initArgsRandom2Data(data.df$x1,data.df$x2)
  setting$fill = c("orangered1", "lightblue")
  setting$gp = gpar()
  setting$vp = NULL
  do.call("grid.propBoxDiff",setting)
  #grid.propBoxDiff(x, group, fill=c("orangered1", "lightblue"),
  #                 label=levels(x),
  #                 width=unit(1, "native"), height=unit(0.2, "npc"),
  #                 pad=unit(0.25, "npc"), arrow=arrow(), show.box=TRUE,
  #                 gp.box1=gpar(lwd=2), gp.box2=gpar(lwd=5),
  #                 gp.bar=gpar(col="red"), gp.indbar=gpar(lty=2),
  #                 name="test", gp=gpar(), vp=NULL)
}

#makeData = function(n=80){
  # # Make up a dataset with 2 groups
#   type    = sample(c(rep("Control", 100), rep("Drug", 100)))
#   disease = sample(c(rep("A", 100), rep("B", 100)))
#   house.df = data.frame(disease=disease, type=type)
#   example.df = house.df[sample(200, size=n),]
#   rownames(example.df) = 1:n
#   example.df
# }
#  write.csv(makeData(n=80), "GUI/data/Disease1.csv", row.names=FALSE)

# for(i in 1:30){
#   grid.propBoxDiff.example(makeData(n=80))
#   Sys.sleep(0.2)  
# }