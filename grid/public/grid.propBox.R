##############################################################################
## Date     : 2011-03-23
## Author   : Danny Chang
## Type     : grid drawing object
## Usage    : proportion box
##############################################################################

grid.propBox = function(...){
  grid.draw(propBoxGrob(...))
}

propBoxGrob = function(p=1, fill="red", horiz=TRUE, label=NULL, show=TRUE,
                       x=unit(0.5, "npc"), y=unit(0.5, "npc"),
                       width=unit(0.8, "npc"), height=unit(0.8, "npc"), cutEdge=c(FALSE,FALSE),
                       just="centre", hjust=NULL, vjust=NULL, gp.lab=gpar(),
                       gp.box=replicate(length(p), gpar()),
                       name=NULL, gp=gpar(), vp=NULL){
  
  igt = gTree(p=p, fill=fill, horiz=horiz, label=label, show=show, x=x, y=y,
              width=width, height=height, cutEdge=cutEdge, just=just, hjust=hjust, vjust=vjust,
              gp.lab=gp.lab, gp.box=gp.box,
              name=name, gp=gp, vp=vp,
              cl="propBox")  
  igt  
}

drawDetails.propBox = function(x, recording){
  if(x$show){
    x = setPropBoxGrob(x)
    for (i in childNames(x)) grid.draw(getGrob(x, i))
  }
}

setPropBoxGrob = function(gt){
  p       = gt$p
  fill    = gt$fill
  horiz   = gt$horiz
  label   = gt$label
  x       = gt$x
  y       = gt$y
  width   = gt$width
  height  = gt$height
  just    = gt$just
  hjust   = gt$hjust
  vjust   = gt$vjust
  gp.lab  = gt$gp.lab
  gp.box  = gt$gp.box
  cutEdge = gt$cutEdge
  
  p         = p/sum(p)
  n         = length(p)
  width     = convertWidth(width, "npc")
  height    = convertHeight(height, "npc")
  fill      = rep(fill, length.out=n)
  hjust     = grid:::resolveHJust(just, hjust)
  vjust     = grid:::resolveVJust(just, vjust)  

  # Notes: Draw the frame of the box
  edgegb    = rectGrob(x=x, y=y, width=width, height=height,
                       hjust=hjust, vjust=vjust, name="edgeBox")  


  # Notes: left and bottom origin point position
  # (x0,y0) = (p.left,p.bottom)
  
  p.left    = convertX(x - convertWidth(hjust*width , "npc"), "npc")
  p.bottom  = convertY(y - convertWidth(vjust*height, "npc"), "npc")


  if(horiz){    
    # Notes: width proportional to the probability
    w         = convertWidth(p*width, "npc")
    h         = rep(convertHeight(height, "npc"), n)
    # Notes: vector with length of 2
    # p.left = c(0,g1right,g2right) 
    # p.bottom = rep(bottom,numofgroups)
    p.left    = convertX(p.left + c(0, cumsum(p))*width, "npc")   
    p.bottom  = rep(p.bottom, n)
    a <<- p.left #??
    lab.x     = unit(diff(as.numeric(p.left))/2, "npc")
    lab.y     = rep(y, n)
    p.left    = p.left[1:n]
    lab.x     = convertX(p.left+lab.x, "npc")
   
    # calculate polygon
    if(any(cutEdge)){
      rw        = as.numeric(width)
      rh        = as.numeric(height)
      rleft     = as.numeric(p.left)
      rbottom   = as.numeric(p.bottom) 
      cutPointsXleft  = c(rleft[1]+rw*0.08,rleft[1],rleft[1]+rw*0.06)
      cutPointsXright = c(rw-rw*0.05,rw,rw-rw*0.05)
      cutPointsY      = c(rbottom[1]+0.75*rh,rbottom[1]+0.5*rh,rbottom[1]+0.25*rh)
      
      if(cutEdge[1]){
        edge.polyX = unit(c(rleft[1],rw,rw,rleft[1],cutPointsXleft),"npc")
        polyY      = unit(c(rbottom,rbottom+rh,cutPointsY),"npc")
        in.polyX   = unit(c(rleft[1],rleft[2],rleft[2],rleft[1],cutPointsXleft),"npc")
        edgegb = polygonGrob(x=edge.polyX,y=polyY,name="edgeBox")
      }
      else if(cutEdge[2]){
        edge.polyX = unit(c(rleft[1],rw,cutPointsXright,rw,rleft[1]),"npc")
        polyY      = unit(c(rbottom,rev(cutPointsY),rbottom+rh),"npc")
        in.polyX   = unit(c(rleft[2],rw,cutPointsXright,rw,rleft[2]),"npc")
        edgegb = polygonGrob(x=edge.polyX,y=polyY,name="edgeBox")       
      }
    }
  } else {
    w         = rep(convertWidth(width, "npc"), n)
    h         = convertHeight(p*height, "npc")
    p.left    = rep(p.left, length(p))
    p.bottom  = convertY(p.bottom + c(0, cumsum(p)[-n])*height, "npc")
  } 
  
  # Notes: Draw group names
  if(!is.null(label)){
    labgb = textGrob(label, x=lab.x, y=lab.y, gp=gp.lab)
  } else
    labgb = NULL
  
  
  gbL = list()
  length(gbL) = n + 2
  gbL[[n+1]] = edgegb
  gbL[[n+2]] = labgb

  for(i in 1:n){
    if(!is.null(fill))
      gp.box[[i]]$fill = fill[i]

      if(cutEdge[i])
        gbL[[i]] = polygonGrob(x=in.polyX,y=polyY,gp=gp.box[[i]],name=paste("fillBox", i, sep=""))
      else
        gbL[[i]] = rectGrob(x=p.left[i], y=p.bottom[i], width=w[i], height=h[i],
                            hjust=0, vjust=0, gp=gp.box[[i]],
                            name=paste("fillBox", i, sep=""))
  }
  
  gt = setChildren(gt, do.call("gList", gbL))
  gt
}

editDetails.propBox = function(x, spec){
  x = propBoxGrob(p=x$p, fill=x$fill, horiz=x$horiz,
                  label=x$label, show=x$show, x=x$x, y=x$y,
                  width=x$width, height=x$height, cutEdge=x$cutEdge,
                  just=x$just, hjust=x$hjust, vjust=x$vjust,
                  gp.lab=x$gp.lab, gp.box=x$gp.box,
                  name=x$name, gp=x$gp, vp=x$vp)    
  x
}

validDetails.propBox = function(x){  
  if(!is.logical(x$horiz))
    stop("horiz must be logical")
  
  if(!is.null(x$label))
    if(length(x$label)!=length(x$p))
      stop("p and label must have same length")
  
  if(!is.unit(x$x) | !is.unit(x$y) | !is.unit(x$width) | !is.unit(x$height))
    stop("x, y, width and height must be unit")

  if(length(x$gp.box)!=length(x$p))
    stop("gp.box and p must have same length")
  
  x
}

grid.propBox.example = function(){
  vp = viewport(width=unit(0.8, "npc"), height=unit(0.8, "npc"),
                xscale=c(-0.5, 1.5), yscale=c(-10, 10))
  pushViewport(vp)
  grid.rect()
  grid.xaxis()
  grid.yaxis()
  browser()
  grid.propBox(p=c(0.2, 0.5, 0.3), fill=c("red", "blue", "green"), horiz=TRUE,
               x=unit(0.5, "npc"), y=unit(0.5, "npc"), label=c("A", "B", "C"),
               gp.lab = gpar(cex=3),
               gp.box = list(gpar(lwd=10), gpar(lwd=5), gpar(lwd=1)),
               width=unit(1, "native"), height=unit(0.5, "npc"),
               name="test")
}

# grid.propBox.example()