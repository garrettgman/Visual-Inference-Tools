##############################################################################
## Date     : 2011-02-25
## Author   : Danny Chang
## Type     : grid drawing object and functions
## Usage    : draw points, support correct "size"
## Note     : only support circle, fiilled circle can be done by gpar()
##############################################################################

grid.circlepts = function(...){
  grid.draw(circleptsGrob(...))
}

circleptsGrob = function(x=stats::runif(10), y=stats::runif(10),
                         size=unit(0.05, "inches"), default.units="native",
                         name=NULL, gp=gpar(), vp=NULL){
  ## define circlepts grob
  ##
  ## Args:
  ##    x             : x-values
  ##    y             : y-values
  ##    size          : point radius
  ##    default.units : A string indicating the default units to use
  ##
  ## Returns:
  ##  grob
  
  igt = gTree(x=x, y=y, size=size, default.units=default.units,
              children=makeCircleptsGrob(x, y, size, default.units),
              gp=gp, name=name, vp=vp, cl="circlepts")
  igt
}

makeCircleptsGrob = function(x, y, size, default.units){
  gList(circleGrob(x=x, y=y, r=size, default.units=default.units,
        name="circle"))
}

editDetails.circlepts = function(x, spec){
  x = circleptsGrob(x$x, x$y, x$size, x$default.units, x$name, x$gp, x$vp)
  x
}

validDetails.circlepts = function(x){
  if(length(x$x)!=length(x$y))
    stop("x and y length different")
    
  if(!is.unit(x$size))
    stop("size has to be unit")
  
  x
}

grid.circlepts.example = function(){
  vp = viewport(width=unit(0.8, "npc"), height=unit(0.8, "npc"),
                xscale=c(-10, 10), yscale=c(-10, 10))
  pushViewport(vp)
  grid.rect()
  grid.xaxis()
  grid.yaxis()
  grid.circlepts(x=stats::runif(10, 0, 5), y=stats::runif(10, 0, 5),
                 name="test", gp=gpar(col="red"))
}
# grid.circlepts.example()
# traceback()