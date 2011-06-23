rm(list = ls())
library(grid)

##############################################################################
## Date     : 2011-02-25
## Author   : Danny Chang
## Type     : grid drawing object and functions
## Usage    : draw points for pts class, support correct "size"
## Note     : only support circle, fiilled circle can be done by gpar()
##############################################################################

grid.circlepts = function(...){
  grid.draw(circleptsGrob(...))
}

circleptsGrob = function(x=stats::runif(10), y=stats::runif(10),
                         size=unit(0.05, "inches"), default.units="native",
                         name=NULL, gp=gpar(), vp=NULL){
  
  igt = gTree(x=x, y=y, size=size, default.units=default.units,
              children=makeCirclePtsGrob(x, y, size, default.units),
              gp=gp, name=name, vp=vp, cl="circlepts")
  igt
}

makeCirclePtsGrob = function(x, y, size, default.units){
  gList(circleGrob(x=x, y=y, r=size, default.units="native",
        name="circle"))
}

editDetails.circlepts = function(x, spec){
  x
}

validDetails.circlepts = function(x){
  x
}

grid.circlepts.example = function(){
  vp = viewport(width=unit(0.8, "npc"), height=unit(0.8, "npc"),
                xscale=c(-10, 10), yscale=c(-10, 10))
  pushViewport(vp)
  grid.rect()
  grid.xaxis()
  grid.yaxis()
  grid.circlepts(x=runif(10, 0, 5), y=runif(10, 0, 5),
                 name="test", gp=gpar(col="red"))
}

grid.circlepts.example()
traceback()