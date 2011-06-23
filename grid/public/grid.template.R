##############################################################################
## Date     : 2011-02-23
## Author   : Danny Chang
## Type     : grid drawing object and functions template
## Usage    : boxplot with stack points
##############################################################################

grid.boxdot = function(...){
  grid.draw(boxdotGrob(...))
}

boxdotGrob = function(){
}

editDetails.boxdot = function(x, spec){
  x
}

validDetails.boxdot = function(x){
  x
}

grid.boxdot.example = function(){
  vp = viewport(width=unit(0.8, "npc"), height=unit(0.8, "npc"),
                xscale=c(-10, 10), yscale=c(-10, 10))
  pushViewport(vp)
  grid.rect()
  grid.xaxis()
  grid.yaxis()
}