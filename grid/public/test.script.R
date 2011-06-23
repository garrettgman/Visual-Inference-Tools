# pts
# n    : number of points
# FUN  : determine the values of points
randomPts = function(n=1,FUN=abs){
  # randomly create a list of points
  FUN <- match.fun(FUN)
  x <- FUN(rnorm(n))
  y <- FUN(rnorm(n))
  pts(x,y)
}

positive = function(v){
  # shift vector by its minimum value
  v-min(v)
}

origin = function(v){
  # remain the same
  v
}

ps<-randomPts(10,abs)
