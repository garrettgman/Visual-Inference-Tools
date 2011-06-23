##############################################################################
## Date     : 2011-02-23
## Author   : Danny Chang
## Notes    : Vivian Li
## Type     : S3 class
## Usage    : a class for points, named "pts"
##            support some generic function such as c, sample, eq ... etc
##############################################################################

# create points (x,y)
# x and y can be vectors
pts = function(x, y){
  ret = list(x=x, y=y)
  class(ret) = "pts"
  ret
}

#??
"[.pts" <- function(obj, idx) {
  x = obj$x[idx]
  y = obj$y[idx]
  pts(x, y)
}

#??
c.pts = function(...){
  p = list(...)
  x = sapply(p, function(p) p$x)
  y = sapply(p, function(p) p$y)
  pts(x, y)
}

length.pts = function(obj, ...) length(obj$x, ...)

sample.pts = function(obj, size, ...){
  # sampling points
  idx = sample(1:length(obj), size, ...)
  obj[idx]
}

dist.pts = function(p1, p2){
  ## euclidean distance
  ## the disctance between p1 (x1,y1) and p2 (x2,y2)
  if(length(p1)!=1 || length(p2)!=1)
    stop("length of p1 and p2 must be 1")
  x = rbind(c(p1$x, p1$y), c(p2$x, p2$y))
  as.numeric(dist(x))
}

order.pts = function(obj, by="x", ...){
  if(by=="x")
    idx = order(obj$x, ...)
  else if(by=="y")
    idx = order(obj$y, ...)
  else
    stop("must by x or y")
    
  idx
}

sort.pts = function(obj, by="x", ...){
  idx = order.pts(obj, by, ...)
  obj[idx]
}

## Notes: generate series of points from (x1,y1) to (x2,y2)
seq.pts = function(from, to, ...){
  ## pts.seq.pts
  x = seq(from$x, to$x, ...)
  y = seq(from$y, to$y, ...)
  pts(x, y)
}

## Notes: whether the points is sitting within a given range
isInRange.pts = function(obj, xrange, yrange, ...){
  ## pts.isInRange.pts
  obj$x>=min(xrange) & obj$x<=max(xrange) &
  obj$y>=min(yrange) & obj$y<=max(yrange)
}