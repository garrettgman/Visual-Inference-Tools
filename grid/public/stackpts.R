##############################################################################
## Date     : 2011-02-23
## Author   : Danny Chang
## Notes    : Vivian Li
## Type     : function
## Usage    : stacking points, return a pts object
## Note     : not support verticle stack yet
##############################################################################

stackpts = function(x, w, v, vrange, wrange, method=1, horiz=TRUE){
  ## stack points
  ##
  ## Args:
  ##    x       : data, numeric vector
  ##    w       : point width,  a scalar
  ##    v       : level height, a scalar
  ##    vrange  : stacking height range, a numeric vector with length 2
  ##    wrange  : stacking width range,  a numeric vector with length 2
  ##
  ## Returns:
  ##  pts object
  
  n = length(x)
  vmin = min(vrange)
  vmax = max(vrange)
  y = rep(vmin, n)
  
  ## stacking in levels
  lv = rep(1, n)

  lv = stackMethod(method, x, w, wrange, lv)
 
  if((vmin+max(lv)*v)>vmax)
    v = diff(vrange)/max(lv)
  
  y = vmin + lv*v
  pts(x=x, y=y)
}

stackMethod = function(method=1, x, w, wrange, lv){
  if(method==1){
    ###################
    ## fixed bins
    ###################

    oo = order(x)
    #x = x[oo]
    # number of bins to contain points
    nbins = round(diff(wrange)/w)
    # index of place a point falls into
    bins  = ceiling(nbins*(x-min(x))/diff(wrange))
    # ??
    xg    = split(x,bins)
    xo    = lapply(xg, seq_along)
    x     = unlist(xg, use.names = FALSE)
    lv    = unlist(xo, use.names = FALSE)[order(x)][order(oo)]
    
    
  } else if(method==2){
    ###################
    ## dynamic bins
    ###################
    oo = order(x)
    x = x[oo]    
    lv.curr = 1
    lv.next = lv.curr + 1
    s = TRUE
    while(s){
      idx = which(lv==lv.curr)
      for(i in idx){
        if(lv[i]==lv.curr){
          r = x[i] + w
          j = x[idx[idx>i]] < r
          lv[idx[idx>i][j]] = lv.next
        }
      }
      lv.curr = lv.next
      lv.next = lv.curr + 1
      if(!any(lv==lv.curr))
        s = FALSE
    }
    lv = lv[order(oo)]
  }
  lv-1
}

# pdf("C:/Users/kcha080/Desktop/temp.pdf", width=12, height=9, onefile=TRUE)
 
# x = rnorm(100)
# u = unit(0.5, "char")
# vrange = c(0, 10)
# n = length(x)
# for(i in 1:n){
  # grid.newpage()
  # pushViewport(viewport(x=unit(0.5, "npc"), y=unit(0.5, "npc"),
                        # width=unit(0.8, "npc"), height=unit(0.8, "npc"),
                        # xscale=range(x), yscale=vrange))
  # grid.rect()
  # grid.xaxis()
  # grid.yaxis()
          
  # w = convertWidth(u, "native", valueOnly=TRUE)*2
  # v = convertHeight(u, "native", valueOnly=TRUE)*2

  # grid.stackpts(x[1:i], w, v, vrange, range(x), r=u, method=1)
  # #Sys.sleep(0.3)
# }

# dev.off()


# #x = rnorm(100)
# x = round(20*rnorm(200))/20
# vrange = c(0, dnorm(0))
# u = unit(0.1, "inches")


# x11(width=12, height=9)
# pushViewport(viewport(x=unit(0.25, "npc"), y=unit(0.5, "npc"),
                      # width=unit(0.4, "npc"), height=unit(0.8, "npc"),
                      # xscale=range(x), yscale=vrange))
# grid.rect()
# grid.xaxis()
# grid.yaxis()
          
# w = convertWidth(u, "native", valueOnly=TRUE)*2
# v = convertHeight(u, "native", valueOnly=TRUE)*2

# grid.stackpts(x, w, v, vrange, method=1)

# grid.text("method=1", x=unit(0.5, "npc"), y=unit(1, "npc")+unit(1, "lines"),
          # gp=gpar(cex=2))
          
# upViewport()
# pushViewport(viewport(x=unit(0.75, "npc"), y=unit(0.5, "npc"),
                      # width=unit(0.4, "npc"), height=unit(0.8, "npc"),
                      # xscale=range(x), yscale=vrange))
# grid.rect()
# grid.xaxis()
# grid.yaxis()

# w = convertWidth(u, "native", valueOnly=TRUE)*2
# v = convertHeight(u, "native", valueOnly=TRUE)*2

# grid.stackpts(x, w, v, vrange, method=2)
# grid.text("method=2", x=unit(0.5, "npc"), y=unit(1, "npc")+unit(1, "lines"),
          # gp=gpar(cex=2))