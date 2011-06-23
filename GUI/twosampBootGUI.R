##############################################################################
## Date     : 2011-05-18
## Author   : Vivian Li
## Type     : function
## Usage    : bootstrap1 GUI
##############################################################################

twosampBootGUI = function(){
  initEnv = function(){
    e             = new.env()
    e$pause       = FALSE
    e$isShow      = FALSE
    e$redrawPanel = FALSE
    e$bootstrapPanel = FALSE
    
    e$n           = 1000
    e$n.cum       = 0    # cumulative number of bootstrapping times
    e$sel         = 0
    e$curr.n      = NULL
    e$x.df        = NULL
    e$filename    = NULL
    e$diff        = NULL
    e$redraw      = NULL
    e
  }
  
  setEnvData = function(x.df){
    bp1Env$x.df   = x.df
  }
  
  simuEnv = function(){
    sim           = bootstrap1Simulation(bp1Env$x.df, bp1Env$n)
    bp1Env$curr.n = 1
    bp1Env$diff   = sim$diff
    bp1Env$redraw = sim$tab
  }
  
  updateEnvCurrN = function(){
    if(bp1Env$curr.n < bp1Env$n)
      bp1Env$curr.n = bp1Env$curr.n + 1
    else{
      bp1Env$curr.n = 1
    }
  }  
  
  initDev = function(){
    if(!is.null(bp1Env$x.df)){
      graphics.off()
      # won't fit the 13" screen
      # should use (11,7)
      x11(width=10, height=8)
      grid.bootstrap1(bp1Env$x.df,
                   diffFun=get(svalue(diffFun.radio)),
                   main=bp1Env$filename,
                   name="Bootstrap1Movie")
      disableButtons()
      svalue(status) = "Loading..."
      simuEnv()
      svalue(status) = "Finished"
      grid.newpage()
      grid.bootstrap1(bp1Env$x.df, x.diff=bp1Env$diff,
                   diffFun=get(svalue(diffFun.radio)),
                   main=bp1Env$filename,
                   name="Bootstrap1Movie")
      bp1Env$isShow = TRUE
      enableButtons()
      enabled(point.but)=FALSE
    } else {
      dialog("No data", "Warning")
    }
    return()
  }
  showWarning = function(){
    dialog("Animation still runing...", "Warnning")
  }
    
  disableButtons = function(){
    enabled(read.data.but) = FALSE
    enabled(show.data.but) = FALSE
    enabled(run1.but)      = FALSE
    enabled(run2.but)      = FALSE
    enabled(diffFun.radio) = FALSE
    enabled(point.but)     = FALSE
  }
  
  enableButtons = function(){
    enabled(read.data.but) = TRUE
    enabled(show.data.but) = TRUE
    enabled(run1.but)      = TRUE
    enabled(run2.but)      = TRUE
    enabled(diffFun.radio) = TRUE
  }
  
  pause = function(){
    while(bp1Env$pause){
    }  
  }  
  handlerDiffFun = function(h, ...){
    #graphics.off()
    if(!is.null(bp1Env$x.df)){
      handlerShowData()
      enableButtons()
    }
    else{ 
      enabled(show.data.but) = FALSE
      enabled(run1.but)      = FALSE
      enabled(pause.but)     = FALSE
      enabled(run2.but)      = FALSE  
      enabled(show.ci.but)   = FALSE
      
    }
    enabled(point.but) = FALSE
  }
  
  handlerReadData = function(h, ...){
    disableButtons()
    path = gfile("Select a csv file...",
                 filter=list("csv" = list(patterns = c("*.csv"))),
                 handler=function(h, ...) NULL)
    
    if(!is.na(path)){
      s = unlist(strsplit(path, "\\\\"))
      s = unlist(strsplit(s[length(s)], "[.]"))[1]
        
      main = gsub("^.*\\\\", "", path)
      main = gsub(".csv$",   "", main)
      bp1Env$filename = main
      
      d = checkGroupBootstrap1(read.csv(path), diffFun=get(svalue(diffFun.radio)))
      setEnvData(d)
      dialog("Data loaded", "Message")
      bp1Env$isShow = FALSE
      enableButtons()
      graphics.off()
    } else {
      enabled(read.data.but) = TRUE
      enabled(diffFun.radio) = TRUE
    }
  }
  
  handlerShowData = function(h, ...){
    initDev()
    return()
  }
  
  handlerRedraw = function(h, ...){
    disableButtons()
    enabled(pause.but)  = TRUE
    enabled(show.ci.but)= FALSE
    
    if(!bp1Env$isShow)
      initDev()
    
    if(!bp1Env$redrawPanel)
      bp1Env$redrawPanel = TRUE
      
    if(bp1Env$bootstrapPanel){
      bp1Env$bootstrapPanel = FALSE
      bp1Env$curr.n = 1
      bootstrap1Refresh()
    }
 
    svalue(status) = "Running..."
    n.redraw       = svalue(redraw.radio)
    sel            = svalue(redraw.radio, index=TRUE)   
    
    if(sel != 1)
      n.redraw     = as.numeric(n.redraw)

    if(sel==1){
      svalue(status) = paste("Running...1/1")
      bootstrap1Refresh()
      index = bootstrap1GetRedrawGroup(bp1Env$redraw, bp1Env$curr.n)
      obj   = bootstrap1InitMovingTxt(index)
      for(i in 1:length(index)){
        bootstrap1MoveTableFromTablevp2(obj,index,i)
        pause()
      }
      bootstrap1UpdateGhostBox(index,bp1Env$curr.n)
      updateEnvCurrN()
      pause()
      Sys.sleep(1)
    } else if(sel==2 || sel==3){
      for(i in 1:n.redraw){
        svalue(status) = paste("Running...", i, "/", n.redraw, sep="")
        index = bootstrap1GetRedrawGroup(bp1Env$redraw, bp1Env$curr.n)      
        bootstrap1UpdateGhostBox(index,bp1Env$curr.n)
        updateEnvCurrN()
        pause()
        Sys.sleep(1)
      }
    } else if(sel==4){
      for(i in 1:n.redraw){
        svalue(status) = paste("Running...", i, "/", n.redraw, sep="")
        index = bootstrap1GetRedrawGroup(bp1Env$redraw, bp1Env$curr.n)      
        bootstrap1UpdateGhostBox(index,bp1Env$curr.n)
        updateEnvCurrN()
        pause()
        Sys.sleep(0.5)
      }
    }    
    svalue(status) = "Finished"
    pause()
    bootstrap1Finalise()
    enableButtons()
    enabled(point.but)  = TRUE
    enabled(pause.but) = FALSE
  }
  
  handlerBootstrapDiff = function(h, ...){
    disableButtons()
    enabled(pause.but)   = FALSE
    enabled(show.ci.but) = FALSE
    
    if(!bp1Env$isShow)
      initDev()

    if(!bp1Env$bootstrapPanel)
      bp1Env$bootstrapPanel = TRUE
      
    if(bp1Env$redrawPanel){
      bp1Env$redrawPanel = FALSE
      bp1Env$curr.n = 1
      bootstrap1Refresh()
    }
    
    svalue(status) = "Running..."
    n.random       = svalue(bootstrap.radio)
    bp1Env$sel = svalue(bootstrap.radio, index=TRUE)   
    sel            = bp1Env$sel
    
    if(bp1Env$curr.n==1 && sel<=4)
      bootstrap1UpdateDistShow(FALSE)
    
    bootstrap1InitAddition()
    if(sel==1){
      svalue(status) = paste("Running...1/1")
      bootstrap1Refresh()
      index = bootstrap1GetRedrawGroup(bp1Env$redraw, bp1Env$curr.n)  
      obj   = bootstrap1InitMovingTxt(index)
      for(i in 1:length(index)){
        bootstrap1MoveTableFromTablevp2(obj,index,i)
        pause()
      }
      bootstrap1UpdateGhostBox(index,bp1Env$curr.n)
      bootstrap1MoveArrowBarFromBstrapvp()
      bootstrap1UpdateDistShow(bp1Env$curr.n)
      pause()
      bootstrap1RemoveArrow()
      Sys.sleep(0.05)
      bootstrap1Finalise()
      updateEnvCurrN()
    } else if(sel==2 | sel==3){
      n.random = as.numeric(n.random)
      for(i in 1:n.random){
        svalue(status) = paste("Running...", i, "/", n.random, sep="")
        index = bootstrap1GetRedrawGroup(bp1Env$redraw, bp1Env$curr.n)
        bootstrap1UpdateGhostBox(index,bp1Env$curr.n)
        bootstrap1MoveArrowBarFromBstrapvp()
        bootstrap1UpdateDistShow(bp1Env$curr.n)
        pause()
        bootstrap1RemoveArrow()
        Sys.sleep(0.05)
        updateEnvCurrN()
      }
      pause()
      bootstrap1Finalise()
    } else if(sel==4){
      n.random = as.numeric(n.random)
      for(i in 1:n.random){
        svalue(status) = paste("Running...", i, "/", n.random, sep="")
        index = bootstrap1GetRedrawGroup(bp1Env$redraw, bp1Env$curr.n)
        bootstrap1UpdateGhostBox(index,bp1Env$curr.n)
        bootstrap1UpdateDistShow(bp1Env$curr.n)
        updateEnvCurrN()
        if(bp1Env$curr.n>bp1Env$n)
          break
      }
      pause()
      bootstrap1Finalise()
    } else if(sel==5){   
      bootstrap1Refresh()
      bootstrap1ClearTable()
      for(i in round(seq(1, bp1Env$n, length.out=10))[-1])
        bootstrap1UpdateDistShow2(1:i,shape=2)    
      
      bootstrap1MoveArrowBarFromDatavp()
      bp1Env$curr.n = 1
      enabled(show.ci.but) = TRUE
    }

    svalue(status) = "Finished"
    enableButtons()
    if(sel != 5)
      enabled(point.but)=TRUE
  }
  
  handlerShowCI = function(h, ...){
    #v = bp1Env$n.cum
    v = bp1Env$sel
    if (v != 5)
       bootstrap1MoveArrowBarFromDatavp(bp1Env$step+5)
    x = bootstrap1GetCurrentData()
    g = bootstrap1GetDataGroup()
    d = abs(diff(tapply(x,g,bootstrap1GetDiffFun())))
    bootstrap1ShowCI(d)
    enabled(show.ci.but) = FALSE
  }
  
  handlerPause = function(h, ...){
    bp1Env$pause = !bp1Env$pause
    
    if(bp1Env$pause){
      svalue(status) = "Pause"
      svalue(pause.but)  = "Continue"
    }
    else{
      svalue(status) = "Running..."
      svalue(pause.but)  = "Pause"
    }     
  }
  
  handlerPointer = function(h,...){
    enabled(point.but) = FALSE
    enabled(pause.but) = TRUE
    enabled(run1.but)  = FALSE
    enabled(run2.but)  = FALSE
    bootstrap1Refresh()
    index = bootstrap1GetRedrawGroup(bp1Env$redraw, bp1Env$curr.n-1)
    pointer=bootstrap1GetPointerInfo(index)
    sleep = 1.5
    for(i in 1:length(index)){  
      idx = which(index==i)
      
      if(length(idx)<2){
        sleep = sleep*(1-i/length(index))
        sleep = ifelse(sleep<0.5,0.5,sleep)
      }
      pointer = bootstrap1ShowPointer(pointer,index,i) 

      Sys.sleep(sleep/3)      
      pause()
      Sys.sleep(sleep/3)
      pause()
      Sys.sleep(sleep/3)
      pause()
    }
    Sys.sleep(sleep/2)
    bootstrap1RemovePointer()
    enabled(point.but) = TRUE
    enabled(pause.but) = FALSE
    enabled(run1.but)  = TRUE
    enabled(run2.but)  = TRUE  
  }

  bp1Env = initEnv()
  window = gwindow("bootstrapping", width=100, height=200)
  diffFun.ops   = c("median", "mean")
  redraw.times  = c("1 (all)",1, 5, 20)
  bootstrap.times  = c("1 (all)", 1, 5, 20, bp1Env$n)
  
  diffFun.radio = gradio(diffFun.ops,  horizontal=TRUE, handler=handlerDiffFun)
  redraw.radio  = gradio(redraw.times,  horizontal=FALSE)
  bootstrap.radio  = gradio(bootstrap.times,  horizontal=FALSE)
  
  parent = ggroup(cont=window)
  group  = ggroup(container=parent, horizontal=FALSE)
  add(group, diffFun.radio)
  read.data.but = gbutton(text="Read Data", container=group,
                          handler=handlerReadData)
  show.data.but = gbutton(text="Show Data", container=group,
                          handler=handlerShowData)
  addSpace(group, 20, horizontal=FALSE)
  
  tmp = gframe("bootstrapping",  container=group)
  add(tmp, redraw.radio)
  run1.but  = gbutton(text="Run", container=group, handler=handlerRedraw)
  point.but = gbutton(text="Track",container=group,handler=handlerPointer)
  pause.but = gbutton(text="Pause", container=group, 
                    handler=handlerPause)
  
  addSpace(group, 40, horizontal=FALSE)
    
  tmp = gframe("Capture Bootstrapping Difference",  container=group)
  add(tmp, bootstrap.radio)
  run2.but = gbutton(text="Run", container=group, handler=handlerBootstrapDiff)
  addSpace(group, 10, horizontal=FALSE)

  show.ci.but = gbutton(text="Show Confidence Interval", container=group,
                          handler=handlerShowCI)
  addSpace(group, 10, horizontal=FALSE)
  
  status = glabel("", container=group)    
    
  enabled(show.data.but) = FALSE
  enabled(run1.but)      = FALSE
  enabled(pause.but)     = FALSE
  enabled(run2.but)      = FALSE  
  enabled(show.ci.but)   = FALSE
  enabled(point.but)     = FALSE
}