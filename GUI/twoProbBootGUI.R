##############################################################################
## Date     : 2011-05-18
## Author   : Vivian Li
## Type     : function
## Usage    : bootstrap3 GUI
##############################################################################

twoProbBootGUI = function(){
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
    bp3Env$x.df   = x.df
  }
  
  simuEnv = function(){
    sim           = bootstrap3Simulation(bp3Env$x.df, bp3Env$n)
    bp3Env$curr.n = 1
    bp3Env$diff   = sim$diff
    bp3Env$redraw = sim$tab
  }
  
  updateEnvCurrN = function(){
    if(bp3Env$curr.n < bp3Env$n)
      bp3Env$curr.n = bp3Env$curr.n + 1
    else{
      bp3Env$curr.n = 1
    }
  }  
  
  initDev = function(){
    if(!is.null(bp3Env$x.df)){
      graphics.off()
      # won't fit the 13" screen
      # should use (11,7)
      x11(width=11, height=7)
      grid.bootstrap3(bp3Env$x.df,
                   main=bp3Env$filename,
                   name="Bootstrap3Movie")
      disableButtons()
      svalue(status) = "Loading..."
      simuEnv()
      svalue(status) = "Finished"
      grid.newpage()
      grid.bootstrap3(bp3Env$x.df, x.diff=bp3Env$diff,
                   main=bp3Env$filename,
                   name="Bootstrap3Movie")
      bp3Env$isShow = TRUE
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
    enabled(point.but)     = FALSE
  }
  
  enableButtons = function(){
    enabled(read.data.but) = TRUE
    enabled(show.data.but) = TRUE
    enabled(run1.but)      = TRUE
    enabled(run2.but)      = TRUE
  }
  
  pause = function(){
    while(bp3Env$pause){
    }  
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
      bp3Env$filename = main
      
      d = checkGroupBootstrap3(read.csv(path))
      setEnvData(d)
      dialog("Data loaded", "Message")
      bp3Env$isShow = FALSE
      enableButtons()
      graphics.off()
    } else {
      enabled(read.data.but) = TRUE
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
    
    if(!bp3Env$isShow)
      initDev()
    
    if(!bp3Env$redrawPanel)
      bp3Env$redrawPanel = TRUE
      
    if(bp3Env$bootstrapPanel){
      bp3Env$bootstrapPanel = FALSE
      bp3Env$curr.n = 1
      bootstrap3Refresh()
    }
 
    svalue(status) = "Running..."
    n.redraw       = svalue(redraw.radio)
    sel            = svalue(redraw.radio, index=TRUE)   
    
    if(sel != 1)
      n.redraw     = as.numeric(n.redraw)

	bootstrap3CheckDist(reset=TRUE)
    if(sel==1){
      svalue(status) = paste("Running...1/1")
      bootstrap3Refresh()
      index = bootstrap3GetRedrawGroup(bp3Env$redraw, bp3Env$curr.n)
      obj   = bootstrap3InitMovingTxt(index)
      for(i in 1:length(index)){
        bootstrap3MoveTableFromTablevp(obj,index,i)
        pause()
      }
      bootstrap3UpdateBox(index,bp3Env$curr.n)
      updateEnvCurrN()
      pause()
      Sys.sleep(1)
    } else if(sel==2 || sel==3){
      for(i in 1:n.redraw){
        svalue(status) = paste("Running...", i, "/", n.redraw, sep="")
        index = bootstrap3GetRedrawGroup(bp3Env$redraw, bp3Env$curr.n)      
        bootstrap3UpdateBox(index,bp3Env$curr.n)
        updateEnvCurrN()
        pause()
        Sys.sleep(1)
      }
    } else if(sel==4){
      for(i in 1:n.redraw){
        svalue(status) = paste("Running...", i, "/", n.redraw, sep="")
        index = bootstrap3GetRedrawGroup(bp3Env$redraw, bp3Env$curr.n)      
        bootstrap3UpdateBox(index,bp3Env$curr.n)
        updateEnvCurrN()
        pause()
        Sys.sleep(0.5)
      }
    }    
    svalue(status) = "Finished"
    pause()
    bootstrap3Finalise()
    enableButtons()
    if(nrow(bp3Env$x.df)<=initArgsBootstrap3()$maxRow)
      enabled(point.but)  = TRUE
    enabled(pause.but) = FALSE
  }
  
  handlerBootstrapDiff = function(h, ...){
    disableButtons()
    enabled(pause.but)   = FALSE
    enabled(show.ci.but) = FALSE
    
    if(!bp3Env$isShow)
      initDev()

    if(!bp3Env$bootstrapPanel)
      bp3Env$bootstrapPanel = TRUE
      
    if(bp3Env$redrawPanel){
      bp3Env$redrawPanel=FALSE
      bp3Env$curr.n = 1
      bootstrap3Refresh()
    }
    
    svalue(status) = "Running..."
    n.random       = svalue(bootstrap.radio)
    bp3Env$sel     = svalue(bootstrap.radio, index=TRUE)   
    sel            = bp3Env$sel
    
	bootstrap3CheckDist()
    if(bp3Env$curr.n==1 && sel<=4)
      bootstrap3UpdateDistShow(FALSE)
  
    bootstrap3InitAddition()
	
    if(sel==1){
      svalue(status) = paste("Running...1/1")
      bootstrap3Refresh()
      index = bootstrap3GetRedrawGroup(bp3Env$redraw, bp3Env$curr.n)  
      obj   = bootstrap3InitMovingTxt(index)
      for(i in 1:length(index)){
        bootstrap3MoveTableFromTablevp(obj,index,i)
        pause()
      }
      bootstrap3UpdateBox(index,bp3Env$curr.n)
      bootstrap3MoveArrowBarFromBstrapvp()
      bootstrap3UpdateDistShow(bp3Env$curr.n)
      pause()
      bootstrap3RemoveArrow()
      Sys.sleep(0.05)
      bootstrap3Finalise()
      updateEnvCurrN()
    } else if(sel==2 | sel==3){
      n.random = as.numeric(n.random)
      for(i in 1:n.random){
        svalue(status) = paste("Running...", i, "/", n.random, sep="")
        index = bootstrap3GetRedrawGroup(bp3Env$redraw, bp3Env$curr.n)
        bootstrap3UpdateBox(index,bp3Env$curr.n)
        bootstrap3MoveArrowBarFromBstrapvp()
        bootstrap3UpdateDistShow(bp3Env$curr.n)
        pause()
        bootstrap3RemoveArrow()
        Sys.sleep(0.05)
        updateEnvCurrN()
      }
      pause()
      bootstrap3Finalise()
    } else if(sel==4){
      n.random = as.numeric(n.random)
      for(i in 1:n.random){
        svalue(status) = paste("Running...", i, "/", n.random, sep="")
        index = bootstrap3GetRedrawGroup(bp3Env$redraw, bp3Env$curr.n)
        bootstrap3UpdateBox(index,bp3Env$curr.n)
        bootstrap3UpdateDistShow(bp3Env$curr.n)
        updateEnvCurrN()
        if(bp3Env$curr.n>bp3Env$n)
          break
      }
      pause()
      bootstrap3Finalise()
    } else if(sel==5){   
      bootstrap3Refresh()
      bootstrap3ClearTable()
      for(i in round(seq(1, bp3Env$n, length.out=10))[-1])
        bootstrap3UpdateDistShow2(1:i,shape=2)    
      
      bootstrap3MoveArrowBarFromDatavp()
      bp3Env$curr.n = 1
      enabled(show.ci.but) = TRUE
    }

    svalue(status) = "Finished"
    enableButtons()
    if(sel != 5 && nrow(bp3Env$x.df)<=initArgsBootstrap3()$maxRow)
      enabled(point.but)=TRUE
  }
  
  handlerShowCI = function(h, ...){
    v = bp3Env$sel
    if (v != 5)
       bootstrap3MoveArrowBarFromDatavp(bp3Env$step+5)
    x = bootstrap3GetCurrentData()
    g = bootstrap3GetDataGroup()
    d = getPropBoxValue(x, g)$d
    bootstrap3ShowCI(d)
    enabled(show.ci.but) = FALSE
  }
  
  handlerPause = function(h, ...){
    bp3Env$pause = !bp3Env$pause
    
    if(bp3Env$pause){
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
    bootstrap3Refresh()
    index = bootstrap3GetRedrawGroup(bp3Env$redraw, bp3Env$curr.n-1)
    pointer=bootstrap3GetPointerInfo(index)
    sleep = 1.5
    for(i in 1:length(index)){  
      idx = which(index==i)
      
      if(length(idx)<2){
        sleep = sleep*(1-i/length(index))
        sleep = ifelse(sleep<0.5,0.5,sleep)
      }
      pointer = bootstrap3ShowPointer(pointer,index,i) 

      Sys.sleep(sleep/3)      
      pause()
      Sys.sleep(sleep/3)
      pause()
      Sys.sleep(sleep/3)
      pause()
    }
    Sys.sleep(sleep/2)
    bootstrap3RemovePointer()
    enabled(point.but) = TRUE
    enabled(pause.but) = FALSE
    enabled(run1.but)  = TRUE
    enabled(run2.but)  = TRUE  
  }

  bp3Env = initEnv()
  window = gwindow("bootstrapping", width=100, height=200)
  redraw.times  = c("1 (all)",1, 5, 20)
  bootstrap.times  = c("1 (all)", 1, 5, 20, bp3Env$n)
  
  redraw.radio  = gradio(redraw.times,  horizontal=FALSE)
  bootstrap.radio  = gradio(bootstrap.times,  horizontal=FALSE)
  
  parent = ggroup(cont=window)
  group  = ggroup(container=parent, horizontal=FALSE)
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