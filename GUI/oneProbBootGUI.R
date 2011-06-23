##############################################################################
## Date     : 2011-05-18
## Author   : Vivian Li
## Type     : function
## Usage    : bootstrap GUI
##############################################################################

oneProbBootGUI = function(){
  initEnv = function(){
    e             = new.env()
    e$pause       = FALSE
    e$isShow      = FALSE
    e$redrawPanel = FALSE
    e$bootstrapPanel = FALSE
    
    e$n           = 1000
    e$sel         = 0
    e$curr.n      = NULL
    e$x.df        = NULL
    e$filename    = NULL
    e$p        = NULL
    e$redraw      = NULL
    e
  }
  
  setEnvData = function(x.df){
    bp2Env$x.df   = x.df
  }
  
  simuEnv = function(){
    sim           = bootstrap2Simulation(bp2Env$x.df[,1], bp2Env$n)
    bp2Env$curr.n = 1
    bp2Env$p      = sim$p
    bp2Env$redraw = sim$tab
  }
  
  updateEnvCurrN = function(){
    if(bp2Env$curr.n < bp2Env$n)
      bp2Env$curr.n = bp2Env$curr.n + 1
    else{
      bp2Env$curr.n = 1
    }
  }  
  
  initDev = function(){
    if(!is.null(bp2Env$x.df)){
      graphics.off()
      # won't fit the 13" screen
      # should use (11,7)
      x11(width=10, height=8)
      vname = names(bp2Env$x.df)[1]
      grid.bootstrap2(bp2Env$x.df,vname=vname,
                      main=bp2Env$filename,name="Bootstrap2Movie")
      disableButtons()
      svalue(status) = "Loading..."
      simuEnv()
      svalue(status) = "Finished"
      grid.newpage()
      grid.bootstrap2(bp2Env$x.df, x.sim=bp2Env$p,vname=vname,
                   main=bp2Env$filename,
                   name="bootstrap2Movie")
      bp2Env$isShow = TRUE
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
    while(bp2Env$pause){
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
      bp2Env$filename = main
      
      d = read.csv(path)
      setEnvData(d)
      dialog("Data loaded", "Message")
      bp2Env$isShow = FALSE
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
    
    if(!bp2Env$isShow)
      initDev()
    
    if(!bp2Env$redrawPanel)
      bp2Env$redrawPanel = TRUE
      
    if(bp2Env$bootstrapPanel){
      bp2Env$bootstrapPanel = FALSE
      bp2Env$curr.n = 1
      bootstrap2Refresh()
    }
 
    svalue(status) = "Running..."
    n.redraw       = svalue(redraw.radio)
    sel            = svalue(redraw.radio, index=TRUE)   
    
    if(sel != 1)
      n.redraw     = as.numeric(n.redraw)

    if(sel==1){
      svalue(status) = paste("Running...1/1")
      bootstrap2Refresh()
      index = bootstrap2GetRedrawGroup(bp2Env$redraw, bp2Env$curr.n)
      obj   = bootstrap2InitMovingTxt(index)
      for(i in 1:length(index)){
        bootstrap2MoveTableFromTablevp(obj,index,i)
        pause()
      }
      bootstrap2UpdateBox(index,bp2Env$curr.n)
      updateEnvCurrN()
      pause()
      Sys.sleep(1)
    } else if(sel==2 || sel==3){
      for(i in 1:n.redraw){
        svalue(status) = paste("Running...", i, "/", n.redraw, sep="")
        index = bootstrap2GetRedrawGroup(bp2Env$redraw, bp2Env$curr.n)      
        bootstrap2UpdateBox(index,bp2Env$curr.n)
        updateEnvCurrN()
        pause()
        Sys.sleep(1)
      }
    } else if(sel==4){
      for(i in 1:n.redraw){
        svalue(status) = paste("Running...", i, "/", n.redraw, sep="")
        index = bootstrap2GetRedrawGroup(bp2Env$redraw, bp2Env$curr.n)      
        bootstrap2UpdateBox(index,bp2Env$curr.n)
        updateEnvCurrN()
        pause()
        Sys.sleep(0.5)
      }
    }    
    svalue(status) = "Finished"
    pause()
    bootstrap2Finalise()
    enableButtons()
    enabled(point.but)  = TRUE
    enabled(pause.but) = FALSE
  }
  
  handlerBootstrapDiff = function(h, ...){
    disableButtons()
    enabled(pause.but)   = FALSE
    enabled(show.ci.but) = FALSE
    
    if(!bp2Env$isShow)
      initDev()

    if(!bp2Env$bootstrapPanel)
      bp2Env$bootstrapPanel = TRUE
      
    if(bp2Env$redrawPanel){
      bp2Env$redrawPanel = FALSE
      bp2Env$curr.n = 1
      bootstrap2Refresh()
    }
    
    svalue(status) = "Running..."
    n.random       = svalue(bootstrap.radio)
    bp2Env$sel = svalue(bootstrap.radio, index=TRUE)   
    sel            = bp2Env$sel
    
    if(bp2Env$curr.n==1 && sel<=4)
      bootstrap2UpdateDistShow(FALSE)
    
    bootstrap2InitAddition()
    if(sel==1){
      svalue(status) = paste("Running...1/1")
      bootstrap2Refresh()
      bootstrap2UpdateDistShow(FALSE)
      index = bootstrap2GetRedrawGroup(bp2Env$redraw, bp2Env$curr.n)  
      obj   = bootstrap2InitMovingTxt(index)
      
      for(i in 1:length(index)){
        bootstrap2MoveTableFromTablevp(obj,index,i)
        pause()
      }
      pause()
      bootstrap2UpdateBox(index,bp2Env$curr.n)
      bootstrap2MoveBarFromBstrapvp()
      bootstrap2UpdateDistShow(bp2Env$curr.n)
      pause()
      Sys.sleep(0.05)
      bootstrap2Refresh()
      #bootstrapFinalise()
      updateEnvCurrN()
    } else if(sel==2 | sel==3){
      n.random = as.numeric(n.random)
      for(i in 1:n.random){
        svalue(status) = paste("Running...", i, "/", n.random, sep="")
        index = bootstrap2GetRedrawGroup(bp2Env$redraw, bp2Env$curr.n)
        bootstrap2UpdateBox(index,bp2Env$curr.n)
        bootstrap2MoveBarFromBstrapvp()
        bootstrap2UpdateDistShow(bp2Env$curr.n)
        pause()
        Sys.sleep(0.05)
        updateEnvCurrN()
      }
      pause()
      bootstrap2Finalise()
    } else if(sel==4){
      n.random = as.numeric(n.random)
      for(i in 1:n.random){
        svalue(status) = paste("Running...", i, "/", n.random, sep="")
        index = bootstrap2GetRedrawGroup(bp2Env$redraw, bp2Env$curr.n)
        bootstrap2UpdateBox(index,bp2Env$curr.n)
        bootstrap2UpdateDistShow(bp2Env$curr.n)
        updateEnvCurrN()
        if(bp2Env$curr.n>bp2Env$n)
          break
      }
      pause()
      bootstrap2Finalise()
    } else if(sel==5){   
      bootstrap2UpdateDistShow(FALSE)
      bootstrap2Refresh()
      bootstrap2ClearTable()
      for(i in round(seq(1, bp2Env$n, length.out=10))[-1])
        bootstrap2UpdateDistShow2(1:i,shape=2)    
      bp2Env$curr.n = 1
      enabled(show.ci.but) = TRUE
    }

    svalue(status) = "Finished"
    enableButtons()
    if(sel != 5)
      enabled(point.but)=TRUE
  }
  
  handlerShowCI = function(h, ...){
    enabled(show.ci.but) = FALSE
    # bootstrap2MoveBarFromDatavp()
    prop = bootstrap2GetCurrentData()
    bootstrap2ShowCI(prop[1])
  }
  
  handlerPause = function(h, ...){
    bp2Env$pause = !bp2Env$pause
    
    if(bp2Env$pause){
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
    bootstrap2Refresh()
    index = bootstrap2GetRedrawGroup(bp2Env$redraw, bp2Env$curr.n-1)
    pointer=bootstrap2GetPointerInfo(index)
    sleep = 1.5
    for(i in 1:length(index)){  
      idx = which(index==i)
      
      if(length(idx)<2){
        sleep = sleep*(1-i/length(index))
        sleep = ifelse(sleep<0.5,0.5,sleep)
      }
      pointer = bootstrap2ShowPointer(pointer,index,i) 

      Sys.sleep(sleep/3)      
      pause()
      Sys.sleep(sleep/3)
      pause()
      Sys.sleep(sleep/3)
      pause()
    }
    Sys.sleep(sleep/2)
    bootstrap2RemovePointer()
    enabled(point.but) = TRUE
    enabled(pause.but) = FALSE
    enabled(run1.but)  = TRUE
    enabled(run2.but)  = TRUE  
  }

  bp2Env = initEnv()
  window = gwindow("bootstrapping", width=100, height=200)
  redraw.times  = c("1 (all)",1, 5, 20)
  bootstrap.times  = c("1 (all)", 1, 5, 20, bp2Env$n)
  
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