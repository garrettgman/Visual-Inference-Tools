##############################################################################
## Date     : 2011-03-10
## Author   : Danny Chang
## Type     : function
## Usage    : random1 GUI
##############################################################################

randomGUI = function(){
  initEnv = function(){
    e             = new.env()
    e$pause       = FALSE
    e$isShow      = FALSE
    e$redrawPanel = FALSE
    e$randomPanel = FALSE
    
    e$n           = 1000
    e$curr.n      = NULL
    e$x.df        = NULL
    e$filename    = NULL
    e$diff        = NULL
    e$redraw      = NULL
    e
  }
  
  setEnvData = function(x.df){
    random1Env$x.df   = x.df
  }
  
  simuEnv = function(){
    sim               = random1Simulation(random1Env$x.df, random1Env$n)
    random1Env$curr.n = 1
    random1Env$diff   = sim$diff
    random1Env$redraw = sim$tab
  }
  
  updateEnvCurrN = function(){
    if(random1Env$curr.n <= random1Env$n)
      random1Env$curr.n = random1Env$curr.n + 1
    else
      random1Env$curr.n = 1
  }  
  
  initDev = function(){
    if(!is.null(random1Env$x.df)){
      graphics.off()
      x11(width=10, height=8)
      grid.random1(random1Env$x.df,
                   diffFun=get(svalue(diffFun.radio)),
                   main=random1Env$filename,
                   name="Random1Movie")
      disableButtons()
      svalue(status) = "Loading..."
      simuEnv()
      svalue(status) = "Finished"
      grid.newpage()
      grid.random1(random1Env$x.df, x.diff=random1Env$diff,
                   diffFun=get(svalue(diffFun.radio)),
                   main=random1Env$filename,
                   name="Random1Movie")
      random1Env$isShow = TRUE
      enableButtons()
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
  }
  
  enableButtons = function(){
    enabled(read.data.but) = TRUE
    enabled(show.data.but) = TRUE
    enabled(run1.but)      = TRUE
    enabled(run2.but)      = TRUE
    enabled(diffFun.radio) = TRUE
  }
  
  pause = function(){
    while(random1Env$pause){
    }  
  }

  handlerDiffFun = function(h, ...){
    if(!is.null(random1Env$x.df)){
      handlerShowData()
      enableButtons()
    }
    else{ 
      enabled(show.data.but) = FALSE
      enabled(run1.but)      = FALSE
      enabled(pause.but)     = FALSE
      enabled(run2.but)      = FALSE  
      enabled(tail.prop.but) = FALSE
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
      random1Env$filename = main
      
      d = checkGroupRandom1(read.csv(path), diffFun=get(svalue(diffFun.radio)))
      setEnvData(d)
      dialog("Data loaded", "Message")
      random1Env$isShow = FALSE
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
    enabled(pause.but)     = TRUE
    enabled(tail.prop.but) = FALSE
    
    if(!random1Env$isShow)
      initDev()
    
    if(!random1Env$redrawPanel)
      random1Env$redrawPanel = TRUE
      
    if(random1Env$randomPanel){
      random1Env$randomPanel = FALSE
      random1Env$curr.n = 1
      random1UpdateDistShow(FALSE)
    }
 
    svalue(status) = "Running..."
    n.redraw = as.numeric(svalue(redraw.radio))
    sel      = svalue(redraw.radio, index=TRUE)    
    
    if(sel==1){
      svalue(status) = paste("Running...1/1")
      random1MoveDataPtsToRandomVP()
      pause()
      group = random1GetRedrawGroup(random1Env$redraw, random1Env$curr.n)      
      random1MoveRandomApart(group)
      random1UpdateGroup(group)
      updateEnvCurrN()
    } else if(sel==2){
      for(i in 1:n.redraw){
        svalue(status) = paste("Running...", i, "/", n.redraw, sep="")
        group = random1GetRedrawGroup(random1Env$redraw, random1Env$curr.n)
        
        random1MoveRandomApart(group)
        random1UpdateGroup(group)
        updateEnvCurrN()
        pause()
        Sys.sleep(1)
      }
    } else if(sel==3){
      for(i in 1:n.redraw){
        svalue(status) = paste("Running...", i, "/", n.redraw, sep="")
        group = random1GetRedrawGroup(random1Env$redraw, random1Env$curr.n)
        random1UpdateGroup(group)
        updateEnvCurrN()
        Sys.sleep(0.5)
        pause()
      }
    }    
    svalue(status) = "Finished"
    
    enableButtons()
    enabled(pause.but) = FALSE
  }
  
  handlerRandomDiff = function(h, ...){
    disableButtons()
    enabled(pause.but)     = FALSE
    enabled(tail.prop.but) = FALSE
    
    if(!random1Env$isShow)
      initDev()

    if(!random1Env$randomPanel)
      random1Env$randomPanel = TRUE
      
    if(random1Env$redrawPanel){
      random1Env$redrawPanel = FALSE
      random1Env$curr.n = 1
    }
    
    svalue(status) = "Running..."
    n.random = svalue(random.radio)
    sel      = svalue(random.radio, index=TRUE)

      
    if(random1Env$curr.n==1 && sel<=4)
      random1UpdateDistShow(FALSE)
    
    random1InitAddition()
    if(sel==1){
      svalue(status) = paste("Running...1/1")
      random1MoveDataPtsToRandomVP()
      group = random1GetRedrawGroup(random1Env$redraw, random1Env$curr.n)
      random1MoveRandomApart(group)
      random1UpdateGroup(group)
      random1MoveArrowBarFromRandomvp()
      random1UpdateDistShow(1:random1Env$curr.n)
      random1RemoveArrow()
      updateEnvCurrN()
    } else if(sel==2 | sel==3){
      n.random = as.numeric(n.random)
      for(i in 1:n.random){
        svalue(status) = paste("Running...", i, "/", n.random, sep="")
        group = random1GetRedrawGroup(random1Env$redraw, random1Env$curr.n)
        random1UpdateGroup(group)
        random1MoveArrowBarFromRandomvp()
        random1UpdateDistShow(1:random1Env$curr.n)
        updateEnvCurrN()
      }
      random1RemoveArrow()
    } else if(sel==4){
      n.random = as.numeric(n.random)
      for(i in 1:n.random){
        svalue(status) = paste("Running...", i, "/", n.random, sep="")
        group = random1GetRedrawGroup(random1Env$redraw, random1Env$curr.n)
        random1UpdateGroup(group)
        random1UpdateDistShow(1:random1Env$curr.n)
        updateEnvCurrN()
        if(random1Env$curr.n>random1Env$n)
          break
      }
    } else if(sel==5){
      random1InitTableGroup()
      for(i in round(seq(1, random1Env$n, length.out=10))[-1])
        random1UpdateDistShow(1:i,shape=2)
        
      random1MoveArrowBarFromDatavp()
      enabled(tail.prop.but) = TRUE
      random1Env$curr.n = 1
    }
    
    
    svalue(status) = "Finished"
    enabled(tail.prop.but) = TRUE
    enableButtons()
  }
  
  handlerTailProp = function(h, ...){
    x = random1GetCurrentData()
    g = random1GetDataGroup()
    d = diff(tapply(x, g, random1GetDiffFun()))
    random1ShowTailProp(d)
    enabled(tail.prop.but) = FALSE
  }
  
  handlerPause = function(h, ...){
    random1Env$pause = !random1Env$pause
    
    if(random1Env$pause){
      svalue(status) = "Pause"
      svalue(pause.but)  = "Play"
    }
    else{
      svalue(status) = "Running..."
      svalue(pause.but)  = "Pause"
    }     
  }
  
  random1Env = initEnv()
  window = gwindow("Randomisation", width=200, height=200)
  diffFun.ops   = c("median", "mean")
  redraw.times  = c(1, 5, 30)
  random.times  = c("1 (all)", 1, 7, 30, random1Env$n)
  
  diffFun.radio = gradio(diffFun.ops,  horizontal=TRUE, handler=handlerDiffFun)
  redraw.radio  = gradio(redraw.times,  horizontal=FALSE)
  random.radio  = gradio(random.times,  horizontal=FALSE)
  
  parent = ggroup(cont=window)
  group  = ggroup(container=parent, horizontal=FALSE)
  add(group, diffFun.radio)
  read.data.but = gbutton(text="Read Data", container=group,
                          handler=handlerReadData)
  show.data.but = gbutton(text="Show Data", container=group,
                          handler=handlerShowData)
  addSpace(group, 20, horizontal=FALSE)
  
  tmp = gframe("Re-randomisation (Idea)",  container=group)
  add(tmp, redraw.radio)
  run1.but  = gbutton(text="Run", container=group, handler=handlerRedraw)
  pause.but = gbutton(text="Pause", container=group, handler=handlerPause)
  addSpace(group, 40, horizontal=FALSE)
    
  tmp = gframe("Capture Randomisation Difference",  container=group)
  add(tmp, random.radio)
  run2.but = gbutton(text="Run", container=group, handler=handlerRandomDiff)
  addSpace(group, 10, horizontal=FALSE)

  tail.prop.but = gbutton(text="Show Tail Proportion", container=group,
                          handler=handlerTailProp)
  addSpace(group, 10, horizontal=FALSE)
  
  status = glabel("", container=group)    
    
  enabled(show.data.but) = FALSE
  enabled(run1.but)      = FALSE
  enabled(pause.but)     = FALSE
  enabled(run2.but)      = FALSE  
  enabled(tail.prop.but) = FALSE
}