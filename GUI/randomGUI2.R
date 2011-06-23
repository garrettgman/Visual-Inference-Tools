##############################################################################
## Date     : 2011-03-31
## Author   : Danny Chang
## Type     : function
## Usage    : random2 GUI
##############################################################################

randomGUI2 = function(){
  initEnv = function(){
    e             = new.env()
    e$pause       = FALSE
    e$isShow      = FALSE
    e$redrawPanel = FALSE
    e$randomPanel = FALSE
    
    e$n           = 1000 # number of simulation
    e$sel         = 0    # which randomisation button
    e$n.cum       = 0    # cumulative number of simulation times
    e$wrange      = c(0,1)
    e$curr.n      = NULL
    e$x.df        = NULL
    e$filename    = NULL
    e$diff        = NULL
    e$redraw      = NULL
    e
  }
  
  setEnvData = function(x.df){
    random2Env$x.df   = x.df
  }
  
  simuEnv = function(){
    # random2Action.random2Simulation
    # calculate the statistics for the data
    sim               = random2Simulation(random2Env$x.df, random2Env$n)
    random2Env$curr.n = 1
    random2Env$diff   = sim$diff # length of difference arrows
    random2Env$redraw = sim$tab
  }
  
  updateEnvCurrN = function(){
    if(random2Env$curr.n <= random2Env$n)
      random2Env$curr.n = random2Env$curr.n + 1
    else
      random2Env$curr.n = 1
  }
  
  # initialise the main frame
  initDev = function(){
    if(!is.null(random2Env$x.df)){
      graphics.off()
      x11(width=10, height=8)
      
      grid.random2(random2Env$x.df,
                   main=random2Env$filename,
                   name="Random2Movie")
      
      disableButtons()
      svalue(status) = "Loading..."
      simuEnv()
      svalue(status) = "Finished"
      grid.newpage()
      grid.random2(random2Env$x.df, x.diff=random2Env$diff,
                   main=random2Env$filename,
                   name="Random2Movie")
      random2Env$wrange = xatRange(random2Env$redraw)
      random2Env$isShow = TRUE
      enableButtons()
    } else {
      dialog("No data", "Warning")
    }
    return()
  }
    
  disableButtons = function(){
    enabled(read.data.but) = FALSE
    enabled(show.data.but) = FALSE
    enabled(run1.but)      = FALSE
    enabled(run2.but)      = FALSE
  }
  
  enableButtons = function(){
    enabled(read.data.but) = TRUE
    enabled(show.data.but) = TRUE
    enabled(run1.but)      = TRUE
    enabled(run2.but)      = TRUE
  }
  
  pause = function(){
    while(random2Env$pause){
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
      random2Env$filename = main
      
      d = checkGroupRandom2(read.csv(path))
      setEnvData(d)
      dialog("Data loaded", "Message")
      random2Env$isShow = FALSE
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
    enabled(pause.but)     = TRUE
    enabled(tail.prop.but) = FALSE
    
    if(!random2Env$isShow)
      initDev()
    
    if(!random2Env$redrawPanel)
      random2Env$redrawPanel = TRUE
      
    if(random2Env$randomPanel){
      random2Env$randomPanel = FALSE
      random2Env$curr.n = 1
      random2UpdateDistShow(FALSE)
    }
 
    svalue(status) = "Running..."
    n.redraw = as.numeric(svalue(redraw.radio))
    sel      = svalue(redraw.radio, index=TRUE)  
    if(all(random2Env$wrange==c(0,1))){
      random2Env$wrange = xatRange(random2Env$redraw[1:n.redraw])
    }

    if(sel==1){
      svalue(status) = paste("Running...1/1")
      group = random2GetRedrawGroup(random2Env$redraw, random2Env$curr.n)
      random2UpdateGroup(group,random2Env$wrange)
      updateEnvCurrN()
    } else if(sel==2){
      for(i in 1:n.redraw){
        svalue(status) = paste("Running...", i, "/", n.redraw, sep="")
        group = random2GetRedrawGroup(random2Env$redraw, random2Env$curr.n)
        random2UpdateGroup(group,random2Env$wrange)
        updateEnvCurrN()
        pause()
        Sys.sleep(1) ## slow down the speed of redrawing
      }
    } else if(sel==3){
      for(i in 1:n.redraw){
        svalue(status) = paste("Running...", i, "/", n.redraw, sep="")
        group = random2GetRedrawGroup(random2Env$redraw, random2Env$curr.n)
        random2UpdateGroup(group,random2Env$wrange)
        updateEnvCurrN()
        Sys.sleep(0.5)
        #pause()
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
    
    if(!random2Env$isShow)
      initDev()

    if(!random2Env$randomPanel)
      random2Env$randomPanel = TRUE
      
    if(random2Env$redrawPanel){
      random2Env$redrawPanel = FALSE
      random2Env$curr.n = 1
    }
    
    svalue(status)   = "Running..."
    n.random         = svalue(random.radio)
    random2Env$n.cum = random2Env$n.cum + as.numeric(n.random)
    random2Env$sel   = svalue(random.radio, index=TRUE)
    sel              = random2Env$sel 
    if(all(random2Env$wrange==c(0,1))){
      random2Env$wrange = xatRange(random2Env$redraw[1:n.random])
    }
    if(random2Env$curr.n==1 && sel<=4)
      random2UpdateDistShow(FALSE)
    
    random2InitAddition()
    if(sel==1){
      svalue(status) = paste("Running...1/1")
      group = random2GetRedrawGroup(random2Env$redraw, random2Env$curr.n)
      random2UpdateGroup(group,random2Env$wrange)
      random2MoveArrowBarFromRandomvp()
      random2UpdateDistShow(1:random2Env$curr.n)
      updateEnvCurrN()
    } else if(sel==2){
      n.random = as.numeric(n.random)
      for(i in 1:n.random){
        svalue(status) = paste("Running...", i, "/", n.random,sep="")
        group = random2GetRedrawGroup(random2Env$redraw, random2Env$curr.n)
        random2UpdateGroup(group,random2Env$wrange)
        # only move the arrow here
        random2MoveArrowBarFromRandomvp()
        random2UpdateDistShow(1:random2Env$curr.n)
        updateEnvCurrN()
        if(random2Env$curr.n>random2Env$n)
          break        
      }
    } else if(sel==3){
      n.random = as.numeric(n.random)
      for(i in 1:n.random){
        svalue(status) = paste("Running...", i, "/", n.random, sep="")
        group = random2GetRedrawGroup(random2Env$redraw, random2Env$curr.n)

        random2UpdateGroup(group,random2Env$wrange)
        random2UpdateDistShow(1:random2Env$curr.n)
        updateEnvCurrN()
        if(random2Env$curr.n>random2Env$n)
          break
      }
    } else if(sel==4){
      random2Env$n.cum = 0
      random2InitTableGroup()
      ## control the speed of display
      for(i in round(seq(1, random2Env$n, length.out=10))[-1]){
        ## show to control the sets of points to be drawn
        random2UpdateDistShow(1:i,shape=2)
      } 
      random2MoveArrowBarFromDatavp()
      enabled(tail.prop.but) = TRUE
      random2Env$curr.n = 1
    }    
    
    svalue(status) = "Finished"
    enabled(tail.prop.but) = TRUE
    enableButtons()
  }
  
  handlerTailProp = function(h, ...){
    v = random2Env$sel
    #v = random2Env$n.cum
    if(v != 4)
       random2MoveArrowBarFromDatavp()
    
    x = random2GetCurrentData()
    g = random2GetDataGroup()
    d = getPropBoxValue(x, g)$d
    random2ShowTailProp(d)
    enabled(tail.prop.but) = FALSE
  }
  
  handlerPause = function(h, ...){
    random2Env$pause = !random2Env$pause
    
    if(random2Env$pause){
      svalue(status) = "Pause"
      svalue(pause.but)  = "Play"
    }
    else{
      svalue(status) = "Running..."
      svalue(pause.but)  = "Pause"
    }     
  }
  
  ##Notes
  # main
  random2Env = initEnv()
  window = gwindow("Randomisation", width=200, height=200)
  diffFun.ops   = c("median", "mean")
  
  # options of redraw or simulation
  redraw.times  = c(1, 5, 30)
  random.times  = c(1, 7, 30, random2Env$n)
  # two radio group
  redraw.radio  = gradio(redraw.times,  horizontal=FALSE)
  random.radio  = gradio(random.times,  horizontal=FALSE)
  
  parent = ggroup(cont=window)
  group  = ggroup(container=parent, horizontal=FALSE)
  read.data.but = gbutton(text="Read Data", container=group,
                          handler=handlerReadData) # read csv file
  show.data.but = gbutton(text="Show Data", container=group,
                          handler=handlerShowData)
 
  addSpace(group, 20, horizontal=FALSE)
  
  ## radios for randomvp
  tmp = gframe("Re-randomisation (Idea)",  container=group)
  add(tmp, redraw.radio)
  run1.but  = gbutton(text="Run", container=group, handler=handlerRedraw)
  pause.but = gbutton(text="Pause", container=group, handler=handlerPause)
  
  addSpace(group, 40, horizontal=FALSE)
  
  ## radios for distvp
  tmp = gframe("Capture Randomisation Difference",  container=group)
  add(tmp, random.radio)
  run2.but = gbutton(text="Run", container=group, handler=handlerRandomDiff)
  
  addSpace(group, 10, horizontal=FALSE)
  ## show tail button
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