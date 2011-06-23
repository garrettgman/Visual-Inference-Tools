##############################################################################
## Date     : 2011-04-07
## Author   : Danny Chang
## Type     : function
## Usage    : random3 GUI
##############################################################################

randomGUI3 = function(){
  initEnv = function(){
    e             = new.env()
    e$pause       = FALSE
    e$isShow      = FALSE
    e$redrawPanel = FALSE
    e$randomPanel = FALSE
    
    e$n           = 1000
	  e$sel         = 0
    e$curr.n      = NULL
    e$x.df        = NULL
    e$filename    = NULL
    e$diff        = NULL
    e$redraw      = NULL
    e$Fobs        = NULL
    e
  }
  
  setEnvData = function(x.df){
    random3Env$x.df   = x.df
  }
  
  simuEnv = function(){
    sim               = random3Simulation(random3Env$x.df, random3Env$n)
    random3Env$curr.n = 1
    random3Env$diff   = sim$p
    random3Env$redraw = sim$tab
    
    x = random3Env$x.df[,1]
    g = random3Env$x.df[,2]
    random3Env$Fobs   = random3GetpFUN()(x, g, random3GetDiffFun())
  }
  
  updateEnvCurrN = function(){
    if(random3Env$curr.n <= random3Env$n)
      random3Env$curr.n = random3Env$curr.n + 1
    else
      random3Env$curr.n = 1
  }  
  
  initDev = function(){
    if(!is.null(random3Env$x.df)){
      graphics.off()
      x11(width=10, height=8)
      grid.random3(random3Env$x.df,
                   diffFun=get(svalue(diffFun.radio)), pFUN=fValRandom3,
                   main=random3Env$filename, name="Random3Movie")
      disableButtons()
      svalue(status) = "Loading..."
      simuEnv()
      svalue(status) = "Finished"
      grid.newpage()
      grid.random3(random3Env$x.df, p=random3Env$diff,
                   diffFun=get(svalue(diffFun.radio)),  pFUN=fValRandom3,
                   main=random3Env$filename,
                   name="Random3Movie")
      random3Env$isShow = TRUE
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
    while(random3Env$pause){
    }  
  }

  handlerDiffFun = function(h, ...){
    graphics.off()
    enabled(show.data.but) = FALSE
    enabled(run1.but)      = FALSE
    enabled(pause.but)     = FALSE
    enabled(run2.but)      = FALSE  
    enabled(tail.prop.but) = FALSE
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
      random3Env$filename = main

      setEnvData(read.csv(path))
      dialog("Data loaded", "Message")
      random3Env$isShow = FALSE
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
    
    if(!random3Env$isShow)
      initDev()
    
    if(!random3Env$redrawPanel)
      random3Env$redrawPanel = TRUE
      
    if(random3Env$randomPanel){
      random3Env$randomPanel = FALSE
      random3Env$curr.n = 1
      random3UpdateDistShow(FALSE)
    }
 
    svalue(status) = "Running..."
    n.redraw = as.numeric(svalue(redraw.radio))
    sel      = svalue(redraw.radio, index=TRUE)    
    
    if(sel==1){
      svalue(status) = paste("Running...1/1")
      random3MoveDataPtsToRandomVP()
      pause()
      group = random3GetRedrawGroup(random3Env$redraw, random3Env$curr.n)
      random3MoveRandomApart(group)
      random3UpdateGroup(group)
      updateEnvCurrN()
    } else if(sel==2){
      for(i in 1:n.redraw){
        svalue(status) = paste("Running...", i, "/", n.redraw, sep="")
        group = random3GetRedrawGroup(random3Env$redraw, random3Env$curr.n)
        random3MoveRandomApart(group)
        random3UpdateGroup(group)
        updateEnvCurrN()
        pause()
        Sys.sleep(1)
      }
    } else if(sel==3){
      for(i in 1:n.redraw){
        svalue(status) = paste("Running...", i, "/", n.redraw, sep="")
        group = random3GetRedrawGroup(random3Env$redraw, random3Env$curr.n)
        random3UpdateGroup(group)
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
    
    if(!random3Env$isShow)
      initDev()

    if(!random3Env$randomPanel)
      random3Env$randomPanel = TRUE
      
    if(random3Env$redrawPanel){
      random3Env$redrawPanel = FALSE
      random3Env$curr.n = 1
    }
    
    svalue(status) = "Running..."
    n.random       = svalue(random.radio)
    sel            = svalue(random.radio, index=TRUE)
	 random3Env$sel = sel

      
    if(random3Env$curr.n==1 && sel<=4)
      random3UpdateDistShow(FALSE)
    
    random3InitAddition()
    if(sel==1){
      svalue(status) = paste("Running...1/1")
      random3MoveDataPtsToRandomVP()
      group = random3GetRedrawGroup(random3Env$redraw, random3Env$curr.n)
      random3MoveRandomApart(group)
      random3UpdateGroup(group)
      random3MoveArrowBarFromRandomvp()
      random3MoveFVal(random3Env$diff[random3Env$curr.n])
      random3UpdateDistShow(1:random3Env$curr.n)
      updateEnvCurrN()
    } else if(sel==2 | sel==3){
      n.random = as.numeric(n.random)
      for(i in 1:n.random){
        svalue(status) = paste("Running...", i, "/", n.random, sep="")
        group = random3GetRedrawGroup(random3Env$redraw, random3Env$curr.n)
        random3UpdateGroup(group)
        random3MoveArrowBarFromRandomvp()
        random3MoveFVal(random3Env$diff[random3Env$curr.n])
        random3UpdateDistShow(1:random3Env$curr.n)
        updateEnvCurrN()
      }
    } else if(sel==4){
      n.random = as.numeric(n.random)
      for(i in 1:n.random){
        svalue(status) = paste("Running...", i, "/", n.random, sep="")
        group = random3GetRedrawGroup(random3Env$redraw, random3Env$curr.n)
        random3UpdateGroup(group)
        random3UpdateDistShow(1:random3Env$curr.n)
        updateEnvCurrN()
        if(random3Env$curr.n>random3Env$n)
          break
      }
    } else if(sel==5){
      random3InitTableGroup()
      for(i in round(seq(1, random3Env$n, length.out=10))[-1])
        random3UpdateDistShow(1:i,shape=2)
        
      random3MoveArrowBarFromDatavp()      
      random3MoveFVal(random3Env$Fobs)
      
      enabled(tail.prop.but) = TRUE
      random3Env$curr.n = 1
    }
    
    
    svalue(status) = "Finished"
    enabled(tail.prop.but) = TRUE
    enableButtons()
  }
  
  handlerTailProp = function(h, ...){
	  if (random3Env$sel != 5){
      random3MoveArrowBarFromDatavp()
      random3MoveFVal(random3Env$Fobs)
	  }
    random3ShowTailProp(random3Env$Fobs)
    enabled(tail.prop.but) = FALSE
  }
  
  handlerPause = function(h, ...){
    random3Env$pause = !random3Env$pause
    
    if(random3Env$pause){
      svalue(status) = "Pause"
      svalue(pause.but)  = "Play"
    }
    else{
      svalue(status) = "Running..."
      svalue(pause.but)  = "Pause"
    }     
  }
  
  random3Env = initEnv()
  window = gwindow("Randomisation", width=200, height=200)
  diffFun.ops   = c("median", "mean")
  redraw.times  = c(1, 5, 30)
  random.times  = c("1 (all)", 1, 7, 30, random3Env$n)
  
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