##############################################################################
## Date     : 2011-06-23
## Author   : Garrett Grolemund
## Type     : function
## Usage    : sampling from a population
##############################################################################

samplingGUI <- function(){

  # constructor for main environment
  initEnv <- function(){
    e             <- new.env()
    e$pause       <- FALSE
    e$isShow      <- FALSE
    e$redrawPanel <- FALSE
    e$bootstrapPanel <- FALSE
    
    e$n           <- 1000
    e$sel         <- 0
    e$curr.n      <- NULL
    e$x.df        <- NULL
    e$filename    <- NULL
    e$p           <- NULL
    e$redraw      <- NULL
    e
  }
  
  # method for setting environment's data source
  setEnvData <- function(x.df){
    bpEnv$x.df   <- x.df
  }
  
  
  # method to generate bootstrap  
  simuEnv <- function(){
    sim           <- bootstrapSimulation(bpEnv$x.df[,1], bpEnv$n)
    bpEnv$curr.n  <- 1
    bpEnv$est     <- sim$est
    bpEnv$redraw  <- sim$tab
  }
  
  # advances environment's curr.n by 1 until it gets to 1000. Then it starts over at 1.
  updateEnvCurrN <- function(){
    if(bpEnv$curr.n < bpEnv$n)
      bpEnv$curr.n <- bpEnv$curr.n + 1
    else{
      bpEnv$curr.n <- 1
    }
  }  
  
  # makes a display with both the original data and a bootstrapped sample?
  initDev = function(){
  	# if bpEnv has data, close all open devices
    if(!is.null(bpEnv$x.df)){
      graphics.off()
      
    # opens window  
      # won't fit the 13" screen
      # should use (11,7)
      x11(width=10, height=8)
      vname = names(bpEnv$x.df)[1] # variable names
      
      # turns the arguments into a bootstrap grob and then draws it
      grid.bootstrap(bpEnv$x.df, vname=vname,
                   diffFun=get(svalue(diffFun.radio)),
                   main=bpEnv$filename,
                   name="bootstrapMovie")
      
      disableButtons() # prevents user manipulations until done
      svalue(status) = "Loading..." # updates value of status
      simuEnv() # adds a new bootstrapped sample to the environment
      svalue(status) = "Finished" # updates value of status
      grid.newpage() # clears the graphics screen
      
      # plots the new bootsrtapped data
      grid.bootstrap(bpEnv$x.df, x.sim=bpEnv$est,vname=vname,
                   diffFun=get(svalue(diffFun.radio)),
                   main=bpEnv$filename,
                   name="bootstrapMovie")
                   
      bpEnv$isShow = TRUE
      enableButtons() # user can resume input
      
      enabled(point.but)=FALSE # point.but no longer greyed out
    } else {
      dialog("No data", "Warning")
    }
    return()
  }
  
  # opens a file choose window and reads the data into the environment
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
      bpEnv$filename = main
      
      d = read.csv(path)
      setEnvData(d)
      
      dialog("Data loaded", "Message")
      bpEnv$isShow = FALSE
      enableButtons()
      graphics.off()
    } else {
      enabled(read.data.but) = TRUE
      enabled(diffFun.radio) = TRUE
    }
  }
  
  # Opens the initial device
  handlerShowData = function(h, ...){
    initDev()
    return()
  }
  
   
  showWarning = function(){
    dialog("Animation still runing...", "Warning")
  }
    
  # greys out all of the buttons and makes them unresponsive
  disableButtons = function(){
    enabled(read.data.but) = FALSE
    enabled(show.data.but) = FALSE
    enabled(run1.but)      = FALSE
    enabled(run2.but)      = FALSE
    enabled(diffFun.radio) = FALSE
    enabled(point.but)     = FALSE
  }
  
  # makes buttons responsive again
  enableButtons = function(){
    enabled(read.data.but) = TRUE
    enabled(show.data.but) = TRUE
    enabled(run1.but)      = TRUE
    enabled(run2.but)      = TRUE
    enabled(diffFun.radio) = TRUE
  }
  
  # creates a pause... how?
  pause = function(){
    while(bpEnv$pause){
    }  
  }  
  
  # if data is loaded, it opens initial plot. If not, it disables buttons
  handlerDiffFun = function(h, ...){
    if(!is.null(bpEnv$x.df)){
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
  
  
  # create the bootstraps when a user clicks among the top four buttons
  handlerRedraw = function(h, ...){
  	
  	# disables everything but pause while it runs
    disableButtons()
    enabled(pause.but)  = TRUE
    enabled(show.ci.but)= FALSE
    
    # if this is the first time, create the initial device
    if(!bpEnv$isShow)
      initDev()
    
    # change redraw panel to true
    if(!bpEnv$redrawPanel)
      bpEnv$redrawPanel = TRUE
      
    # if a bootstrap has been previously drawn, replace it with a ghost box? or erase it?
    if(bpEnv$bootstrapPanel){
      bpEnv$bootstrapPanel = FALSE
      bpEnv$curr.n = 1
      bootstrapRefresh()
    }
 
    svalue(status) = "Running..."
    n.redraw       = svalue(redraw.radio) # see how many redraws the user is asking for
    sel            = svalue(redraw.radio, index=TRUE)   
    
    if(sel != 1)
      n.redraw     = as.numeric(n.redraw)

	# the different behaviors for the different buttons
    if(sel==1){
      svalue(status) = paste("Running...1/1")
      bootstrapRefresh()
      index = bootstrapGetRedrawGroup(bpEnv$redraw, bpEnv$curr.n)
      obj   = bootstrapInitMovingTxt(index)
      for(i in 1:length(index)){
        bootstrapMoveTableFromTablevp(obj,index,i)
        pause()
      }
      bootstrapUpdateBox(index,bpEnv$curr.n)
      updateEnvCurrN()
      pause()
      Sys.sleep(1)
    } else if(sel==2 || sel==3){
      for(i in 1:n.redraw){
        svalue(status) = paste("Running...", i, "/", n.redraw, sep="")
        index = bootstrapGetRedrawGroup(bpEnv$redraw, bpEnv$curr.n)      
        bootstrapUpdateBox(index,bpEnv$curr.n)
        updateEnvCurrN()
        pause()
        Sys.sleep(1)
      }
    } else if(sel==4){
      for(i in 1:n.redraw){
        svalue(status) = paste("Running...", i, "/", n.redraw, sep="")
        index = bootstrapGetRedrawGroup(bpEnv$redraw, bpEnv$curr.n)      
        bootstrapUpdateBox(index,bpEnv$curr.n)
        updateEnvCurrN()
        pause()
        Sys.sleep(0.5)
      }
    }    
    svalue(status) = "Finished"
    pause()
    bootstrapFinalise()
    enableButtons()
    enabled(point.but)  = TRUE
    enabled(pause.but) = FALSE
  }
  
  
  
  # creates the bootstrapped differences when the user clicks among the bottom four buttons  
  handlerBootstrapDiff = function(h, ...){
    disableButtons()
    enabled(pause.but)   = FALSE
    enabled(show.ci.but) = FALSE
    
    if(!bpEnv$isShow)
      initDev()

    if(!bpEnv$bootstrapPanel)
      bpEnv$bootstrapPanel = TRUE
      
    if(bpEnv$redrawPanel){
      bpEnv$redrawPanel = FALSE
      bpEnv$curr.n = 1
      bootstrapRefresh()
    }
    
    svalue(status) = "Running..."
    n.random       = svalue(bootstrap.radio)
    bpEnv$sel = svalue(bootstrap.radio, index=TRUE)   
    sel            = bpEnv$sel
    
    if(bpEnv$curr.n==1 && sel<=4)
      bootstrapUpdateDistShow(FALSE)
    
    bootstrapInitAddition()
    if(sel==1){
      svalue(status) = paste("Running...1/1")
      bootstrapRefresh()
      bootstrapUpdateDistShow(FALSE)
      index = bootstrapGetRedrawGroup(bpEnv$redraw, bpEnv$curr.n)  
      obj   = bootstrapInitMovingTxt(index)
      
      for(i in 1:length(index)){
        bootstrapMoveTableFromTablevp(obj,index,i)
        pause()
      }
      pause()
      bootstrapUpdateBox(index,bpEnv$curr.n)
      bootstrapMoveBarFromBstrapvp()
      bootstrapUpdateDistShow(bpEnv$curr.n)
      pause()
      Sys.sleep(0.05)
      #bootstrapRefresh()
      bootstrapFinalise()
      updateEnvCurrN()
    } else if(sel==2 | sel==3){
      n.random = as.numeric(n.random)
      for(i in 1:n.random){
        svalue(status) = paste("Running...", i, "/", n.random, sep="")
        index = bootstrapGetRedrawGroup(bpEnv$redraw, bpEnv$curr.n)
        bootstrapUpdateBox(index,bpEnv$curr.n)
        bootstrapMoveBarFromBstrapvp()
        bootstrapUpdateDistShow(bpEnv$curr.n)
        pause()
        Sys.sleep(0.05)
        updateEnvCurrN()
      }
      pause()
      bootstrapFinalise()
    } else if(sel==4){
      n.random = as.numeric(n.random)
      for(i in 1:n.random){
        svalue(status) = paste("Running...", i, "/", n.random, sep="")
        index = bootstrapGetRedrawGroup(bpEnv$redraw, bpEnv$curr.n)
        bootstrapUpdateBox(index,bpEnv$curr.n)
        bootstrapUpdateDistShow(bpEnv$curr.n)
        updateEnvCurrN()
        if(bpEnv$curr.n>bpEnv$n)
          break
      }
      pause()
      bootstrapFinalise()
    } else if(sel==5){   
      bootstrapRefresh()
      bootstrapUpdateDistShow(FALSE)
      bootstrapClearTable()
      for(i in round(seq(1, bpEnv$n, length.out=10))[-1])
        bootstrapUpdateDistShow2(1:i,shape=2)    
      bpEnv$curr.n = 1
      enabled(show.ci.but) = TRUE
    }

    svalue(status) = "Finished"
    enableButtons()
    if(sel != 5)
      enabled(point.but)=TRUE
  }
  
  # Creates the confidence interval animation
  handlerShowCI = function(h, ...){
    # bootstrapMoveBarFromDatavp()
    x = bootstrapGetCurrentData()
    FUN = bootstrapGetDiffFun()
    est = FUN(x)
    bootstrapShowCI(est)
    enabled(show.ci.but) = FALSE
  }
  
  handlerPause = function(h, ...){
    bpEnv$pause = !bpEnv$pause
    
    if(bpEnv$pause){
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
   
    bootstrapRefresh()
    
    index = bootstrapGetRedrawGroup(bpEnv$redraw, bpEnv$curr.n-1)
    maxRow = initArgsBootstrap()$maxRow
    len    = ifelse(length(index)<=maxRow, length(index),maxRow)
    index  = index[index %in% 1:len]
    pointer=bootstrapGetPointerInfo(index)
    sleep = 1.5
    for(i in 1:len){  
      idx = which(index==i)
      if(length(idx)<2){
        sleep = sleep*(1-i/length(index))
        sleep = ifelse(sleep<0.5,0.5,sleep)
      }
      pointer = bootstrapShowPointer(pointer,index,i) 

      Sys.sleep(sleep/3)      
      pause()
      Sys.sleep(sleep/3)
      pause()
      Sys.sleep(sleep/3)
      pause()
    }
    Sys.sleep(sleep/2)
    bootstrapRemovePointer()
    enabled(point.but) = TRUE
    enabled(pause.but) = FALSE
    enabled(run1.but)  = TRUE
    enabled(run2.but)  = TRUE  
  }

  # create an environment, name it bpEnv
  bpEnv = initEnv()
  window = gwindow("Sampling", width=100, height=200)
  diffFun.ops   = c("median", "mean")
  redraw.times  = c("1 (all)",1, 5, 20)
  bootstrap.times  = c("1 (all)", 1, 5, 20, bpEnv$n)
  
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
  
  tmp = gframe("Draw Sample(s)",  container=group)
  add(tmp, redraw.radio)
  run1.but  = gbutton(text="Run", container=group, handler=handlerRedraw)
  point.but = gbutton(text="Track",container=group,handler=handlerPointer)
  pause.but = gbutton(text="Pause", container=group, 
                    handler=handlerPause)
  
  addSpace(group, 40, horizontal=FALSE)
    
  tmp = gframe("Capture Sample statistic",  container=group)
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