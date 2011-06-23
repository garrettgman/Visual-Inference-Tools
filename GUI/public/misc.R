##############################################################################
## Date     : 2011-03-10
## Author   : Danny Chang
## Type     : function
## Usage    : GUI misc functions
##############################################################################

dialog = function(message, main, width=200, height=100, handler=NULL){
  window = gwindow(main, width=width, height=height)
  group  = ggroup(container = window)
  gimage("info",  dirname="stock",  size="dialog",  container=group)

  inner.group = ggroup(horizontal=FALSE,  container  =  group)
  glabel(message,  container=inner.group,  expand=TRUE)

  button.group = ggroup(container=inner.group)
  addSpring(button.group)
  gbutton("     OK     ",  handler=function(h,...) dispose(window),
          container=button.group)
  
  return()
}