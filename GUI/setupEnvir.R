##############################################################################
## Date     : 2011-02-21
## Author   : Danny Chang
## Type     : script
## Usage    : Install script for GTK+ and gWidgets
##            only for Windows
##            just source this file into an R session
## Ref      : http://wiener.math.csi.cuny.edu/pmg/installPMG.R
##############################################################################

installGTK = function(){
  gtk.url = "http://downloads.sourceforge.net/gtk-win/gtk2-runtime-2.22.0-2010-10-21-ash.exe?download"
  destfile = file.path(tempdir(), gsub("[?].*", "", basename(gtk.url)))
  if(download.file(gtk.url, destfile, mode="wb")>0)
    stop("Failed to download gtk2 runtime installer")
  shell(destfile)
}

loadPkgs = function(){
  if (!require(gWidgets))  install.packages("gWidgets", dep=TRUE)
 
  # not used yet
  # if (!require(animation)) install.packages('animation')
  # if (!require(XML)) install.packages('XML')
  
  library(gWidgets)
  options(guiToolkit="RGtk2")
  library(grid)
}

checkPath = function(){
  print(strsplit(Sys.getenv('PATH'), ';'))
}

sourceFiles = function(){
  ## not used yet
  ## source(".....xxx.R")
}

installGTK()
q(save="no")