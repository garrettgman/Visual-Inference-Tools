##############################################################################
## Date     : 2011-03-10
## Author   : Danny Chang
## Type     : script
## Usage    : pre-setting of GUI
##############################################################################

# sessionInfo()
# R version 2.12.1 (2010-12-16)
# Platform: i386-pc-mingw32/i386 (32-bit)
# locale:
# [1] LC_COLLATE=English_New Zealand.1252 
# [2] LC_CTYPE=English_New Zealand.1252   
# [3] LC_MONETARY=English_New Zealand.1252
# [4] LC_NUMERIC=C                        
# [5] LC_TIME=English_New Zealand.1252    
# attached base packages:
# [1] grid      stats     graphics  grDeevices utils     datasets 
# [7] methods   base     

# other attached packages:
# [1] XML_3.2-0.2     animation_2.0-2 gWidgets_0.0-43

# strsplit(Sys.getenv('PATH'), ';')
# $PATH
# [1] "C:\\Program Files\\MiKTeX 2.7\\miktex\\bin"      
# [2] "C:\\Windows\\system32"                           
# [3] "C:\\Windows"                                     
# [4] "C:\\Windows\\System32\\Wbem"                     
# [5] "C:\\Windows\\System32\\WindowsPowerShell\\v1.0\\"
# [6] "C:\\Program Files\\SAS\\SharedFiles\\Formats"    
# [7] "C:\\Program Files\\GTK2-Runtime\\bin"


rm(list = ls())
setwd("C:/Users/kcha080/Desktop/mydoc/Chris/")
library(gWidgets)
options(guiToolkit="RGtk2")
library(grid)
source("grid/public/grid.bxp.R")
source("grid/public/grid.rectlines.R")
source("grid/public/grid.circlepts.R")
source("grid/public/grid.boxdot.R")
source("grid/public/grid.boxdotsDiff.R")
source("grid/public/grid.table.R")
source("grid/public/grid.stackpts.R")
source("grid/public/pts.R")
source("grid/public/stackpts.R")
source("grid/public/misc.R")

source("grid/random1/grid.random1.R")
source("grid/random1/random1Action.R")
source("grid/random1/random1Args.R")

source("GUI/public/randomGUI.R")
source("GUI/public/misc.R")