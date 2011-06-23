rm(list = ls())
X11.options(type = "cairo")

# need to change the directory to where you put /Chris
#setwd("/Users/garrettgrolemund/Documents/research/new-zealand/Chris")

library(gWidgets)
options(guiToolkit="RGtk2")
library(grid)
source("grid/public/grid.bxp.R")
source("grid/public/grid.rectlines.R")
source("grid/public/grid.circlepts.R")
source("grid/public/grid.propBox.R")
source("grid/public/grid.propBoxDiff.R")
source("grid/public/grid.boxdot.R")
source("grid/public/grid.boxdotsDiff.R")
source("grid/public/grid.boxdotsDiffMulti.R")
source("grid/public/grid.table.R")
source("grid/public/grid.stackpts.R")
source("grid/public/pts.R")
source("grid/public/stackpts.R")
source("grid/public/misc.R")
source("grid/public/createdata.R")

source("grid/random1/grid.random1.R")
source("grid/random1/random1Action.R")
source("grid/random1/random1Args.R")

source("grid/random2/grid.random2.R")
source("grid/random2/random2Action.R")
source("grid/random2/random2Args.R")

source("grid/random3/grid.random3.R")
source("grid/random3/random3Action.R")
source("grid/random3/random3Args.R")

source("GUI/randomGUI.R")
source("GUI/randomGUI2.R")
source("GUI/randomGUI3.R")

# Run one of tbe 3 lines that follow

randomGUI()
#randomGUI2()
#randomGUI3()
