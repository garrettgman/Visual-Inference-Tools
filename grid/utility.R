# loading library
library(gWidgets)
options(guiToolkit="RGtk2")
library(grid)

# basic elements
source("grid/public/grid.bxp.R")
source("grid/public/pts.R")
source("grid/public/stackpts.R")
source("grid/public/grid.stackpts.R")
source("grid/public/grid.circlepts.R")
source("grid/public/grid.rectlines.R")
source("grid/public/grid.table.R")

# drawing group
source("grid/public/grid.propBox.R")
source("grid/public/grid.propBoxDiff.R")
source("grid/public/grid.boxdot.R")
source("grid/public/grid.boxdotsDiff.R")
source("grid/public/grid.boxdotsDiffMulti.R")

# special
source("grid/public/grid.ghostBox.R")
source("grid/public/grid.ghost.R")
source("grid/public/grid.pointer.R")
source("grid/public/grid.meanBar.R")
source("grid/public/grid.ci.R")

# useful functions
source("grid/public/misc.R")
source("grid/public/createdata.R")