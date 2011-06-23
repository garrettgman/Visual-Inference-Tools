rm(list = ls())
#setwd("C:/Users/kcha080/Desktop/mydoc/Chris/")
setwd("Z:/From Danny")
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
source("grid/public/grid.table.R")
source("grid/public/grid.stackpts.R")
source("grid/public/pts.R")
source("grid/public/stackpts.R")
source("grid/public/misc.R")

source("grid/random2/grid.random2.R")
source("grid/random2/random2Action.R")
source("grid/random2/random2Args.R")

source("Vivian/createdata.R")

# makeData = function(n=80){
  # # Make up a dataset with 2 groups
  # type       = sample(c("Control", "Drug"), n, replace=TRUE, prob=c(0.7, 0.3))
  # disease    = sample(c("A", "B"), n, replace=TRUE)  
  # example.df = data.frame(disease=disease, type=type)
  # example.df
# }
# write.csv(makeData(n=80), "GUI/data/Disease1.csv", row.names=FALSE)

# mydf = read.csv("GUI/data/Disease1.csv")
# grid.random2(mydf, name="random2"
# grid.edit("randomPropBox", show.box=TRUE)
# random2UpdateGroup(makeData()[,2])
# random2UpdateDistData(rnorm(100, sd=0.1))
# random2UpdateDistShow()
# random2ShowTailProp(-0.05)

