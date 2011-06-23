rm(list = ls())
 
# change the directory here
# setwd(...)
 
######################################
# grid objects and useful functions
######################################
source("grid/utility.R")

#######################################
# bootstrapping
#######################################
# one sample mean/median
source("grid/onesampBootGrid.R")
source("GUI/onesampBootGUI.R")
#onesampBootGUI()

# two samples difference of mean/median
source("grid/twosampBootGrid.R")
source("GUI/twosampBootGUI.R")
#twosampBootGUI()

# one sample proportion
source("grid/bootstrap2/grid.bootstrap2.R")
source("grid/bootstrap2/bootstrap2Args.R")
source("grid/bootstrap2/bootstrap2Action.R")
#bootstrapGUI2()

# two sample proportion difference
source("grid/twoProbBootGrid.R")
source("GUI/twoProbBootGUI.R")
#twoProbBootGUI()

#######################################
# randomisation
#######################################
# two-sample difference of mean/median
source("GUI/randomGUI.R")
source("grid/random1/grid.random1.R")
source("grid/random1/random1Action.R")
source("grid/random1/random1Args.R")

#randomGUI()

# two-sample proportions
source("GUI/randomGUI2.R")
source("grid/random2/grid.random2.R")
source("grid/random2/random2Action.R")
source("grid/random2/random2Args.R")
#randomGUI2()

# multiple samples proportions
source("GUI/randomGUI3.R")
source("grid/random3/grid.random3.R")
source("grid/random3/random3Action.R")
source("grid/random3/random3Args.R")
#randomGUI3()
# onesampBootGUI()
# randomGUI()
# randomGUI2()
# randomGUI3()