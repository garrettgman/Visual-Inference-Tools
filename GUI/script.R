rm(list = ls())

# change the directory here
setwd("D:/university/R anime/Chris_03Jun/From Danny")

######################################
# grid objects and useful functions
######################################
source("grid/utility.R")

#######################################
# bootstrapping
#######################################
# one sample mean/median
# bootstrapxxx.R
source("grid/onesampBootGrid.R")
source("GUI/onesampBootGUI.R")
#onesampBootGUI()

# two samples difference of mean/median
# bootstrap1xxx.R
source("grid/twosampBootGrid.R")
source("GUI/twosampBootGUI.R")
#twosampBootGUI()

# one sample proportion
# bootstrap2xxx.R
source("grid/oneProbBootGrid.R")
source("GUI/oneProbBootGUI.R")
#oneProbBootGUI()

# two sample proportion difference
#bootstrap3xxx.R
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
