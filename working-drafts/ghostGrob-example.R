# ghost example
setwd("/Users/garrettgrolemund/Documents/git/vit")

# makes and plots a ghostGrob
data <- read.csv("Data/BP.csv")
ghostgb <- NULL
diffFun <- median
gbfmt=initArgsBootstrapGhostBox(data[,1],diffFun)
gbfmt$vp <- NULL
tab <- list(sample(1:nrow(data), nrow(data), replace = TRUE), sample(1:nrow(data), nrow(data), replace = TRUE))
args  = list(data, tab, diffFun, gb="boxdotGrob", gbfmt=gbfmt, name="ghostBox")
grid.newpage()
do.call("grid.ghost", args)
