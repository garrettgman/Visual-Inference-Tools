setwd("/Users/garrettgrolemund/Documents/git/vit")
library(devtools)
load_all("package")
vit()

bp <- read.csv("data/BloodPressure.csv")
canvas$new(x = bp$type, y = NULL) -> c2


# building a vit canvas
require(ggplot2)

c1 <- canvas$new(x = mpg$hwy)
c1$drawCanvas()
writeData(c1)
writeSample(c1)
plotData(c1)
plotSample(c1)
plotStat(c1)

