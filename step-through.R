setwd("/Users/garrettgrolemund/Documents/git/vit")
library(devtools)
load_all("package")

# building a vit canvas
require(ggplot2)

vit()
c1 <- canvas$new(mpg$hwy)
c1$drawCanvas()
writeData(c1)
writeSample(c1)
plotData(c1)
plotSample(c1)
plotStat(c1)

