# building a vit canvas
library(ggplot2)

vit <- makeCanvas(mpg$hwy)

drawCanvas(vit) #displays the initial version of the canvas
drawBackground(vit)
plotData(vit)
boxplotData(vit)
listData(vit)