# R_code_land_cover.r

library(raster)
library(RStoolbox)
library(ggplot2)
library(gridExtra)

setwd("C:/lab/")
# carico la prima immagine e la plotto
defor1 <- brick("defor1.jpg")
# NIR=1, RED=2, GREEN=3
plotRGB(defor1,r=1,g=2,b=3,stretch="LIN")
# uso il unovo pacchetto
ggRGB(defor1,r=1,g=2,b=3,stretch="LIN")
# stessa cosa con la seconda immagine
defor2 <- brick("defor2.jpg")
ggRGB(defor2,r=1,g=2,b=3,stretch="LIN")
# metto i plott in un'unica immagine
par(mfrow=c(2,1))
plotRGB(defor1,r=1,g=2,b=3,stretch="LIN")
plotRGB(defor2,r=1,g=2,b=3,stretch="LIN")
# con ggRGB non funziona, ci vuole una funzione particolare. Ci vuole "gridExtra" come pacchetto
# usiamo la funzione grid.arrange
p1 <- ggRGB(defor1,r=1,g=2,b=3,stretch="LIN")
p2 <- ggRGB(defor2,r=1,g=2,b=3,stretch="LIN")
grid.arrange(p1,p2,nrow=2)
