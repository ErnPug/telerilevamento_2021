# R_code_no2.r
library(raster)
library(RStoolbox)
library(ggplot2) # fare i ggplot
library(gridExtra) # plottare insieme i ggplot
library(viridis)

# 1. setto la working directory in EN

setwd("C:/lab/EN/")

# 2. riporto la prima immagine (singola banda)

EN1 <- raster("EN_0001.png")

# 3. plotto la prima immagine con una color ramp palette

color <- colorRampPalette(c("blue","light blue","green","yellow","red"))(100)
plot(EN1, col=color)

# 4. plotto l'ultima immagine (13th) e la plotto con la vecchia color ramp palette

EN13 <- raster("EN_0013.png")
plot(EN13, col=color)

# 5. faccio la differenza tra le due immagini

ENdiff <- EN13 - EN1
plot(ENdiff, col=color)

# 6. plotto tutto insieme

par(mfrow=c(3,1))
plot(EN1, col=color, main="NO2 in Gennaio")
plot(EN13, col=color, main="NO2 in Marzo")
plot(ENdiff, col=color, main="Differenza di NO2 (Marzo - Gennaio)")

# 7. plotto l'ntero set

rlist <- list.files(pattern="EN")
rlist

import <- lapply(rlist, raster)
import

EN <- stack(import)
plot(EN, col=color)

# 8. Tiro fuori le immagini 1 e 13 usando lo stack
par(mfrow=c(2,1))
plot(EN$EN_0001, col=color)
plot(EN$EN_0013, col=color)

# 9. Calcolo la PCA sulle 13 immagini

EN_PCA <- rasterPCA(EN)
summary(EN_PCA$model)
plotRGB(EN_PCA$map,r=1,g=2,b=3,stretch="lin")

# 10. Calcolo la variabilità (deviazione standard) sulla prima componente

PC1sd <- focal(EN_PCA$map$PC1, w=matrix(1/9, nrow=3, ncol=3), fun=sd)
plot(PC1sd, col=color)





