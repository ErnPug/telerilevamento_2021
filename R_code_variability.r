# R_code_variability.r
library(raster)
library(RStoolbox)
library(ggplot2) # fare i ggplot
library(gridExtra) # plottare insieme i ggplot
library(viridis) # colorare in automatico i plot

setwd("C:/lab/")
 
# carico tutte le bande dell'immagine
sent <- brick("sentinel.png")
# plotto in RGB
# NIR 1, RED=2, GREEN=3
# r=1, g=2, b=3
plotRGB(sent, stretch="lin")
 
nir <- sent$sentinel.1
red <- sent$sentinel.2
 
ndvi <- (nir-red) / (nir+red)
plot(ndvi)
cl <- colorRampPalette(c('black','white','red','blue','green'))(100)
plot(ndvi,col=cl)
# calcolo la deviazione standard usando una moving window 3X3
ndvisd3 <- focal(ndvi,w=matrix(1/9,nrow=3,ncol=3),fun=sd)
plot(ndvisd3)
clsd <- colorRampPalette(c('blue','green','pink','magenta','orange','brown','red','yellow'))(100)
plot(ndvisd3, col=clsd)
# faccio la stessa cosa con la media 
ndvimean3 <- focal(ndvi,w=matrix(1/9,nrow=3,ncol=3),fun=mean)
plot(ndvimean3, col=clsd)
# aumento il numero della griglia 13X13
ndvisd13 <- focal(ndvi,w=matrix(1/169,nrow=13,ncol=13),fun=sd)
plot(ndvisd13, col=clsd)
# l'ideale sarebbe avere 5X5
ndvisd5 <- focal(ndvi,w=matrix(1/25,nrow=5,ncol=5),fun=sd)
plot(ndvisd5, col=clsd)
# adesso calcolo la PCA e poi usero la moving qindow per il calcolo della deviazione standard
# PCA
sentpca <- rasterPCA(sent)
summary(sentpca$model)
# la prima PC contiene il 67.36804 dell'informazione originale
pc1 <- sentpca$map$PC1 
pc1sd5 <- focal(pc1,w=matrix(1/25,nrow=5,ncol=5),fun=sd)
plot(pc1sd5,col=clsd)
 # carichiamo uno script usando "source"
source("source_test_lezione.r")
source("source_ggplot.r")
# vedo lo script "source_ggplot.r"

p1 <- ggplot() + # carico una finestra vuota e col + aggiungo blocchi
geom_raster(pc1sd5,mappin=aes(x = x, y = y, fill = layer)) + # attenzione spazi x y fill 
scale_fill_viridis() + # colorRampPalette di default di viridis
ggtitle("Standard deviation of PC1 by viridis colour scale") # scelgo il titolo

# provo un'altra legenda
p2 <- ggplot() + # carico una finestra vuota e col + aggiungo blocchi
geom_raster(pc1sd5,mappin=aes(x = x, y = y, fill = layer)) + # attenzione spazi x y fill 
scale_fill_viridis(option="magma") + # colorRampPalette di viridis
ggtitle("Standard deviation of PC1 by magma colour scale") # scelgo il titolo

# provo un'altra legenda
p3 <- ggplot() + # carico una finestra vuota e col + aggiungo blocchi
geom_raster(pc1sd5,mappin=aes(x = x, y = y, fill = layer)) + # attenzione spazi x y fill 
scale_fill_viridis(option="turbo") + # colorRampPalette di viridis
ggtitle("Standard deviation of PC1 by turbo colour scale") # scelgo il titolo
# plotto i tre plot in un'unica immagine 
grid.arrange(p1,p2,p3,nrow=1)

