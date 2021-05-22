# R_code_variability.r
 library(raster)
 library(RStoolbox)
 
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
 
