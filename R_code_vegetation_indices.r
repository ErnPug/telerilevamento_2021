# R_code_vegetation_indices.r

library(raster)
library(RStoolbox)
library(rasterdiv)
setwd("C:/lab/")
# carico le immagini della deforazione
defor1 <- brick("defor1.jpg")
defor2 <- brick("defor2.jpg")
# plotto le due immagini
# b1=NIR, b2=red, b3=green
par(mfrow=c(2,1))
plotRGB(defor1,r=1,g=2,b=3,stretch="lin")
plotRGB(defor2,r=1,g=2,b=3,stretch="lin")
# calcolo la dvi facendo banda dell' infrarosso meno la banda del rosso
dvi1 <- (defor1$defor1.1 - defor1$defor1.2)

#dev.off() # per il par
cl <- colorRampPalette(c('darkblue','yellow','red','black'))(100) # specifico la palette di colori
plot(dvi1, col=cl, main="DVI at time 1")
# faccio la stressa cosa per il secondo periodo
dvi2 <- (defor2$defor2.1 - defor2$defor2.2)
cl <- colorRampPalette(c('darkblue','yellow','red','black'))(100) # specifico la palette di colori
plot(dvi2, col=cl, main="DVI at time 2")

# faccio la differenza tra i due DVI
difdvi <- dvi1 - dvi2
cld <- colorRampPalette(c('blue','white','red'))(100)
plot(difdvi, col=cld)

# ndvi
# (NIR-RED)/(NIR+RED)

ndvi1 <- (defor1$defor1.1 - defor1$defor1.2) / (defor1$defor1.1 + defor1$defor1.2)
plot(ndvi1, col=cl)
# calcoliamo l'ndvi2 per la seconda immagine
ndvi2 <- (defor2$defor2.1 - defor2$defor2.2) / (defor2$defor2.1 + defor2$defor2.2)
plot(ndvi2, col=cl)
# uso la funzione "spectralIndices" del pacchetto RStoolbox
vi1 <- spectralIndices(defor1,green=3,red=2,nir=1)
plot(vi1,col=cl)

vi2 <- spectralIndices(defor2,green=3,red=2,nir=1)
plot(vi2,col=cl)
# faccio la differenza tra NDVI
difndvi <- ndvi1 - ndvi2
plot(difndvi, col=cld)

# worldwide NDVI
plot(copNDVI)
# riclassifichiamo l'immagine precedente, eliminando il contributo dell'acqua. 
copNDVI <- reclassify(copNDVI, cbind(253:255,NA))
plot(copNDVI)






