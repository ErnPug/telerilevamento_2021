# Analisi multivariata
library(raster)
library(RStoolbox)
setwd("C:/lab/")
# carico l'immagine
p224r63_2011 <- brick("p224r63_2011_masked.grd")
plot(p224r63_2011) # plotto l'immagine
# metto su un grafico x-y due bande, la banda 1 e la banda 2
plot(p224r63_2011$B1_sre,p224r63_2011$B2_sre,col="red",pch=19,cex=2)
# pairs fa tutte le correlazioni che si possono fare
pairs(p224r63_2011)
# ricampioniamo usando meno pixel.
p224r63_2011res <- aggregate(p224r63_2011,fact=10)
# vedo le due immagini, originaria e ricampionata
par(mfrow=c(2,1))
plotRGB(p224r63_2011, r=4,g=3,b=2,stretch="lin")
plotRGB(p224r63_2011res, r=4,g=3,b=2,stretch="lin")
# faccio l'analisi PCA
p224r63_2011res_pca <- rasterPCA(p224r63_2011res)
# controllo il risultato del rasterPCA, il modello in questo caso. Quanta varianza spiega ogni banda.
summary(p224r63_2011res_pca$model)
# plotto la map del risultato di rasterPCA
dev.off() # così il par finisce
plotRGB(p224r63_2011res_pca$map,r=1,g=2,b=3,stretch="lin")



