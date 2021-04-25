# Analisi multivariata
library(raster)
setwd("C:/lab/")
# carico l'immagine
p224r63_2011 <- brick("p224r63_2011_masked.grd")
plot(p224r63_2011) # plotto l'immagine
# metto su un grafico x-y due bande, la banda 1 e la banda 2
plot(p224r63_2011$B1_sre,p224r63_2011$B2_sre,col="red",pch=19,cex=2)
# pairs fa tutte le correlazioni che si possono fare
pairs(p224r63_2011)
