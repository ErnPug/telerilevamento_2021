# R_file_project.r
# NON ANCORA COMPLETATO

library(raster)
library(RStoolbox)
library(ggplot2)
library(gridExtra)

setwd("C:/lab/Nilo/Step1")

# BANDE 
# BANDA 1 Coastal/Aerosol
# BANDA 2 Blue
# BANDA 3 Green
# BANDA 4 Red 
# BANDA 5 Near Infrared
# BANDA 6 Short Wavelength Infrared
# BANDA 7 Short Wavelength Infrared
# BANDA 8 Panchromatic
# BANDA 9 Cirrus

# STEP 1 Studio dell'oasi più grande del deserto occidentale, al-Fayyum.
# carico le due immagini. Creo una lista raccogliendo tutte le bande di ogni immagine.
step12020 <- list.files(pattern="20201025")
step12013 <- list.files(pattern="20200912")

# applico ad ogni immagine della lista la funzione "raster"
importstep12020 <- lapply(step12020,raster)
importstep12013 <- lapply(step12013,raster)

# unisco in un unico file
TGrstep12020 <- stack(importstep12020)
TGrstep12013 <- stack(importstep12013)

# Rispetto ai vari layer nell'immagine, la banda 1 è nel terzo layer, la 2 nel quarto e così via.
# Raggruppo le immagini. Uso plotRGB per plottarle.
par(mfrow=c(2,1))
plotRGB(TGr2013,r=7,g=6,b=5,stretch="lin")
plotRGB(TGr2020,r=7,g=6,b=5,stretch="lin")

# provo il plottaggio con ggplot
p2020 <- ggRGB(TGr2020,r=7,g=6,b=5,stretch="LIN")
p2013 <- ggRGB(TGr2013,r=7,g=6,b=5,stretch="LIN")
grid.arrange(p2013,p2020,nrow=2)

setwd("C:/lab/Nilo/Step1")

