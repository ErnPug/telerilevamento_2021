# R_file_project.r
# NON ANCORA COMPLETATO

library(raster)
library(RStoolbox)
library(ggplot2)
library(gridExtra)
library(rasterVis) # richiamo il pacchetto "rasterVis"
library(knitr) # richiamo il pacchetto "knitr"
library(zoom) # per richiamare il pacchetto zoom, utile a zommare nel momento in cui scegliamo il pixel per la firma 

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
step12021 <- list.files(pattern="20210615")
step12013 <- list.files(pattern="20200912")

# applico ad ogni immagine della lista la funzione "raster"
importstep12021 <- lapply(step12021,raster)
importstep12013 <- lapply(step12013,raster)

# unisco in un unico file
TGrstep12021 <- stack(importstep12021)
TGrstep12013 <- stack(importstep12013)

# Rispetto ai vari layer nell'immagine, la banda 1 è nel terzo layer, la 2 nel quarto e così via.
# Raggruppo le immagini. Uso plotRGB per plottarle.
par(mfrow=c(2,1))
plotRGB(TGrstep12013,r=7,g=6,b=5,stretch="lin")
plotRGB(TGrstep12021,r=7,g=6,b=5,stretch="lin")

# provo il plottaggio con ggplot
p2021 <- ggRGB(TGrstep12021,r=7,g=6,b=5,stretch="LIN")
p2013 <- ggRGB(TGrstep12013,r=7,g=6,b=5,stretch="LIN")
grid.arrange(p2013,p2021,nrow=2)

# Calcolo il DVI delle due immagini
# Metto le bande del rosso e del NIR in due oggetti, dato che non riesco a prenderlo direttamente da TGRstep1
# Anno 2013
NIR2013 <- raster("LC08_L2SP_176040_20130413_20200912_02_T1_SR_B5.TIF")
RED2013 <- raster("LC08_L2SP_176040_20130413_20200912_02_T1_SR_B4.TIF")

DVI2013 <- NIR2013 - RED2013
cl <- colorRampPalette(c('darkblue','yellow','red','black'))(100) # specifico la palette di colori


# Anno 2021
NIR2021 <- raster("LC08_L2SP_176040_20210606_20210615_02_T1_SR_B5.TIF")
RED2021 <- raster("LC08_L2SP_176040_20210606_20210615_02_T1_SR_B4.TIF")

DVI2021 <- (NIR2021 - RED2021)

# Plotto i risultati
par(mfrow=c(2,1))
plot(DVI2013, col=cl)
plot(DVI2021, col=cl)

# Faccio la differenza tra i risultati
DIFDVI <- DVI2013 - DVI2021
cld <- colorRampPalette(c('blue','white','red'))(100)
plot(DIFDVI, col=cld)

# Calcolo adesso NDVI
# 2013
NDVI2013 <- DVI2013 / (RED2013 + NIR2013)
plot(NDVI2013, col=cl)

#2021
NDVI2021 <- DVI2021 / (RED2021 + NIR2021)
plot(NDVI2021, col=cl)

# Faccio la differenza dei due NDVI
DIFFNDVI <- NDVI2013 - NDVI2021
plot(DIFFNDVI, col=cld)

# facciamo una unsupervised classification, classificazione effettuata dal software, per capire quanti pixel hanno un NDVI positivo, quanti negativo.
soc <- unsuperClass(DIFFNDVI, nClas=3)
plot(soc$map)
freq(soc$map)
# Zona con DIFFNDVI positivo 1  1681744
# Zona con DIFFNDVI negativo 3   872752

s1 <- 1681744 + 872752 
perc1 <- 1681744/s1 # 0.6583467
perc2 <- 872752 /s1 # 0.3416533
percent <- c(65.83,34.17)
Change <- c("Positive","Negative")
percentages <- data.frame(Change,percent)
ggplot(percentages,aes(x=Change,y=percent)) + geom_bar(stat="identity",fill="dark green")

# STEP 3 



setwd("C:/lab/Nilo/Step3")

step3 <- list.files(pattern="B")

# applico ad ogni immagine della lista la funzione "raster"
importstep3 <- lapply(step3,raster)

# unisco in un unico file
TGrstep3 <- stack(importstep3)


# Rispetto ai vari layer nell'immagine, la banda 1 è nel terzo layer, la 2 nel quarto e così via.
# Raggruppo le immagini. Uso plotRGB per plottarle.
# Avendo caricato solo i file con all'inetrno B, ogni numero corrispnde alla banda con quel numero. (es 5 = B5)
plotRGB(TGrstep3,r=5,g=4,b=3,stretch="lin")
click(TGrstep3, id=T, xy=T, cell=T, type="p", pch=16, col="yellow")

# NILO BIANCO

#  x       y     cell LC08_L2SP_173049_20210601_20210608_02_T1_QA_PIXEL
#1 442230 1713450 40575447                                             21952
#  LC08_L2SP_173049_20210601_20210608_02_T1_QA_RADSAT
#1                                                  0
#  LC08_L2SP_173049_20210601_20210608_02_T1_SR_B1
#1                                           9004
#  LC08_L2SP_173049_20210601_20210608_02_T1_SR_B2
#1                                           9895
#  LC08_L2SP_173049_20210601_20210608_02_T1_SR_B3
#1                                          11686
#  LC08_L2SP_173049_20210601_20210608_02_T1_SR_B4
#1                                          12106
#  LC08_L2SP_173049_20210601_20210608_02_T1_SR_B5
#1                                           9765
#  LC08_L2SP_173049_20210601_20210608_02_T1_SR_B6
#1                                           9031
#  LC08_L2SP_173049_20210601_20210608_02_T1_SR_B7
#1                                           8708
#  LC08_L2SP_173049_20210601_20210608_02_T1_SR_QA_AEROSOL
#1                                                    224
#  LC08_L2SP_173049_20210601_20210608_02_T1_ST_ATRAN
#1                                              7783
#  LC08_L2SP_173049_20210601_20210608_02_T1_ST_B10
#1                                           45816
#  LC08_L2SP_173049_20210601_20210608_02_T1_ST_CDIST
#1                                              3866
#  LC08_L2SP_173049_20210601_20210608_02_T1_ST_DRAD
#1                                              946
#  LC08_L2SP_173049_20210601_20210608_02_T1_ST_EMIS
#1                                             9904
#  LC08_L2SP_173049_20210601_20210608_02_T1_ST_EMSD
#1                                              120
#  LC08_L2SP_173049_20210601_20210608_02_T1_ST_QA
#1                                            159
#  LC08_L2SP_173049_20210601_20210608_02_T1_ST_TRAD
#1                                             9936
#  LC08_L2SP_173049_20210601_20210608_02_T1_ST_URAD
#1                                             1887

# NILO AZZURRO

#   x       y     cell LC08_L2SP_173049_20210601_20210608_02_T1_QA_PIXEL
#1 457380 1720650 38754112                                             21952
#  LC08_L2SP_173049_20210601_20210608_02_T1_QA_RADSAT
#1                                                  0
#  LC08_L2SP_173049_20210601_20210608_02_T1_SR_B1
#1                                           8312
#  LC08_L2SP_173049_20210601_20210608_02_T1_SR_B2
#1                                           9065
#  LC08_L2SP_173049_20210601_20210608_02_T1_SR_B3
#1                                          10731
#  LC08_L2SP_173049_20210601_20210608_02_T1_SR_B4
#1                                          11049
#  LC08_L2SP_173049_20210601_20210608_02_T1_SR_B5
#1                                          10484
#  LC08_L2SP_173049_20210601_20210608_02_T1_SR_B6
#1                                           9893
#  LC08_L2SP_173049_20210601_20210608_02_T1_SR_B7
#1                                           9277
#  LC08_L2SP_173049_20210601_20210608_02_T1_SR_QA_AEROSOL
#1                                                    224
#  LC08_L2SP_173049_20210601_20210608_02_T1_ST_ATRAN
#1                                              7845
#  LC08_L2SP_173049_20210601_20210608_02_T1_ST_B10
#1                                           48451
#  LC08_L2SP_173049_20210601_20210608_02_T1_ST_CDIST
#1                                              2525
#  LC08_L2SP_173049_20210601_20210608_02_T1_ST_DRAD
#1                                              923
#  LC08_L2SP_173049_20210601_20210608_02_T1_ST_EMIS
#1                                             9624
#  LC08_L2SP_173049_20210601_20210608_02_T1_ST_EMSD
#1                                              113
#  LC08_L2SP_173049_20210601_20210608_02_T1_ST_QA
#1                                            167
#  LC08_L2SP_173049_20210601_20210608_02_T1_ST_TRAD
#1                                            10795
#  LC08_L2SP_173049_20210601_20210608_02_T1_ST_URAD
#1                                             1839


#  LC08_L2SP_173049_20210601_20210608_02_T1_SR_B2
#1                                           9195
#  LC08_L2SP_173049_20210601_20210608_02_T1_SR_B3
#1                                          11003
#  LC08_L2SP_173049_20210601_20210608_02_T1_SR_B4
#1                                          11606
#  LC08_L2SP_173049_20210601_20210608_02_T1_SR_B5
#1                                          10124
#  LC08_L2SP_173049_20210601_20210608_02_T1_SR_B6
#1                                           9293
#  LC08_L2SP_173049_20210601_20210608_02_T1_SR_B7
#1                                           8843
#  LC08_L2SP_173049_20210601_20210608_02_T1_ST_B10
#1                                           46353


# Creo argomenti con le bande 2, 3, 4 e 5 e la firma spettrale
Bande <- c(2,3,4,5)
Nilo <- c(9195,11003,11606,10124)
NiloBianco <- c(9895,11686,12106,9765)
NiloAzzurro <- c(9065,10731,11049,10484)
# Creo il dataframe
spectrals <- data.frame(Bande,Nilo, NiloBianco, NiloAzzurro)


ggplot(spectrals, aes(x=Bande)) + 
 geom_line(aes(y=Nilo), color="black") +
 geom_line(aes(y=NiloBianco), color="green") + 
 geom_line(aes(y=NiloAzzurro), color="blue") + 
 labs(x="band", y="reflectance")

