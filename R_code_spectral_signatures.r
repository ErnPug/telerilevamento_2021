# R_code_spectral_signatures.r

library(raster)
library(rgdal)
library(ggplot2)

setwd("C:/lab/")

# Carico il file "defor2"

defor2 <- brick("defor2.jpg")

# defor2.1, defor2.2, defor2.3
# NIR, red, green

plotRGB(defor2, r=1, g=2, b=3, stretch="lin")
plotRGB(defor2, r=1, g=2, b=3, stretch="hist")

# click serve a cliccare sulla mappa ed avere le informazioni di quel punto
click(defor2, id=T, xy=T, cell=T, type="p", pch=16, col="yellow")

# risultati
#    x     y     defor2.1 defor2.2 defor2.3
# 1 87.5 444.5      219       21       36
#    x     y     defor2.1 defor2.2 defor2.3
# 1 457.5 128.5       37       84      126

# creo un dataframe con 3 colonne. Qui creo le tre colonne
band <- c(1,2,3)
forest <- c(219,21,36)
water <- c(37,84,126)
# Creo il dataframe
spectrals <- data.frame(band, forest, water)
# plotto con ggplot
ggplot(spectrals, aes(x=band)) + 
 geom_line(aes(y=forest), color="green") + 
 geom_line(aes(y=water), color="blue") + 
 labs(x="band", y="reflectance")

# Analisi multitemporale
defor1 <- brick("defor1.jpg")
plotRGB(defor1, r=1, g=2, b=3, stretch="lin")

# Spectral signature del defor1 
click(defor1, id=T, xy=T, cell=T, type="p", pch=16, col="yellow")

#     x     y   cell   defor1.1 defor1.2 defor1.3
# 1 45.5 346.5 93580      176       48       49
#     x     y   cell   defor1.1 defor1.2 defor1.3
# 1 73.5 374.5 73616      222       16       37
#     x     y   cell   defor1.1 defor1.2 defor1.3
# 1 69.5 324.5 109312      210       13       30
#     x     y   cell   defor1.1 defor1.2 defor1.3
# 1 81.5 335.5 101470      209       10       29
#     x     y   cell   defor1.1 defor1.2 defor1.3
# 1 92.5 367.5 78633      231       18       40

# Controllo la stessa zona nella seconda immagine
plotRGB(defor2, r=1, g=2, b=3, stretch="lin")
click(defor2, id=T, xy=T, cell=T, type="p", pch=16, col="yellow")

#     x     y   cell   defor2.1 defor2.2 defor2.3
# 1 38.5 354.5 88230      155      138      122
#     x     y   cell   defor2.1 defor2.2 defor2.3
# 1 50.5 369.5 77487      210      161      157
#     x     y   cell   defor2.1 defor2.2 defor2.3
#1 64.5 342.5 96860       199      199      187
#     x     y   cell   defor2.1 defor2.2 defor2.3
# 1 79.5 352.5 89705      211      194      187
#     x     y   cell   defor2.1 defor2.2 defor2.3
# 1 66.5 376.5 72484      193       29       53

# Dataframe con i due intervalli temporali
time1 <- c(176,45,49)
time1p2 <- c(222,16,37)
time2 <- c(155,138,122)
time2p2 <- c(210,161,157)

spectralst <- data.frame(band,time1,time2,time1p2,time2p2)

ggplot(spectralst, aes(x=band)) + 
 geom_line(aes(y=time1), color="green") + 
 geom_line(aes(y=time2), color="blue") + 
 geom_line(aes(y=time1p2), color="green") + 
 geom_line(aes(y=time2p2), color="blue") + 
 labs(x="band", y="reflectance")

# Prova con un'immagine su earth observatory
eo <- brick("june_puzzler.jpg")
plotRGB(eo, r=1, g=2, b=3, stretch="hist")
click(eo, id=T, xy=T, cell=T, type="p", pch=16, col="yellow")

# BLU      x     y     cell     june_puzzler.1 june_puzzler.2 june_puzzler.3
#    1   181.5 379.5   72182            45             38             22
# GIALLO   x     y     cell     june_puzzler.1 june_puzzler.2 june_puzzler.3
#    1   181.5 406.5   52742           215            193              0
# VERDE    x     y     cell     june_puzzler.1 june_puzzler.2 june_puzzler.3
#    1    38.5 178.5   216759          167            125              4

blu <- c(45,38,22)
giallo <- c(215,193,0)
verde <- c(167,125,4)

spectraleo <- data.frame(band,blu,giallo,verde)
ggplot(spectraleo, aes(x=band)) + 
 geom_line(aes(y=blu), color="blue") + 
 geom_line(aes(y=giallo), color="yellow") + 
 geom_line(aes(y=verde), color="green") + 
 labs(x="band", y="reflectance")


