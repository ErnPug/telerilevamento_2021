# R code complete.r

#----------------------------------------------------------------------------------------------------
# Sommario

# 1. Remote sensing
# 2. Time series
# 3. R code Copernicus 
# 4. Codice knitr
# 5. Analisi multivariata
# 6. Classificazione dell'immagine
# 7. Uso di ggplot2
# 8. Indici di vegetazione
# 9. Copertura del suolo
# 10. Variabilità
# 11. Spectral signature
# 12. NO2
# 13. PROGETTO: Viaggio lungo le sponde del Nilo

#----------------------------------------------------------------------------------------------------

# 1. Remote sensing
# My first code in R for remote sensing!!!

# install.packages("raster")
library(raster) # richiamo il pacchetto installato 
setwd("C:/lab/") # imposto come working directory la cartella "lab" in C

p224r63_2011 <- brick("p224r63_2011_masked.grd") # birck importa tutte le bande dell'immagine
p224r63_2011 # scrivendo il nome del file, possiamo vedere le sue informazioni

plot(p224r63_2011) # Plotto l'immagine, ogni banda verrà plottatta in un'immagine a sé

# Day 2
# cambio i colori dei plot
color <- colorRampPalette(c("black","grey","light grey"))(100) 

plot(p224r63_2011,col=color) # plotto l'immagine con la prima palette di colori

color <- colorRampPalette(c("blue","light blue","green","yellow","red"))(100) # cambio i colori

plot(p224r63_2011,col=color) # plotto l'immagine con la seconda palette di colori

# Day 3
# Bande Landsat
# B1: blu
# B2: verde
# B3: rosso
# B4: infrarosso vicino
# B5: infrarosso medio
# B6: infrarosso termico
# B7: infrarosso medio

# Cancello i plot precedente
dev.off()

# plotto solo la banda 1. "$" lega la banda all'immagine
plot(p224r63_2011$B1_sre)
# cambio la color palette della banda 1
dev.off()
color <- colorRampPalette(c("blue","light blue","green","yellow","red"))(100)
plot(p224r63_2011$B1_sre,col=color)
# uso la funzione "par" per poter devidere come plottare. Plotto la banda del blu vicino alla banda del verde
par(mfrow=c(1,2))
plot(p224r63_2011$B1_sre)
plot(p224r63_2011$B2_sre)

# plotto le prime 4 bande cambiando per ognuno la palette di colori
par(mfrow=c(4,1))
plot(p224r63_2011$B1_sre)
plot(p224r63_2011$B2_sre)
plot(p224r63_2011$B3_sre)
plot(p224r63_2011$B4_sre)
# lo rifaccio in un par 2X2 cambiando per ogni plot la palette di colori
par(mfrow=c(2,2))
clb <- colorRampPalette(c("dark blue","blue","light blue"))(100)
clg <- colorRampPalette(c("dark green","green","light green"))(100)
clr <- colorRampPalette(c("dark red","red","pink"))(100)
clnir <- colorRampPalette(c("red","orange","yellow"))(100)
plot(p224r63_2011$B1_sre,col=clb)
plot(p224r63_2011$B2_sre,col=clg)
plot(p224r63_2011$B3_sre,col=clr)
plot(p224r63_2011$B4_sre,col=clnir)

# Day 5
# Utilizzo di RGB plotting per la visualizzazione dei dati
# Bande Landsat
# B1: blu
# B2: verde
# B3: rosso
# B4: infrarosso vicino
# B5: infrarosso medio
# B6: infrarosso termico
# B7: infrarosso medio
# Ad ogni banda di landsat associamo una compenente dell'RGB. B=>1, G=>2, R=>3. 
plotRGB(p224r63_2011,r=3,g=2,b=1,stretch="Lin") #uso "stretch" per plottare tutti i valori di riflettanza incrementando il contrasto dell'immagine.
# Modifico le bande usare per il plot RGB
plotRGB(p224r63_2011,r=4,g=3,b=2,stretch="Lin")
plotRGB(p224r63_2011,r=3,g=4,b=2,stretch="Lin") # la banda 4 è l'infrarosso vicino, con riflettanza alta nelle piante.
plotRGB(p224r63_2011,r=3,g=2,b=4,stretch="Lin")

# Inseriamo in un multiframe i diversi plotRGB
pdf("primo_pdf_con_R.pdf") # Salviamo in pdf il grafico e salvarlo nella cartella "lab"
par(mfrow=c(2,2))
plotRGB(p224r63_2011,r=3,g=2,b=1,stretch="Lin")
plotRGB(p224r63_2011,r=4,g=3,b=2,stretch="Lin")
plotRGB(p224r63_2011,r=3,g=4,b=2,stretch="Lin")
plotRGB(p224r63_2011,r=3,g=2,b=4,stretch="Lin")
dev.off()
# cambiamo tipo di stretch, "hist"
plotRGB(p224r63_2011,r=3,g=4,b=2,stretch="hist")
# usando la funzione par, inseriamo tre polot in un unico plot
par(mfrow=c(3,1))
plotRGB(p224r63_2011,r=3,g=2,b=1,stretch="Lin") # primo caso
plotRGB(p224r63_2011,r=3,g=4,b=2,stretch="Lin") # infrarosso nel Green
plotRGB(p224r63_2011,r=3,g=4,b=2,stretch="hist") # stretch diverso

# Day 6
# uso un nuovo file, un'immagine dello stesso luogo, ma del 1988
p224r63_1988 <- brick("p224r63_1988_masked.grd")
p224r63_1988

# Bande Landsat
# B1: blu
# B2: verde
# B3: rosso
# B4: infrarosso vicino
# B5: infrarosso medio
# B6: infrarosso termico
# B7: infrarosso medio

plot(p224r63_1988) # plotto tutte le bande
plotRGB(p224r63_1988,r=3,g=2,b=1,stretch="Lin")
plotRGB(p224r63_1988,r=4,g=3,b=2,stretch="Lin") # controllo la vegetazione usando l'infrarosso

# con un par inserisco entrambi i periosi in un'unica immagine 
par(mfrow=c(2,1))
plotRGB(p224r63_1988,r=4,g=3,b=2,stretch="Lin")
plotRGB(p224r63_2011,r=4,g=3,b=2,stretch="Lin")

# insetiamo in un par 2X2 anche le immmagini con lo stretch="hist"
pdf("Multitemp.pdf") # salvo in pdf
par(mfrow=c(2,2))
plotRGB(p224r63_1988,r=4,g=3,b=2,stretch="Lin")
plotRGB(p224r63_2011,r=4,g=3,b=2,stretch="Lin")
plotRGB(p224r63_1988,r=4,g=3,b=2,stretch="hist")
plotRGB(p224r63_2011,r=4,g=3,b=2,stretch="hist")
dev.off()

#----------------------------------------------------------------------------------------------------

# 2. Time series
# Time series analysis
# Grerenland increase of temperature
# Data and code from Emanuela Cosma

# install.packages("raster")
library(raster) # richiamo il pacchetto "raster"
library(rasterVis) # richiamo il pacchetto "rasterVis"
library(knitr) # richiamo il pacchetto "knitr"
setwd("C:/lab/Groenlandia") # imposto come working directory la cartella "Groenlandia" in "lab" ed in C

# carico un unico file usando la function "raster"
lst_2000 <- raster("lst_2000.tif")
plot(lst_2000) # plotto l'immagine
lst_2005 <- raster("lst_2005.tif")
plot(lst_2005)
lst_2010 <- raster("lst_2010.tif")
plot(lst_2010)
lst_2015 <- raster("lst_2015.tif")
plot(lst_2015)
# raggruppo tutto in un'unica immagine 2X2 
par(mfrow=c(2,2))
plot(lst_2000)
plot(lst_2005)
plot(lst_2010)
plot(lst_2015)

# invece di caricare un'immagine alla volta, carico direttamente tutte le immagini.
# creo una lista con la funzione "list.files"
rlist <- list.files(pattern="lst") # prendo tutti i file con "lst" nel nome
import <- lapply(rlist,raster) # applico la funzione "raster" a tutti gli elementi della lista "rlist". 
# impacchetto tutto in un unico file
TGr <- stack(import)
plot(TGr)
# utilizzando plotRGB ed usando 3 layers diversi, quindi per diversi anni, posso notare in che anno lst è maggiore nei diversi punti, a seconda del colore che uscirà fuori. 
# Rosso=più alto nel 2000, verde=più alto nel 2005, blu=più alto nel 2010
plotRGB(TGr,1,2,3,stretch="Lin")
# usiamo adesso anche il 2015, elimenando il 2000
plotRGB(TGr,2,3,4,stretch="Lin")


# uso la funzione "levelplot", che usa un'unica leggenda per tutte le immagini
levelplot(TGr)
# uso levelpot per un singolo strato
levelplot(TGr$lst_2000)

# cambiamo la palette di colori
cl <- colorRampPalette(c("blue","light blue","pink","red"))(100)
# la sezione per vambiare la palette in "levelplot" è "col.regions"
levelplot(TGr,col.regions=cl)
# nomino i vari layer dell'immagine 
levelplot(TGr,col.regions=cl, names.attr=c("July 2000","July 2005", "July 2010", "July 2015"))
# metto anche il nome
levelplot(TGr,col.regions=cl, main="LST variation in time", 
          names.attr=c("July 2000","July 2005", "July 2010", "July 2015"))

# creiamo una lista con i file melt scaricati nella cartella "Groenlandia"
meltlist <- list.files(pattern="melt")
# applichiamo a tutti i file la function "raster"
import2 <- lapply(meltlist,raster)
# impacchetto tutto
melt <- stack(import2)
# plotto i miei dati con "levelplot"
levelplot(melt)

# per vedere la differenza tra i due periodi, facciamo una sottrazione tra i due layer
melt_amount <- melt$X2007annual_melt - melt$X1979annual_melt # usiamo $ perché sono layers di un file
# cambio la palette di colori
clb <- colorRampPalette(c("blue","white","red"))(100)
plot(melt_amount,col=clb)
levelplot(melt_amount,col.regions=clb)

#-----------------------------------------------------------------------------------------------------

# 3. R code Copernicus 
# Visualizzazione dei dati Copernicus

library(raster) # richiamo il pacchetto "raster"
# install.packages("ncdf4")
library(ncdf4) # richiamo il pacchetto "ncdf4"
setwd("C:/lab/") # imposto la working directory

albedo <- raster("c_gls_ALBH_202006130000_GLOBE_PROBAV_V1.5.1.nc") # importo il dato

cl <- colorRampPalette(c("light blue","red","green","yellow"))(100)
plot(albedo,col=cl)

albedores <- aggregate(albedo,fact=50) # diminuisco il numero di pixel, li accorpo. Ricampionamento
plot(albedores,col=cl)

#----------------------------------------------------------------------------------------------------

# 4. Codice knitr
# creiamo un report utilizzando un codice già scritto

setwd("C:/lab/")
library(knitr)
library(tinytex)
# tinytex::install_tinytex()
# serve per poter usare la funzione    
tinytex::tlmgr_update()
# Richiamo un codice per poter creare il template
stitch("R_code_Greenland.txt", template=system.file("misc", "knitr-template.Rnw", package="knitr"))

#----------------------------------------------------------------------------------------------------

# 5. Analisi multivariata
# Analisi multivariata
library(raster)
library(RStoolbox)
setwd("C:/lab/")
# carico l'immagine
p224r63_2011 <- brick("p224r63_2011_masked.grd")
plot(p224r63_2011) # plotto l'immagine
# plotto su un grafico x-y due bande, la banda 1 e la banda 2
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

#----------------------------------------------------------------------------------------------------

# 6. Classificazione dell'immagine
# R_code_classification.r
library(raster)
library(RStoolbox)
setwd("C:/lab/") # sistemo la working directiry

so <- brick("Solar_Orbiter_s_first_views_of_the_Sun_pillars.jpg")
so
# visualizzo l'immagine
plotRGB(so,1,2,3,stretch="lin")

# unsupervised classification, classificazione effettuata dal software
soc <- unsuperClass(so, nClas=3)

# la funzione ha creato in output diversi elementi, tra cui la mappa, che dovremmo plottare.

plot(soc$map)

# prova con 20 classi
soc20 <- unsuperClass(so, nClass=20)
plot(soc20$map)
# carico una nuova immagine
sun <- brick("Sun.png")
# classifico la nuova immagine
sunc <- unsuperClass(sun, nClass=3)
plot(sunc$map)

# utilizzo adesso immagini del Gran Canyon

GC <- brick("dolansprings_oli_2013088_canyon_lrg.jpg")
plotRGB(GC,1,2,3,stretch="lin")
plotRGB(GC,1,2,3,stretch="hist")
GCC2 <- unsuperClass(GC, nClass=2)
plot(GCC2$map)
# provo con 4 classi
GCC4 <- unsuperClass(GC, nClass=4)
plot(GCC4$map)

#----------------------------------------------------------------------------------------------------

# 7. Uso di ggplot2

library(raster)
library(RStoolbox)
library(ggplot2)
library(gridExtra)
setwd("C:/lab/")
# carico l'immagine
p224r63 <- brick("p224r63_2011_masked.grd")
# plotto le immagini con ggRGB
ggRGB(p224r63,3,2,1, stretch="lin")
ggRGB(p224r63,4,3,2, stretch="lin")
# per poter inserire entrambe le immagini in una sola finestra, prima le lego ad un file, poi uso "grid.arrange"
p1 <- ggRGB(p224r63,3,2,1, stretch="lin")
p2 <- ggRGB(p224r63,4,3,2, stretch="lin")
grid.arrange(p1, p2, nrow = 2) # this needs gridExtra

#----------------------------------------------------------------------------------------------------

# 8. Indici di vegetazione 

# R_code_vegetation_indices.r

library(raster)
library(RStoolbox)
library(rasterdiv)
library(rasterVis)
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
# riclassifichiamo l'immagine precedente, eliminando il contributo dell'acqua. (cbind cambia dei valori) 
copNDVI <- reclassify(copNDVI, cbind(253:255,NA))
plot(copNDVI)
# funzione che ha bisogno del pacchetto rasterVis
levelplot(copNDVI)

#----------------------------------------------------------------------------------------------------

# 9. Copertura del suolo

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

# unsupervised classification, unsupervised perché è fatta dal sistema 
# una classe per la zona agricola, una classe per la foresta
d1c <- unsuperClass(defor1,nClasses=2)
d2c <- unsuperClass(defor2,nClasses=2)
plot(d1c$map)
plot(d2c$map)
# uso 3 classi per la seconda immagine
d2c3 <- unsuperClass(defor2,nClasses=3)
plot(d2c3$map)
# controllo la frequenza delle due classi nella prima immagine 
freq(d1c$map)
#[1,]     1  35026
#[2,]     2 306266
s1 <- 306266 + 35026
# faccio la proporzione, sempre per la prima immagine
prop1 <- freq(d1c$map)/s1
# prop foresta 0.8973723
# prop agricoltura 0.1026277
# faccio la stessa cosa con la seconda immagine
s2 <- 342726 # numero preso dalle proprietà
prop2 <- freq(d2c$map)/s2
# prop agricoltura 0.4805326
# prop foresta 0.5194674
# costruisco un dataset
cover <- c("Forest","Agriculture")
percent_1992 <- c(89.74,10.26)
percent_2006 <- c(51.95,48.05)
# uso la funzione data.frame per creare il dataset 
percentages <- data.frame(cover,percent_1992,percent_2006)
percentages # così vedo il dataset
# li plottiamo con ggplot
ggplot(percentages,aes(x=cover,y=percent_1992,color=cover)) + geom_bar(stat="identity",fill="red")
ggplot(percentages,aes(x=cover,y=percent_2006,color=cover)) + geom_bar(stat="identity",fill="orange")
# li metto in un'unica immagine
plot1 <- ggplot(percentages,aes(x=cover,y=percent_1992,color=cover)) + geom_bar(stat="identity",fill="red")
plot2 <- ggplot(percentages,aes(x=cover,y=percent_2006,color=cover)) + geom_bar(stat="identity",fill="orange")
grid.arrange(plot1,plot2,nrow=1)

#----------------------------------------------------------------------------------------------------

# 10. Variabilità
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
# estraggo due bande, la NIR e la RED dal file "sent"
nir <- sent$sentinel.1
red <- sent$sentinel.2
# Calcolo l'NDVI e la plotto
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
# l'ideale sarebbe avere 5X5, qundi faccio una griglia 5X5
ndvisd5 <- focal(ndvi,w=matrix(1/25,nrow=5,ncol=5),fun=sd)
plot(ndvisd5, col=clsd)
# adesso calcolo la PCA e poi usero la moving qindow per il calcolo della deviazione standard
# PCA
sentpca <- rasterPCA(sent)
summary(sentpca$model)
# la prima PC contiene il 67.36804 dell'informazione originale
# Raccolgo il primo componente e faccio la deviazione standard con una moving window 5X5
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

#----------------------------------------------------------------------------------------------------

# 11. Spectral signature

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

#----------------------------------------------------------------------------------------------------
# 12. NO2
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

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# 13. PROGETTO: Viaggio lungo le sponde del Nilo
# R_file_project.r
# COMPLETO

library(raster)
library(RStoolbox)
library(ggplot2)
library(gridExtra)
library(rasterVis) # richiamo il pacchetto "rasterVis"
library(zoom) # per richiamare il pacchetto zoom, utile a zommare nel momento in cui scegliamo il pixel per la firma 

setwd("C:/lab/Nilo/Step1")

# BANDE Fonte Wikipedia
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
 # immagine
par(mfrow=c(2,1))
plotRGB(TGrstep12013,r=7,g=6,b=5,stretch="lin")
plotRGB(TGrstep12021,r=7,g=6,b=5,stretch="lin")
dev.off()
#jpeg("Desktop/Univesità Bologna/Telerilevamento/immagini/grafico_oasi.jpg", 1600, 800)
# provo il plottaggio con ggplot
jpeg("grafico_oasi.jpg", 600, 800)
p2021 <- ggRGB(TGrstep12021,r=7,g=6,b=5,stretch="LIN")
p2013 <- ggRGB(TGrstep12013,r=7,g=6,b=5,stretch="LIN")
grid.arrange(p2013,p2021,nrow=2)
dev.off()

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

# Faccio la differenza tra i due anni
DIFDVI <- DVI2013 - DVI2021
cld <- colorRampPalette(c('blue','white','red'))(100)
plot(DIFDVI, col=cld)

# Calcolo adesso NDVI
# 2013
NDVI2013 <- DVI2013 / (RED2013 + NIR2013)

#2021
NDVI2021 <- DVI2021 / (RED2021 + NIR2021)
#salvo l'immagine
jpeg("grafico_NDVI.jpg", 1600, 800)
par(mfrow=c(1,2))
plot(NDVI2013, col=cl, main="NDVI 2013")
plot(NDVI2021, col=cl, main="NDVI 2021")
dev.off()

# Faccio la differenza dei due NDVI nei due periodi
DIFFNDVI <- NDVI2013 - NDVI2021

jpeg("grafico_DIFFNDVI.jpg", 800, 800)
plot(DIFFNDVI, col=cld)
dev.off()

# facciamo una unsupervised classification, classificazione effettuata dal software, per capire quanti pixel hanno un NDVI positivo, quanti negativo.
soc <- unsuperClass(DIFFNDVI, nClas=3)
jpeg("grafico_US.jpg", 800, 800)
plot(soc$map, main="Unsupervised Classification con 3 classi")
dev.off()
freq(soc$map)
# Zona con DIFFNDVI positivo 1  1681744
# Zona con DIFFNDVI negativo 3   872752

s1 <- 1681744 + 872752 
perc1 <- 1681744/s1 # 0.6583467
perc2 <- 872752 /s1 # 0.3416533
Percent <- c(65.83,34.17)
Change <- c("Positive","Negative")
percentages <- data.frame(Change,Percent)

# Produco un istogramma con le percentuali di differenza dell'NDVI nei pixel dove è avvenuto un cambiamento forte dell'NDVI. 
# Una differenza positiva indica un miglior NDVI nel 2013, mentre negativa il contrario 
jpeg("grafico_ISTO.jpg", 800, 800)
ggplot(percentages,aes(x=Change,y=Percent)) + geom_bar(stat="identity",fill="dark green") + 
  geom_text(aes(label = Percent),position=position_dodge(width=0.7), vjust=-0.25, size = 6)
dev.off()



# STEP 2
# Impronta spettrale del Nilo, Nilo Bianco e Nilo Azzurro

setwd("C:/lab/Nilo/Step2")

step2 <- list.files(pattern="B")

# applico ad ogni immagine della lista la funzione "raster"
importstep2 <- lapply(step2,raster)

# unisco in un unico file
TGrstep2 <- stack(importstep2)


# Rispetto ai vari layer nell'immagine, la banda 1 è nel terzo layer, la 2 nel quarto e così via.
# Raggruppo le immagini. Uso plotRGB per plottarle.
# Avendo caricato solo i file con all'inetrno B, ogni numero corrispnde alla banda con quel numero. (es 5 = B5)
jpeg("grafico_BiancoAzzurro.jpg", 1600, 800)
plotRGB(TGrstep2,r=5,g=4,b=3,stretch="lin")
dev.off()

# Faccio lo zoom sulle zone che voglio analizzare 
# zm()

# Clicco sul Nilo Bianco, sul Nilo Azzurro e sul Nilo
click(TGrstep2, id=T, xy=T, cell=T, type="p", pch=16, col="yellow")

# RISULTATI
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

# NILO

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
spectrals <- data.frame(Bande, Nilo, NiloBianco, NiloAzzurro)

# plotto le impronte spettrali dei tre fiumi
jpeg("grafico_Riflettanza.jpg", 1000, 600)
ggplot() + 
 geom_line(data=spectrals,aes(x=Bande,y=Nilo,color="Nilo"),size=1) +
 geom_line(data=spectrals,aes(x=Bande,y=NiloBianco,color="Nilo Bianco"),size=1) + 
 geom_line(data=spectrals,aes(x=Bande,y=NiloAzzurro,color="Nilo Azzurro"),size=1) + 
 labs(title="Flusso riflesso dei tre fiumi",x="Bande", y="Flusso luminoso riflesso") +
 scale_color_manual(name = "Fiume", values = c("Nilo" = "black", "Nilo Bianco" = "red", "Nilo Azzurro" = "blue"))
dev.off()


# STEP 3 
# Lago Vittoria, sorgenti del Nilo. Differenza tra firma spettrale della parte centrale del lago e l'Homa Bay. Differenza temporale, 2013 e 2021.

setwd("C:/lab/Nilo/Step3")
# Carico le immagini
step32021 <- list.files(pattern="20210622")
step32013 <- list.files(pattern="20200912")
# Applico la funzione raster a tutte le immagini
importstep32021 <- lapply(step32021,raster)
importstep32013 <- lapply(step32013,raster)
# unisocin un unico file
TGrstep32021 <- stack(importstep32021)
TGrstep32013 <- stack(importstep32013)
# Plotto e salvo le immagini in RGB
jpeg("grafico_LagoVittoria.jpg", 1000, 600)
par(mfrow=c(1,2))
plotRGB(TGrstep32013,r=7,g=6,b=5,stretch="lin")
plotRGB(TGrstep32021,r=7,g=6,b=5,stretch="lin")
dev.off()
# Analizzo la firma spettrale di due parti del lago in due periodi. Apro l'immagine e scelgo i pixel
plotRGB(TGrstep32013,r=7,g=6,b=5,stretch="lin")
click(TGrstep32013, id=T, xy=T, cell=T, type="p", pch=16, col="yellow")

# Centro del lago
# x      y     cell LC08_L2SP_170060_20130419_20200912_02_T1_QA_PIXEL
#1 549720 -60120 42336189                                             21952
#  LC08_L2SP_170060_20130419_20200912_02_T1_QA_RADSAT
#1                                                  0
#  LC08_L2SP_170060_20130419_20200912_02_T1_SR_B1
#1                                           6983
#  LC08_L2SP_170060_20130419_20200912_02_T1_SR_B2
#1                                           7256
#  LC08_L2SP_170060_20130419_20200912_02_T1_SR_B3
#1                                           7639
#  LC08_L2SP_170060_20130419_20200912_02_T1_SR_B4
#1                                           7269
#  LC08_L2SP_170060_20130419_20200912_02_T1_SR_B5
#1                                           7306
#  LC08_L2SP_170060_20130419_20200912_02_T1_SR_B6
#1                                           7743
#  LC08_L2SP_170060_20130419_20200912_02_T1_SR_B7
#1                                           7768
#  LC08_L2SP_170060_20130419_20200912_02_T1_SR_QA_AEROSOL
#1                                                    192
#  LC08_L2SP_170060_20130419_20200912_02_T1_ST_ATRAN
#1                                              5192
#  LC08_L2SP_170060_20130419_20200912_02_T1_ST_B10
#1                                           42625
#  LC08_L2SP_170060_20130419_20200912_02_T1_ST_CDIST
#1                                              5113
#  LC08_L2SP_170060_20130419_20200912_02_T1_ST_DRAD
#1                                             1603
#  LC08_L2SP_170060_20130419_20200912_02_T1_ST_EMIS
#1                                             9880
#  LC08_L2SP_170060_20130419_20200912_02_T1_ST_EMSD
#1                                                0
#  LC08_L2SP_170060_20130419_20200912_02_T1_ST_QA
#1                                            241
#  LC08_L2SP_170060_20130419_20200912_02_T1_ST_TRAD
#1                                             8114
#  LC08_L2SP_170060_20130419_20200912_02_T1_ST_URAD
#1                                             3552

# Homa Bay

#   x      y     cell LC08_L2SP_170060_20130419_20200912_02_T1_QA_PIXEL
#1 654420 -43080 38079111                                             21952
#  LC08_L2SP_170060_20130419_20200912_02_T1_QA_RADSAT
#1                                                  0
#  LC08_L2SP_170060_20130419_20200912_02_T1_SR_B1
#1                                           9454
#  LC08_L2SP_170060_20130419_20200912_02_T1_SR_B2
#1                                           9829
#  LC08_L2SP_170060_20130419_20200912_02_T1_SR_B3
#1                                          10913
#  LC08_L2SP_170060_20130419_20200912_02_T1_SR_B4
#1                                          10678
#  LC08_L2SP_170060_20130419_20200912_02_T1_SR_B5
#1                                           8380
#  LC08_L2SP_170060_20130419_20200912_02_T1_SR_B6
#1                                           7807
#  LC08_L2SP_170060_20130419_20200912_02_T1_SR_B7
#1                                           7720
#  LC08_L2SP_170060_20130419_20200912_02_T1_SR_QA_AEROSOL
#1                                                     96
#  LC08_L2SP_170060_20130419_20200912_02_T1_ST_ATRAN
#1                                              5362
#  LC08_L2SP_170060_20130419_20200912_02_T1_ST_B10
#1                                           43708
#  LC08_L2SP_170060_20130419_20200912_02_T1_ST_CDIST
#1                                              1534
#  LC08_L2SP_170060_20130419_20200912_02_T1_ST_DRAD
#1                                             1573
#  LC08_L2SP_170060_20130419_20200912_02_T1_ST_EMIS
#1                                             9880
#  LC08_L2SP_170060_20130419_20200912_02_T1_ST_EMSD
#1                                                0
#  LC08_L2SP_170060_20130419_20200912_02_T1_ST_QA
#1                                            268
#  LC08_L2SP_170060_20130419_20200912_02_T1_ST_TRAD
#1                                             8463
#  LC08_L2SP_170060_20130419_20200912_02_T1_ST_URAD
#1                                             3480


# Faccio la stessa cosa per il 2021
plotRGB(TGrstep32021,r=7,g=6,b=5,stretch="lin")
click(TGrstep32021, id=T, xy=T, cell=T, type="p", pch=16, col="yellow")

#Centro lago

#  x      y     cell LC08_L2SP_170060_20210612_20210622_02_T1_QA_PIXEL
#1 549870 -52950 42700065                                             21952
#  LC08_L2SP_170060_20210612_20210622_02_T1_QA_RADSAT
#1                                                  0
#  LC08_L2SP_170060_20210612_20210622_02_T1_SR_B1
#1                                           7263
#  LC08_L2SP_170060_20210612_20210622_02_T1_SR_B2
#1                                           7493
#  LC08_L2SP_170060_20210612_20210622_02_T1_SR_B3
#1                                           7952
#  LC08_L2SP_170060_20210612_20210622_02_T1_SR_B4
#1                                           7412
#  LC08_L2SP_170060_20210612_20210622_02_T1_SR_B5
#1                                           7327
#  LC08_L2SP_170060_20210612_20210622_02_T1_SR_B6
#1                                           7683
#  LC08_L2SP_170060_20210612_20210622_02_T1_SR_B7
#1                                           7670
#  LC08_L2SP_170060_20210612_20210622_02_T1_SR_QA_AEROSOL
#1                                                    228
#  LC08_L2SP_170060_20210612_20210622_02_T1_ST_ATRAN
#1                                              5551
#  LC08_L2SP_170060_20210612_20210622_02_T1_ST_B10
#1                                           43876
#  LC08_L2SP_170060_20210612_20210622_02_T1_ST_CDIST
#1                                               347
#  LC08_L2SP_170060_20210612_20210622_02_T1_ST_DRAD
#1                                             1501
#  LC08_L2SP_170060_20210612_20210622_02_T1_ST_EMIS
#1                                             9880
#  LC08_L2SP_170060_20210612_20210622_02_T1_ST_EMSD
#1                                                0
#  LC08_L2SP_170060_20210612_20210622_02_T1_ST_QA
#1                                            276
#  LC08_L2SP_170060_20210612_20210622_02_T1_ST_TRAD
#1                                             8529
#  LC08_L2SP_170060_20210612_20210622_02_T1_ST_URAD
#1                                             3326

# HOMA BAY

# x      y     cell LC08_L2SP_170060_20210612_20210622_02_T1_QA_PIXEL
#1 655170 -45600 40843780                                             21952
#  LC08_L2SP_170060_20210612_20210622_02_T1_QA_RADSAT
#1                                                  0
#  LC08_L2SP_170060_20210612_20210622_02_T1_SR_B1
#1                                           7274
#  LC08_L2SP_170060_20210612_20210622_02_T1_SR_B2
#1                                           7904
#  LC08_L2SP_170060_20210612_20210622_02_T1_SR_B3
#1                                           9733
#  LC08_L2SP_170060_20210612_20210622_02_T1_SR_B4
#1                                           8726
#  LC08_L2SP_170060_20210612_20210622_02_T1_SR_B5
#1                                           7595
#  LC08_L2SP_170060_20210612_20210622_02_T1_SR_B6
#1                                           7624
#  LC08_L2SP_170060_20210612_20210622_02_T1_SR_B7
#1                                           7606
#  LC08_L2SP_170060_20210612_20210622_02_T1_SR_QA_AEROSOL
#1                                                    224
#  LC08_L2SP_170060_20210612_20210622_02_T1_ST_ATRAN
#1                                              5665
#  LC08_L2SP_170060_20210612_20210622_02_T1_ST_B10
#1                                           44623
#  LC08_L2SP_170060_20210612_20210622_02_T1_ST_CDIST
#1                                              1328
#  LC08_L2SP_170060_20210612_20210622_02_T1_ST_DRAD
#1                                             1494
#  LC08_L2SP_170060_20210612_20210622_02_T1_ST_EMIS
#1                                             9880
#  LC08_L2SP_170060_20210612_20210622_02_T1_ST_EMSD
#1                                                0
#  LC08_L2SP_170060_20210612_20210622_02_T1_ST_QA
#1                                            258
#  LC08_L2SP_170060_20210612_20210622_02_T1_ST_TRAD
#1                                             8801
#  LC08_L2SP_170060_20210612_20210622_02_T1_ST_URAD
#1                                             3287


# Credo un data frame con i dati di 4 bande per poi tracciare le impronte spettrali dei due eleme nti nel tempo
HomaBay2021 <- c(7904,9733,8726,7595)
Centro2021 <- c(7489,7984,7487,7414)
HomaBay2013 <- c(9829,10913,10678,8380)
Centro2013 <- c(7256,7639,7269,7306)

Bande <- c(2,3,4,5)

spectrals <- data.frame(Bande, HomaBay2013, Centro2013, HomaBay2021, Centro2021)

# plotto le impronte spetrrali su un grafico x-y
jpeg("grafico_Rifl_Lago_Vittoria.jpg", 1000, 600)
ggplot() + 
 geom_line(data=spectrals,aes(x=Bande,y=HomaBay2013,color="Homa Bay"),size=1) +
 geom_line(data=spectrals,aes(x=Bande,y=Centro2013,color="Centro lago"),size=1) + 
 geom_line(data=spectrals,linetype=2,aes(x=Bande,y=HomaBay2021,color="Homa Bay"),size=1) +
 geom_line(data=spectrals,linetype=2,aes(x=Bande,y=Centro2021,color="Centro lago"),size=1) +
 labs(title="Flusso riflesso Lago Vittoria",x="Bande", y="Flusso luminoso riflesso") +
 scale_color_manual(name = "Fiume", values = c("Centro lago" = "black", "Homa Bay" = "blue"))
dev.off()

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------







