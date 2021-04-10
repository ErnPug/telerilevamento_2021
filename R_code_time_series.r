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
# raggruppo tutto in un'unica immagine 
par(mfrow=c(2,2))
plot(lst_2000)
plot(lst_2005)
plot(lst_2010)
plot(lst_2015)

# invece di caricare un'immagine alla volta, carico direttamente tutte le immagini.
# creo una lista con la funzione "list.files"
rlist <- list.files(pattern="lst") # prendo tutti i file con "lst" nel nome
import <- lapply(rlist,raster) # applico la funzione "raster" a tutti gli elementi della llista "rlist". 
# impacchetto tutto in un unico file
TGr <- stack(import)
plot(TGr)
# utilizzando plotRGB ed usando 3 layers diversi, quindi per diversi anni, posso notare in che anno lst è maggiorne nei diversi punti, a seconda del colore che uscirà fuori. 
# Rosso=più alto nel 2000, verde=più alto nel 2005, blu=più alto nel 2010
plotRGB(TGr,1,2,3,stretch="Lin")
# usiamo adesso anche il 2015, elimenando il 2000
plotRGB(TGr,2,3,4,stretch="Lin")


# uso la funzione "levelplot", che usa un'unica leggenda per tutte le immagini
levelplot(TGr)
# uso levellot per un silngolo strato
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





