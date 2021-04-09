# Time series analysis
# Grerenland increase of temperature
# Data and code from Emanuela Cosma

# install.packages("raster")
library(raster) # richiamo il pacchetto installato 
setwd("C:/lab/Groenlandia") # imposto come working directory la cartella "Groenlandia" in "lab" ed in C

# carico un unico file usando la function "raster"
lst_2000 <- raster("lst_2000.tif")
plot(lst_2000) # plotto l'immagine
lst_2005 <- raster("lst_2005.tif")
plot(lst_2005)
lst_2010 <- raster("lst_2010.tif")
plot(lst_2010)
lst_2015 <- raster("lst_2015.tif")
plot(lst_2015)ù
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






