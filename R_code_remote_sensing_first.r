# My first code in R for remote sensing!!!

# install.packages("raster")
library(raster) # richiamo il pacchetto installato 
setwd("C:/lab/") # imposto come working directory la cartella "lab" in C

p224r63_2011 <- brick("p224r63_2011_masked.grd") # birck importa tutte le bande dell'immagine
p224r63_2011

plot(p224r63_2011)

# cambio i colori dei plot
color <- colorRampPalette(c("black","grey","light grey"))(100)

plot(p224r63_2011,col=color)

color <- colorRampPalette(c("blue","light blu","green","yellow","red"))(100)

