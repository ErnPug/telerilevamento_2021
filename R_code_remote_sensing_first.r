# My first code in R for remote sensing!!!

# install.packages("raster")
library(raster) # richiamo il pacchetto installato 
setwd("C:/lab/") # imposto come working directory la cartella "lab" in C

p224r63_2011 <- brick("p224r63_2011_masked.grd") # birck importa tutte le bande dell'immagine
p224r63_2011

plot(p224r63_2011)

# Day 2
# cambio i colori dei plot
color <- colorRampPalette(c("black","grey","light grey"))(100) 

plot(p224r63_2011,col=color)

color <- colorRampPalette(c("blue","light blue","green","yellow","red"))(100) # cambio i colori

plot(p224r63_2011,col=color)

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

# plotto solo la banda 1. $lega la banda all'immagine
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
plot(p224r63_2011$B3_sre,)
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
 

