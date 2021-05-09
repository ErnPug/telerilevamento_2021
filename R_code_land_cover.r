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
percentages
# li plottiamo con ggplot
ggplot(percentages,aes(x=cover,y=percent_1992,color=cover)) + geom_bar(stat="identity",fill="red")
ggplot(percentages,aes(x=cover,y=percent_2006,color=cover)) + geom_bar(stat="identity",fill="orange")
# li metto in un'unica immagine
plot1 <- ggplot(percentages,aes(x=cover,y=percent_1992,color=cover)) + geom_bar(stat="identity",fill="red")
plot2 <- ggplot(percentages,aes(x=cover,y=percent_2006,color=cover)) + geom_bar(stat="identity",fill="orange")
grid.arrange(plot1,plot2,nrow=1)

