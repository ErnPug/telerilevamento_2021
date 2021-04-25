# R_code_classification.r
library(raster)
library(RStoolbox)
setwd("C:/lab/") # sistemo la working directiry

so <- brick("Solar_Orbiter_s_first_views_of_the_Sun_pillars.jpg")
so
# visualizzo l'immagine
plotRGB(so,1,2,3,stretch="lin")

# unsupervised classification
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
