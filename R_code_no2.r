# R_code_no2.r
library(raster)

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


