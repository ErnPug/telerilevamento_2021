# R_code_no2.r
library(raster)

# 1. setto la working directory in EN

setwd("C:/lab/EN/")

# 2. riporto la prima immagine (singola banda)

EN1 <- raster("EN_0001.png")

# 3. plotto la prima immagine con una color ramp palette

color <- colorRampPalette(c("blue","light blue","green","yellow","red"))(100)
plot(EN1, col=color)

