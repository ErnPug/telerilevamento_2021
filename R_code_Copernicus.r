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
