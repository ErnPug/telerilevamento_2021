# R_file_project.r

library(raster)
library(ncdf4)
setwd("C:/lab/Progetto/")

miss2006 <- brick("20060512missr.jpeg")
miss2011 <- brick("20110510missr.jpeg")
par(mfrow=c(2,1))
plotRGB(miss2006,r=1,g=2,b=3,stretch="Lin")
plotRGB(miss2011,r=1,g=2,b=3,stretch="Lin")

prova <- brick("c_gls_WB100_202105010000_GLOBE_S2_V1.0.1.nc")
