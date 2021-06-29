# R_file_project.r
# NON ANCORA COMPLETATO

library(raster)
library(ncdf4)
setwd("C:/lab/Nilo/")

rlist2020 <- list.files(pattern="20201025")
rlist2013 <- list.files(pattern="20200912")

import2020 <- lapply(rlist2020,raster)
import2013 <- lapply(rlist2013,raster)

TGr2020 <- stack(import2020)
TGr2013 <- stack(import2013)

plot(TGr2013)
plot(TGr2020)


nilo2013 <- brick("20060512missr.jpeg")
nilo2020 <- brick("20110510missr.jpeg")




par(mfrow=c(2,1))
plotRGB(miss2006,r=1,g=2,b=3,stretch="Lin")
plotRGB(miss2011,r=1,g=2,b=3,stretch="Lin")

prova <- brick("c_gls_WB100_202105010000_GLOBE_S2_V1.0.1.nc")
